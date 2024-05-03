{******************************************************************************}
{* Unit: Objekt für DSfG-Abruf                                                *}
{* 22.01.2003  WW                                                             *}
{* 12.07.2010  GD  Neuen Erweiterungsgrad 2 als Erweiterungsgrad 1 behandeln  *}
(* 19.04.2013 GD/WW  Simulationsmodus					                                *)
{* 02.05.2013  WW  Verbindung halten       		                                *}
{* 07.04.2014  WW  Base64-kodierte Rohdaten in XML-Response E, M, B; Signatur-*}
{*                 Lizenz                                                     *}
{* 06.10.2015  WW  Beschleunigte Rohdateizugriffe mit TFileOfCharStream       *}
{* 31.01.2017  WW  DSfG-DFÜ-Konfigurationsdaten mit Erweiterungsgrad der      *}
{*                 Login-DSfG-DFÜ                                             *}
{* 13.03.2018  WW  mit freiem Text für erweiterte Ergebnis-Rückgabe (Gas-X);  *}
{*                 Fehler beim Transparentschalten der DSfG-DFÜ unter neuer   *}
{*                 Fehlergruppe EST_DSFGTRANSPARENTERROR zurückgeben          *}
{* 11.07.2019  WW  DSfG-Verbindung optional nicht beenden, wenn nach dem      *}
{*                 Transparent-Schalten keine Teilnehmer erkannt werden       *}
{* 24.02.2020  WW  mit COM-Tracelog für Archivdaten; Zeitsynchronisation mit  *}
{*                 optionalem UTC-Normalzeit-Offset des Geräts                *}
{* 06.08.2021  WW  DSfG-Ressourcedaten aus Liste statt Datei lesen            *}
{* 03.01.2022  WW  Überarbeitung COM-Tracelog                                 *}
{* 22.06.2022  WW  Anpassung für Standardabfragen mit Archiveigenschaft       *}
{******************************************************************************}
Unit AbrufObjDSfG;

INTERFACE

uses
{$IFDEF SIMU}
  DSG_Utils, O_DSfGSimu,
{$ENDIF}
  Windows, Forms, Classes, SysUtils, SerDSfG, ErrConst, T_Zeit, T_Tools, WSysCon,
  WStrUtils, GD_Utils, DSfGUtil, AbrufConst, UnixDT, DDELList, DD_Allg, WChars,
  AbrufCmd, FupModemInit, AbrufObj, DAbrufList, DListen, DALKonv, WComm,
  DAufmTelegr, DValidAnswer, DDfueParaList, DDfuParaNr, TCPIP_DSfG,
  LogFile, ErrPrc32, O_Comm, GPRS_Util, O_Signatur, T_FirmwareBin, WicomSrvUtil,
  SrvCfgIni, AbrufSrvIniFile, WStream, REClntThr, EncdDecd, O_ResFilesList;

Type
  { Objekt zum DSfG-Datenabruf }

  TDSfGAbruf = class (TAbruf)
  private
    DSfGTimeouts: TDSfGTimeouts;
    HerstellerChar: char;
    Extensionmode_Dfue: byte;  { der von der DSfG-DFÜ bevorzugte Erweiterungsgrad }
    TID: integer;
    Dfue_ist_transparent_geschaltet: boolean;
    Dfue_Kommandos: string;         { von der DSfG-DFÜ unterstützte Kommandos }
    FSignaturClient: TSignaturClient;
    EAdr_VerbHalten: string;  { Busadresse für "Verbindung halten"-Befehl }
    FDSfGQuittungText: string;
{$IFDEF SIMU}  // Nur für Simulationsmodus verwendet
    FSimuFileArch : TSimuFile;
    FSimuFileLogb : TSimuFile;
    FSimuDSfGKommunikationZeiten: TSimuDSfGKommunikationZeiten;
    FSimuLinienKeineVerbindung: TStringList;
    function GetDatenelementWert_Simulation (sDea: string): string;
{$ENDIF}
    procedure SetDSfGQuittungText (sQuittung: string);
    procedure InitDSfGDfueKonfigData (var ADSfGDfueKonfigData: TDSfGDfueKonfigData);
    procedure IncrementTID;
    function Get_ArchivLogbuchBefehl(EAdr: char; DEL: string; Modus: byte; von, bis: TDateTime): string;
    function Get_DatenelementeBefehl(EAdr: char; Del_von, Del_bis: string): string;
    function Get_DatenelementEinstellBefehl(EAdr: char; Del: string; NeuerWert: string;
                                            Zugangscode1, Zugangscode2: string): string;
    function Get_DfueEinstellbefehl (Befehlscode: string; ParaAdr: string; NeuerWert: string): string;
    function Get_ZeitSynchAufmerksamkeitsTelegramm (EAdr: char; DatumZeit: TDateTime): string;
    function GetWieserDfueKommando_C (DfueParaNr: integer; NeuerWert: string): string;
    function F_IsWieserDfueBefehl (Befehl: string): boolean;
    function CheckRohfile_Antworttelegramm (Filename: TFileName; Receiver: char;
                                            DEL_ArLb: string;
                                            Anfragemodus: byte;
                                            isDE_VonBis: boolean;
                                            var alles_da: boolean;
                                            var NextDE_Adr: string;
                                            var NextOrdNr_Zeit: TDateTime;
                                            var sRohsatz_ungueltig_Merk: string): integer;
    function DSfG_Datenelement_geaendert (Receiver: char; DELAdr: string; WertNeuSoll: string;
                                          Rohstring: string; var WertNeuIst: string): boolean;
    function KennungAbfragen: boolean;
    function PasswortLogin (Passwort: string; TransparentModus: TDSfG_TransparentModus;
                            var dtLogin: TDateTime): boolean;
    function GetWieserAktiveBusteilnehmer (TransparentModus: TDSfG_TransparentModus): boolean;
    function GetDfueVersionsdaten (ResponseLogList: TResponseLogList): boolean;
    function GetWieserDfueAdressen (ResponseLogList: TResponseLogList): boolean;
    function GetWieserDfueParameterEinstellbar (var sRohAntwort: string): boolean;
    function DfueTransparentSchalten (AufmTelegrammList: TAufmTelegrammList): boolean;
    function GetDfueKommandos: boolean;
    procedure AddKonfiguration_LoginDfue_Typ_D (DELList: TDELList);
    function AbrufKonfiguration_Allgemein (EAdr: char; DELList: TDELList): boolean;
    function AbrufKonfiguration_ZeitAbruf(  // 17.08.2010
      cEAdr: char; var sUnixTime, sTimeBias: string;
      EST_Fehlergruppe: integer): boolean;
    procedure ZeitSynch_WieserDfue (ZeitSyncCmdData: TDSfG_ZeitSyncCmdData;
                                    var ZeitSyncInfoData: TZeitSyncInfoData;
                                    var ZS_Fehlergruppe: integer;
                                    var ZS_Fehlercode: integer);
    function RufEntgegennehmen_Modem_IP (var dtVerbSteht: TDateTime): boolean;
    procedure RufDeaktivierung (var RDeakt_RufNrZentrale_Alt: string;
                                var RDeakt_Fehlergruppe: integer;
                                var RDeakt_Fehlercode: integer);
  protected
    CommObj: TDSfGCommObj;
    DSfGDfueKonfigData: TDSfGDfueKonfigData;
    Procedure FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode: integer;
                                      bInit: boolean = false); override;
    function GetRetFreierText: string; override;
    function GetWieserDfueFabrikNr_Baujahr (bFwUpdate: boolean;
      ResponseLogList: TResponseLogList): boolean;
    function GetWieserDfueGeraeteInfo: boolean;
    function SaveWieserDfueParameterEinstellbar (sFilename: string): boolean;
    function WieserDfueFirmwareUpdate (sBinFilename, sFilename_ParaSave: string;
      var FwUpdateInfoData: TFwUpdateInfoData): boolean;
  public
    Constructor Create (ACOMNr: integer; ACOMNr_Kommando: integer;
                        ACommObject: TDSfGCommObj;
                        ADSfGTimeouts: TDSfGTimeouts; AModemname: string;
                        ADebugCOMProtokoll: boolean; ARohdatenLoeschen: boolean;
                        AKonfigPath, AWorkPath, ANetProgPath, ALogPath,
                        AFirmwareBinFilePath: string;
                        ASignatur_freigeschaltet: boolean;
                        AXMLResponseEncodeRohdaten: integer;
                        AResourceFilesList: TResourceFilesList;
                        ACBFwUpdStatus: TCBAbrufIntegerProc);
    Destructor Destroy; override;
    function VerbAufbau (VerbAufbauCmdData: TVerbAufbauCmdData;
                         AufmTelegrammList: TAufmTelegrammList;
                         ResponseLogList: TResponseLogList;
                         var DSfGDfueKonfigDataRet: TDSfGDfueKonfigData;
                         var StationsKennungRet: string;
                         var ZeitSyncInfoData: TZeitSyncInfoData;
                         var ZS_Fehlergruppe: integer;
                         var ZS_Fehlercode: integer;
                         var FwUpdateInfoData: TFwUpdateInfoData;
                         var VerbInfoData: TVerbInfoData): boolean;
    function VerbAbbau: boolean;
    Function AbrufDaten (Datentyp: integer;
                         vonDZ, bisDZ: TDateTime; vonOrdNr, bisOrdNr: integer;
                         DSfG_AdresslistData: TDSfG_AdresslistData;
                         ArLbDatenListe: TDSfGDataList; DELList: TDELList;
                         ResponseLogList: TResponseLogList;
                         ResponseRohdatenList: TDSfGDataList;
                         ResponseTraceLogList: TDSfGDataList;
                         Timeout: integer): boolean;
    function AbrufKonfiguration (DELList: TDELList): boolean;
    function UebertragungDatenelement (ParaEinstellCmdData: TParaEinstellCmdData;
                                       var ParaEinstellResultData: TParaEinstellResultData): boolean;
    function AbrufDfueMomentanwerte (DSfG_AdresslistData: TDSfG_AdresslistData;
                                     DSfGDfueParaList: TDfueParaList;
                                     var EAdr_Dfue: string): boolean;
    function UebertragungDfueParameter (ParaEinstellCmdData: TParaEinstellCmdData;
                                        var ParaEinstellResultData: TParaEinstellResultData;
                                        var EAdr_Dfue: string): boolean;
    function AbrufTransparent (TransparentCmdData: TTransparentCmdData;
                               var TransparentAntw: string): boolean;
    function RufEntgegennahme (var StationsKennungRet: string;
                               var VerbInfoData: TVerbInfoData): boolean;
    function Rufannahme (RufannahmeCmdData: TRufannahmeCmdData;
                         AufmTelegrammList: TAufmTelegrammList;
                         var RDeakt_RufNrZentrale_Alt: string;
                         var RDeakt_Fehlergruppe: integer;
                         var RDeakt_Fehlercode: integer;
                         var EAdr_Dfue: string;
                         var VerbInfoData: TVerbInfoData): boolean;
    function ZeitAbruf (cEADR: char; var sUnixTime, sTimeBias: string): boolean;
    function RestoreWieserDfueParameterEinstellbar (
      sFilenameSave, sFilenameRestoreResult: string;
      pStatusTextProc: TCBAbrufStatusText; pErrorTextProc: TCBAbrufErrorText): boolean;
    function VerbHalten: boolean;
    procedure ZeitSynchronisation (ZeitSyncCmdData: TZeitSyncCmdData;
                                   var ZeitSyncInfoData: TZeitSyncInfoData);
    procedure CreateCOMTraceLog; override;
    procedure FreeCOMTraceLog; override;
  end;

IMPLEMENTATION

uses DateUtils;

const
  CMaxVersuche = 3;  // max. Versuche für Senden des gleichen Abfragebefehls


{ TDSfGAbruf }

{-------------------------------------------------------------------------------------}
Constructor TDSfGAbruf.Create (ACOMNr: integer; ACOMNr_Kommando: integer;
                               ACommObject: TDSfGCommObj;
                               ADSfGTimeouts: TDSfGTimeouts; AModemname: string;
                               ADebugCOMProtokoll: boolean; ARohdatenLoeschen: boolean;
                               AKonfigPath, AWorkPath, ANetProgPath, ALogPath,
                               AFirmwareBinFilePath: string;
                               ASignatur_freigeschaltet: boolean;
                               AXMLResponseEncodeRohdaten: integer;
                               AResourceFilesList: TResourceFilesList;
                               ACBFwUpdStatus: TCBAbrufIntegerProc);
{-------------------------------------------------------------------------------------}
var
  SI: TSrvCfg32Ini;
{$IFDEF SIMU}
  AI: TAbrufSrvIni;
  sCOM: string;
{$ENDIF}

Begin
  inherited Create (ACOMNr, ACOMNr_Kommando,
                    AModemname, ADebugCOMProtokoll, ARohdatenLoeschen,
                    AKonfigPath, AWorkPath, ANetProgPath, ALogPath,
                    AFirmwareBinFilePath, ASignatur_freigeschaltet,
                    AXMLResponseEncodeRohdaten, AResourceFilesList,
                    ACBFwUpdStatus);
  CommObj:=ACommObject;
  DSfGTimeouts:=ADSfGTimeouts;

{$IFDEF GAS-X}
  // DSfG-Quittungszeichen als Zusatzinformation für Ergebnis-Rückgabe nur für
  // Gas-X-Version
  if not Assigned (CommObj.CBQuittung) then
    CommObj.CBQuittung := SetDSfGQuittungText;  // 13.03.2018, WW
{$ENDIF}

  HerstellerChar:=NUL;
  Extensionmode_Dfue:=0;
  TID:=0;
  Dfue_ist_transparent_geschaltet:=false;  { Vorbelegung: DSfG-DFÜ ist nicht transparent geschaltet }
  Dfue_Kommandos:='';
  EAdr_VerbHalten:='';

  InitDSfGDfueKonfigData (DSfGDfueKonfigData);

  FDSfGQuittungText:='';

  { Signaturclient initialisieren, wenn:
    - in der Schnittstellenkonfiguration die Verbindung zum Signaturserver
      aktiviert ist: 15.03.2012, WW
    - Signatur-Lizenz freigeschaltet ist, 07.04.2014 WW
    - Signatur-Logdatei nur bei aktiviertem ServiceIO-Debugschalter, 30.04.2019 WW }
  FSignaturClient:=nil;
  if (length (FNetProgPath) > 0) AND (FSignatur_freigeschaltet) then begin
    SI:=TSrvCfg32Ini.Create (FNetProgPath);
    try
      if SI.SigServerVerbindung then
        FSignaturClient:=TSignaturClient.Create (FLogPath,
                                                 FLogInfo_AbrufLinie,
                                                 SI.SigServerIPAdresse,
                                                 SI.SigServerPort,
                                                 SI.DebServiceIO);  // 30.04.2019, WW
    finally
      SI.Free;
    end;
  end;

{$IFDEF SIMU}
  // 19.04.2013  // Simulationsmodus
  if (C_TestFlag) then begin
    if FCOMNr > 0 then  { COM-Port }
      sCOM:='COM' + IntToStr (FCOMNr)
    else  { IP-Abrufnummer }
      sCOM:='IP' + IntToStr (Abs(FCOMNr));

    // Datei mit Simulationsdaten für DSfG-Archiv:
    FSimuFileArch := TSimuFile.Create (FWorkPath + C_WicomSrv_DSfGSimuFileArchive,
                                       sCOM, FLogPath);
    // Datei mit Simulationsdaten für DSfG-Logbuch:
    FSimuFileLogb := TSimuFile.Create (FWorkPath + C_WicomSrv_DSfGSimuFileLogbook,
                                       sCOM, FLogPath);
    // Liste mit Nummern der Linien, für die eine nicht erfolgreiche Verbindung
    // simuliert werden soll:
    FSimuLinienKeineVerbindung:=TStringList.Create;  // 31.07.2015, WW
    { Simulierte Kommunikationszeiten aus Programm-Ini lesen: 14.01.2015, WW }
    AI:=TAbrufSrvIni.Create;
    try
      with FSimuDSfGKommunikationZeiten do begin
        Verbindungsaufbau:=AI.SimuZeitVerbindungsaufbau;
        AntwortTelegramm:=AI.SimuZeitAntwortTelegramm;
        FSimuLinienKeineVerbindung.CommaText:=AI.SimuLinienKeineVerbindung;
      end;
    finally
      AI.Free;
    end;
  end
  else begin
    FSimuFileArch := nil;
    FSimuFileLogb := nil;

    with FSimuDSfGKommunikationZeiten do begin
      Verbindungsaufbau:=0;
      AntwortTelegramm:=0;
    end;
  end;
{$ENDIF}
End;

{----------------------------}
Destructor TDSfGAbruf.Destroy;
{----------------------------}
begin
{$IFDEF SIMU}
  // 19.04.2013  // Simulationsmodus
  FreeAndNil (FSimuLinienKeineVerbindung);
  FreeAndNil (FSimuFileArch);
  FreeAndNil (FSimuFileLogb);
{$ENDIF}

  FSignaturClient.Free;
  inherited Destroy;
end;  

{-------------------------------------------------------------------------------}
Procedure TDSfGAbruf.FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode: integer;
                                             bInit: boolean = false);
{-------------------------------------------------------------------------------}
{ setzt Fehlergruppe und Fehlercode für DSfG-Abruf;
  Übergabe: AFehlergruppe
            AFehlercode
            Flag zum Initialisieren der erweiterten Ergebnis-Rückgabe }
Begin
  inherited FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode, bInit);

  if bInit then
    SetDSfGQuittungText('');  // Gemerktes letztes DSfG-Quittungszeichen löschen
End;

{-----------------------------------------------------------}
procedure TDSfGAbruf.SetDSfGQuittungText (sQuittung: string);
{-----------------------------------------------------------}
{ Merkt sich letztes DSfG-Quittungszeichen;
  Übergabe: Quittungszeichen im Klartext }
begin
  FDSfGQuittungText:=sQuittung;
end;

{-------------------------------------------}
function TDSfGAbruf.GetRetFreierText: string;
{-------------------------------------------}
{ Liefert freien Text für DSfG-Ergebnis-Rückgabe }
var
  S: string;

begin
  S:=inherited GetRetFreierText;
  
  // Letzte DSfG-Quittung anhängen
  if (S <> '') AND (FDSfGQuittungText <> '') then
    S:=S + '; ';
  S:=S + FDSfGQuittungText;

  Result:=S;
end;

{-----------------------------------------------------------------------------------------}
procedure TDSfGAbruf.InitDSfGDfueKonfigData (var ADSfGDfueKonfigData: TDSfGDfueKonfigData);
{-----------------------------------------------------------------------------------------}
{ übergebenen DSfGDfueKonfigData-Record mit Leer-Vorbelegung zurückgaben }
var
  i: integer;
begin
  with ADSfGDfueKonfigData do begin
    EAdr_Dfue:=NUL;  // Kennzeichnung, daß keine DSfG-DFÜ-Konfigdaten abgefragt wurden
    Hersteller:='';
    ProgName:='';
    Version:='';
    Extensionmode:=0;  // 31.01.2017, WW
    Wieser_Teilnehmer:='';
    for i:=Low (Wieser_Adresse) to High (Wieser_Adresse) do
      Wieser_Adresse [i]:='';
    Wieser_Fabriknummer:='';  // 02.10.2009, WW
    Wieser_Baujahr:='';  // 02.10.2009, WW

    Wieser_NG_Typ:='';  // 21.02.2012, WW
    Wieser_NG_Version:='';  // 21.02.2012, WW
    Wieser_NG_Build:='';  // 21.02.2012, WW
    Wieser_NG_Flashgroesse:='';  // 21.02.2012, WW
    Wieser_NG_SysConfig:='';  // 21.02.2012, WW
  end;
end;

{--------------------------------}
procedure TDSfGAbruf.IncrementTID;
{--------------------------------}
{ TID inkrementieren (max. 4-stellig lt. G485, Anhang A, Tabelle 2) }
begin
  inc (TID);
  if TID > 9999 then
    TID:=1;
end;

{-------------------------------------------------------------------------------------------------------------}
function TDSfGAbruf.Get_ArchivLogbuchBefehl(EAdr: char; DEL: string; Modus: byte; von, bis: TDateTime): string;
{-------------------------------------------------------------------------------------------------------------}
{ Befehl zum Auslesen eines Archivkanals bilden;
  Übergabe: EAdr = Busadresse (der Registrierinstanz)
            DEL = Datenelementadresse (der Registrierinstanz),
                  z.B. Archive: caafd = AG 1, AK 1
                  z.B. Logbuch: cbaad = Logbuch für Instanz mit Busadresse A
            von, bis: enthält je nach Modus Zeit oder Ordnungsnummer }
Const
  DID = '255';
  BLO = '1';
  BNR = '1';
  NTY = 'A';
  DFO = 'J';
  ZAE = '2';

var
  Daten: string;
  DEB: char;
  Unix_vonStr, Unix_bisStr: string;
  OrdNr_vonStr, OrdNr_bisStr: string;

begin
  { Vorbelegungen: }
  Unix_vonStr:='';
  Unix_bisStr:='';
  OrdNr_vonStr:='';
  OrdNr_bisStr:='';

  if Modus = d_OrdNr then begin
    DEB:='O';
    { von-Ordnungsnummer, bis-Ordnungsnummer in String wandeln: }
    OrdNr_vonStr:=US + IntToStr (Trunc (von));
    OrdNr_bisStr:=US + IntToStr (Trunc (bis));
  end
  else begin
    DEB:='Z';
    { von-Zeit, bis-Zeit in Unix-Format wandeln: }
    DateTimeToUnixTimeStr (von, Unix_vonStr);
    DateTimeToUnixTimeStr (bis, Unix_bisStr);

    // Abruf über Zeit beim MRG 910 mit Status im Bis-Teil des Kommando wegen
    // Fehler im MRG (falsche Ordnungsnummern-Bildung in den gelieferten Daten !)
    // WW, 22.03.2004
    // sicherheitshalber auch bei MRG 905; 16.03.2006 WW
    if (HerstellerChar = 'W') AND
       ((DSfGDfueKonfigData.ProgName = 'MRG910') OR (DSfGDfueKonfigData.ProgName = 'MRG905')) then
      Unix_bisStr:=Unix_bisStr + US + US + '1';
  end;

  Daten:=DEL+US+US+Unix_vonStr+OrdNr_vonStr+GS+DEL+US+US+Unix_bisStr+OrdNr_bisStr;
  Result:=EAdr+US+DID+US+IntToStr(TID)+US+BLO+US+BNR+US+
          DSfGDfueKonfigData.EAdr_Dfue+
          US+NTY+US+DFO+US+DEB+US+ZAE+US+Daten+FS;
end;

{----------------------------------------------------------------------------------------}
function TDSfGAbruf.Get_DatenelementeBefehl(EAdr: char; Del_von, Del_bis: string): string;
{----------------------------------------------------------------------------------------}
{ Befehl zum Auslesen von Datenelementen bilden (einzelne oder Bereich);
  Übergabe: EAdr = Busadresse (der Registrierinstanz)
            Del_von = von-Datenelementadresse
                      -> für Abfrage mehrerer Einzel-Datenelemente diese mit <GS> getrennt übergeben
            Del_bis = bis-Datenelementadresse für Bereichsabfrage
                      -> für Abfrage eines oder mehrerer Einzel-Datenelemente Leer-String übergeben }
Const
  DID = '255';
  BLO = '1';
  BNR = '1';
  NTY = 'A';
  DFO = 'J';

var
  Daten: string;
  DEB: char;
  ZAE: integer;

begin
  if Del_bis <> '' then begin                    { Bereich von Datenelementen }
    DEB:='V';
    ZAE:=2;
    Daten:=Del_von+GS+Del_bis;
  end
  else begin                          { ein oder mehrere Einzel-Datenelemente }
    DEB:='M';
    ZAE:=F_TotalChars (Del_von, GS) + 1;
    Daten:=Del_von;
  end;
  Result:=EAdr+US+DID+US+IntToStr(TID)+US+BLO+US+BNR+US+
          DSfGDfueKonfigData.EAdr_Dfue+
          US+NTY+US+DFO+US+DEB+US+IntToStr(ZAE)+US+Daten+FS;
end;

{---------------------------------------------------------------------------------------------}
function TDSfGAbruf.Get_DatenelementEinstellBefehl(EAdr: char; Del: string; NeuerWert: string;
                                                   Zugangscode1, Zugangscode2: string): string;
{---------------------------------------------------------------------------------------------}
{ Befehl zum Einstellen eines Datenelements bilden (nur 1 DE je Telegramm möglich lt. A. Weber);
  Übergabe: EAdr = Busadresse
            Del = Datenelementadresse
            Neuer Wert für Datenelement
            Zugangscodes 1 und 2 }
Const
  DID = '255';
  BLO = '1';
  BNR = '1';
  NTY = 'E';
  DFO = 'J';

var
  Daten: string;
  DEB: char;
  ZAE: integer;
  ZC1, ZC2: string;

begin
  DEB:='M';
  ZAE:=3;

  { Vorbelegungen: }
  ZC1:='';
  ZC2:='';

  if Zugangscode1 <> '' then
    ZC1:=US+Zugangscode1;
  if Zugangscode2 <> '' then
    ZC2:=US+Zugangscode2;

  Daten:=CD_ALLG_Zugangscode1+ZC1+GS+CD_ALLG_Zugangscode2+ZC2+GS+Del+US+NeuerWert;
  Result:=EAdr+US+DID+US+IntToStr(TID)+US+BLO+US+BNR+US+DSfGDfueKonfigData.EAdr_Dfue+
          US+NTY+US+DFO+US+DEB+US+IntToStr(ZAE)+US+Daten+FS;
end;

{--------------------------------------------------------------------}
function TDSfGAbruf.Get_ZeitSynchAufmerksamkeitsTelegramm (EAdr: char;
  DatumZeit: TDateTime): string;
{--------------------------------------------------------------------}
{ Aufmerksamkeitstelegramm "Zeitsynchronisation" bilden;
  Übergabe: EAdr = Busadresse (@ = Rundsendung an alle Instanzen)
            Datum/Zeit, auf die synchronisiert werden soll }
Const
  DID = '12543';
  BLO = '1';
  BNR = '1';
  NTY = 'Z';
  DFO = 'N';
  DEB = 'M';
  ZAE = '0';

var
  TDA: string;
  TTI: string;

begin
  TDA:=FormatDateTime ('yymmdd', DatumZeit);
  TTI:=FormatDateTime ('hhnnss', DatumZeit);  // ohne Zeitzonen-Kennzeichen
  Result:=EAdr+US+DID+US+IntToStr(TID)+US+BLO+US+BNR+US+
          DSfGDfueKonfigData.EAdr_Dfue+
          US+NTY+US+DFO+US+DEB+US+ZAE+US+TDA+US+TTI+FS;
end;
                                                         
{-------------------------------------------------------------------------------}
function TDSfGAbruf.Get_DfueEinstellbefehl (Befehlscode: string; ParaAdr: string;
                                            NeuerWert: string): string;
{-------------------------------------------------------------------------------}
{ Befehl zum Einstellen eines Parameters oder einer NTY-Maske bilden }
var
  Befehl: string;
begin
  { Befehlscode für NTY-Maske einstellen: ParaAdr = Busadresse (DNO); Wert = NTY-Maske }
  if Befehlscode = 'A' then
    Result:=STX+Befehlscode+NeuerWert+'/'+ParaAdr+ETX

  { Befehlscodes für Einstellen aller übrigen Parameter (K-, E-, I-, R-Befehl, Wieser C-Befehl): }
  else begin
    if Befehlscode = 'YWB' then begin   { Wieser-Parameter werden mit dem YWC-Befehl geändert ! }
      Befehl:='C';
      if not (CommObj is TDSfGSerialCommObj) then  // 01.03.2012, WW
        Befehl:='YW'+Befehl;
      Result:=STX+Befehl+ParaAdr+NeuerWert+ETX;
    end else                           { übrige Parameter: Befehl ohne Parameter-Adresse }
      Result:=STX+Befehlscode+NeuerWert+ETX;
  end;
end;

{-------------------------------------------------------------------------------------------}
function TDSfGAbruf.GetWieserDfueKommando_C (DfueParaNr: integer; NeuerWert: string): string;
{-------------------------------------------------------------------------------------------}
{ Befehl zum Einstellen eines Wieser-DSfG-DFÜ-Parameters;
  Übergabe: Parameter-Nummer
            neuer Parameterwert
  Ergebnis: Befehl }
begin
  Result:=STX + 'YWC' + Format ('%.3d', [DfueParaNr]) + NeuerWert + ETX;
end;

{-----------------------------------------------------------------}
function TDSfGAbruf.F_IsWieserDfueBefehl (Befehl: string): boolean;
{-----------------------------------------------------------------}
{ Ergebnis: true, wenn übergebener Befehl ein Wieser-spezifischer DSfG-DFÜ-Befehl ist }
begin
  if CommObj is TDSfGSerialCommObj then
    Result:=true  // bei serieller Kommunikation werden nur Wieser-Geräte unterstützt; 01.03.2012, WW
  else
    Result:=Pos ('YW', Befehl) > 0;  { YW? -> hersteller-spezifischer Befehl "Wieser" }
end;

{-----------------------------------------------------------------------------------------------}
function TDSfGAbruf.CheckRohfile_Antworttelegramm (Filename: TFileName; Receiver: char;
                                                   DEL_ArLb: string;
                                                   Anfragemodus: byte;
                                                   isDE_VonBis: boolean;
                                                   var alles_da: boolean;
                                                   var NextDE_Adr: string;
                                                   var NextOrdNr_Zeit: TDateTime;
                                                   var sRohsatz_ungueltig_Merk: string): integer;
{-----------------------------------------------------------------------------------------------}
{ - Untersucht ein Rohdatenfile mit DSfG-Antworttelegramm auf Vollständigkeit
  - Überprüft die Absenderadresse
  - Liest bei unvollständiger Antwort je nach Anfragemodus die DEL-Adresse, den
    Unix-Zeitstempel oder die Ordnungsnummer des letzten Datenelements und
    ermittelt daraus das von-Element für den Folgeabruf (Rückgabe von DEL-Adresse,
    Datum/Zeit, Ordnungsnummer in NextDE_Adr bzw. NextOrdNr_Zeit) }
const
  CLogfileName_DataErr = 'WICOMSRV_DataErr';  // Filename ohne Extension

  {----------------------------------------------------------------------------------}
  procedure WriteDataErrLog_RohsatzUngueltig (aKonvErr: integer; aRohRec: TDSfGRohRec;
                                              aDebDatentyp: string; aCOMNr: integer);
  {----------------------------------------------------------------------------------}
  { DataErr-Logfile schreiben "ungültiger Rohsatz" }
  var
    S: string;
  begin
    if DebugCOMProtokoll then begin  // nur bei aktiviertem COM-Protokoll; 15.05.2007, WW
      with aRohRec do begin
        S:='DSfG-Rohdatensatz ungültig (' + aDebDatentyp + '): ' +
           GetDSfGKonvErrorText (aKonvErr) + ' (DSFGKONVERR = ' + IntToStr (aKonvErr) + ')';
        if aCOMNr > 0 then  { COM-Port }
          S:=S + ' (COM' + IntToStr (aCOMNr) + ')' + CR + LF
        else  { IP-Abrufnummer }
          S:=S + ' (IP' + IntToStr (Abs (aCOMNr)) + ')' + CR + LF;
        S:=S +
           'DE-Adresse: ' + Adresse + CR + LF +
           'Wert: ' + Wert + CR + LF +
           'Zeit: ' + UTime + CR + LF +
           'Ordnungsnummer: ' + OrdNr + CR + LF +
           'Status: ' + Status + CR + LF +
           'CRC: ' + CRC + CR + LF;
      end;
      WriteDebugLog (FLogPath, CLogfileName_DataErr, S, true, lt_Error);
    end;
  end;

  {-----------------------------------------------------------------------------------}
  procedure WriteDataErrLog_ZAE (aKonvErr: integer; aDataCount, aRohsatzCount: integer;
                                 aDebDatentyp: string; aCOMNr: integer);
  {-----------------------------------------------------------------------------------}
  { DataErr-Logfile schreiben "Abweichung ZAE/tatsächliche Anzahl Rohdatensätze" }
  var
    S: string;
  begin
    if DebugCOMProtokoll then begin  // nur bei aktiviertem COM-Protokoll; 15.05.2007, WW
      S:='DSfG-Rohdaten ungültig (' + aDebDatentyp + '): ' +
         GetDSfGKonvErrorText (aKonvErr) + ' (DSFGKONVERR = ' + IntToStr (aKonvErr) + ')';
      if aCOMNr > 0 then  { COM-Port }
        S:=S + ' (COM' + IntToStr (aCOMNr) + ')' + CR + LF
      else  { IP-Abrufnummer }
        S:=S + ' (IP' + IntToStr (Abs (aCOMNr)) + ')' + CR + LF;
      S:=S +
         'Anzahl ZAE: ' + IntToStr (aDataCount) + CR + LF +
         'Anzahl Ist: ' + IntToStr (aRohsatzCount) + CR + LF;
      WriteDebugLog (FLogPath, CLogfileName_DataErr, S, true, lt_Error);
    end;
  end;

var
  FS_Rohfile: TFileOfCharStream;  // 06.10.2015, WW
  FS_RohfileCopy: TFileStream;  // 06.10.2015, WW
  FS_Rohfile_err: TFileStream;
  i: integer;
  DataCount: integer;
  Erg: integer;
  sRohsatz: string;
  RohRec: TDSfGRohRec;
  DEBCheck: TDEBCheck;
  Filename_err: string;
  FilePos_gueltig: Int64;
  bRohsatz_ungueltig: boolean;
  sDebDatentyp: string;
  bletzt_ascii: boolean;
  RohsatzCount: integer;
  bLetztRohsatz_gelesen: boolean;
  NextDE_Adr_Merk: string;
  NextOrdNr_Zeit_Merk: TDateTime;
  dtBuf: TDateTime;
  KonvErr_Ext: integer;
  sDummy: string;
  iDummy: integer;
  bIsAbfrage_ArchivZeile: boolean;
  cMaxKanal_Zeile: char;
  cKanal: char;

begin
  alles_da:=false;  { Vorbelegung Rückgabe: Antwort unvollständig }

  if not FileExists (FileName) then begin
    Result:=DSFGKONVERR_FILENOTFOUND;  // 05.04.2007, WW
    exit;
  end;

  try
    FS_Rohfile:=TFileOfCharStream.Create (FileName, fmOpenRead OR fmShareDenyWrite);
    try
      if Anfragemodus = d_DelAdr then begin
        DEBCheck:=debc_MengeDE;
        sDebDatentyp:='Datenelemente';
      end
      else begin
        DEBCheck:=debc_OrdNr_Zeit;
        sDebDatentyp:='Archiv-/Logbuchdaten';
      end;

      // Zeilenweise abgefragte Archivgruppe ?
      bIsAbfrage_ArchivZeile:=((Anfragemodus = d_OrdNr) OR (Anfragemodus = d_VonBis)) AND
                              (Copy (DEL_ArLb, 1, 1) = 'c') AND  // nur bei Registrierinstanzen; 22.06.2022, WW
                              (length (DEL_ArLb) = 3);      

      // 'Next'-Übergaben merken für evtl. notwendige Wiederholung des gleichen Befehls:
      NextDE_Adr_Merk:=NextDE_Adr;
      NextOrdNr_Zeit_Merk:=NextOrdNr_Zeit;

      { DSfG-Header prüfen: }
      Erg:=CheckDSfGHeader (FS_Rohfile, Receiver, DEBCheck, DataCount, alles_da,
                            sDummy, iDummy);
      { außerplanmäßige Antwort: 20.07.2004, WW
        -> OK, weitere Rohdaten-Prüfung beendet
        -> Auswertung erfolgt in entsprechender Daten-Konvertierung }
      if Erg >= C_MinCode_DSfGKonvErr_AA then begin
        Result:=DSFGKONVERR_OK;
        exit;
      end;
      if Erg = DSFGKONVERR_NODATA then  { 'Keine Daten vorhanden' ist OK, kein Fehler ! }
        Erg:=DSFGKONVERR_OK;
      if Erg <> DSFGKONVERR_OK then begin  { Fehler DSfG-Header }
        Result:=Erg;
        exit;
      end;

      bRohsatz_ungueltig:=false;  { es ist kein ungültiger Rohsatz enthalten }
      FilePos_gueltig:=FS_Rohfile.Position;
      RohsatzCount:=0;  // Vorbelegung: Anzahl der tatsächlich vorhandenen Rohdatensätze
      bLetztRohsatz_gelesen:=false;
      cMaxKanal_Zeile:=#0;  // Vorbelegung: Höchster gefundener Kanal in Archivzeile
      cKanal:=#0;

      for i:=0 to DataCount-1 do begin
        Application.ProcessMessages;
        if read_ascii(FS_Rohfile, sRohsatz, bletzt_ascii) then begin { 1 Rohdatensatz lesen }
          if not bLetztRohsatz_gelesen then
            inc (RohsatzCount);  // Anzahl Rohdatensätze weiterzählen
          if bletzt_ascii then  // letzter Rohdatensatz wurde gelesen
            bLetztRohsatz_gelesen:=true;
        end;
        split_ascii(sRohsatz, RohRec);  { in Rohdaten-Struktur konvertieren }

        if sRohsatz = (US + US) then begin  // leerer Rohsatz
          { DataErr-Logfile schreiben: Leerer Rohsatz zwar nicht ungültig, aber unsinnig ! }
          WriteDataErrLog_RohsatzUngueltig (DSFGKONVERR_EMPTYREC, RohRec, sDebDatentyp, FCOMNr);
          { leerer Rohsatz wird übergangen (MRG910-Fehler <GS><GS>);
            24.05.2007, WW }
        end
        else begin
          if not bRohsatz_ungueltig then begin  // noch kein ungültiger Rohdatensatz gefunden
            { Gültigkeitsprüfung: Rohsatz syntaktisch richtig ? }
            if Anfragemodus = d_DelAdr then begin
              Erg:=CheckDSfGDatenelementRohsatz (RohRec, KonvErr_Ext);
              if KonvErr_Ext <> DSFGKONVERR_OK then begin  // Rohdatensatz ist ungültig (erweiterte Rohdatensatzprüfung für DE)
                { DataErr-Logfile schreiben: Rohsatz ungültig ! }
                WriteDataErrLog_RohsatzUngueltig (KonvErr_Ext, RohRec, sDebDatentyp, FCOMNr);
                { keine Wiederholung, ungültiger Rohsatz wird übergangen (MRG910-Fehler bei DE ca??d);
                  11.05.2007, WW }
              end;
            end else
              Erg:=CheckDSfGArchivLogbuchRohsatz (RohRec, false, false, nil);

            if Erg <> DSFGKONVERR_OK then begin  // Rohdatensatz ist ungültig
              // DSFGKONVERR-Fehlercode nicht als Funktionscode zurückgeben. Es wird
              // DSFGKONVERR_OK zurückgegeben, um ein Aufsetzen auf die gültigen
              // Rohdatensätze zu ermöglichen. Ungültig-Fehlercode wird in anschließender
              // Datenkonvertierung geschrieben.

              { DataErr-Logfile schreiben: Rohsatz ungültig ! }
              WriteDataErrLog_RohsatzUngueltig (Erg, RohRec, sDebDatentyp, FCOMNr);

              if ((length (sRohsatz_ungueltig_Merk) > 0) AND (sRohsatz <> sRohsatz_ungueltig_Merk)) OR
                 (length (sRohsatz_ungueltig_Merk) = 0) then begin
                // es ist ein anderer Ungültig-Rohsatz als der gemerkte
                // oder es existiert kein gemerkter Ungültig-Rohsatz
                bRohsatz_ungueltig:=true;  // Ungültig-Flag setzen
                sRohsatz_ungueltig_Merk:=sRohsatz;  // ungültigen Rohsatz merken
                alles_da:=false;  // nochmal abrufen
              end
              else begin  // es ist wieder der gleiche Ungültig-Rohsatz
                // Ungültigen Rohsatz hinnehmen und weiterlesen
                sRohsatz_ungueltig_Merk:='';
              end;
            end;
          end;

          if not bRohsatz_ungueltig then begin  // noch kein ungültiger Rohdatensatz gefunden
            // Bei zeilenweise ausgelesener Archivgruppe den höchsten Kanal im
            // Rohfile merken; 11.07.2016, WW
            if bIsAbfrage_ArchivZeile then begin
              if length (RohRec.Adresse) >= 4 then begin
                cKanal:=RohRec.Adresse [4];
                if (cKanal > cMaxKanal_Zeile) then
                  cMaxKanal_Zeile:=cKanal;
              end;
            end;

            // letzte gültige Fileposition aktualisieren:
            FilePos_gueltig:=FS_Rohfile.Position;

            { letzte DE-Adresse bzw. Ordnungsnummer/Zeit eines gültigen Rohsatzes
              aktualisieren; Rückgabe: nächste DE-Adresse bzw. Ordnungsnummer/Zeit }
            case Anfragemodus of
              d_OrdNr:
                begin
                  try
                    // Wenn bei zeilenweise ausgelesener Archivgruppe die letzte Zeile
                    // unvollständig ist, die letzte Zeile nochmal auslesen, damit
                    // wir sie vollständig bekommen (überlappendes Auslesen); 11.07.2016, WW
                    if bIsAbfrage_ArchivZeile AND bLetztRohsatz_gelesen AND
                      (cKanal < cMaxKanal_Zeile) then
                      NextOrdNr_Zeit:=StrToInt (RohRec.OrdNr)  // gleiche Ordnungsnummer
                    else
                      NextOrdNr_Zeit:=StrToInt (RohRec.OrdNr) + 1;  // nächste Ordnungsnummer
                  except
                    Result:=DSFGKONVERR_INVNUMBER;  // 05.04.2007, WW
                    exit;
                  end;
                end;

              d_DelAdr:
                begin
                  if length (sRohsatz) > 0 then begin  // Mehr Datensätze als in ZAE angegeben erlauben (GC9000 a-z); 23.10.2012, WW
                    if length (RohRec.Adresse) > 0 then begin
                      if isDE_VonBis then  { von-bis-Bereichsanfrage von Datenelementen }
                        NextDE_Adr:=GetNextDatenelementAdresse (RohRec.Adresse)  // nächstmögliche DE-Adresse
                      else
                        NextDE_Adr:=GetNextDatenelementeMenge (NextDE_Adr, RohRec.Adresse);  // Rest-DE-Menge
                    end
                    else begin
                      Result:=DSFGKONVERR_INVADDR;  // 05.04.2007, WW
                      exit;
                    end;
                  end;
                end;
            else  { bei Anfrage über Zeitbereich }
              try
                if UnixTimeStrToDateTime (RohRec.UTime, dtBuf) then begin
                  // Wenn bei zeilenweise ausgelesener Archivgruppe die letzte Zeile
                  // unvollständig ist, die letzte Zeile nochmal auslesen, damit
                  // wir sie vollständig bekommen (überlappendes Auslesen); 11.07.2016, WW
                  if bIsAbfrage_ArchivZeile AND bLetztRohsatz_gelesen AND
                    (cKanal < cMaxKanal_Zeile) then
                    NextOrdNr_Zeit:=dtBuf  // gleiche Zeit
                  else
                    // nächste Zeit (kleinster Zeitschritt bei Unix-Zeit ist 1 s): }
                    NextOrdNr_Zeit:=dtBuf + EncodeTime (0, 0, 1, 0)
                end
                else begin
                  Result:=DSFGKONVERR_INVTIME;  // 05.04.2007, WW
                  exit;
                end;
              except
                Result:=DSFGKONVERR_INVTIME;  // 05.04.2007, WW
                exit;
              end;
            end;
          end;
        end;  { if sRohsatz }
      end;  { for i }
      Result := DSFGKONVERR_OK;
    finally
      FS_Rohfile.Free;
    end;

    // Fehlerbehandlung:
    // 1. Vergleich Soll-Anzahl an Datensätzen aus Header mit tatsächlicher Anzahl:
    if DataCount <> RohsatzCount then begin  // weicht ab
      if bRohsatz_ungueltig then begin  // ungültiger Rohdatensatz gefunden (Fehler "ERZ2000, Vs. 1.3.1")
        Result:=DSFGKONVERR_ZAE_SOLL_IST;
        sRohsatz_ungueltig_Merk := '';  // Ungültig-Merk-Rohsatz löschen (die ganzen Rohdaten sind ungültig)
        alles_da:=false;  // nochmal abrufen
        NextDE_Adr:=NextDE_Adr_Merk;  // ...mit gleichem von-Bereich
        NextOrdNr_Zeit:=NextOrdNr_Zeit_Merk;

        { DataErr-Logfile schreiben: ZAE ungleich Anzahl Rohsätze ! }
        WriteDataErrLog_ZAE (Result, DataCount, RohsatzCount, sDebDatentyp, FCOMNr);

        // Rohfile mit ungültigen Rohdaten umbenennen (als Rohdaten-Sicherung):
        Filename_err:=Filename + '.err';
        DeleteFile (Filename_err);
        RenameFile (Filename, Filename_err);
        exit;
      end
      else begin  // kein ungültiger Rohdatensatz gefunden (Fehler "GC9000 a-z")
        { OK, nur DataErr-Logfile schreiben: ZAE ungleich Anzahl Rohsätze ! }
        WriteDataErrLog_ZAE (DSFGKONVERR_ZAE_SOLL_IST, DataCount, RohsatzCount, sDebDatentyp, FCOMNr);
      end;
    end;

    // 2. Wenn ZAE-Vergleich OK: Prüfung auf ungültigen Rohdatensatz
    if bRohsatz_ungueltig then begin  // Rohfile enthält ungültigen Datensatz
      // Rohfile mit ungültigem Datensatz umbenennen (als Rohdaten-Sicherung):
      Filename_err:=Filename + '.err';
      DeleteFile (Filename_err);
      RenameFile (Filename, Filename_err);
      // ursprügliches Rohfile bis zum letzten gültigen Datensatz wiederherstellen:
      // ZAE im Header paßt nicht mehr zur Anzahl enthaltener Datensätze ! Rohdaten-
      // Konvertierung muß dies berücksichtigen !
      FS_Rohfile_err:=TFileStream.Create (FileName_err, fmOpenRead OR fmShareDenyWrite);
      try
        FS_RohfileCopy:=TFileStream.Create (FileName, fmCreate);
        try
          FS_RohfileCopy.CopyFrom(FS_Rohfile_err, FilePos_gueltig);
        finally
          FS_RohfileCopy.Free;
        end;
      finally
        FS_Rohfile_err.Free;
      end;
    end;
  except
    Result:=DSFGKONVERR_DATACHECK_INTERN;  // 05.04.2007 WW
  end;
end;

{---------------------------------------------------------------------------------------------------}
function TDSfGAbruf.DSfG_Datenelement_geaendert (Receiver: char; DELAdr: string; WertNeuSoll: string;
                                                 Rohstring: string; var WertNeuIst: string): boolean;
{---------------------------------------------------------------------------------------------------}
{ Antwort auf Einstelltelegramm auswerten;
  Übergabe: Busadresse der Instanz, deren DE geändert werden soll
            zu ändernde Datenelement-Adresse
            im Einstelltelegramm gesendeter neuer Datenelement-Wert (Soll)
            Antwort-Rohstring
  Rückgabe: tatsächlicher Datenelement-Wert nach der Einstellung (Ist)
  Ergebnis: true, wenn Datenelement geändert wurde }
var
  Code: Integer;
  S: string;
  DID: integer;
  StringPos: integer;
  ZAE: integer;
  sbuf, zbuf: PChar;
  Len: Word;
  DELAdr_Antwort: string;
  fWertNeuSoll: double;
  fWertNeuIst: double;
  iCode: integer;
  bGeaendert: boolean;

begin
  Result:=false;
  WertNeuIst:='';                                { Vorbelegung für WertNeuIst }

  StringPos:=1;
  S := read_hdcl_fromString(Rohstring, StringPos);         { alles bis zum ersten US (keine Auswertung) }

  { HDCL auswerten: }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DID }
  val(S, DID, Code);
  if Code <> 0 then begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;
  if DID < C_DID_DataSimple then begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { TID }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { BLO }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { BNR }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DNO }
  if S <> Receiver then begin                      { Überprüfen des Absenders }
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { NTY }
  if S <> '' then begin
    if S[1] <> 'R' then begin
      FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
      exit;
    end;
  end
  else begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { DFO }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DEB }
  if S <> '' then begin
    if S[1] = 'A' then begin
      { außerplanmäßige Antwort: Zugangscode(s) falsch o. unbekanntes Datenelement
        Anm.: Kann auch bei ungültigem Format des Einstellwertes auftreten; 11.11.2020, WW }
      FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGCODENR_UNKNOWNDE);
      exit;
    end;
    if S[1] <> 'M' then begin
      FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
      exit;
    end;
  end
  else begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { ZAE }
  val(S, ZAE, Code);                       { Anzahl der Datenelemente im File }
  if Code <> 0 then begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;
  if ZAE <> 3 then begin     { Zugriffscode 1 (add), Zugriffscode 2 (ade), zu änderndes DE }
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  { Datenfeld aufsplitten: Datenelement-Adresse und Wert }
  Len := Length (Rohstring) + 1;
  getmem(sbuf,Len);
  try
    getmem(zbuf,Len);
    try
      StrPCopy(sbuf,Rohstring);
      If FilterString (zbuf, sbuf, GS, FS, nil, 1) <> Nil Then Begin  { Einstell-DE-Adresse/-Wert zwischen 2. GS und FS }
        StrCopy (sbuf, zbuf);            { Source ist jetzt der DE-Teilstring }
        If FilterString (zbuf, sbuf, nil, US, nil, 0) <> Nil Then      { DEL-Adresse }
          DELAdr_Antwort:=string(zbuf)
        else
          DELAdr_Antwort:='';
        If FilterString (zbuf, sbuf, US, nil, nil, 0) <> Nil Then      { Datenelement-Wert }
          WertNeuIst:=string(zbuf)
        else
          WertNeuIst:='';
      end
      else begin
        FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
        exit;
      end;
    finally
      freemem(zbuf,Len);
    end;
  finally
    freemem(sbuf,Len);
  end;

  { zu ändernde Datenelement-Adresse in Antwort prüfen: }
  if DELAdr_Antwort <> DelAdr then begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  { neuen Datenelement-Wert prüfen, ob er wirklich geändert wurde }
  if F_RightTrunc (WertNeuSoll, ' ') <>
     F_RightTrunc (WertNeuIst, ' ') then begin  // 1. Prüfung: Stringvergleich Soll-Wert mit Ist-Wert
                                                // abschließende Spaces ignorieren; 16.05.2014 WW     
    bGeaendert:=false;
    { 2. Prüfung: Float-Vergleich Soll-Wert mit Ist-Wert; 07.05.2012, WW }
    S:=WertNeuSoll;
    StrSubst (S, ',', '.');  { evtl. vorhandenes Dezimal-Komma durch Punkt ersetzen }
    Val (S, fWertNeuSoll, iCode);
    if iCode = 0 then begin
      S:=WertNeuIst;
      StrSubst (S, ',', '.');  { evtl. vorhandenes Dezimal-Komma durch Punkt ersetzen }
      Val (S, fWertNeuIst, iCode);
      if iCode = 0 then begin
        if not (fWertNeuSoll <> fWertNeuIst) then                                 
          bGeaendert:=true;
      end;
    end;

    if not bGeaendert then begin
      { Datenelement nicht veränderbar o. Eichschalter geschlossen }
      FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_NOCHANGEABLEDE_NOES);
      exit;
    end;
  end;
  Result:=true;
end;

{-------------------------------------------}
function TDSfGAbruf.KennungAbfragen: boolean;
{-------------------------------------------}
{ Kennung der DSfG-DFÜ abfragen;
  Ergebnis: true, wenn Kennungsabfrage erfolgreich durchgeführt wurde }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;

  Befehl:=STX+'K'+ETX;
  { Kennungsabfragekommando senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Login, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Kennungsabfrage-Kommando auswerten: }
  if R.Antwort[2] <> 'K' then begin
    FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_WRONGANSWER);
    exit;
  end;

  { Kennungsprüfung: }
  StationsKennung:=F_RightTrunc (Copy(R.Antwort,3,12), ' ');  { StationsKennung enthält jetzt die richtige Gerätekennung }
  Result:=true;                                                                                                            
end;

{---------------------------------------------------------------------------}
function TDSfGAbruf.PasswortLogin  (Passwort: string;
  TransparentModus: TDSfG_TransparentModus; var dtLogin: TDateTime): boolean;
{---------------------------------------------------------------------------}
{ Paßwort übertragen;
  Übergaben: Passwort
             TransparentModus
  Rückgabe: Zeitpunkt 'Login erfolgt'
  Ergebnis: true, wenn Passwort-Login ok }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;

  Befehl:=STX+'I'+F_RightPad (Passwort,' ',szlen_DSfGPasswort)+ETX;
  { Paßwortübertragungskommando senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Login, ad_String, R, NoCarrier) then begin
    if NoCarrier then                      { wenn die Verbindung jetzt nicht meht steht, war das Passwort falsch ! }
      FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW)
    else
      FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  { Antwort auf Paßwortübertragungskommando auswerten: }
  if R.Antwort[2] <> 'I' then begin             { Passwort falsch ! }
    FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
    exit;
  end;

  { Login erfolgreich ! }
  dtLogin:=Now;  // Zeitpunkt 'Login erfolgt'; 20.12.2013, WW

  DSfGDfueKonfigData.EAdr_Dfue:=R.Antwort[3];      { lokale EADR der DSfG-DFÜ }
  HerstellerChar:=R.Antwort[4];                    { Hersteller }
  if R.Antwort[5] <> ETX then                      { Extensionmode }
    Extensionmode_Dfue:=StrToInt (R.Antwort [5])
  else
    ExtensionMode_Dfue:=0;
  DSfGDfueKonfigData.Extensionmode:=Extensionmode_Dfue;  // 31.01.2017, WW

  if DSfGDfueKonfigData.EAdr_Dfue = '0' then begin
    { wenn DFÜ transparent geschaltet werden soll und die DFÜ-Login-Adresse 0
      ist: 11.07.2019, WW }
    if TransparentModus = dtm_Ja then begin  // Nur bei "Ja" Verbindung abbrechen
      FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_NODFUEBUSADDRESS);
      exit;
    end;
  end;
  Result:=true;
end;

{---------------------------------------------------------------------------------------------------}
function TDSfGAbruf.GetWieserAktiveBusteilnehmer (TransparentModus: TDSfG_TransparentModus): boolean;
{---------------------------------------------------------------------------------------------------}
{ Aktive Busteilnehmer auslesen (nur bei Wieser-DSfG-DFÜ !);
  Übergabe: TransparentModus
  Ergebnis: true, wenn Lesen der Teilnehmer ok }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;

  Befehl:=STX+'YWT'+ETX;
  { Aktive-Busteilnehmer-Kommando senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Login, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Aktive-Busteilnehmer-Kommando auswerten: }
  if R.Antwort[2] = 'T' then begin
    DSfGDfueKonfigData.Wieser_Teilnehmer:=Copy (R.Antwort, 3, length (R.Antwort)-3);
    if length (DSfGDfueKonfigData.Wieser_Teilnehmer) < 1 then begin     { keine aktiven Teilnehmer }
      { wenn DFÜ transparent geschaltet werden soll und keine Teilnehmer vorhanden
        sind: }
      if TransparentModus = dtm_Ja then begin  // Nur bei "Ja" Verbindung abbrechen; 11.07.2019, WW
        FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_NOACTIVEUSER);
        exit;
      end;
    end
    else if Pos ('!!' , DSfGDfueKonfigData.Wieser_Teilnehmer) > 0 then begin
      { wenn DFÜ transparent geschaltet werden soll und mehrere Leitstationen aktiv
        sind: 25.07.2012, WW }
      if TransparentModus = dtm_Ja then begin  // Nur bei "Ja" Verbindung abbrechen; 11.07.2019, WW
        FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_MEHRERELEITSTATIONEN);
        exit;
      end;
    end;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------------}
function TDSfGAbruf.GetDfueVersionsdaten (ResponseLogList: TResponseLogList): boolean;
{------------------------------------------------------------------------------------}
{ Versionsdaten der DSfG-DFÜ auslesen (optional im Erweiterungsgrad 1);
  Rückgabe: Responselog-Liste (für Name der erzeugten Rohdaten-Datei)
  Ergebnis: true, wenn Lesen der Versionsdaten ok }
var
  Befehl, S: string;
  R: TRueckgabe;
  sbuf, zbuf: PChar;
  Len : Word;
  LogListObj: TRohfileLogObj;

begin
  Result:=false;

  Befehl:=STX+'V'+ETX;
  { Versionsdaten-Kommando senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Login, ad_File, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);        // 10.10.2003, WW
    exit;
  end;

  S:=StringFromFile (R.Antwort); { Rohfile-Inhalt in String kopieren }

  if not RohdatenLoeschen then begin
    { Objekt mit Rohfilename in Log-Liste eintragen: 02.04.2007, WW }
    if ResponseLogList <> nil then begin
      LogListObj:=TRohfileLogObj.Create;
      LogListObj.SetData (DSfGDfueKonfigData.EAdr_Dfue, R.Antwort);
      ResponseLogList.Add (LogListObj);
    end;
  end else
    DeleteFile (R.Antwort);

  { Antwort auf Versionsdaten-Kommando auswerten: }
  S:=ExtractString (S, STX, ETX, 0);
  S:=ExtractString (S, 'V', NUL, 0);
  if length (S) > 0 then begin  // STX ? ETX abfangen; 14.12.2011, WW 
    S:=US + S;
    Len := Length (s) + 1;
    getmem(sbuf,Len);
    try
      getmem(zbuf,Len);
      try
        StrPCopy(sbuf,s);
        If FilterString (zbuf, sbuf, US, US, nil, 0) <> Nil Then    { Hersteller }
          DSfGDfueKonfigData.Hersteller := string(zbuf);
        If FilterString (zbuf, sbuf, US, US, nil, 1) <> Nil Then    { Programmname }
          DSfGDfueKonfigData.ProgName := string(zbuf);
        If FilterString (zbuf, sbuf, US, US, nil, 2) <> Nil Then    { Version }
          DSfGDfueKonfigData.Version := string(zbuf);
      finally
        freemem(zbuf,Len);
      end;
    finally
      freemem(sbuf,Len);
    end;
  end;

  Result:=true;
end;

{-------------------------------------------------------------------------------------}
function TDSfGAbruf.GetWieserDfueAdressen (ResponseLogList: TResponseLogList): boolean;
{-------------------------------------------------------------------------------------}
{ alle in der DSfG-DFÜ eingestellten Adressen abfragen (nur Wieser);
  Rückgabe: Responselog-Liste (für Name der erzeugten Rohdaten-Datei)
  Ergebnis: true, wenn Lesen der Adressen ok }
var
  Befehl, S: string;
  R: TRueckgabe;
  sbuf, zbuf: PChar;
  Len : Word;
  LogListObj: TRohfileLogObj;

begin
  Result:=false;
  { geändert 14.11.2001 WW: Parameter 010..014 (jetzt alle 5 Adressen; bisher nur DFÜ-Adressen 1-4) }
  Befehl:=STX+'YWB010014'+ETX;
  { Adressenabfrage-Kommando senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Login, ad_File, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);        // 10.10.2003, WW
    exit;
  end;

  S:=StringFromFile (R.Antwort); { Rohfile-Inhalt in String kopieren }

  if not RohdatenLoeschen then begin
    { Objekt mit Rohfilename in Log-Liste eintragen: 02.04.2007, WW }
    if ResponseLogList <> nil then begin
      LogListObj:=TRohfileLogObj.Create;
      LogListObj.SetData (DSfGDfueKonfigData.EAdr_Dfue, R.Antwort);
      ResponseLogList.Add (LogListObj);
    end;
  end else
    DeleteFile (R.Antwort);

  { Antwort auf Adressen-Kommando auswerten: }
  S:=US + Copy (S, 3, length (S));
  Len := Length (s) + 1;
  getmem(sbuf,Len);
  try
    getmem(zbuf,Len);
    try
      StrPCopy(sbuf,s);
      If FilterString (zbuf, sbuf, US, US, nil, 0) <> Nil Then        { Adresse 1 }
        if Copy (string(zbuf), 1, 3) = '010' then
          DSfGDfueKonfigData.Wieser_Adresse [1]:= Copy (string(zbuf), 4, 1);
      If FilterString (zbuf, sbuf, US, US, nil, 1) <> Nil Then        { Adresse 2 }
        if Copy (string(zbuf), 1, 3) = '011' then
          DSfGDfueKonfigData.Wieser_Adresse [2]:= Copy (string(zbuf), 4, 1);
      If FilterString (zbuf, sbuf, US, US, nil, 2) <> Nil Then        { Adresse 3 }
        if Copy (string(zbuf), 1, 3) = '012' then
          DSfGDfueKonfigData.Wieser_Adresse [3]:= Copy (string(zbuf), 4, 1);
      If FilterString (zbuf, sbuf, US, US, nil, 3) <> Nil Then        { Adresse 4 }
        if Copy (string(zbuf), 1, 3) = '013' then
          DSfGDfueKonfigData.Wieser_Adresse [4]:= Copy (string(zbuf), 4, 1);
      If FilterString (zbuf, sbuf, US, US, nil, 4) <> Nil Then        { Adresse 5 }
        if Copy (string(zbuf), 1, 3) = '014' then
          DSfGDfueKonfigData.Wieser_Adresse [5]:= Copy (string(zbuf), 4, 1);
    finally
      freemem(zbuf,Len);
    end;
  finally
    freemem(sbuf,Len);
  end;
  Result:=true;
end;

{--------------------------------------------------------------------}
function TDSfGAbruf.GetWieserDfueFabrikNr_Baujahr (bFwUpdate: boolean;
  ResponseLogList: TResponseLogList): boolean;
{--------------------------------------------------------------------}
{ Fabriknummer und Baujahr der DSfG-DFÜ abfragen (nur Wieser);
  Übergabe: Flag 'FwUpdate' (true setzen, wenn Fabriknummer/Bauhjahr im Rahmen
                             eines Firmware-Updates für RMG DFU-NG gelesen werden)
  Rückgabe: Responselog-Liste (für Name der erzeugten Rohdaten-Datei)
  Ergebnis: true, wenn Lesen von Fabriknummer und Baujahr ok }
var
  Befehl, S: string;
  R: TRueckgabe;
  sBuf: string;
  LogListObj: TRohfileLogObj;

begin
  Result:=false;

  // Bei NG-Firmware-Update die NG-spezifischen Parameternummern > 300 verwenden,
  // da DFY-kompatible Parameter < 200 in frühen NG-Versionen nicht vollständig
  // vorhanden sind (Parameter 103 "Fabriknummer" fehlt in NG-Version 00.01);
  // 25.11.2014, WW
  if bFwUpdate then
    Befehl:='B322323'
  else
    Befehl:='B103104';

  if not (CommObj is TDSfGSerialCommObj) then
    Befehl:='YW'+Befehl;
  Befehl:=STX+Befehl+ETX;

  { Kommando für Abfrage von Fabriknummer und Baujahr senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Login, ad_File, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  S:=StringFromFile (R.Antwort); { Rohfile-Inhalt in String kopieren }

  if not RohdatenLoeschen then begin
    { Objekt mit Rohfilename in Log-Liste eintragen: }
    if ResponseLogList <> nil then begin
      LogListObj:=TRohfileLogObj.Create;
      LogListObj.SetData (DSfGDfueKonfigData.EAdr_Dfue, R.Antwort);
      ResponseLogList.Add (LogListObj);
    end;
  end else
    DeleteFile (R.Antwort);

  { Antwort auf Kommando auswerten: }
  S:=ExtractString (S, STX, ETX, 0);

  sBuf:=ExtractString (S, 'B', US, 0);
  if bFwUpdate then begin
    { Baujahr: }
    if Copy (sBuf, 1, 3) = '322' then
      DSfGDfueKonfigData.Wieser_Baujahr:=Copy (sBuf, 4, length (sBuf));
  end
  else begin
    { Fabriknummer: }
    if Copy (sBuf, 1, 3) = '103' then
      DSfGDfueKonfigData.Wieser_Fabriknummer:=Copy (sBuf, 4, length (sBuf));
  end;

  sBuf:=ExtractString (S, US, NUL, 0);
  if bFwUpdate then begin
    { Fabriknummer: }
    if Copy (sBuf, 1, 3) = '323' then
      DSfGDfueKonfigData.Wieser_Fabriknummer:=Copy (sBuf, 4, length (sBuf));
  end
  else begin
    { Baujahr: }
    if Copy (sBuf, 1, 3) = '104' then
      DSfGDfueKonfigData.Wieser_Baujahr:=Copy (sBuf, 4, length (sBuf));
  end;

  Result:=true;
end;

{----------------------------------------------------}
function TDSfGAbruf.GetWieserDfueGeraeteInfo: boolean;
{----------------------------------------------------}
{ Geräteinformationen der NG-DSfG-DFÜ abfragen (nur Wieser);
  Ergebnis: true, wenn Lesen der Geräteinformationen ok }
var
  Befehl, S: string;
  R: TRueckgabe;
  sBuf: string;
  dtBuild: TDateTime;
  dtBuf: TDateTime;

begin
  Result:=false;

  { Geräteversion: }
  Befehl:='B320';
  if not (CommObj is TDSfGSerialCommObj) then
    Befehl:='YW'+Befehl;
  Befehl:=STX+Befehl+ETX;

  { Kommando für Abfrage der Geräteversion senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Login, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Geräteversion-Kommando auswerten: }
  if not ValidDSfGDfueAntwort (true, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  S:=ExtractString (R.Antwort, STX, ETX, 0);
  sBuf:=F_Zerlegen (S, ' ');
  if Copy (sBuf, 1, 4) = 'B320' then begin
    DSfGDfueKonfigData.Wieser_NG_Typ:=Copy (sBuf, 5, length (sBuf));  { Typ-Nummer }
    DSfGDfueKonfigData.Wieser_NG_Version:=S;  { Version }
  end;

  { Build: }
  Befehl:='1B59';
  if not (CommObj is TDSfGSerialCommObj) then
    Befehl:='YW'+Befehl;
  Befehl:=STX+Befehl+ETX;

  { Kommando für Abfrage der Build-Info senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Login, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Build-Kommando auswerten: }
  if not ValidDSfGDfueAntwort (true, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  S:=ExtractString (R.Antwort, STX, ETX, 0);
  if Copy (S, 1, 4) = '1B59' then begin
    S:=Copy (S, 5, length (S));  // 1B59 wegschneiden, Format ddmmyyyyhhmm
    // Ablage in DSfG-DFÜ-Konfigurationsdaten im Format yyyymmddhhmmss:
    DSfGDfueKonfigData.Wieser_NG_Build:=Copy (S, 5, 4) + Copy (S, 3, 2) + Copy (S, 1, 2) +
                                        Copy (S, 9, length (S)) + '00';
  end;

  { Gerätekonfiguration: }
  Befehl:='1B41';
  if not (CommObj is TDSfGSerialCommObj) then
    Befehl:='YW'+Befehl;
  Befehl:=STX+Befehl+ETX;

  { Kommando für Abfrage der Gerätekonfiguration senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Login, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Gerätekonfiguration-Kommando auswerten: }
  if not ValidDSfGDfueAntwort (true, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  S:=ExtractString (R.Antwort, STX, ETX, 0);

  if Copy (S, 1, 4) = '1B41' then begin
    S:=Copy (S, 5, length (S));  // 1B41 wegschneiden
    { Flashgröße: }
    DSfGDfueKonfigData.Wieser_NG_Flashgroesse:=ExtractString (S, NUL, RS, 0);
    { SysConfig: Spannungsversorgung, Ausführung (Gehäuse, Kassette) }
    DSfGDfueKonfigData.Wieser_NG_SysConfig:=ExtractString (S, RS, RS, 0);
  end;

  EncodeDateStr (Copy (DSfGDfueKonfigData.Wieser_NG_Build, 1, 8), 'YYYYMMDD', dtBuild);
  EncodeTimeStr (Copy (DSfGDfueKonfigData.Wieser_NG_Build, 9, 6), 'HHMMSS', dtBuf);
  dtBuild:=dtBuild + dtBuf;
  S:='Geräteinformation der DSfG-DFÜ: Gerätetyp ' + DSfGDfueKonfigData.Wieser_NG_Typ +
     ', Version ' + DSfGDfueKonfigData.Wieser_NG_Version +
     ', erstellt am ' + FormatDateTime (C_FormatDateTime, dtBuild);
  WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, S, true);

  if Assigned (FCBFwUpdStatus) then
    FCBFwUpdStatus (sfwupd_AktFwVersionLesenOK);

  Result:=true;
end;

{---------------------------------------------------------------------------------------}
function TDSfGAbruf.GetWieserDfueParameterEinstellbar (var sRohAntwort: string): boolean;
{---------------------------------------------------------------------------------------}
{ Einstellbare Parameter aus NG-DSfG-DFÜ lesen und Rohantwort zurückgeben (nur Wieser);
  Rückgabe: Rohantwort
  Ergebnis: true, wenn Lesen der einstellbaren Parameter ok }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  { Vorbelegung Rückgabe: }
  sRohAntwort:='';

  Befehl:='sb';  // Befehl "Einstellbare Parameter auslesen"
  if not (CommObj is TDSfGSerialCommObj) then
    Befehl:='YW'+Befehl;
  Befehl:=STX+Befehl+ETX;

  { Kommando für Abfrage der einstellbaren Parameter senden (mit Systemzeit-
    Passwort; 25.07.2012, WW) }
  if not CommObj.SendCommandWithSysTimePW (Befehl, [ETX], 1, DSfGTimeouts.Login,
                                           ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Parameterauslese-Kommando auswerten: }
  if not ValidDSfGDfueAntwort (true, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  sRohAntwort:=R.Antwort;  { Rückgabe Rohantwort }
  Result:=true;
end;

{----------------------------------------------------------------------------------}
function TDSfGAbruf.SaveWieserDfueParameterEinstellbar (sFilename: string): boolean;
{----------------------------------------------------------------------------------}
{ Einstellbare Parameter aus NG-DSfG-DFÜ abfragen und Rohantwort in Datei
  speichern (nur Wieser);
  Übergabe: Name der Datei, in der die Parameter gespeichert werden sollen
  Ergebnis: true, wenn Lesen und Speichern der einstellbaren Parameter ok }
var
  sRohAntwort: string;
  S: string;
  sFilenameBak: string;
  sExt: string;

begin
  Result:=false;

  { Einstellbare Parameter aus NG-DSfG-DFÜ lesen: }
  if not GetWieserDfueParameterEinstellbar (sRohAntwort) then exit;

  { Falls Datei mit gespeicherten Parametern bereits existiert, umbenennen in
    Dateiname mit Zeitstempel: }
  if FileExists (sFilename) then begin
    sExt:=ExtractFileExt (sFileName);
    sFilenameBak:=ChangeFileExt (sFileName, '') + '_' +
                  FormatDateTime ('yyyymmddhhnnsszzz', Now) + sExt;  // mit ms; 06.11.2014, WW
    RenameFile (sFileName, sFileNameBak);
  end;

  { Antwort in Datei schreiben:
    -> Bestehende Datei überschreiben, kein CR LF Abschluß, mit Net-Zugriff }
  if not StringToFile (sRohAntwort, sFilename, true, false, true) then begin
    FehlerGruppeCodeUpdate (ST_FILEERROR, FILEERR_FIRMWUPD_PARA_COULDNOTWRITE);
    exit;
  end;

  S:='Parameter der DSfG-DFÜ gespeichert in ' + sFilename;
  WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, S, true);

  if Assigned (FCBFwUpdStatus) then
    FCBFwUpdStatus (sfwupd_ParaSichernOK);

  Result:=true;
end;

{------------------------------------------------------------------------------}
function TDSfGAbruf.RestoreWieserDfueParameterEinstellbar (
  sFilenameSave, sFilenameRestoreResult: string;
  pStatusTextProc: TCBAbrufStatusText; pErrorTextProc: TCBAbrufErrorText): boolean;
{------------------------------------------------------------------------------}
{ In Datei gespeicherte, einstellbare Parameter einer NG-DSfG-DFÜ (Rohantwort auf
  b-Befehl) ins Gerät übertragen (nur Wieser);
  Übergabe: Name der Datei, in der die Parameter gespeichert sind
            Name der Datei, in der die Parametrier-Ergebnisse geschrieben werden
  Ergebnis: true, wenn Übertragen der einstellbaren Parameter ok }
var
  sRohParaSave: string;
  sRohParaAktu: string;
  DfueParaListSave: TDfueParaList;
  DfueParaListAktu: TDfueParaList;
  DfueParaListObjAktu: TDfueParaListObj;
  i: integer;
  ParaEinstellCmdData: TParaEinstellCmdData;
  ParaEinstellResultData: TParaEinstellResultData;
  dummy: string;
  S: string;
  bParaOK: boolean;
  R: TRueckgabe;
  iFehlerGruppe_Merk: integer;
  iFehlerCode_Merk: integer;
  sParaRestoreResult: string;
  bParaNameAenderung: boolean;
  sParaErrorText: string;

begin
  Result:=false;

  if not FileExists (sFilenameSave) then begin  { Datei mit gespeicherten Roh-Parametern nicht gefunden }
    FehlerGruppeCodeUpdate (ST_FILEERROR, FILEERR_FIRMWUPD_PARA_DOESNOTEXIST);
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet_Param, 'Datei mit gespeicherten DSfG-DFÜ-Parametern suchen' +
      ' (' + sFilenameSave + ')', false);
    exit;
  end;

  { Gespeicherte Parameter aus Datei lesen und in String ablegen (Rohformat),
    mit Net-Zugriff: }
  sRohParaSave:=StringFromFile (sFilenameSave, true);
  if length (sRohParaSave) = 0  then begin
    FehlerGruppeCodeUpdate (ST_FILEERROR, FILEERR_FIRMWUPD_PARA_COULDNOTREAD);
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet_Param, 'Gespeicherte DSfG-DFÜ-Parameter aus Datei lesen' +
      ' (' + sFilenameSave + ')', false);
    exit;
  end;

  S:='Datei mit gespeicherten DSfG-DFÜ-Parametern: ' + sFilenameSave;
  WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet_Param, S, true);

  { Für den Befehl zum Lesen der einstellbaren Parameter ist das Systemzeit-
    Passwort erforderlich: 25.07.2012, WW
    -> Zwingend erforderlich  nach einem Firmwareupdate (Gerätezeit hat sich geändert !) }
  if not CommObj.InitializeSysTimePW (DSfGTimeouts.Login, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, 'System-Passwort initialisieren', false);
    exit;
  end;

  { Aktuelle einstellbare Parameter aus NG-DSfG-DFÜ lesen: }
  if not GetWieserDfueParameterEinstellbar (sRohParaAktu) then begin
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet_Param, 'DSfG-DFÜ-Parameter abfragen', false);
    exit;
  end;
  WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet_Param, 'DSfG-DFÜ-Parameter abfragen', true);

  Result:=true;
  iFehlerGruppe_Merk:=0;  // Default: Restore OK
  iFehlerCode_Merk:=0;

  DfueParaListSave:=TDfueParaList.Create;
  try
    { Gespeicherte Parameter-Rohdaten in DSfG-DFÜ-Parameter-Save-Liste konvertieren: }
    DfueParaListSave.LoadFromRohstring_kleinB (sRohParaSave, '');

    DfueParaListAktu:=TDfueParaList.Create;
    try
      { Aktuelle Parameter-Rohdaten in DSfG-DFÜ-Parameter-Aktu-Liste konvertieren: }
      DfueParaListAktu.LoadFromRohstring_kleinB (sRohParaAktu, '');

      WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet_Param, 'Aktuelle DSfG-DFÜ-Parameter mit den gespeicherten vergleichen', true);
      { Von allen gespeicherten Parametern nur diejenigen ins Gerät übertragen,
        die nicht dem aktuellen Parameterwert entsprechen: }
      for i:=0 to DfueParaListSave.Count-1 do begin
        { Eintrag für Parameternummer in Aktu-Liste suchen: }
        DfueParaListObjAktu:=DfueParaListAktu.GetDfueParaListObj ('',
          TDfueParaListObj (DfueParaListSave[i]).ParaAdr);
        if DfueParaListObjAktu <> nil then begin  { Listeneintrag gefunden }
          // Prüfung, ob sich der Name des Parameters beim Versionswechsel geändert hat
          bParaNameAenderung:=DfueParaListObjAktu.Name <>
                              TDfueParaListObj (DfueParaListSave[i]).Name;
          sParaErrorText:='';  // Default: kein Fehlertext

          if DfueParaListObjAktu.Wert <>
             TDfueParaListObj (DfueParaListSave[i]).Wert then begin
            // Aktueller und gespeicherter Wert unterscheiden sich
            with ParaEinstellCmdData do begin
              ParaTyp:='';  // nicht verwendet
              BAdr:='YWB';
              ParaAdr:=TDfueParaListObj (DfueParaListSave[i]).ParaAdr;  // Parameternummer
              ZCode1:='';  // nicht verwendet
              ZCode2:='';  // nicht verwendet
              ParaWertNeu:=TDfueParaListObj (DfueParaListSave[i]).Wert;  // Neuer Parameterwert
            end;
            // DSfG-DFÜ-Parameter übertragen:
            bParaOK:=UebertragungDfueParameter (ParaEinstellCmdData,
                                                ParaEinstellResultData,
                                                dummy);
            if bParaOK then begin
              sParaRestoreResult:='Parametrierung OK'
            end
            else begin
              iFehlerGruppe_Merk:=Fehlergruppe;  // Fehlergruppe, -code merken; 06.11.2014, WW
              iFehlerCode_Merk:=Fehlercode;

              sParaRestoreResult:='Parametrierung Fehler';
              if Assigned (pStatusTextProc) then
                sParaErrorText:=pStatusTextProc (iFehlerGruppe_Merk) + ': ';
              if Assigned (pErrorTextProc) then
                sParaErrorText:=sParaErrorText +
                  pErrorTextProc (iFehlerGruppe_Merk, iFehlerCode_Merk);
                                         
              Result:=false;  // bei Fehler weitermachen
            end;

            // Log-Datei schreiben:
            S:='Parameter ' + ParaEinstellCmdData.ParaAdr +
               ' wiederherstellen (Alter Wert: ' + DfueParaListObjAktu.Wert +
               ', neuer Wert: ' + ParaEinstellCmdData.ParaWertNeu + ')';
            WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet_Param, S, bParaOK);
          end
          else begin
            // Aktueller und gespeicherter Wert sind gleich
            sParaRestoreResult:='Keine Parametrierung';
          end;

          // Parametrier-Ergebnis-Datei schreiben; 06.11.2014, WW
          if bParaNameAenderung then
            S:='Ja'
          else
            S:='Nein';

          sParaRestoreResult:=sParaRestoreResult + #9 +             // Ergebnis Parametrierung
             TDfueParaListObj (DfueParaListSave[i]).ParaAdr + #9 +  // Parameternummer
             S + #9 +                                               // Parametername geändert
             TDfueParaListObj (DfueParaListSave[i]).Name + #9 +     // Parametername der Vorversion
             DfueParaListObjAktu.Name + #9 +                        // Parametername der neuen Version
             DfueParaListObjAktu.Wert + #9 +                        // Parameterwert alt der neuen Version
             TDfueParaListObj (DfueParaListSave[i]).Wert + #9 +     // Parameterwert neu der neuen Version
             sParaErrorText;                                        // Fehlerbeschreibung
          Write_FirmwareUpdate_ParaRestoreResultFile (sFilenameRestoreResult, sParaRestoreResult);
        end;  { if DfueParaListObjAktu <> nil }
      end;  { for i }
    finally
      DfueParaListAktu.Free;
    end;
  finally
    DfueParaListSave.Free;
  end;

  FehlerGruppeCodeUpdate (iFehlerGruppe_Merk, iFehlerCode_Merk);  // 06.11.2014, WW

  { Dateien mit gespeicherten Parametern und Parametrierergebnis löschen: }
  if Result then begin  // nur wenn Übertragung aller Parameter OK war; 06.11.2014, WW
    DeleteFile (sFilenameSave);
    DeleteFile (sFilenameRestoreResult); 
  end;

  if Assigned (FCBFwUpdStatus) then
    FCBFwUpdStatus (sfwupd_ParaRestoreOK);
end;

{-------------------------------------------------------------------------------------}
function TDSfGAbruf.WieserDfueFirmwareUpdate (sBinFilename, sFilename_ParaSave: string;
  var FwUpdateInfoData: TFwUpdateInfoData): boolean;
{-------------------------------------------------------------------------------------}
{ Firmware in der NG-DSfG-DFÜ updaten (nur Wieser);
  Übergabe: Name der Firmware-Binärdatendatei (vollständig, mit Pfad)
            Name der Datei, in der die Parameter gespeichert werden sollen
  Rückgabe: Firmware-Update-Info-Daten
  Ergebnis: true, wenn Firmwareupdate ok }
var
  Befehl: string;
  R: TRueckgabe;
  sNewBaud: string;
  iNewBaud: integer;
  sBinData: string;
  FwBinInfo: TFwBinInfo;
  sStartAdrHex: string;
  sEndAdrHex: string;
  S: string;
  FwFabrikationsdaten: TFwFabrikationsdaten_NG;
  bGeraeteFwIstAktuell: boolean;
  dtBuild: TDateTime;
  dtBuf: TDateTime;

begin
  Result:=false;

  { Vorbelegung Rückgabe Firmware-Update-Infodaten: }
  with FwUpdateInfoData do begin  // Keine Firmware-Update-Infodaten vorhanden
    Version_neu:='';  // unbekannt
    Build_neu:='';    // unbekannt
  end;

  { Datei mit Firmware-Binärdaten prüfen: }
  if not CheckFirmwareBinFile (sBinFilename) then begin
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, 'Firmware-Binärdatendatei prüfen', false);
    exit;
  end;

  S:='Firmware-Updatedatei: ' + sBinFilename;
  WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, S, true);

  { Datei mit Firmware-Binärdaten laden: }
  sBinData:=StringFromFile (sBinFilename);

  { Firmware-Binärdaten scannen: }
  ScanFirmwareBinData_NG (sBinData, FwBinInfo);

  { Firmware-Binärdaten analysieren und auf Gültigkeit prüfen: }
  if not ValidFirmwareBinData (FwBinInfo, DSfGDfueKonfigData.Wieser_NG_Typ,
                               DSfGDfueKonfigData.Wieser_NG_Build,
                               bGeraeteFwIstAktuell) then begin
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, 'Firmware-Binärdatendatei auf Gültigkeit prüfen', false);
    exit;
  end;
  if bGeraeteFwIstAktuell then begin  { Geräte-Firmware ist aktuell }
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, 'Firmware in der DSfG-DFÜ ist aktuell', true);
    Result:=true;
    exit;  // OK, kein Update erforderlich
  end;

  { Für den Befehl zum Lesen der einstellbaren Parameter ist das Systemzeit-Passwort
    erforderlich: 25.07.2012, WW }
  if not CommObj.InitializeSysTimePW (DSfGTimeouts.Login, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, 'System-Passwort initialisieren', false);
    exit;
  end;

  { Vor dem Firmwareupdate die einstellbaren Parameter auslesen und in Rohdatendatei
    speichern: }
  if not SaveWieserDfueParameterEinstellbar (sFilename_ParaSave) then begin
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, 'Parameter der DSfG-DFÜ abfragen und speichern', false);
    exit;
  end;

  { Binärdaten mit Fabrikationsdaten des vorliegenden Geräts updaten (Binärdatendatei
    enthält Default-Fabrikationsdaten !): }
  with FwFabrikationsdaten do begin
    Fabriknummer:=DSfGDfueKonfigData.Wieser_Fabriknummer;
    Baujahr:=StrToIntDef (DSfGDfueKonfigData.Wieser_Baujahr, 0);
    SysConfig:=StrToIntDef ('$' + DSfGDfueKonfigData.Wieser_NG_SysConfig, 0);
    Flashgroesse:=StrToIntDef (DSfGDfueKonfigData.Wieser_NG_Flashgroesse, 0);
  end;
  UpdateFirmwareBinData_NG (FwFabrikationsdaten, sBinData);

  { Start- und Endadresse der Binärdaten liegen im Bereich $00000..$FFFFF.
    Für Befehl muß die Start- und Endadresse im Bereich $F00000..$FFFFFF sein: }
  sStartAdrHex:=IntToHex (C_FwAdrOffsetBefehl + FwBinInfo.StartAdresse, 6);
  sEndAdrHex:=IntToHex (C_FwAdrOffsetBefehl + FwBinInfo.EndAdresse, 6);
  Befehl:='1Fc' + US + sStartAdrHex + US + sEndAdrHex;  // c = Flash im Gerät komplett löschen

  if not (CommObj is TDSfGSerialCommObj) then
    Befehl:='YW'+Befehl;
  Befehl:=STX+Befehl+ETX;

  { Kommando für Firmwareupdate senden (mit Systemzeit-Passwort) }
  if not CommObj.SendCommandWithSysTimePW (Befehl, [ETX], 1, DSfGTimeouts.Login,
                                           ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, 'Kommando für Initialisierung des Firmware-Updates senden', false);
    exit;
  end;

  { Antwort auf Firmwareupdate-Kommando auswerten: }
  if not ValidDSfGDfueAntwort (true, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, 'Antwort auf Firmware-Update-Initialisierung prüfen', false);
    exit;
  end;

  if not ValidFirmwareUpdateAntwort (R.Antwort, R.Fehlergruppe, R.Fehlercode, sNewBaud) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, 'Antwort auf Firmware-Update-Initialisierung prüfen', false);
    exit;
  end;

  if CommObj is TDSfGSerialCommObj then begin  // Abruf über serielle Schnittstelle
    { Antwort enthält Info über neue Baudrate, mit der ab jetzt weiter kommuniziert wird: }
    if not ValidFirmwareUpdateAntwort_NewBaud (sNewBaud, iNewBaud) then begin
      WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, 'Neue Baudrate in Antwort auf Firmware-Update-Initialisierung prüfen', false);
      exit;
    end;
    if iNewBaud <> -1 then begin
      { Auf neue Baudrate umschalten, übrige Schnittstellen-Parameter bleiben gleich: }
      with TDSfGSerialCommObj (CommObj).Serial do
        SetCommParameter (KonvertBaudrate (iNewBaud), DataBits, ParityBit, StopBits);
    end;
  end;

  { Firmware-Binärdaten ins Gerät übertragen: }
  if not FirmwareBinData_Uebertragen (sBinData, FwBinInfo.StartAdresse, CommObj) then begin
    WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, 'Firmware-Binärdaten übertragen', false);
    exit;
  end;

  { Firmware-Update erfolgreich: Infodaten mit neuer Version belegen }
  S:=Format ('%.4d', [FwBinInfo.Version]);
  Insert ('.', S, length (S) - 1);
  FwUpdateInfoData.Version_neu:=S;
  FwUpdateInfoData.Build_neu:=FwBinInfo.Build;

  EncodeDateStr (Copy (FwUpdateInfoData.Build_neu, 1, 8), 'YYYYMMDD', dtBuild);
  EncodeTimeStr (Copy (FwUpdateInfoData.Build_neu, 9, 6), 'HHMMSS', dtBuf);
  dtBuild:=dtBuild + dtBuf;
  S:='Firmware-Update durchgeführt: Version ' + FwUpdateInfoData.Version_neu +
     ', erstellt am ' + FormatDateTime (C_FormatDateTime, dtBuild);
  WriteLog_FirmwareUpdate (C_CmdFktCode_FwUpdGeraet, S, true);
                                   
  Result:=true;
end;

{-------------------------------------------------------------------------------------------}
function TDSfGAbruf.DfueTransparentSchalten (AufmTelegrammList: TAufmTelegrammList): boolean;
{-------------------------------------------------------------------------------------------}
{ DSfG-DFÜ transparent schalten und anstehende Telegramme auslesen;
  Rückgabe: Liste mit empfangenen Aufmerksamkeitstelegrammen
  Ergebnis: true, wenn Transparentschalten ok }
var
  Befehl: string;
  R: TRueckgabe;
  Extensionmode_Abruf: byte;  { der Erweiterungsgrad mit dem tatsächlich abgerufen wird }
  Antwort: string;
  MaxExtensionMode: byte;
  OnePushTelegramm: string;

begin
  Result:=false;

  { wenn Modul den von der DSfG-DFÜ bevorzugten Erweiterungsgrad nicht bedienen kann:
    -> mit Erweiterungsgrad 0 abrufen, den muß jede DSfG-DFÜ beherrschen können
    12.07.2010, GD: Erweiterungsgrad 2 als 1 behandeln (Erweiterungsgrad 0 wird
                    in der Praxis nicht von allen DSfG-DFÜs beherrscht)
    20.07.2012, WW: Verallgemeinert -> nicht unterstützte DFÜ-Erweiterungsgrade
                    als maximal vom Modul unterstützte Erweiterungsgrade behandeln }
  MaxExtensionMode:=CommObj.GetMaxExtensionmode;
  if (ExtensionMode_Dfue > MaxExtensionmode) then
    ExtensionMode_Abruf := MaxExtensionmode
  else
    ExtensionMode_Abruf:=Extensionmode_Dfue;

  Befehl:=STX+'T'+ETX;

  { Transparentschalten-Kommando senden: Es werden die Antwort des Transparent-Befehls
    und evtl. spontan gesendete Aufmerksamkeits-Telegramme zusammen erwartet. SendCommand
    muß daher hier in einen Timeout laufen, um alle Aufmerksamkeits-Telegramme auslesen zu können ! }
  CommObj.SendCommand (Befehl, [ETX], 100000, DSfGTimeouts.DFUETransparent, ad_String, R, NoCarrier);
  if not (((R.Fehlergruppe = 0) AND (R.Fehlercode = 0)) OR
          ((R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT))) then begin
    { Fehler in Antwort }
    { mit erweiterter Fehlergruppe; 13.03.2018, WW }
    FehlerGruppeCodeUpdate (EST_DSFGTRANSPARENTERROR + R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwortteil auf Transparent-Befehl analysieren: }
  if length (R.Antwort) = 0 then begin     { "echter" Timeout ist aufgetreten }
    { mit erweiterter Fehlergruppe; 13.03.2018, WW }
    FehlerGruppeCodeUpdate (EST_DSFGTRANSPARENTERROR + COM_KOMMERROR, KOMMERR_TIMEOUT);
    exit;
  end;

  Antwort:=ExtractString (R.Antwort, STX, ETX, 0);
  if Pos ('?', Antwort) <> 0 then begin  { lt. DSfG-Spezifikation }
    { neue Fehlergruppe, -code; 13.03.2018, WW }
    FehlerGruppeCodeUpdate (EST_DSFGTRANSPARENTERROR, DSFGTRANSPERR_DFUEBUSADDRESS_KEINLOKALERTEILNEHMER);
    exit;
  end;
  Dfue_ist_transparent_geschaltet:=true; { Flag setzen: DSfG-DFÜ ist jetzt transparent geschaltet }

  { Extensionmode setzen, mit dem tatsächlich abgerufen wird und mit dem auch schon eventuelle
    Aufmerksamkeits-Telegramme gesendet wurden: }
  CommObj.SetExtensionMode (Extensionmode_Abruf);

  Antwort:=ExtractString (R.Antwort, ETX, NUL, 0);  { Antwortteil mit den Aufmerksamkeits-Telegrammen }
  if CommObj is TDSfGGPRSCommObj then begin
    { GPRS-Pull-Abruf: es könnten GPRS-Push-Telegramme enthalten sein (MRG 910) }
    if length (Antwort) > 0 then begin
      if CommObj.CheckForPushData (Antwort) = at_PushData_MRG900 then begin
        while length (Antwort) > 0 do begin
          OnePushTelegramm:=FCutOnePushTelegramm (Antwort, mrgtyp_MRG910);  { ein Push-Telegramm ausschneiden }
WriteDebugLog (FLogPath, '_push.txt', 'n' + #9 + SonderzeichenString (OnePushTelegramm), true);
          { Push-Telegramm in Telegrammliste eintragen: }
          TDSfGGPRSCommObj (CommObj).AddPushTelegramToList (OnePushTelegramm, mrgtyp_MRG910);
        end;  { while length (Antwort) > 0 }
      end;
    end;
  end
  else begin
    { Telegramme in Telegramm-Liste konvertieren: }
    if not GetAufmerksamkeitsTelegramme (Antwort, Extensionmode_Abruf, AufmTelegrammList) then
      { mit erweiterter Fehlergruppe; 13.03.2018, WW }
      FehlerGruppeCodeUpdate (EST_DSFGTRANSPARENTERROR + ST_KONVERROR, SKERR_AUFMTELEGRAMMKONV);
  end;

  Result:=true;
end;

{--------------------------------------------}
function TDSfGAbruf.GetDfueKommandos: boolean;
{--------------------------------------------}
{ von der DSfG-DFÜ unterstützte Kommandos abfragen und nach "DfueKommandos" schreiben;
  Ergebnis: true, wenn DFÜ-Komandos abgefragt werden konnten }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  Befehl:='?';
  if not CommObj.SendCommand (STX+Befehl+ETX, [ETX], 1, DSfGTimeouts.DFUEParameter, ad_String,
                      R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  if ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
    Dfue_Kommandos:=Copy (R.Antwort, 3, length (R.Antwort)-3);
  Result:=true;
end;

{--------------------------------------------------------------------------------}
function TDSfGAbruf.VerbAufbau (VerbAufbauCmdData: TVerbAufbauCmdData;
                                AufmTelegrammList: TAufmTelegrammList;
                                ResponseLogList: TResponseLogList;
                                var DSfGDfueKonfigDataRet: TDSfGDfueKonfigData;
                                var StationsKennungRet: string;
                                var ZeitSyncInfoData: TZeitSyncInfoData;
                                var ZS_Fehlergruppe: integer;
                                var ZS_Fehlercode: integer;
                                var FwUpdateInfoData: TFwUpdateInfoData;
                                var VerbInfoData: TVerbInfoData): boolean;
{--------------------------------------------------------------------------------}
{ Aufbau einer Verbindung mit DSfG-Station;
  Übergabe: Verbindungsaufbau-Kommandodaten
  Rückgabe: Liste mit empfangenen Aufmerksamkeits-Telegrammen
            Responselog-liste
            Konfigurationsdaten der DSfG-DFÜ-Instanz, über die der Login erfolgte
            Kennung der DSfG-DFÜ
            ZeitSync-Info-Daten
            Fehlergruppe/-code der Zeitsynchronisation
            Firmwareupdate-Info-Daten
            Verbindungsinformationen-Record
  Ergebnis: true, wenn kompletter Verbindungsaufbau incl. Kennungsvergleich und Login erfolgreich
            (Flag "NoCarrier" gibt bei fehlerhaftem Verbindungsaufbau Auskunft, ob die Verbindung noch steht oder nicht) }

Var
  Befehl: string;
  R: TRueckgabe;
  S: string;
  IPAdr: string;
  PortId: integer;
  AFehlergruppe: integer;
  AFehlercode: integer;
  sFilename_ParaSave: string;
  sFilename_FwBin: string;
  sFilename_ParaRestoreResult: string;
  bFwUpdate: boolean;

Begin
  Result:=false;
  // Vorbelegung für Rückgabe der DSfG-DFÜ-Konfigurationsdaten:
  InitDSfGDfueKonfigData (DSfGDfueKonfigDataRet);
  StationsKennungRet:='';
  try
    // Rückgabe-Vorbelegung für optionale ZeitSynchronisation der Login-DSfG-DFÜ:
    ZS_Fehlergruppe:=-1;            // Kein ZeitSync-Ergebnis vorhanden
    ZS_Fehlercode:=-1;
    with ZeitSyncInfoData do begin  // Keine ZeitSync-Infodaten vorhanden
      DZ_Server:=0;
      DZ_Geraet:=0;
    end;

    { Rückgabe-Vorbelegung für optionales Update der DSfG-DFÜ-Firmware: }
    with FwUpdateInfoData do begin  // Keine Firmware-Update-Infodaten vorhanden
      Version_neu:='';
      Build_neu:='';
    end;

    { Rückgabe-Vorbelegung für Verbindungsinformationen: }
    with VerbInfoData do begin  // Keine Verbindungsinformationen vorhanden
      DZ_VerbindungSteht:=0;
      DZ_Login:=0;
    end;

    NoCarrier:=true;                 { Vorbelegung: es besteht keine Verbindung }
    FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }

    StationsKennung:=VerbAufbauCmdData.Kennung;       { Vorbelegung: Kennung aus Verbindungsaufbau-Kommando }

    if (not C_TestFlag) then begin  // keine Simulation; 14.04.2011
      { Extensionmode 0 setzen (Voreinstellung): }
      CommObj.SetExtensionMode (0);
      if CommObj is TDSfGModemCommObj then
        TDSfGModemCommObj (CommObj).SetDCDCheck (false);    { DCD-Überwachung aus }

      if CommObj is TDSfGModemCommObj then begin    { Modem-Abruf über serielle Schnittstelle }
        { Modem initialisieren: }
        if not Modem_Initialisieren_Wico (CommObj, false, ModemName, FNetProgPath,
                                          DSfGTimeouts.ModemInit, DSfGTimeouts.GSMModem,
                                          TDSfGModemCommObj (CommObj).Serial.COMPort,
                                          false, nil, Fehlergruppe, Fehlercode) then begin
          WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'Modem initialisieren', false);
          exit;
        end;

        Befehl:='atd'+VerbAufbauCmdData.Rufnummer+CR;
        { Verbindungsaufbaukommando senden }
        if not TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, DSfGTimeouts.Verbindungsaufbau, R) then begin
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
          WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'Modem-Verbindung aufbauen zu ' +
                                   VerbAufbauCmdData.Rufnummer, false);
          exit;
        end;

        { Antwort auf Verbindungsaufbau-Kommando auswerten: }
        if not CheckModemConnect (R.Antwort) then begin
          WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'Modem-Verbindung aufbauen zu ' +
                                   VerbAufbauCmdData.Rufnummer, false);
          exit;
        end;

        VerbInfoData.DZ_VerbindungSteht:=Now;  // Zeitpunkt 'Verbindung hergestellt'; 20.12.2013, WW
        WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'Modem-Verbindung aufbauen zu ' +
                                 VerbAufbauCmdData.Rufnummer, true);

        Delay (1500);  { Wartezeit nach Connect-Antwort vor nachfolgendem ersten Gerätebefehl;
                         -> um sicherzustellen, daß Verbindung ins Gerät durchgeschaltet ist
                         -> Für MRG 910: 200 ms
                         -> Für Verbindungen zu Tainy-Zentrale hochgesetzt auf 1500 ms; 05.05.2011, WW
                         Achtung: Tatsächliche Wartezeit bei Modemverbindung plus 500 ms
                                  für Wartezeit auf Modemantwort ! }
      end
      else if CommObj is TDSfGClientSocketCommObj then begin  { Abruf über TCP/IP, Client-Socket }
        { Kommandodaten: Rufnummer muß IP-Adresse/Host und Port durch Leerzeichen getrennt enthalten }
        S:=VerbAufbauCmdData.Rufnummer;
        IPAdr:=F_Zerlegen (S, ' ');
        try
          PortId:=StrToInt (F_LeftTrunc (S, ' '));
        except
          PortId:=8000;
        end;
        { ClientSocket-Verbindung öffnen: }
        if not TDSfGClientSocketCommObj (CommObj).Connect (IPAdr, PortId, DSfGTimeouts.Verbindungsaufbau,
                                                           AFehlergruppe, AFehlercode) then begin
          FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
          WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'IP-Verbindung aufbauen zu ' +
                                   IPAdr + ':' + IntToStr (PortId), false);
          exit;
        end;

        VerbInfoData.DZ_VerbindungSteht:=Now;  // Zeitpunkt 'Verbindung hergestellt'; 20.12.2013, WW
        WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'IP-Verbindung aufbauen zu ' +
                                 IPAdr + ':' + IntToStr (PortId), true);

        Delay (200);  { Wartezeit nach aufgebauter IP-Verbindung vor nachfolgendem ersten Gerätebefehl;
                         -> um sicherzustellen, daß Verbindung ins Gerät durchgeschaltet ist
                         -> Für MRG 910: 200 ms }
      end;

      { Verbindung steht jetzt: }
      NoCarrier:=false;
      if CommObj is TDSfGModemCommObj then
        TDSfGModemCommObj (CommObj).SetDCDCheck (true);     { DCD-Überwachung ein }

      { Kennungsabfrage und -überprüfung: }
      if not KennungAbfragen then begin
        WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'Kennung abfragen', false);
        exit;
      end;
      if StationsKennung <> VerbAufbauCmdData.Kennung then begin
        if VerbAufbauCmdData.KennPruef then begin
          FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_KEINE_VERBINDUNG);
          WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'Kennungsvergleich', false);
          exit;  { Stop bei falscher Kennung }
        end else
          FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_VERBINDUNG);
      end;

      { Paßwort übertragen: }
      if not PasswortLogin (VerbAufbauCmdData.Passwort,
                            VerbAufbauCmdData.DSfG_TransparentModus,
                            VerbInfoData.DZ_Login) then begin
        WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'DSfG-Login', false);
        exit;
      end;

      { Wieser-DSfG-DFÜ: aktive Teilnehmer ermitteln }
      if HerstellerChar = 'W' then
        if not GetWieserAktiveBusteilnehmer (VerbAufbauCmdData.DSfG_TransparentModus) then begin
          WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'DSfG-Busteilnehmer abfragen', false);
          exit;
        end;

      { folgende DFÜ-Informationen sind eigentlich nur für das Konfiguration-Einlesen nötig,
        werden aber trotzdem standardmäßig immer gelesen: }
      if Extensionmode_Dfue > 0 then  { wenn DSfG-DFÜ ab Erweiterungsgrad 1 vorliegt, können... }
        if not GetDfueVersionsdaten (ResponseLogList) then begin  { ...Versionsdaten der DSfG-DFÜ abgefragt werden }
          WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'Versionsdaten der DSfG-DFÜ abfragen', false);
          exit;
        end;
      if HerstellerChar = 'W' then begin
        if not GetWieserDfueAdressen (ResponseLogList) then begin  { alle in der DSfG-DFÜ eingestellten Adressen abfragen (nur Wieser) }
          WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'Adressen der DSfG-DFÜ abfragen', false);
          exit;
        end;
        { Fabriknummer und Baujahr der DSfG-DFÜ abfragen (für Konfiguration-Einlesen
          und NG-Firmware-Update); 02.10.2009, WW }
        bFwUpdate:=(VerbAufbauCmdData.FktCode = C_CmdFktCode_FwUpdGeraet) OR
                   (VerbAufbauCmdData.FktCode = C_CmdFktCode_FwUpdGeraet_Param);
        if not GetWieserDfueFabrikNr_Baujahr (bFwUpdate, ResponseLogList) then begin
          WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'Fabriknummer und Baujahr der DSfG-DFÜ abfragen', false);
          exit;
        end;
      end;

      { Zeitsynchronisation der DSfG-DFÜ (nur Wieser): }
      if VerbAufbauCmdData.DSfG_ZeitSyncCmdData.ZeitSync then
        ZeitSynch_WieserDfue (VerbAufbauCmdData.DSfG_ZeitSyncCmdData,
                              ZeitSyncInfoData, ZS_Fehlergruppe, ZS_Fehlercode);

      { Firmware-Update der DSfG-DFÜ: }
      if (VerbAufbauCmdData.FktCode = C_CmdFktCode_FwUpdGeraet) OR
         (VerbAufbauCmdData.FktCode = C_CmdFktCode_FwUpdGeraet_Param) then begin
        if HerstellerChar = 'W' then begin
          { Name der Datei mit gespeicherten, einstellbaren DSfG-DFÜ-Parameter: }
          sFilename_ParaSave:=GetDSfGDfueParaSaveFilename (FWorkPath,
                                                           VerbAufbauCmdData.Rufnummer);
          if VerbAufbauCmdData.FktCode = C_CmdFktCode_FwUpdGeraet then begin  { Flashen }
            { Geräteinfo lesen: Version, Build, Konfiguration }
            if not GetWieserDfueGeraeteInfo then begin
              WriteLog_FirmwareUpdate (VerbAufbauCmdData.FktCode, 'Geräteinformation der DSfG-DFÜ abfragen', false);
              exit;
            end;
            { Name der Datei mit Firmware-Binärdaten für Hersteller und Typ des
              vorliegenden Geräts bilden: }
            sFilename_FwBin:=GetFirmwareBinFilename (Herstellerchar,
                                                     DSfGDfueKonfigData.Wieser_NG_Typ,
                                                     FFirmwareBinFilePath);
            { Firmware in der NG-DSfG-DFÜ updaten: }
            if not WieserDfueFirmwareUpdate (sFilename_FwBin, sFilename_ParaSave,
                                             FwUpdateInfoData) then exit;
          end
          else if VerbAufbauCmdData.FktCode = C_CmdFktCode_FwUpdGeraet_Param then begin  { Parameter nach dem Flashen prüfen }
            { Name für Datei mit Ergebnis des Parameter-Wiederherstellens: }
            sFilename_ParaRestoreResult:=GetDSfGDfueParaRestoreResultFilename (
              FWorkPath, VerbAufbauCmdData.Rufnummer);
            { Gespeicherte, einstellbare Parameter einer NG-DSfG-DFÜ ins Gerät
              zurückübertragen: }
            if not RestoreWieserDfueParameterEinstellbar (sFilename_ParaSave,
                                                          sFilename_ParaRestoreResult,
                                                          GetStatusText, GetErrorText) then exit;
          end;
        end else  { DSfG-DFÜ-Firmware-Update wird für Fremdhersteller nicht unterstützt }
          FehlerGruppeCodeUpdate (SYS_ABRUFERROR, SYSABRFERR_KOMMANDOUNGUELTIG);
      end
      else begin  // kein DSfG-DFÜ-Firmwareupdate, weiter im Verbindungsaufbau-Ablauf
        if VerbAufbauCmdData.DSfG_TransparentModus in [dtm_Ja, dtm_Immer] then begin  // 11.07.2019, WW
          { DSfG-DFÜ transparent schalten und spontane Aufmerksamkeits-Telegramme lesen: }
          if not DFUETransparentSchalten (AufmTelegrammList) then exit;
        end
        else begin
          if Extensionmode_Dfue > 0 then  { wenn DSfG-DFÜ ab Erweiterungsgrad 1 vorliegt, können... }
            if not GetDfueKommandos then exit;  { ...von der DSfG-DFÜ unterstützte Kommandos abgefragt werden }
        end;
      end;
    end
    else begin  // Simulation
{$IFDEF SIMU}
      { Wartezeit zum Simulieren der Verbindungsaufbauzeit: }
      if FSimuDSfGKommunikationZeiten.Verbindungsaufbau > 0 then
        Delay (FSimuDSfGKommunikationZeiten.Verbindungsaufbau);  // 14.01.2015, WW

      { Simulierter nicht erfolgreicher Verbindungsaufbau: 31.07.2015, WW }
      if CommObj is TDSfGModemCommObj then begin    { Simulierter Modem-Abruf }
        if (FSimuLinienKeineVerbindung.IndexOf('0') >= 0) OR
           (FSimuLinienKeineVerbindung.IndexOf(IntToStr(FCOMNr)) >= 0) then begin
          FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_NOCARRIER);
          exit;
        end;
      end
      else if CommObj is TDSfGClientSocketCommObj then begin  { Simulierter Abruf über TCP/IP }
        if (FSimuLinienKeineVerbindung.IndexOf('0') >= 0) OR
           (FSimuLinienKeineVerbindung.IndexOf(IntToStr(FCOMNr)) >= 0) then begin
          FehlerGruppeCodeUpdate (COM_TCPIP_ERROR, TCPIP_ERR_CONNECT);
          exit;
        end;
      end;

      NoCarrier := False;  // OK, Verbindung aufgebaut
      VerbInfoData.DZ_VerbindungSteht:=Now;  // Zeitpunkt 'Verbindung hergestellt'
      VerbInfoData.DZ_Login:=VerbInfoData.DZ_VerbindungSteht;
{$ENDIF}
    end;

    if Assigned (FSignaturClient) then
      FSignaturClient.LogInfo_Kennung:=StationsKennung;  // Kennung für Signaturclient-Protokollierung
  finally  // 14.06.2011, WW
    // Rückgabe belegen (DSfG-DFÜ-Konfigurationsdaten, Stations-Kennung):
    DSfGDfueKonfigDataRet:=DSfGDfueKonfigData;
    StationsKennungRet:=StationsKennung;
  end;
  Result:=true;
End;

{-------------------------------------}
function TDSfGAbruf.VerbAbbau: boolean;
{-------------------------------------}
{ Verbindung wird abgebaut (Modem: Rücksetzen der DTR-Leitung) }
var
  R: TRueckgabe;
  AFehlergruppe: integer;
  AFehlercode: integer;
Begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);         { Vorbelegung Fehlergruppe/-code: OK }

  if (not C_TestFlag) then begin  // keine Simulation; 14.01.2015, WW
    { Extensionmode 0 wieder setzen (Voreinstellung): }
    CommObj.SetExtensionMode (0);
    if CommObj is TDSfGModemCommObj then
      TDSfGModemCommObj (CommObj).SetDCDCheck (false);    { DCD-Überwachung aus }

    if CommObj is TDSfGModemCommObj then begin  { Modem-Abruf über serielle Schnittstelle }
      TDSfGModemCommObj (CommObj).Serial.ClearDTRSignal;      { DTR-Leitung rücksetzen }
      { OK vom Modem empfangen: Verbindung beendet }
      if TDSfGModemCommObj (CommObj).SendModemCommand ('', DSfGTimeouts.Verbindungsabbau, R) then begin
        if Pos ('OK', AnsiUpperCase (R.Antwort)) <> 0 then begin
          NoCarrier:=true;
          Result:=true;
        end;
      end;
      TDSfGModemCommObj (CommObj).Serial.SetDTRSignal;     { DTR-Leitung wieder setzen }
    end
    else if CommObj is TDSfGClientSocketCommObj then begin  { Abruf über TCP/IP, Client-Socket }
      { ClientSocket-Verbindung schließen: }
      if not TDSfGClientSocketCommObj (CommObj).Disconnect (DSfGTimeouts.Verbindungsabbau,
                                                            AFehlergruppe, AFehlercode) then
        FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode)
      else
        Result:=true;
    end
    else if CommObj is TDSfGREClntThreadCommObj then begin  { Anruf über TCP/IP-Rufentgegennahme-Thread }
      TDSfGREClntThreadCommObj (CommObj).Disconnect;  // 08.01.2018, WW
      Result:=true;
    end;
  end
  else begin  // Simulation
    // OK, Verbindung beendet
    NoCarrier:=true;
    Result:=true;
  end;
End;

{-----------------------------------------------------------------------------------}
Function TDSfGAbruf.AbrufDaten (Datentyp: integer;
                                vonDZ, bisDZ: TDateTime; vonOrdNr, bisOrdNr: integer;
                                DSfG_AdresslistData: TDSfG_AdresslistData;
                                ArLbDatenListe: TDSfGDataList; DELList: TDELList;
                                ResponseLogList: TResponseLogList;
                                ResponseRohdatenList: TDSfGDataList;
                                ResponseTraceLogList: TDSfGDataList;
                                Timeout: integer): boolean;
{-----------------------------------------------------------------------------------}
{ Abrufen von DSfG-Daten (Archive, Logbücher, Datenelemente);
  Übergabe: Datentyp
            von, bis Abfrage-Zeitraum (für Archiv- und Logbuchabfrage über Datum/Zeit)
            von, bis Abfrage-Ordnungsnummernbereich (für Archiv- und Logbuchabfrage
            über Ordnungsnummer -> hat Vorrang vor Datum/Zeitabfrage !)
            DSfG-Adresslisten-Daten
            Timeout für Antwort auf Abfrage
  Rückgabe: Archiv/Logbuch-DatenListe (Namen der Dateien mit konvertierten Archiv- oder Logbuchdaten,
                                       für Datenelemente-Abfrage kann nil übergeben werden)
            Datenelemente-Liste (für Datenelemente-Abfrage nötig, sonst kann nil übergeben werden)
            Responselog-Liste (Prüf-/Konvertierungsergebnis der abgerufenen Archivkanäle/Logbuchdaten/
                               Datenelemente; Namen der erzeugten Rohdaten-Dateien)
            Response-Rohdaten-Liste (Namen der Dateien mit kodierten Rohdaten für
                                     Response)
            Response-TraceLog-Liste (COM-Tracelog-Strings für Response);
  Ergebnis: true, wenn Datenabruf ok }
var
  Befehl: string;
  R: TRueckgabe;
  Modus: byte;
  iTimeout: integer;
  i: integer;
  EAdr: char;
  DEL, DEL_bis, DEL_von: string;
  Abruf_von, Abruf_bis: TDateTime;
  Antwort_vollstaendig: boolean;
  Erg: integer;
  DSfGFehler: boolean;                      { Flag für Fehler bei Kommunikation mit DSfG-DFÜ }
  RohdatenFehler: boolean;                  { Flag für Fehler in den Rohdaten }
  KonvFehler: boolean;                      { Flag für Fehler beim Konvertieren }
  DSfGAbrufListe: TDSfGAbrufList;
  DEL_einzeln: boolean;
  RohfileListe: TDSfGDataList;
  DArchivLogbuchKonv: TDArchivLogbuchKonv;
  KonvFileName: TFileName;
  AFehlergruppe: integer;
  AFehlercode: integer;
  DSfGDataListObj: TDSfGDataListObj;
  sRohsatz_ungueltig_Merk: string;
  isDE_VonBis: boolean;
  Versuche: byte;
  LogListObj: TRohfileLogObj;
  Abbruch: boolean;
  sPublicKey_x: string;  { Öffentlicher Schlüssel X für digitale Signatur }
  sPublicKey_y: string;  { Öffentlicher Schlüssel Y für digitale Signatur }
  sVirtualDEA: string;
  bDoSigVerify_Request: boolean;
  bPublicKey_vorhanden: boolean;
  sBase64: string;
  TraceLogListObj: TDSfGDataListObj;
{$IFDEF SIMU}
  // Deklarationen für Simulation
  pSimuFile    : TSimuFile;
  j, iONr      : integer;
  pDSfGRohRec  : TDSfGRohRec;
  cDEB         : char;
  pDEList      : TDSfGTelegrDEList;
  pSl          : TStrings;
  sRohFileName : TFileName;
  sDebug, sDea : string;
  iSimuWert    : integer;
  iLastUnixTime: Longword;
  iLastONo     : Longword;
  iUnixTime    : cardinal;
  sCOMLogFilename: string;
  sLog: string;
{$ENDIF}

Begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);         { Vorbelegung Fehlergruppe/-code: OK }

// Full/Partial-Abruf ist nicht realisiert (es wird immer die ganze Liste abgerufen)

  if Datentyp = C_IsDatenelemente then
    Modus:=d_DelAdr
  else begin
    if (vonOrdNr > -1) AND (bisOrdNr > -1) then
      Modus:=d_OrdNr
    else
      Modus:=d_VonBis;
  end;

  DSfGAbrufListe:=TDSfGAbrufList.Create;
  try
    // DSfG-Adressliste aus Abrufkommando in AbrufListe konvertieren:
    DEL_einzeln:=Datentyp <> C_IsDatenelemente;
    DSfGAbrufListe.LoadFromXMLAdressliste (DSfG_AdresslistData.AdrList, DEL_einzeln);

{$IFDEF SIMU}
    if FCOMNr > 0 then  { COM-Port }
      sCOMLogFilename:=FLogPath + 'COM_' + Format('%.3d', [FCOMNr]) + '.LOG'
    else begin  { IP-Abrufnummer, 14.01.2015, WW }
      if (FCOMNr = FCOMNr_Kommando) then
        sCOMLogFilename:=FLogPath + 'IP_' + Format ('%.3d', [Abs(FCOMNr)]) + '.LOG'
      else
        // Logfilename mit IP-Linien-Nr. aus Kommando und intern verwendeter:
        // IP_<IP-Linie Kommando>_<IP-Linie intern>.log
        sCOMLogFilename:=FLogPath + 'IP_' + Format ('%.3d', [Abs (FCOMNr_Kommando)]) +
                   '_' + Format ('%.3d', [Abs (FCOMNr)]) + '.LOG';  // 21.04.2015, WW
    end;
{$ENDIF}

    DSfGFehler:=false;
    RohdatenFehler:=false;
    KonvFehler:=false;
    Abbruch:=false;
    i:=0;
    { DSfG-Abrufliste abarbeiten: }
    while (i <= DSfGAbrufListe.Count - 1) AND (not NoCarrier) do begin
      Application.ProcessMessages;
      { Busadresse der Instanz, von der die Daten abgerufen werden sollen: }
      if length (TDSfGAbrufListObj (DSfGAbrufListe [i]).Data.EAdr) > 0 then begin
        { Prüfen, ob virtuelle DE-Adresse enthalten ist (Gerätezustand); 12.12.2014, WW }
        sVirtualDEA:=DSfGAbrufListe.CheckForVirtualDEA (i);

        EAdr:=TDSfGAbrufListObj (DSfGAbrufListe [i]).Data.EAdr[1];
        DEL:=TDSfGAbrufListObj (DSfGAbrufListe [i]).Data.DEL;
        DEL_bis:=TDSfGAbrufListObj (DSfGAbrufListe [i]).Data.DEL_bis;
        sPublicKey_x:=TDSfGAbrufListObj (DSfGAbrufListe [i]).Data.PublicKey_x;
        sPublicKey_y:=TDSfGAbrufListObj (DSfGAbrufListe [i]).Data.PublicKey_y;

    //  GerTypNr_Quelle:=TAbrufListObj (AbrufListe [i]).GerTypNr_Quelle;
    //  -> wird (falls erforderlich) für Ermittlung der allg. Meldungsnummer benötigt
    //  -> Da die Gerätetyp-Nr. nicht aus den Stammdaten geholt werden kann, müßte
    //     die Info wohl online ausgelesen werden.

        if Modus = d_OrdNr then begin
          Abruf_von:=vonOrdNr;
          Abruf_bis:=bisOrdNr;
        end
        else if Modus = d_VonBis then begin
          if vonDZ > 0 then
            Abruf_von:=vonDZ
          else
            Abruf_von:=EncodeDate (2000, 1, 1);   { Ersatz-von-Zeitpunkt für "Alles abrufen" }
          Abruf_bis:=bisDZ;
        end
        else begin
          DEL_von:=DEL;    { für Folgetelegramme beim Datenelemente-Abruf wird DEL_von jeweils neu belegt }
          Abruf_von:=-1;   { nicht benutzt }
          Abruf_bis:=-1;   { nicht benutzt }
        end;

        RohfileListe:=TDSfGDataList.Create;         { Liste für Rohdateinamen }
        try
          sRohsatz_ungueltig_Merk:='';  // kein gemerkter Ungültig-Rohsatz vorhanden
          Antwort_vollstaendig:=false;
          Versuche:=0;

          if (not C_TestFlag) then begin  // keine Simulation; 19.04.2013
            while (not Antwort_vollstaendig) AND (not NoCarrier) do begin { evtl. mehrere Abrufe pro Kanal/Logbuch nötig }
              Application.ProcessMessages;
              { Befehl zusammensetzen: }
              IncrementTID;
              if Datentyp = C_IsArchive then begin                                                                { Archivkanal }
                Befehl:=Get_ArchivLogbuchBefehl(EAdr, DEL, Modus, Abruf_von, Abruf_bis);
                iTimeout:=DSfGTimeouts.Archive;
              end
              else if Datentyp = C_IsLogbuecher then begin                                                            { Logbuch }
                Befehl:=Get_ArchivLogbuchBefehl(EAdr, DEL, Modus, Abruf_von, Abruf_bis);
                iTimeout:=DSfGTimeouts.Logbuecher;
              end
              else if Datentyp = C_IsDatenelemente then begin                             { einzelne Datenelemente oder Bereich }
                Befehl:=Get_DatenelementeBefehl(EAdr, DEL_von, DEL_bis);
                iTimeout:=DSfGTimeouts.Datenelemente;
              end
              else begin
                Befehl:='';
                iTimeout:=0;
              end;

              // Übergebener Timeout übersteuert den konfigurierten: 16.08.2012, WW
              if Timeout > -1 then
                iTimeout:=Timeout;

              { Datenabruf-Kommando senden }
              if not CommObj.SendCommand (Befehl, [FS], 1, iTimeout, ad_File, R, NoCarrier) then begin
                FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                if R.Fehlergruppe = ST_DSFGERROR then
                  DSfGFehler:=true
                else
                  Abbruch:=true;  { Stop, keine weiteren Telegramme }
                Break;  { weiteren Abruf dieser DE-Adresse abbrechen, Daten konvertieren; 10.07.2007 WW }
              end;

              if length (EAdr_VerbHalten) = 0 then
                EAdr_VerbHalten:=EAdr;  // EAdr für "Verbindung halten"-Befehl; 02.05.2013, WW

              { Rohfile prüfen (Antwort = Rohfilename), bei unvollständiger/ungültiger Antwort
                von-Zeitpunkt/Ordnungsnummer/DE-Adresse für Folgebefehl zurückgeben: }
              isDE_VonBis:=Del_bis <> '';  // von-bis-Datenelemente-Abruf
              Erg:=CheckRohfile_Antworttelegramm (R.Antwort, EAdr, DEL, Modus, isDE_VonBis,
                                                  Antwort_vollstaendig, DEL_von, Abruf_von,
                                                  sRohsatz_ungueltig_Merk);
              if Erg = DSFGKONVERR_ZAE_SOLL_IST then begin  { ZAE-Soll/Ist-Fehler; 14.03.2007, WW }
                if Versuche < CMaxVersuche then begin  // max. Anzahl an Versuchen noch nicht erreicht
                  inc (Versuche);
                  Continue;  // nochmal gleicher Befehl
                end;
              end;
              Versuche:=0;  // Anzahl der Versuche rücksetzen

              if ResponseLogList <> nil then begin
                { Rohfile-Prüfungsergebnis in Responselog-Liste eintragen: }
                if Datentyp = C_IsDatenelemente then
                  ResponseLogList.InsertDataKonvLog (EAdr, '', Erg)
                else
                  ResponseLogList.InsertDataKonvLog (EAdr, DEL, Erg);

                { Rohfilename in Log-Liste eintragen: 02.04.2007, WW }
                if not RohdatenLoeschen then begin
                  LogListObj:=TRohfileLogObj.Create;
                  LogListObj.SetData (EAdr, R.Antwort);
                  ResponseLogList.Add (LogListObj);
                end;
              end;

              if Erg <> DSFGKONVERR_OK then begin           { Fehler im Rohfile }
                RohdatenFehler:=true;
                Break;  { weiter mit nächstem Telegramm }
              end;

              { in Rohdatenfile-Liste eintragen: }
              RohfileListe.Add (R.Antwort);
            end; { while not Antwort_vollstaendig }
          end

          else begin  // Simulation
{$IFDEF SIMU}
            try
              if (Datentyp = C_IsArchive) or (Datentyp = C_IsLogbuecher) then
              begin
                while (not Antwort_vollstaendig) do begin
                  sDebug := 'Simu 1';
                  pDEList := TDSfGTelegrDEList.Create(True);  // Datenliste
                  if (Datentyp = C_IsArchive)
                  then pSimuFile := FSimuFileArch
                  else pSimuFile := FSimuFileLogb;
                  pSl := TStringList.Create;  // Liste für Unixzeit und Ordnungsnummer
                  sDebug := 'Simu 2';
                  try
                    if Modus = d_OrdNr then begin
                      pSl.CommaText := pSimuFile.GetRecordRangeONo(
                        Trunc(Abruf_von), Trunc(Abruf_bis));
                    end
                    else if Modus = d_VonBis then begin
                      pSl.CommaText :=
                        pSimuFile.GetRecordRangeUnixTime(
                          DateTimeToUnix(Abruf_von), DateTimeToUnix(Abruf_bis));
                    end;

                    sDebug := 'Simu 3';
                    for iONr := 0 to pSl.Count-1 do begin
                      with pDSfGRohRec do begin
                        Adresse := DEL;
                        if (Datentyp = C_IsArchive)
                        then Wert := FloatToStr(Random(150))
                        else Wert := FloatToStr(Random(999)+1);
                        UTime := GetStringPart(pSl[iONr], 1, #9);
                        OrdNr := GetStringPart(pSl[iONr], 2, #9);
                        Status := '0';
                        CRC := '';
                      end;
                      pDEList.Add(TDSfGTelegrDEObj.Create(pDSfGRohRec));
                    end;

                    { Wartezeit zum Simulieren der Gerätereaktionszeit: }
                    if FSimuDSfGKommunikationZeiten.AntwortTelegramm > 0 then
                      Delay (FSimuDSfGKommunikationZeiten.AntwortTelegramm);  // 14.01.2015, WW

                    // DSfG-Antwort-Telegramm erstellen
                    sDebug := 'Simu 4';
                    if (Modus = d_OrdNr) then cDEB := 'O' else cDEB := 'Z';
                    R.Antwort :=
                      GetAntwortTelegramm_Anfrage(EAdr, '_', cDEB, pDEList);

                    // Protokollierung in COM-Log
                    if (DebugCOMProtokoll) then
                      WriteErrorLog(CharStrToString(R.Antwort), sCOMLogFilename, elt_Info);

                    sRohFileName := CreateTempRohFile(FWorkPath, prefix_DSfG_Roh);
                    WriteRohfile(sRohfileName, R.Antwort);
                    { in Rohdatenfile-Liste eintragen: }
                    RohfileListe.Add(sRohFileName);

                    // Prüfung auf evtl. notwendige Folge-Antwort-Telegramme
                    sDebug := 'Simu 5';
                    if (pDEList.Count > 0) then begin  // 14.01.2014, WW
                      if (pSimuFile.GetLastTimeAndONo(iLastUnixTime, iLastONo)) then begin
                        if (Modus = d_OrdNr) then begin
                          if (StrToInt(pDSfGRohRec.OrdNr) < Trunc(Abruf_bis)) and
                             (StrToInt(pDSfGRohRec.OrdNr) < Integer(iLastONo))
                          then Abruf_von := StrToInt(pDSfGRohRec.OrdNr) + 1
                          else Antwort_vollstaendig := True;
                        end
                        else if (Modus = d_VonBis) then begin
                          GetUnixSekundenFromUnixTimeStr (pDSfGRohRec.UTime, iUnixTime);
                          if (iUnixTime < GetUnixSekundenFromDateTime (Abruf_bis)) and
                             (iUnixTime < iLastUnixTime)
                          then begin
                            UnixTimeStrToDateTime (pDSfGRohRec.UTime, Abruf_von);
                            Abruf_von:=IncSecond(Abruf_von);
                          end else
                            Antwort_vollstaendig := True;
                        end;
                      end else
                        Antwort_vollstaendig := True;
                    end else
                      Antwort_vollstaendig := True;
                  finally
                    pDEList.Free;
                    pSl.Free;
                  end;
                end; { while not Antwort_vollstaendig }
              end
              else if (Datentyp = C_IsDatenelemente) then begin
                Antwort_vollstaendig := True;
                pDEList := TDSfGTelegrDEList.Create(True);
                try
                  j := 1;
                  sDea := Trim(GetStringPart(DEL_von, j, GS));
                  while (sDea <> '') do begin
                    with pDSfGRohRec do begin
                      Adresse := sDea;
                      Wert := GetDatenelementWert_Simulation (Adresse);  // 14.01.2015, WW
                    end;
                    Inc(j);
                    sDea := Trim(GetStringPart(DEL_von, j, GS));
                    pDEList.Add(TDSfGTelegrDEObj.Create(pDSfGRohRec));
                  end;
                  if (DEL_bis <> '') then begin
                    with pDSfGRohRec do begin
                      Adresse := DEL_bis;
                      Wert := GetDatenelementWert_Simulation (Adresse);  // 14.01.2015, WW
                    end;
                    pDEList.Add(TDSfGTelegrDEObj.Create(pDSfGRohRec));
                  end;

                  { Wartezeit zum Simulieren der Gerätereaktionszeit: }
                  if FSimuDSfGKommunikationZeiten.AntwortTelegramm > 0 then
                    Delay (FSimuDSfGKommunikationZeiten.AntwortTelegramm);  // 14.01.2015, WW

                  // DSfG-Antwort-Telegramm erstellen
                  if (DEL_bis <> '') then cDEB := 'M' else cDEB := 'V';
                  R.Antwort :=
                    GetAntwortTelegramm_Anfrage(EAdr, '_', cDEB, pDEList);

                  if DebugCOMProtokoll OR Assigned (ResponseTraceLogList) then begin
                    sLog:=CharStrToString(R.Antwort);
                    // Protokollierung in COM-Log
                    if (DebugCOMProtokoll) then
                      WriteErrorLog(sLog, sCOMLogFilename, elt_Info);
                    // COM-Tracelog; 24.02.2020, WW
                    if Assigned (ResponseTraceLogList) then
                      WriteCOMTraceLog(sLog);
                  end;

                  sRohFileName := CreateTempRohFile(FWorkPath, prefix_DSfG_Roh);
                  WriteRohfile(sRohfileName, R.Antwort);
                  { in Rohdatenfile-Liste eintragen: }
                  RohfileListe.Add(sRohFileName);
                finally
                  pDEList.Free;
                end;
              end;
            except
              on E: Exception do begin
                WriteErrorLog ('!!! ' + sDebug + ': ' +  E.Message + ' !!!',
                               '', elt_Error, FLogPath);  { Logfile-Protokollierung }
              end;
            end;
{$ENDIF}
          end;

          { COM-Tracelog für DSfG-Busadresse in TraceLog-Liste eintragen; 24.02.2020, WW }
          AddCOMTraceLogToList (ResponseTraceLogList, EAdr);

          { Prüfen, ob Kommando-gesteuert Signaturen verifiziert werden sollen
            oder nicht: }
          bDoSigVerify_Request:=not ((sPublicKey_x = CPubKey_DoNotVerify) AND
                                     (sPublicKey_y = CPubKey_DoNotVerify));

          { Dateien mit kodierten Rohdaten für Response erstellen, wenn Signatur
            freigeschaltet ist, Rohdaten in Response per Konfiguration aktiviert
            sind und Kommando-gesteuert verifiziert werden soll: }
          if FSignatur_freigeschaltet AND (FXMLResponseEncodeRohdaten > 0) AND
             bDoSigVerify_Request AND (Assigned (ResponseRohdatenList)) then begin
            if not CreateRohdatenFiles_XMLResponse (RohfileListe, ResponseRohdatenList,
                                                    EAdr, DEL) then begin  // DEL von abgefragter AG, AK oder LB
              FehlerGruppeCodeUpdate (ST_FILEERROR, FILEERR_COULDNOTWRITE);
              KonvFehler:=true;
            end;
          end;

          { Den abgefragten Daten zugeordneten öffentlichen Schlüssel im
            Signaturclient-Objekt setzen: 15.03.2012, WW }
          if Assigned (FSignaturClient) then
            FSignaturClient.SetPublicKey (sPublicKey_x, sPublicKey_y);

          { Prüfen, ob der öffentliche Schlüssel vorliegt; 13.03.2018, WW }
          bPublicKey_vorhanden:=(sPublicKey_x <> CPubKey_DoNotVerify) AND
                                (sPublicKey_y <> CPubKey_DoNotVerify) AND
                                (sPublicKey_x <> '') AND
                                (sPublicKey_y <> '');

          { alle einer Busadresse und DE-Adresse zugehörigen Teil-Antworten konvertieren: }
          if (Datentyp = C_IsArchive) OR (Datentyp = C_IsLogbuecher) then begin
            { Archiv/Logbuch-Rohdatenfiles -> strukturierte Datei }
            DArchivLogbuchKonv:=TDArchivLogbuchKonv.Create;
            try
              if bDoSigVerify_Request then
                DArchivLogbuchKonv.SignaturClient:=FSignaturClient
              else
                DArchivLogbuchKonv.SignaturClient:=nil;  // es soll Kommando-gesteuert
                                                         // NICHT verifiziert werden; 07.04.2014, WW
              Erg:=DArchivLogbuchKonv.KonvertToFile (RohfileListe, Datentyp = C_IsArchive,
                                                     EAdr, DEL,
                                                     RohdatenLoeschen,
                                                     FSignatur_freigeschaltet,
                                                     bPublicKey_vorhanden,
                                                     KonvFileName,
                                                     AFehlergruppe, AFehlercode);
              { Kanal-/Logbuch-Konvertierungsergebnis in Responselog-Liste eintragen: }
              if ResponseLogList <> nil then
                ResponseLogList.InsertDataKonvLog (EAdr, DEL, Erg);  // DEL von abgefragter AG, AK oder LB
            finally
              DArchivLogbuchKonv.Free;
            end;

            { Name der Konvertierungs-Datei mit Busadresse und DE-Adresse in Archiv/Logbuch-DatenListe eintragen: }
            if ArLbDatenListe <> nil then begin
              DSfGDataListObj:=TDSfGDataListObj.Create;
              DSfGDataListObj.SetData (EAdr, DEL);  // DEL von abgefragter AG, AK oder LB
              ArLbDatenListe.AddObject (KonvFileName, DSfGDataListObj);
            end;
          end
          else begin
            { Datenelemente-Rohdatenfiles -> DELList }
            if DELList <> nil then begin
              if bDoSigVerify_Request then
                DELList.SignaturClient:=FSignaturClient  // 04.01.2013, WW
              else
                DELList.SignaturClient:=nil;  // es soll Kommando-gesteuert
                                              // NICHT verifiziert werden; 07.04.2014, WW
              Erg:=DELList.LoadFromRohfileList (RohfileListe, EAdr, RohDatenloeschen,
                                                bPublicKey_vorhanden,
                                                AFehlergruppe, AFehlercode); 

              { Wert für virtuelles Gerätezustand-DE bilden und in DELList
                eintragen: 12.02.2014, WW }
              if length (sVirtualDEA) > 0 then
                Erg:=DELList.LoadGeraeteZustand (EAdr, sVirtualDEA);

              { Datenelemente-Konvertierungsergebnis in Responselog-Liste eintragen: }
              if ResponseLogList <> nil then
                ResponseLogList.InsertDataKonvLog (EAdr, '', Erg);
            end;
          end;

          { Fehlergruppe/-code aus Konvertierung aktualisieren:
            -> Signatur-Lizenz berücksichtigen; 07.04.2014, WW }
          if not FehlerGruppeCode_OK (AFehlergruppe, AFehlercode) then begin
            if bDoSigVerify_Request AND IsSignaturFehler (AFehlergruppe) then begin
              if (not FSignatur_freigeschaltet) then
                FehlerGruppeCodeUpdate (SYS_SIGNSRV_ERROR, SIGNSRVERR_LICENCE)
              else
                FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
              KonvFehler:=true;
            end
            else if not bDoSigVerify_Request AND IsSignaturFehler (AFehlergruppe) then begin
              // Keinen Fehler ausgeben (Fehler "Signatur nicht verifiziert" unterdrücken)
            end
            else begin
              FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
              KonvFehler:=true;
            end;
          end
          else begin  // OK
            if bDoSigVerify_Request AND (not FSignatur_freigeschaltet) then begin
              FehlerGruppeCodeUpdate (SYS_SIGNSRV_ERROR, SIGNSRVERR_LICENCE);
              KonvFehler:=true;
            end;
          end;

        finally
          RohfileListe.Free;
        end;
      end;  { if length }

      if Abbruch then
        Break  { Stop, keine weiteren Telegramme; 10.07.2007 WW }
      else
        inc(i);
    end;  { while (i <= DSfGAbrufListe.Count - 1) AND (not NoCarrier) }
  finally
    DSfGAbrufListe.Free;
  end;

  { bei fehlerhaften Rohdaten:
    -> umgestellt auf ST_DATACHECK statt ST_KONVERROR, da Ergebnis aus Rohfile-
       Checkroutine; 14.03.2007 WW }
  if RohdatenFehler then begin
    case Datentyp of
      C_IsArchive:       FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_ARINVALID);
      C_IsLogbuecher:    FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_LBINVALID);
      C_IsDatenelemente: FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_DEINVALID);
    end;
  end;

  if not Abbruch AND not NoCarrier AND not DSfGFehler AND not Rohdatenfehler AND
     not KonvFehler then
    Result:=true;
End;

{------------------------------------------------------------------------}
procedure TDSfGAbruf.AddKonfiguration_LoginDfue_Typ_D (DELList: TDELList);
{------------------------------------------------------------------------}
{ Instanzdaten der Login-DFÜ vom Typ D in Datenelemente-Liste schreiben;
  Übergabe/Rückgabe: Datenelemente-Liste }
begin
  if DELList <> nil then begin
    { nur Instanztyp unter "Pseudo-Datenelementadresse" aaa eintragen: }
    DELList.Eintragen (DSfGDfueKonfigData.EAdr_Dfue, CD_ALLG_eigInstanztyp, C_D_Instanztyp_DFU);
  end;
end;

{----------------------------------------------------------------------------------------}
function TDSfGAbruf.AbrufKonfiguration_Allgemein (EAdr: char; DELList: TDELList): boolean;
{----------------------------------------------------------------------------------------}
{ Abrufen der allgemeinen Konfigurationsdaten einer DSfG-Instanz;
  Übergabe: Busadresse der Instanz
  Rückgabe: Datenelemente-Liste
  Ergebnis: true, wenn Abruf der allgemeinen Konfigurationsdaten ok }
var
  Befehl: string;
  R: TRueckgabe;
  DELs: string;
  Erg: integer;
  RohfileListe: TDSfGDataList;
  sRohsatz_ungueltig_Merk: string;
  Antwort_vollstaendig: boolean;
  dummy: TDateTime;
  AFehlergruppe, AFehlercode: integer;
  Versuche: byte;

begin
  Result:=false;

  DELs:=CD_ALLG_eigInstanztyp + GS + CD_ALLG_Hersteller + GS + CD_ALLG_Geraetetyp + GS + CD_ALLG_FabrikNr + GS +
        CD_ALLG_Baujahr + GS + CD_ALLG_SWVersion + GS + CD_ALLG_Inbetriebnahme;

  RohfileListe:=TDSfGDataList.Create;         { Liste für Rohdateinamen }
  try
    sRohsatz_ungueltig_Merk:='';  // kein gemerkter Ungültig-Rohsatz vorhanden
    Antwort_vollstaendig:=false;
    Versuche:=0;
    while (not Antwort_vollstaendig) AND (not NoCarrier) do begin { evtl. mehrere Abrufe bei ungültigen Rohdaten nötig }
      Application.ProcessMessages;
      { Befehl zusammensetzen: }
      IncrementTID;
      Befehl:=Get_DatenelementeBefehl(EAdr, DELs, '');

      { Allg. Konfigurationsdatenabruf-Kommando senden }
      if not CommObj.SendCommand (Befehl, [FS], 1, DSfGTimeouts.Datenelemente, ad_File, R, NoCarrier) then begin
        { DSfG-Fehler oder Timeout -> Teilnehmer wird als "nicht erreicht" behandelt
                                   -> Konfiguration-Einlesen OK
                                   -> Warnung, wenn Teilnehmerliste vorhanden ist }
        if R.Fehlergruppe = ST_DSFGERROR then begin
          { Anm. WW: CAN kann auftreten bei Wieser-DFÜs, wenn ein Teilnehmer nicht
                     erreicht wird (plausibel). ENQ kann auftreten unter bestimmten
                     (bislang nicht bekannten) Umständen bei FlowComp-DFÜs (irgendwie
                     nicht plausibel, es liegt ja kein formaler Fehler im Sendebefehl vor !). }
          if DSfGDfueKonfigData.Wieser_Teilnehmer <> '' then  { Warnung, wenn Teilnehmerliste vorhanden, sonst ok }
            FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
          Result:=true;
        end
        else if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) then begin
          if DSfGDfueKonfigData.Wieser_Teilnehmer <> '' then  { Warnung, wenn Teilnehmerliste vorhanden, sonst ok }
            FehlerGruppeCodeUpdate (ST_DSFGERROR, DSFGERR_CAN);     { "Teilnehmer nicht vorhanden" }
          Result:=true;
        end else
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;

      if length (EAdr_VerbHalten) = 0 then
        EAdr_VerbHalten:=EAdr;  // EAdr für "Verbindung halten"-Befehl; 02.05.2013, WW

      { Rohfile prüfen (Antwort = Rohfilename), bei ungültiger Antwort
        DE-Adressen für Folgebefehl zurückgeben: }
      Erg:=CheckRohfile_Antworttelegramm (R.Antwort, EAdr, '', d_DelAdr, false,
                                          Antwort_vollstaendig, DELs, dummy,
                                          sRohsatz_ungueltig_Merk);
      if Erg = DSFGKONVERR_ZAE_SOLL_IST then begin  { ZAE-Soll/Ist-Fehler; 14.03.2007, WW }
        if Versuche < CMaxVersuche then begin  // max. Anzahl an Versuchen noch nicht erreicht
          inc (Versuche);
          Continue;  // nochmal gleicher Befehl
        end;
      end;
      Versuche:=0;  // Anzahl der Versuche rücksetzen

      if Erg <> DSFGKONVERR_OK then begin           { Fehler im Rohfile }
        FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_DEINVALID);
        exit;
      end;

      { in Rohdatenfile-Liste eintragen: }
      RohfileListe.Add (R.Antwort);
    end; { while not Antwort_vollstaendig }

    { Datenelemente-Rohdatenfiles -> DELList }
    if DELList <> nil then begin
      Erg:=DELList.LoadFromRohfileList (RohfileListe, EAdr, RohDatenloeschen,
                                        false,  // unbenutzt, Signatur spielt hier keine Rolle; 13.03.2018, WW
                                        AFehlergruppe, AFehlercode);
      if not FehlerGruppeCode_OK (AFehlergruppe, AFehlercode) then begin
        if not IsSignaturFehler (AFehlergruppe) then begin  // Signaturfehler ignorieren; 17.05.2013, WW
          if (Erg >= C_MinCode_DSfGKonvErr_AA) then begin  { außerplanmäßige Antwort }
            FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_KONFIG);  // 20.07.2004, WW
            // spezieller Fehlercode für Stammdateneinlesen, 22.02.2005, WW
            Result:=true;
          end else
            FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_INSTWERTKONV);
          exit;
        end;
      end;
    end;
  finally
    RohfileListe.Free;
  end;

  Result:=true;
end;

{----------------------------------------------------------------------------------------}
function TDSfGAbruf.AbrufKonfiguration_ZeitAbruf(cEAdr: char;
  var sUnixTime, sTimeBias: string; EST_Fehlergruppe: integer): boolean;  // 17.08.2010
{----------------------------------------------------------------------------------------}
{ Abrufen der Zeitdaten einer DSfG-Instanz;
  Übergabe: Busadresse der Instanz
            Erweiterte Fehlergruppe (Offset zu allgemeiner Fehlergruppe)
  Rückgabe: Zeit und Zeitzone
  Ergebnis: true, wenn Abruf ok }
var
  Befehl: string;
  R: TRueckgabe;
  DELs: string;
  Erg: integer;
  RohfileListe: TDSfGDataList;
  sRohsatz_ungueltig_Merk: string;
  Antwort_vollstaendig: boolean;
  dummy: TDateTime;
  AFehlergruppe, AFehlercode: integer;
  Versuche: byte;

begin
  Result:=false;

  DELs:=CD_ALLG_DatumUhrzeit + GS + CD_ALLG_Zeitzone;

  RohfileListe:=TDSfGDataList.Create;         { Liste für Rohdateinamen }
  try
    sRohsatz_ungueltig_Merk:='';  // kein gemerkter Ungültig-Rohsatz vorhanden
    Antwort_vollstaendig:=false;
    Versuche:=0;
    while (not Antwort_vollstaendig) AND (not NoCarrier) do begin { evtl. mehrere Abrufe bei ungültigen Rohdaten nötig }
      Application.ProcessMessages;
      { Befehl zusammensetzen: }
      IncrementTID;
      Befehl:=Get_DatenelementeBefehl(cEAdr, DELs, '');

      { Allg. Konfigurationsdatenabruf-Kommando senden }
      if not CommObj.SendCommand (Befehl, [FS], 1, DSfGTimeouts.Datenelemente, ad_File, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (EST_Fehlergruppe + R.Fehlergruppe, R.Fehlercode);  // 13.02.2012, WW
        exit;
      end;

      { Rohfile prüfen (Antwort = Rohfilename), bei ungültiger Antwort
        DE-Adressen für Folgebefehl zurückgeben: }
      Erg:=CheckRohfile_Antworttelegramm (R.Antwort, cEAdr, '', d_DelAdr, false,
                                          Antwort_vollstaendig, DELs, dummy,
                                          sRohsatz_ungueltig_Merk);
      if Erg = DSFGKONVERR_ZAE_SOLL_IST then begin  { ZAE-Soll/Ist-Fehler; 14.03.2007, WW }
        if Versuche < CMaxVersuche then begin  // max. Anzahl an Versuchen noch nicht erreicht
          inc (Versuche);
          Continue;  // nochmal gleicher Befehl
        end;
      end;
      Versuche:=0;  // Anzahl der Versuche rücksetzen

      if Erg <> DSFGKONVERR_OK then begin           { Fehler im Rohfile }
        FehlerGruppeCodeUpdate (EST_Fehlergruppe + ST_DATACHECK, DCH_DEINVALID);  // 13.02.2012, WW
        exit;
      end;

      { in Rohdatenfile-Liste eintragen: }
      RohfileListe.Add (R.Antwort);
    end; { while not Antwort_vollstaendig }

    { Datenelemente-Rohdatenfiles -> DELList }
    with TDELList.Create do
    try
      LoadFromRohfileList (RohfileListe, cEAdr, RohDatenloeschen,
        false,  // unbenutzt, Signatur spielt hier keine Rolle; 13.03.2018, WW
        AFehlergruppe, AFehlercode);
      GetWert(cEAdr, CD_ALLG_DatumUhrzeit, sUnixTime);
      GetWert(cEAdr, CD_ALLG_Zeitzone, sTimeBias);
    finally
      Free;
    end;
    { Fehlergruppe/-code aus Konvertierung aktualisieren: }
    if not FehlerGruppeCode_OK (AFehlergruppe, AFehlercode) then begin
      if not IsSignaturFehler (AFehlergruppe) then begin  // Signaturfehler ignorieren; 17.05.2013, WW
        FehlerGruppeCodeUpdate (EST_Fehlergruppe + AFehlergruppe, AFehlercode);  // 17.05.2013, WW
        exit;
      end;
    end;
  finally
    RohfileListe.Free;
  end;

  Result:=true;                                 
end;

{------------------------------------------------------------------}
Function TDSfGAbruf.AbrufKonfiguration (DELList: TDELList): boolean;
{------------------------------------------------------------------}
{ Abrufen der Konfigurationsdaten einer DSfG-Station (nur ausgewählte Datenelemente
  der Gruppe 'Allgemein' aller Instanzen);
  Übergabe/Rückgabe: Datenelemente-Liste
  Ergebnis: true, wenn Abruf der Konfigurationsdaten ok }
var
  EAdr: char;
  Adressen: string;
  i: integer;

begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);         { Vorbelegung Fehlergruppe/-code: OK }

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  { Instanzdaten der Login-DFÜ in Datenelement-Liste schreiben, wenn vom
    Typ D (Extensionmode der DFÜ bestimmt den Instanztyp): }
  if DSfGDfueKonfigData.Extensionmode < 2 then  // 07.05.2018, WW
    AddKonfiguration_LoginDfue_Typ_D (DELList);

  { allgemeine Instanz-Stammdaten auslesen:
    bei Wieser-DFÜ nur die in der Teilnehmerliste enthaltenen Instanzen abfragen (ohne DSfG-DFÜ-Adressen, wenn bekannt),
    ansonsten alle möglichen Busadressen anpollen (Teilnehmerliste leer) }
  if DSfGDfueKonfigData.Wieser_Teilnehmer <> '' then begin
    Adressen:=DSfGDfueKonfigData.Wieser_Teilnehmer;
    for i:=Low (DSfGDfueKonfigData.Wieser_Adresse) to High (DSfGDfueKonfigData.Wieser_Adresse) do
      if DSfGDfueKonfigData.Wieser_Adresse[i] <> '' then begin
       { Die Adresse der Login-DFÜ nur ausfiltern, wenn DFÜ vom Typ D ist (Erweiterungsgrad < 2): }
        if (DSfGDfueKonfigData.Wieser_Adresse[i] <> DSfGDfueKonfigData.EAdr_Dfue) OR
           ((DSfGDfueKonfigData.Wieser_Adresse[i] = DSfGDfueKonfigData.EAdr_Dfue) AND
            (DSfGDfueKonfigData.Extensionmode < 2)) then  // 07.05.2018, WW
        Adressen:=StrFilter (Adressen, DSfGDfueKonfigData.Wieser_Adresse[i][1]);
      end;
  end else
    Adressen:=CMaxDSfGTeilnehmer;

  for i:=1 to length (Adressen) do begin  { alle Adressen der Teilnehmerliste abfragen }
    Application.ProcessMessages;
    EAdr:=Adressen[i];

    { Von der Login-DFÜ vom Typ D können keine Daten angefordert werden, vom Typ E schon; 07.05.2018, WW }
    if (EAdr <> DSfGDfueKonfigData.EAdr_Dfue) OR
       ((EAdr = DSfGDfueKonfigData.EAdr_Dfue) AND
        (DSfGDfueKonfigData.Extensionmode >= 2)) then begin  // 07.05.2018, WW
      if not AbrufKonfiguration_Allgemein (EAdr, DELList) then exit;
    end;
  end;

  Result:=true;
end;

{---------------------------------------------------------------------------------}
procedure TDSfGAbruf.ZeitSynch_WieserDfue (ZeitSyncCmdData: TDSfG_ZeitSyncCmdData;
                                           var ZeitSyncInfoData: TZeitSyncInfoData;
                                           var ZS_Fehlergruppe: integer;
                                           var ZS_Fehlercode: integer);
{---------------------------------------------------------------------------------}
{ Zeit in der Wieser/RMG DSfG-DFÜ auf PC-Zeit setzen (YWC-Befehl);
  Übergabe: DSfG-Zeitsynchronisations-Kommandodaten
  Rückgabe: Infodaten zur Zeitsynchronisation
            Fehlergruppe/code aus Zeitsynchronisation }
Const
  C_Sicherheitsabstand = 30; { s }  { keine Sync in diesem Zeitraum vor einer vollen Stunde }
  C_KorrGetDSfGZeit = 250; { ms }   { ungefährer Korrekturwert für ausgelesene DSfG-Zeit  }
  C_KorrSetDSfGZeit = 750; { ms }   { ungefährer Korrekturwert für Übertragung der neuen DSfG-Zeit }

var
  R: TRueckgabe;
  Befehl: string;
  PCDateTime: TDateTime;
  DSfGDateTime: TDateTime;
  NewDSfGDateTime: TDateTime;
  DSfGHour, PCHour: word;
  dummy1, dummy2, dummy3: word;
  Naechste_volle_Stunde_DateTime: TDateTime;
  DiffSec: longint;
  DiffSec_StdWechsel: longint;
  DSfGTimeStr, DSfGDateStr: string;
  NextSZToWZ: TDateTime;
  NextSZToWZHour: word;
  dummy4, dummy5: string;
  ZeitSync_erfolgreich: boolean;
  AktSZ_WZ_Flag: integer;
  SZ_WZ_Umstellung_im_Geraet: boolean;
  AFehlergruppe: integer;
  AFehlercode: integer;
  DfueParaList: TDfueParaList;   { Zwischenpuffer für DFÜ-Parameter-Konvertierung }
  i: integer;
  ParaNr: string;
  DTBuf: TDateTime;
  sUtcOffsetDev: string;
  bResourceOK: boolean;

begin
  with ZeitSyncInfoData do begin   { Vorbelegung Rückgabe: Keine ZeitSync-Infodaten vorhanden }
    DZ_Server:=0;
    DZ_Geraet:=0;
  end;

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR+COM_KOMMERROR;
    ZS_Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
    exit;
  end;

  Befehl:=STX+'YWB002'+ETX;
  { Kommando für "Datum abfragen" senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.DFUEParameter, ad_String, R, NoCarrier) then begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR+R.Fehlergruppe;
    ZS_Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort auf Kommando für "Datum abrufen" auswerten: }
  if not ValidDSfGDfueAntwort (true, R.Antwort, AFehlergruppe, AFehlercode) then begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR+AFehlergruppe;
    ZS_Fehlercode:=AFehlercode;
    exit;
  end;

  { Rohantwort konvertieren: }
  DSfGDateStr:='';
  DfueParaList:=TDfueParaList.Create;
  try
    DfueParaList.LoadFromRohstring_B (R.Antwort, '');
    for i:=0 to DfueParaList.Count-1 do begin
      ParaNr:=TDfueParaListObj (DfueParaList[i]).ParaAdr;
      if StrToIntDef (ParaNr, -1) = CDDfu_Datum then
        DSfGDateStr:=TDfueParaListObj (DfueParaList[i]).Wert;
    end;
  finally
    DfueParaList.Free;
  end;

  Befehl:=STX+'YWB001'+ETX;
  { Kommando für "Zeit abfragen" senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.DFUEParameter, ad_String, R, NoCarrier) then begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR+R.Fehlergruppe;
    ZS_Fehlercode:=R.Fehlercode;
    exit;
  end;

  { PC-Zeit ermitteln:
    -> um sie mit der Gerate-Zeit vergleichen zu können, wird die PC-Zeit gleich auf
       die in der DSfG-DFÜ herrschende Zeitzone umgerechnet }
  bResourceOK:=false;
  if Assigned (FWZ_SZResourceList) then
    { Zeitzoneninfo für PC-Zeit ermitteln: }
    bResourceOK:=FWZ_SZResourceList.FindSZ_WZ_Info (AktSZ_WZ_Flag, NextSZToWZ);  // 06.08.2021, WW
  if not bResourceOK then begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR+ST_KONFIGERROR;
    ZS_Fehlercode:=KFERR_KONFIGDATANOTFOUND;
    exit;
  end;
  
  SZ_WZ_Umstellung_im_Geraet:=Copy (ZeitSyncCmdData.Zeitbasis_Geraet, 1, 1) = C_CmdZSyncBasis_S;
  PCDateTime:=Now + GetWZ_SZ_Offset (AktSZ_WZ_Flag, SZ_WZ_Umstellung_im_Geraet);

  // ab 24.02.2020, WW: Optionalen UTC-Normalzeit-Offset der DSfG-DFÜ in PC-Zeit einrechnen
  sUtcOffsetDev:=Copy (ZeitSyncCmdData.Zeitbasis_Geraet, 2, length (ZeitSyncCmdData.Zeitbasis_Geraet));
  PCDateTime:=PCDateTime + Get_DevicePC_TimezoneDiff (sUtcOffsetDev);

  { Antwort auf Kommando für "Zeit abrufen" auswerten: }
  if not ValidDSfGDfueAntwort (true, R.Antwort, AFehlergruppe, AFehlercode) then begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR+AFehlergruppe;
    ZS_Fehlercode:=AFehlercode;
    exit;
  end;

  { Rohantwort konvertieren: }
  DSfGTimeStr:='';
  DfueParaList:=TDfueParaList.Create;
  try
    DfueParaList.LoadFromRohstring_B (R.Antwort, '');
    for i:=0 to DfueParaList.Count-1 do begin
      ParaNr:=TDfueParaListObj (DfueParaList[i]).ParaAdr;
      if StrToIntDef (ParaNr, -1) = CDDfu_Zeit then
        DSfGTimeStr:=TDfueParaListObj (DfueParaList[i]).Wert;
    end;
  finally
    DfueParaList.Free;
  end;

  { aktuelle DSfG-Zeit bereitstellen:
    -> String in DateTime konvertieren
    -> Korrektur wegen Übertragungslaufzeit }
  EncodeDateStr (DSfGDateStr, 'YYMMDD', DSfGDateTime);
  EncodeTimeStr (DSfGTimeStr, 'HHMMSS', DTBuf);
  DSfGDateTime:=DSfGDateTime + DTBuf + EncodeTime (0, 0, 0, C_KorrGetDSfGZeit);

  { DSfG-Zeit und PC-Zeit: Rückgabe in ZeitSyncInfoData }
  with ZeitSyncInfoData do begin
    DZ_Server:=PCDateTime;
    DZ_Geraet:=DSfGDateTime;
  end;

  { Abweichung zwischen DSfG-Zeit und PC-Zeit ermitteln: }
  F_TimeDiff(PCDateTime, DSfGDateTime, DiffSec);
  DiffSec:=Abs(DiffSec);

  { ZeitSync nur, wenn Abweichung >= ZeitSyncMin, ansonsten alles OK (innerhalb Toleranz)
    und fertig: }
  if DiffSec < ZeitSyncCmdData.Abweichung_min then begin
    ZS_Fehlergruppe:=0;  { OK }
    ZS_Fehlercode:=0;
    exit;
  end;

  { ZeitSync nur, wenn Abweichung <= ZeitSyncMax: }
  if DiffSec > ZeitSyncCmdData.Abweichung_max then begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR;
    ZS_Fehlercode:=ZSYNCERR_HIGHERMAX;
    exit;
  end;

  { Abweichung liegt innerhalb des Zeitfensters -> es muß synchronisiert werden }

  { keine ZeitSynch zum Zeitpunkt der Umstellung von Sommer- auf Winterzeit wegen
    doppelt vorhandener Stunde: }
  DecodeTime (Now, PCHour, dummy1, dummy2, dummy3);
  DecodeTime (NextSZToWZ, NextSZToWZHour, dummy1, dummy2, dummy3);
  if (Int (PCDateTime) = Int (NextSZToWZ)) AND (PCHour = (NextSZToWZHour-1)) then begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR;
    ZS_Fehlercode:=ZSYNCERR_SZTOWZ;
    exit;
  end;

  { ZeitSync nur bei gleichem Datum und gleicher Stunde: }
  DecodeTime(DSfGDateTime, DSfGHour, dummy1, dummy2, dummy3);
  DecodeTime(PCDateTime, PCHour, dummy1, dummy2, dummy3);
  if not ((int (DSfGDateTime) = int (PCDateTime)) AND (DSfGHour = PCHour)) then begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR;
    ZS_Fehlercode:=ZSYNCERR_DIFFERENTHOURS;
    exit;
  end;

  { DSfG-Zeit und PC-Zeit müssen den Sicherheitsabstand zum nächsten
    Stundenwechsel einhalten: }
  Naechste_volle_Stunde_DateTime:=int (DSfGDateTime) +
                                  EncodeTime(DSfGHour, 0, 0, 0) +
                                  EncodeTime(1, 0, 0, 0);
  if DSfGDateTime < PCDateTime then
    F_TimeDiff(PCDateTime, Naechste_volle_Stunde_DateTime, DiffSec_StdWechsel)
  else
    F_TimeDiff(DSfGDateTime, Naechste_volle_Stunde_DateTime, DiffSec_StdWechsel);

  { ZeitSync nur bei genügend Sicherheitsabstand zum nächsten Stundenwechsel: }
  if DiffSec_StdWechsel <= C_Sicherheitsabstand then begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR;
    ZS_Fehlercode:=ZSYNCERR_PERIODEND;
    exit;
  end;

  { neue DSfG-Zeit bereitstellen:
    -> Korrektur der zu sendenden PC-Zeit wegen Übertragungslaufzeit
    -> in NewDSfGDateTime ist Stundenoffset berücksichtigt über PCDateTime }
  NewDSfGDateTime:=PCDateTime + EncodeTime (0, 0, 0, C_KorrSetDSfGZeit);
  DSfGTimeStr:=FormatDateTime ('hhnnss', NewDSfGDateTime);

  Befehl:=GetWieserDfueKommando_C (CDDfu_Zeit, DSfGTimeStr);
  { Kommando für "Zeit übertragen" senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Einstellen, ad_String, R, NoCarrier) then begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR+R.Fehlergruppe;
    ZS_Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort auf "Zeit übertragen"-Kommando auswerten: }
  if not ValidDSfGDfueAntwort (true, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    if (R.Fehlergruppe = COM_DSFGDFUERROR) AND
       (R.Fehlercode = DSFGDFUERR_WIESER_AENDEREUNGNICHTERLAUBT) then begin
      ZS_Fehlergruppe:=EST_ZEITSYNCERROR;
      ZS_Fehlercode:=ZSYNCERR_NOSUCCESS;
    end
    else begin
      ZS_Fehlergruppe:=EST_ZEITSYNCERROR+R.Fehlergruppe;
      ZS_Fehlercode:=R.Fehlercode;
    end;
    exit;
  end;

  { Prüfen, ob Zeit in der DSfG-DFÜ tatsächlich geändert wurde: }
  ZeitSync_erfolgreich:=true;
  if not DSfGDfueParameter_geaendert (6, R.Antwort, dummy4, dummy5) then begin
    if DiffSec > 1 then
      ZeitSync_erfolgreich:=false;
    { bei DiffSec = 1 liefert das Gerät evtl. Altwert = Neuwert. Das ist in diesem Fall
      aber kein Fehler, da wegen unvermeidbarer Ungenauigkeiten evtl. versucht wurde,
      die aktuelle Gerätezeit wieder zu parametrieren. }
  end;
  if ZeitSync_erfolgreich then begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR;
    ZS_Fehlercode:=ZSYNCERR_SUCCESS;
  end
  else begin
    ZS_Fehlergruppe:=EST_ZEITSYNCERROR;
    ZS_Fehlercode:=ZSYNCERR_NOSUCCESS;
  end;
end;

{----------------------------------------------------------------------------------------------------------}
function TDSfGAbruf.UebertragungDatenelement (ParaEinstellCmdData: TParaEinstellCmdData;
                                              var ParaEinstellResultData: TParaEinstellResultData): boolean;
{----------------------------------------------------------------------------------------------------------}
{ Datenelement in DSfG-Instanz übertragen;
  Übergabe: Parametrier-Kommandodaten
  Rückgabe: Parametrier-Ergebnisdaten
  Ergebnis: true, wenn Parametrierung erfolgreich }
Var
  Befehl: string;
  R: TRueckgabe;
  WertNeuIst: string;

begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }
  with ParaEinstellResultData do begin  { Vorbelegung Rückgabe }
    ParaTyp:=ParaEinstellCmdData.ParaTyp;  // 03.01.2022, WW
    BAdr:=ParaEinstellCmdData.BAdr;  // Busadresse; 03.01.2022, WW
    ParaAdr:='';
    WertAlt:='';
    WertNeu:='';
  end;

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (EST_PARAMERROR + COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  if length (ParaEinstellCmdData.BAdr) > 0 then begin
    Befehl:=Get_DatenelementEinstellBefehl(ParaEinstellCmdData.BAdr[1],
                                           ParaEinstellCmdData.ParaAdr,
                                           ParaEinstellCmdData.ParaWertNeu,
                                           ParaEinstellCmdData.ZCode1,
                                           ParaEinstellCmdData.ZCode2);

    { Datenelementeinstell-Kommando senden }
    if not CommObj.SendCommand (Befehl, [FS], 1, DSfGTimeouts.Einstellen, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_PARAMERROR + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { prüfen, ob Datenelement in der DSfG-Instanz tatsächlich geändert wurde: }
    if not DSfG_Datenelement_geaendert (ParaEinstellCmdData.BAdr[1],
                                        ParaEinstellCmdData.ParaAdr,
                                        ParaEinstellCmdData.ParaWertNeu,
                                        R.Antwort, WertNeuIst) then exit;

    { Ergebnis der erfolgreichen Parametrierung: }
    with ParaEinstellResultData do begin
      ParaAdr:=ParaEinstellCmdData.ParaAdr;
      WertAlt:='';  // leer, da Information aus DSfG-Telegramm nicht verfügbar
      WertNeu:=WertNeuIst;
    end;
    Result:=true;
  end;
end;

{------------------------------------------------------------------------------------}
Function TDSfGAbruf.AbrufDfueMomentanwerte (DSfG_AdresslistData: TDSfG_AdresslistData;
                                            DSfGDfueParaList: TDfueParaList;
                                            var EAdr_Dfue: string): boolean;
{------------------------------------------------------------------------------------}
{ ruft DSfG-DFÜ-Momentanwerte (Parameter, NTY-Masken) ab;
  Übergabe: DSfG-Adresslisten-Daten
  Rückgabe: DSfG-DFÜ-Parameterliste
            Busadresse der DSfG-DFÜ
  Ergebnis: true, wenn DFÜ-Momentanwerteabruf erfolgreich (Kommunikation) }
var
  R: TRueckgabe;
  Befehl: string;
  i: integer;
  DSfGAbrufListe: TDSfGAbrufList;
  isWieserDfueBefehl: boolean;
  Befehl_abrufen: boolean;
  bFertig: boolean;

begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }
  EAdr_Dfue:=DSfGDfueKonfigData.EAdr_Dfue;  { Rückgabe: Busadresse der Login-DFÜ }

  DSfGAbrufListe:=TDSfGAbrufList.Create;
  try
    // DSfG-Adressliste aus Abrufkommando (Befehle der abzurufenden DFÜ-Momentanwerte)
    // in AbrufListe konvertieren:
    DSfGAbrufListe.LoadFromXMLAdressliste (DSfG_AdresslistData.AdrList, true);

    i:=0;
    { DSfG-Abrufliste abarbeiten: }
    while (i <= DSfGAbrufListe.Count - 1) AND (not NoCarrier) do begin
      Application.ProcessMessages;
      { Befehl, über den ein oder mehrere bestimmte Momentanwerte abgerufen werden sollen: }
      Befehl:=TDSfGAbrufListObj (DSfGAbrufListe [i]).Data.DEL;  { DSfG-DFÜ-Befehl }
      if Befehl = 'YWB' then
        Befehl:='YWa';  // statt YWB-Befehl zuerst mit neuem YWa-Befehl versuchen abzurufen; 20.07.2011, WW

      isWieserDfueBefehl:=F_IsWieserDfueBefehl (Befehl);

      { Prüfen, ob Befehl bei DSfG-DFÜ abgerufen werden kann: }
      Befehl_abrufen:=false;
      if isWieserDfueBefehl AND (HerstellerChar = 'W') then
        { Wieser-spezifische Befehle nur bei Wieser-DSfG-DFÜ möglich }
        Befehl_abrufen:=true
      else if (Pos (Befehl, Dfue_Kommandos) > 0) OR (length (Dfue_Kommandos) = 0) then begin
        { Norm-Befehle nur möglich, wenn in DFÜ-Kommandoliste enthalten oder Liste leer ist }
        if (Befehl = 'D') OR (Befehl = 'U') OR (Befehl = 'V') then begin
          if Extensionmode_Dfue > 0 then  { wenn DSfG-DFÜ ab Erweiterungsgrad 1 vorliegt, werden... }
            Befehl_abrufen:=true;  { ...die Befehle D, U, V unterstützt }
        end else
          Befehl_abrufen:=true;
      end;

      if Befehl_abrufen then begin
        repeat
          if not CommObj.SendCommand (STX+Befehl+ETX, [ETX], 1, DSfGTimeouts.DFUEParameter,
                              ad_String, R, NoCarrier) then begin
            FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
            exit;
          end;

          bFertig:=true;
          if ValidDSfGDfueAntwort (isWieserDfueBefehl, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
            if Befehl = 'A' then
              DSfGDfueParaList.LoadFromRohstring_A (R.Antwort, Befehl)
            else if Befehl = 'YWB' then
              DSfGDfueParaList.LoadFromRohstring_B (R.Antwort, Befehl)
            else if Befehl = 'YWa' then
              DSfGDfueParaList.LoadFromRohstring_kleinA (R.Antwort, 'YWB')  // 20.07.2011, WW
            else
              DSfGDfueParaList.LoadFromRohstring_Allg (R.Antwort, Befehl);
          end
          else begin
            { kein Journal-Eintrag bei ungültiger Antwort auf Wieser-Befehl, da MRG910
              nicht alle Wieser-Befehle unterstützt ! }
            if not isWieserDfueBefehl then
              FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode)
            else begin
              if (R.Fehlergruppe = COM_DSFGDFUERROR) AND
                 (R.Fehlercode = DSFGDFUERR_WIESER_UNBEKANNT) then begin
                // wenn YWa-Befehl unbekannt, nochmal mit altem YWB-Befehl abrufen; 20.07.2011, WW
                if Befehl = 'YWa' then begin
                  Befehl:='YWB';
                  bFertig:=false;
                end;
              end;
            end;
          end;
        until bFertig;
      end;  { if Befehl_abrufen }

      inc(i);
    end;  { while DSfGAbrufListe }
  finally
    DSfGAbrufListe.Free;
  end;

  if not NoCarrier then
    Result:=true;
end; 

{-------------------------------------------------------------------------------------------------}
function TDSfGAbruf.UebertragungDfueParameter (ParaEinstellCmdData: TParaEinstellCmdData;
                                               var ParaEinstellResultData: TParaEinstellResultData;
                                               var EAdr_Dfue: string): boolean;
{-------------------------------------------------------------------------------------------------}
{ Parameter in DSfG-DFÜ-Instanz übertragen (echte Parameter, NTY-Masken);
  Übergabe: Parametrier-Kommandodaten
  Rückgabe: Parametrier-Ergebnisdaten
            Busadresse der parametrierten DSfG-DFÜ
  Ergebnis: true, wenn Parametrierung erfolgreich }
Var
  Befehl: string;
  R: TRueckgabe;
  WertAltIst: string;
  WertNeuIst: string;
  isWieserDfueBefehl: boolean;
  WertAltPos: integer;
  bNochmal: boolean;
  WertNeuSoll: string;
  ParaAdr_Befehl: string;
begin
  Result:=false;
  with ParaEinstellResultData do begin  { Vorbelegung Rückgabe }
    ParaTyp:=ParaEinstellCmdData.ParaTyp;  // 03.01.2022, WW
    BAdr:='';
    ParaAdr:='';
    WertAlt:='';
    WertNeu:='';
  end;
  EAdr_Dfue:=DSfGDfueKonfigData.EAdr_Dfue;  { Rückgabe: Busadresse der parametrierten Login-DFÜ }

  ParaAdr_Befehl:=ParaEinstellCmdData.ParaAdr;

  { Wieser-Parameter: erster Parametrierversuch mit Wert ohne abschließende Leerzeichen
    -> Für Parameter, deren Wert nicht auf volle Stellenzahl mit Space aufgefüllt
       werden muß/darf ! }
  if ParaEinstellCmdData.BAdr = 'YWB' then begin         { Wieser-Parameter }
    WertNeuSoll:=F_RightTrunc (ParaEinstellCmdData.ParaWertNeu, ' ');
    // YWC-Befehl braucht 3-stellige Parameternummern; 20.07.2011, WW
    ParaAdr_Befehl:=Copy (ParaAdr_Befehl, length (ParaAdr_Befehl) - 2, 3);  // die letzten 3 Stellen
  end else
    WertNeuSoll:=ParaEinstellCmdData.ParaWertNeu;

  bNochmal:=true;
  while bNochmal do begin
    bNochmal:=false;  // Default: nur 1 Parametrierversuch

    FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }
    if NoCarrier then begin                  { es besteht keine Verbindung mehr }
      FehlerGruppeCodeUpdate (EST_PARAMERROR + COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
      exit;
    end;

    Befehl:=Get_DfueEinstellbefehl (ParaEinstellCmdData.BAdr,
                                    ParaAdr_Befehl,
                                    WertNeuSoll);
    { Einstell-Kommando senden: }
    if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Einstellen, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Antwort auf "Einstell"-Kommando auswerten: }
    isWieserDfueBefehl:=F_IsWieserDfueBefehl (ParaEinstellCmdData.BAdr);
    if not ValidDSfGDfueAntwort (isWieserDfueBefehl, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
      FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);

      { wenn Antwort ungültig, erneuter Parametrierversuch mit Parameterwert wie
        in CmdData übergeben; 29.07.2009, WW }
      if WertNeuSoll <> ParaEinstellCmdData.ParaWertNeu then begin
        WertNeuSoll:=ParaEinstellCmdData.ParaWertNeu;
        bNochmal:=true;
        Continue;
      end else
        exit;
    end;

    { Prüfen, ob Wert in der DSfG-DFÜ tatsächlich geändert wurde: }
    if ParaEinstellCmdData.BAdr = 'A' then begin     { Änderung einer NTY-Maske }
      if not DSfGDfueNTYMaske_geaendert (ParaEinstellCmdData.ParaAdr, WertNeuSoll,
                                         R.Antwort, WertNeuIst) then begin
        FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
        exit;
      end;
      WertAltIst:='';  // leer, da Information aus A-Antwort nicht verfügbar
      Result:=true;
    end
    else begin                             { Änderung aller übrigen "Parameter" }
      { Stelle in der Antwort, an welcher der Alt-Wert beginnt, festlegen: }
      if ParaEinstellCmdData.BAdr = 'YWB' then          { Wieser-Parameter }
        WertAltPos:=6
      else
        WertAltPos:=3;
      if not DSfGDfueParameter_geaendert (WertAltPos, R.Antwort, WertAltIst,
                                          WertNeuIst) then begin
        FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);

        { wenn Parametrierung nicht erfolgreich war, erneuter Parametrierversuch
          mit Parameterwert wie in CmdData übergeben; 29.07.2009, WW }
        if WertNeuSoll <> ParaEinstellCmdData.ParaWertNeu then begin
          WertNeuSoll:=ParaEinstellCmdData.ParaWertNeu;
          bNochmal:=true;
        end;
      end else
        Result:=true;
    end;
  end;  { while bNochmal }

 { Ergebnis der erfolgreichen Parametrierung: }
  with ParaEinstellResultData do begin
    BAdr:=ParaEinstellCmdData.BAdr;
    ParaAdr:=ParaEinstellCmdData.ParaAdr;
    WertAlt:=WertAltIst;
    WertNeu:=WertNeuIst;
  end;
end;

{----------------------------------------------------------------------------}
function TDSfGAbruf.AbrufTransparent (TransparentCmdData: TTransparentCmdData;
                                      var TransparentAntw: string): boolean;
{----------------------------------------------------------------------------}
{ Senden eines beliebigen Befehls an DSfG-Instanz oder DSfG-DFÜ, Empfangen der Antwort;
  Übergabe: Transparent-Kommandodaten
  Rückgabe: Rohantwort auf Transparentbefehl
  Ergebnis: true, wenn Transparentbefehl-Abruf ok }
Var
  R: TRueckgabe;
  Endezeichen: char;
  isWieserDfueBefehl: boolean;
  TO_Transparent: integer;
  EndezeichenAnzahl: integer;

begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }
  TransparentAntw:='';             { Vorbelegung Rückgabe }

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  { Endezeichen abhängig vom Befehl: }
  if length (TransparentCmdData.Befehl) > 0 then
    Endezeichen:=TransparentCmdData.Befehl [length (TransparentCmdData.Befehl)]
  else
    Endezeichen:=FS;                                  { Standard-Belegung }

  { Timeout für Transparent-Kommando: }
  if TransparentCmdData.Timeout >= 0 then
    TO_Transparent:=TransparentCmdData.Timeout  // aus Transparent-Kommandodaten
  else
    TO_Transparent:=DSfGTimeouts.Binaerdatei;  // INI-Einstellung

  { Empfangsmodus für Transparent-Kommando: }
  if TransparentCmdData.EmpfModus = C_CmdEmpfModus_Telegr then  // es soll ein vollständiges Telegramm empfangen werden
    EndezeichenAnzahl:=1  // vollständiges Telegramm wird durch 1 Endezeichen begrenzt
  else  // es sollen alle Zeichen empfangen werden
    EndezeichenAnzahl:=100000;  // damit SendCommand in Timeout läuft, um alle Zeichen zu empfangen

  { Transparent-Kommando senden }
  if not CommObj.SendCommand (TransparentCmdData.Befehl, [Endezeichen], EndezeichenAnzahl,
                      TO_Transparent, ad_String, R, NoCarrier) then begin
    { Im Empfangsmodus 'Timeout' ist Timeout kein Fehler: }
    if not ((TransparentCmdData.EmpfModus <> C_CmdEmpfModus_Telegr) AND
            (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT)) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end;

  { nur Antwort auf Transparent-Kommando an DSfG-DFÜ auswerten: }
  if not Dfue_ist_transparent_geschaltet then begin  { wenn DSfG-DFÜ nicht transparent geschaltet ist }
    isWieserDfueBefehl:=F_IsWieserDfueBefehl (TransparentCmdData.Befehl);
    if not ValidDSfGDfueAntwort (isWieserDfueBefehl, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end;
  TransparentAntw:=R.Antwort;   { Rückgabe: Rohantwort }
  Result:=true;
end;

{-----------------------------------------------------------------------------------}
function TDSfGAbruf.RufEntgegennehmen_Modem_IP (var dtVerbSteht: TDateTime): boolean;
{-----------------------------------------------------------------------------------}
{ angekommenen Ruf per Modem oder IP entgegennehmen;
  Rückgabe: Zeitpunkt 'Verbindung steht'
  Ergebnis: true, wenn Rufentgegennahme ok }
var
  Befehl: string;
  R: TRueckgabe;
begin
  if CommObj is TDSfGModemCommObj then begin   { bei serieller DFÜ-Kommunikation per Modem }
    Result:=false;
    TDSfGModemCommObj (CommObj).SetDCDCheck (false);   { DCD-Überwachung aus }
    Befehl:='ata' + CR;
    { Rufannahmekommando senden: }
    if not TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, DSfGTimeouts.RufAnnahme, R) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Antwort auf Rufannahme-Kommando auswerten: }
    if not CheckModemConnect (R.Antwort) then exit;

    TDSfGModemCommObj (CommObj).SetDCDCheck (true);    { DCD-Überwachung ein }
  end;

  // bei IP hier keine Rufannahme-Aktion, Client-Verbindung ist schon hergestellt; 08.01.2018, WW

  dtVerbSteht:=Now;  // Zeitpunkt 'Verbindung hergestellt'; 20.12.2013, WW
  Result:=true;
end;

{------------------------------------------------------------------------------}
function TDSfGAbruf.RufEntgegennahme (var StationsKennungRet: string;
                                      var VerbInfoData: TVerbInfoData): boolean;
{------------------------------------------------------------------------------}
{ angekommenen Ruf entgegennehmen, Kennung abfragen;
  Rückgabe: Kennung der rufenden Station
            Verbindungsinformationen-Record
  Ergebnis: true, wenn Rufentgegennahme ok }
Begin
  Result:=false;
  { Rückgabe-Vorbelegung für Verbindungsinformationen: }
  with VerbInfoData do begin  // Keine Verbindungsinformationen vorhanden
    DZ_VerbindungSteht:=0;
    DZ_Login:=0;
  end;

  NoCarrier:=true;                 { Vorbelegung: es besteht keine Verbindung }
  FehlerGruppeCodeUpdate (0, 0, true);         { Vorbelegung Fehlergruppe/-code: OK }
  StationsKennungRet:='';
  try
    if not RufEntgegennehmen_Modem_IP (VerbInfoData.DZ_VerbindungSteht) then exit;

    { Ruf wurde entgegengenommen, Verbindung steht jetzt: }
    NoCarrier:=false;
    Delay (200);       { Wartezeit (generell empfehlenswert, nötig für MRG 910) }
    if not KennungAbfragen then exit;

    if Assigned (FSignaturClient) then
      FSignaturClient.LogInfo_Kennung:=StationsKennung;  // Kennung für Signaturclient-Protokollierung
  finally  // 14.06.2011, WW
    StationsKennungRet:=StationsKennung;
  end;
  Result:=true;
End;

{------------------------------------------------------------------------}
function TDSfGAbruf.Rufannahme (RufannahmeCmdData: TRufannahmeCmdData;
                                AufmTelegrammList: TAufmTelegrammList;
                                var RDeakt_RufNrZentrale_Alt: string;
                                var RDeakt_Fehlergruppe: integer;
                                var RDeakt_Fehlercode: integer;
                                var EAdr_Dfue: string;
                                var VerbInfoData: TVerbInfoData): boolean;
{------------------------------------------------------------------------}
{ eingegangenen DSfG-Ruf annehmen (Login durchführen, Bus transparent schalten);
  Übergabe: Rufannahme-Kommandodaten
  Rückgabe: Liste mit empfangenen Aufmerksamkeits-Telegrammen
            Rufnummer der Zentrale vor der Deaktivierung
            Fehlergruppe-/code der Rufdeaktivierung
            Busadresse der DSfG-DFÜ
            Verbindungsinformationen-Record
  Ergebnis: true, wenn Rufannahme erfolgreich }
Begin
  Result:=false;

  // Rückgabe-Vorbelegung für optionale Rufdeaktivierung in der DSfG-DFÜ:
  RDeakt_Fehlergruppe:=-1;       // Kein Rufdeaktivierungs-Ergebnis vorhanden
  RDeakt_Fehlercode:=-1;
  RDeakt_RufNrZentrale_Alt:='';  // Keine Rufnummer der Zentrale bekannt

  { Rückgabe-Vorbelegung für Verbindungsinformationen: }
  with VerbInfoData do begin  // Keine Verbindungsinformationen vorhanden
    DZ_VerbindungSteht:=0;
    DZ_Login:=0;
  end;

  NoCarrier:=false;                { Vorbelegung: Verbindung besteht bereits }
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }

  { Extensionmode 0 setzen (Voreinstellung): }
  CommObj.SetExtensionMode (0);
  if CommObj is TDSfGModemCommObj then
    TDSfGModemCommObj (CommObj).SetDCDCheck (true);     { DCD-Überwachung ein }

  { Paßwort übertragen: }
  if not PasswortLogin (RufannahmeCmdData.Passwort,
                        dtm_Ja,  // 11.07.2019, WW
                        VerbInfoData.DZ_Login) then exit;
  { Busadresse der Login-DFÜ ist erst nach dem Passwort-Login bekannt: }
  EAdr_Dfue:=DSfGDfueKonfigData.EAdr_Dfue;  { Rückgabe: Busadresse der Login-DFÜ }

  { Rufnummer der Zentrale in der DSfG-DFÜ deaktivieren, wenn lt. Kommandodaten
    gefordert: }
  if RufannahmeCmdData.DSfG_RufDeakt then
    RufDeaktivierung (RDeakt_RufNrZentrale_Alt, RDeakt_Fehlergruppe, RDeakt_Fehlercode);

  { Wieser-DSfG-DFÜ: aktive Teilnehmer ermitteln }
  if HerstellerChar = 'W' then
    if not GetWieserAktiveBusteilnehmer (dtm_Ja) then exit;  // bei Rufannahme immer Transparentmodus

  { DSfG-DFÜ transparent schalten und spontane Aufmerksamkeits-Telegramme lesen: }
  if not DFUETransparentSchalten (AufmTelegrammList) then exit;
  Result:=true;
End;

{ Zeitinformationen einer Instanz abrufen            }
{ Parameter: Instanzadresse, Unixzeit, Zeitzone     }
{ Rückgabe: Erfolg ja / nein                        }
{---------------------------------------------------}
function TDSfGAbruf.ZeitAbruf (cEADR: char; var sUnixTime, sTimeBias: string): boolean;  // 17.08.2010
{---------------------------------------------------}
begin
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK; 13.03.2018, WW }
  Result := AbrufKonfiguration_ZeitAbruf(cEADR, sUnixTime, sTimeBias,
                                         EST_ZEITBEFEHLERROR);
end;

{--------------------------------------------------------------------------}
procedure TDSfGAbruf.RufDeaktivierung (var RDeakt_RufNrZentrale_Alt: string;
                                       var RDeakt_Fehlergruppe: integer;
                                       var RDeakt_Fehlercode: integer);
{--------------------------------------------------------------------------}
{ Ruffunktion in der DSfG-DFÜ deaktivieren
  Rückgaben: Rufnummer der Zentrale vor der Deaktivierung
             Fehlergruppe-/code der Rufdeaktivierung }
var
  R: TRueckgabe;
  Befehl: string;
  RufNrZentrale_Neu: string;
  WertAltPos: integer;

begin
  { Vorbelegung Rückgabe: Rufnummer der Zentrale nicht vorhanden
    -> Leer-String kann als Vorbelegung verwendet werden, da die Station nie anrufen kann,
       wenn keine Zentrale-Rufnummer parametriert ist }
  RDeakt_RufNrZentrale_Alt:='';

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    RDeakt_Fehlergruppe:=EST_RUFDEAKTERROR+COM_KOMMERROR;
    RDeakt_Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
    exit;
  end;

  { "Rufnummer Zentrale" in der DSfG-DFÜ auf "N" parametrieren: }
  RufNrZentrale_Neu:='N';
  Befehl:=STX + 'R' + RufNrZentrale_Neu + ETX;

  { Kommando für "Rufnummer Zentrale übertragen" senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, DSfGTimeouts.Einstellen, ad_String, R, NoCarrier) then begin
    RDeakt_Fehlergruppe:=EST_RUFDEAKTERROR+R.Fehlergruppe;
    RDeakt_Fehlercode:=R.Fehlercode;
    exit;
  end;

  if not ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    RDeakt_Fehlergruppe:=EST_RUFDEAKTERROR+R.Fehlergruppe;
    RDeakt_Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Prüfen, ob die Rufnummer der Zentrale in der DSfG-DFÜ tatsächlich geändert wurde: }
  WertAltPos:=3;         { Stelle in der Antwort, an welcher der Alt-Wert beginnt }
  if not DSfGDfueParameter_geaendert (WertAltPos, R.Antwort,
                                      RDeakt_RufNrZentrale_Alt, RufNrZentrale_Neu) then begin
    RDeakt_Fehlergruppe:=EST_RUFDEAKTERROR;
    RDeakt_Fehlercode:=RUFDEAKTERR_NOSUCCESS;
    exit;
  end;

  RDeakt_Fehlergruppe:=EST_RUFDEAKTERROR;
  RDeakt_Fehlercode:=RUFDEAKTERR_SUCCESS;  { OK }
end;

{--------------------------------------}
function TDSfGAbruf.VerbHalten: boolean;
{--------------------------------------}
{ Befehl senden, um Verbindung zur Station zu halten;
  Ergebnis: true, wenn Senden des Haltebefehls ok }
var
  Befehl: string;
  R: TRueckgabe;
  EAdr: char;
  iTimeout: integer;

Begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);         { Vorbelegung Fehlergruppe/-code: OK }

  if (not C_TestFlag) then begin  // keine Simulation
    { Busadresse der Instanz, an die der "Verbindung halten"-Befehl gesendet werden soll: }
    if length (EAdr_VerbHalten) = 0 then
      EAdr:='A' // Default-Busadresse, falls noch keine erfolgreiche
                // Kommunikation mit einer Instanz erfolgte
    else
      EAdr:=EAdr_VerbHalten [1];

    { Befehl zusammensetzen: }
    IncrementTID;
    Befehl:=Get_DatenelementeBefehl(EAdr, CD_ALLG_eigInstanztyp, '');
    iTimeout:=30000;  // knapp unterhalb Flowcomp Modem-Timeout

    { Kommando zum Halten der Verbindung senden }
    if not CommObj.SendCommand (Befehl, [FS], 1, iTimeout, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end;
  Result:=true;
end;

{---------------------------------------------------------------------------------}
procedure TDSfGAbruf.ZeitSynchronisation (ZeitSyncCmdData: TZeitSyncCmdData;
                                          var ZeitSyncInfoData: TZeitSyncInfoData);
{---------------------------------------------------------------------------------}
{ Zeit in der Login-DSfG-DFÜ auf PC-Zeit setzen (Aufmerksamkeitstelegramm
  "Zeitsynchronisation");
  Übergabe: Zeitsynchronisations-Kommandodaten
  Rückgabe: Infodaten zur Zeitsynchronisation }
Const
  C_Sicherheitsabstand = 30; { s }  { keine Sync in diesem Zeitraum vor einer vollen Stunde }
  C_KorrGetDSfGZeit = 250; { ms }   { ungefährer Korrekturwert für ausgelesene DSfG-Zeit  }
  C_KorrSetDSfGZeit = 250; { ms }   { ungefährer Korrekturwert für Übertragung der neuen DSfG-Zeit }
  C_TimeDiffFenster = 3;  { s }     { Zeitdifferenz-Fenster, innerhalb dem die DSfG-Zeit auf
                                      PC-Zeit als synchronisiert angesehen wird }
var
  R: TRueckgabe;
  Befehl: string;
  PCDateTime: TDateTime;
  DSfGDateTime: TDateTime;
  NewDSfGDateTime: TDateTime;
  DSfGHour, PCHour: word;
  dummy1, dummy2, dummy3: word;
  Naechste_volle_Stunde_DateTime: TDateTime;
  DiffSec: longint;
  DiffSec_StdWechsel: longint;
  NextSZToWZ: TDateTime;
  NextSZToWZHour: word;
  ZeitSync_erfolgreich: boolean;
  AktSZ_WZ_Flag: integer;
  SZ_WZ_Umstellung_im_Geraet: boolean;
  sDSfGUnixTime: string;
  sDummy: string;
  cEADR: char;
  iTimeout: integer;
  dtWZ_SZ_Offset: TDateTime;
  dtCheckEnd: TDateTime;
  sUtcOffsetDev: string;
  bResourceOK: boolean;

begin
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: Abweichung < min. Abweichung (OK) }
  with ZeitSyncInfoData do begin   { Vorbelegung Rückgabe: Keine ZeitSync-Infodaten vorhanden }
    DZ_Server:=0;
    DZ_Geraet:=0;
  end;

  if length (ZeitSyncCmdData.BAdr_DSfG) > 0 then
    cEADR:=ZeitSyncCmdData.BAdr_DSfG [1]
  else
    // eratzweise Login-DFÜ-Adresse (Datenelemente abfragen geht prinzipiell ab Erweiterungsgrad 2): }
    cEADR:=DSfGDfueKonfigData.EAdr_Dfue;

  { Aktuelle Zeit aus DSfG-Instanz lesen: }
  if not AbrufKonfiguration_ZeitAbruf (cEADR, sDSfGUnixTime, sDummy,
                                       EST_ZEITSYNCERROR) then exit;
  { Prüfen, ob Zeit der DSfG-Instanz gelesen werden konnte: }
  if length (sDSfGUnixTime) = 0 then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_READDEVICETIME);
    exit;
  end;

  { PC-Zeit ermitteln:
    -> um sie mit der Gerate-Zeit vergleichen zu können, wird die PC-Zeit gleich auf
       die in der DSfG-DFÜ herrschende Zeitzone umgerechnet }
  bResourceOK:=false;
  if Assigned (FWZ_SZResourceList) then
    { Zeitzoneninfo für PC-Zeit ermitteln: }
    bResourceOK:=FWZ_SZResourceList.FindSZ_WZ_Info (AktSZ_WZ_Flag, NextSZToWZ);  // 06.08.2021, WW
  if not bResourceOK then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
    exit;
  end;

  SZ_WZ_Umstellung_im_Geraet:=Copy (ZeitSyncCmdData.Zeitbasis_Geraet, 1, 1) = C_CmdZSyncBasis_S;
  dtWZ_SZ_Offset:=GetWZ_SZ_Offset (AktSZ_WZ_Flag, SZ_WZ_Umstellung_im_Geraet);
  PCDateTime:=Now + dtWZ_SZ_Offset;

  // ab 24.02.2020, WW: Optionalen UTC-Normalzeit-Offset der DSfG-DFÜ in PC-Zeit einrechnen
  sUtcOffsetDev:=Copy (ZeitSyncCmdData.Zeitbasis_Geraet, 2, length (ZeitSyncCmdData.Zeitbasis_Geraet));
  PCDateTime:=PCDateTime + Get_DevicePC_TimezoneDiff (sUtcOffsetDev);

  { aktuelle DSfG-Zeit bereitstellen:
    -> Unix-String in DateTime konvertieren
    -> Korrektur wegen Übertragungslaufzeit }
  if not UnixTimeStrToDateTime (sDSfGUnixTime, DSfGDateTime) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_INVALIDDEVICETIME);
    exit;
  end;
  DSfGDateTime:=DSfGDateTime + EncodeTime (0, 0, 0, C_KorrGetDSfGZeit);

  { DSfG-Zeit und PC-Zeit: Rückgabe in ZeitSyncInfoData }
  with ZeitSyncInfoData do begin
    DZ_Server:=PCDateTime;
    DZ_Geraet:=DSfGDateTime;
  end;

  { Abweichung zwischen DSfG-Zeit und PC-Zeit ermitteln: }
  F_TimeDiff(PCDateTime, DSfGDateTime, DiffSec);
  DiffSec:=Abs(DiffSec);

  { ZeitSync nur, wenn Abweichung >= ZeitSyncMin, ansonsten alles OK (innerhalb Toleranz)
    und fertig: }
  if DiffSec < ZeitSyncCmdData.Abweichung_min then begin
    FehlerGruppeCodeUpdate (0, 0);  { OK }
    exit;
  end;

  { ZeitSync nur, wenn Abweichung <= ZeitSyncMax: }
  if DiffSec > ZeitSyncCmdData.Abweichung_max then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_HIGHERMAX);
    exit;
  end;

  { Abweichung liegt innerhalb des Zeitfensters -> es muß synchronisiert werden }

  { keine ZeitSynch zum Zeitpunkt der Umstellung von Sommer- auf Winterzeit wegen
    doppelt vorhandener Stunde: }
  DecodeTime (Now, PCHour, dummy1, dummy2, dummy3);
  DecodeTime (NextSZToWZ, NextSZToWZHour, dummy1, dummy2, dummy3);
  if (Int (PCDateTime) = Int (NextSZToWZ)) AND (PCHour = (NextSZToWZHour-1)) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_SZTOWZ);
    exit;
  end;

  { ZeitSync nur bei gleichem Datum und gleicher Stunde: }
  DecodeTime(DSfGDateTime, DSfGHour, dummy1, dummy2, dummy3);
  DecodeTime(PCDateTime, PCHour, dummy1, dummy2, dummy3);
  if not ((int (DSfGDateTime) = int (PCDateTime)) AND (DSfGHour = PCHour)) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_DIFFERENTHOURS);
    exit;
  end;

  { DSfG-Zeit und PC-Zeit müssen den Sicherheitsabstand zum nächsten
    Stundenwechsel einhalten: }
  Naechste_volle_Stunde_DateTime:=int (DSfGDateTime) +
                                  EncodeTime(DSfGHour, 0, 0, 0) +
                                  EncodeTime(1, 0, 0, 0);
  if DSfGDateTime < PCDateTime then
    F_TimeDiff(PCDateTime, Naechste_volle_Stunde_DateTime, DiffSec_StdWechsel)
  else
    F_TimeDiff(DSfGDateTime, Naechste_volle_Stunde_DateTime, DiffSec_StdWechsel);

  { ZeitSync nur bei genügend Sicherheitsabstand zum nächsten Stundenwechsel: }
  if DiffSec_StdWechsel <= C_Sicherheitsabstand then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_PERIODEND);
    exit;
  end;

  { neue DSfG-Zeit bereitstellen:
    -> Korrektur der zu sendenden PC-Zeit wegen Übertragungslaufzeit
    -> in NewDSfGDateTime ist Stundenoffset berücksichtigt über PCDateTime }
  NewDSfGDateTime:=PCDateTime + EncodeTime (0, 0, 0, C_KorrSetDSfGZeit);

  { Aufmerksamkeitstelegramm "Zeitsynchronisation" als Rundsendung schicken: }
  Befehl:=Get_ZeitSynchAufmerksamkeitsTelegramm ('@', NewDSfGDateTime);
  iTimeout:=3000;  // Max. Zeit bis auf Antwort-Quittierungszeichen gewartet wird
                   // (nicht zu lang, falls kein Quittierungszeichen kommt (z.B.
                   // RMG DSfG-DFÜ NG) !
  { Es wird max. 1 Quittierungszeichen als Antwort erwartet: }
  if not CommObj.SendCommand (Befehl, [], 1, iTimeout, ad_String, R, NoCarrier, 1) then begin
    { Timeout ist hier kein Fehler: }
    if not ((R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT)) then begin
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end;

  { Prüfen, ob Zeit in der DSfG-Instanz tatsächlich geändert wurde: }
  ZeitSync_erfolgreich:=false;
  dtCheckEnd:=Now + EncodeTime (0, 0, 20, 0);  // max. 20 s lang prüfen
  while Now < dtCheckEnd do begin
    { Aktuelle Zeit aus DSfG-Instanz lesen: }
    if not AbrufKonfiguration_ZeitAbruf (cEADR, sDSfGUnixTime, sDummy,
                                         EST_ZEITSYNCERROR) then exit;
    { Prüfen, ob Zeit der DSfG-Instanz gelesen werden konnte: }
    if length (sDSfGUnixTime) = 0 then begin
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_READDEVICETIME);
      exit;
    end;

    { PC-Zeit ermitteln:
      -> um sie mit der Gerate-Zeit vergleichen zu können, wird die PC-Zeit gleich auf
         die in der DSfG-DFÜ herrschende Zeitzone umgerechnet }
    PCDateTime:=Now + dtWZ_SZ_Offset;

    { aktuelle DSfG-Zeit bereitstellen:
      -> Unix-String in DateTime konvertieren
      -> Korrektur wegen Übertragungslaufzeit }
    if not UnixTimeStrToDateTime (sDSfGUnixTime, DSfGDateTime) then begin
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_INVALIDDEVICETIME);
      exit;
    end;
    DSfGDateTime:=DSfGDateTime + EncodeTime (0, 0, 0, C_KorrGetDSfGZeit);

    { Abweichung zwischen aktueller DSfG-Zeit und PC-Zeit ermitteln: }
    F_TimeDiff(PCDateTime, DSfGDateTime, DiffSec);
    if Abs(DiffSec) < C_TimeDiffFenster then begin  { Zeitfenster wegen unvermeidbarer Ungenauigkeiten }
      ZeitSync_erfolgreich:=true;  { Zeitsynchronisation erfolgreich }
      Break;
    end;

    Delay (2000);
  end;

  if ZeitSync_erfolgreich then
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_SUCCESS)
  else
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_NOSUCCESS);
end;


{------------------------ COM-Tracelog ----------------------------------------}

{-------------------------------------}
procedure TDSfGAbruf.CreateCOMTraceLog;
{-------------------------------------}
// COM-Tracelog für DSfG-Abruf createn
begin
  inherited CreateCOMTraceLog;

  if Assigned (CommObj) then
    CommObj.ComTraceLog:=FComTraceLog;
end;

{-----------------------------------}
procedure TDSfGAbruf.FreeCOMTraceLog;
{-----------------------------------}
// COM-Tracelog für DSfG-Abruf freigeben
begin
  inherited FreeCOMTraceLog;

  if Assigned (CommObj) then
    CommObj.ComTraceLog:=FComTraceLog;
end;


{---------------------------- Für Simulation ----------------------------------}

{$IFDEF SIMU}
{------------------------------------------------------------------------}
function TDSfGAbruf.GetDatenelementWert_Simulation (sDea: string): string;
{------------------------------------------------------------------------}
{ Liefert Wert zu einer Datenelement-Adresse für Simulationsbetrieb
  -> DE's für Archiv- oder Logbuch-Füllstände von, bis: Füllstände des
     Simulations-Datenarchivs
  -> übrige DE's: Zufallszahl }
begin
  // DE-Adresse für Archiv- oder Logbuch-Füllstand ?
  if (sDea[1] = 'c') and (sDea[2] in ['a','b']) then begin
    if (Length(sDea) = 4) and (sDea[2] = 'a') and (sDea[3] in ['a'..'z']) then begin
      if (sDea[4] = 'c') or (sDea[4] = 'd') then begin
        // DE-Adresse für Archiv-Füllstand
        Result:=IntToStr (FSimuFileArch.GetRecordRangeONo (sDea));
        exit;
      end;
    end

    else if (Length(sDea) = 5) and (sDea[2] = 'b') and (sDea[3] in ['a'..'d']) and
            (sDea[4] in ['a'..'j']) then begin
      if (sDea[5] = 'a') or (sDea[5] = 'b') then begin
        // DE-Adresse für Logbuch-Füllstand
        Result:=IntToStr (FSimuFileLogb.GetRecordRangeONo (sDea));
        exit;
      end;
    end;
  end;

  // Alle übrigen DE-Adressen: 
  Result:=FloatToStr (Random(150));
end;
{$ENDIF}

end.

