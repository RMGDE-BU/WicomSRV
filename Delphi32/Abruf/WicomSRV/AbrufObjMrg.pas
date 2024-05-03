{******************************************************************************}
{* Unit: Objekt f�r MRG-Abruf                                                 *}
{* 02.01.2003 WW                                                              *}
{* 13.01.2011 GD t-Befehl f�r GasX                                            *}
{* 26.09.2012 WW Parametergruppen-Befehl f�r Actaris-Corus                    *}
{* 28.05.2013 WW mit Abruf der k-Faktoren-Parameter f�r MRG 905/910           *}
{* 25.04.2014 WW Parameterabruf f�r EC694 freigegeben                         *}
{* 16.05.2014 WW Parameter �bertragen Wieser/RMG-Ger�te: bei Fehlparametrie-  *}
{*               rung Wiederholung mit modifiziertem neuem Parameterwert      *}
{* 15.07.2014 WW mit Ger�tetyp�berpr�fung beim Einlesen der Konfiguration     *}
{* 08.08.2014 WW Eingeschr�nkte Menge an abzurufenden Konvertierungsparametern*}
{*               bei Messwerte-Abruf LGZ-normiert                             *}
{* 11.04.2017 WW Kennungsabfrage �ber frei definierbare Kennziffer f�r        *}
{*               Kamstrup UNIGAS 300 und Actaris Sparklog                     *}
{* 08.03.2019 WW Modbus-Abruf (Primus)                                        *}
{* 30.04.2019 WW Parametrierung Tritschler VC2, VC3, VCC                      *}
{* 22.07.2019 WW Primus: Einbau Parametrierung; Zeitsynchronisation erweitert *}
{*               um Senden des Passworts; Abruf Meldungs- und Messwert-Archiv *}
{*               �ber Archivzeiger-Parameter; Workaround Archivzeiger f�r     *}
{*               auf Sommerzeit laufendes Ger�t                               *}
{* 10.12.2019 WW Bugfix Zeitsynchronisation UNIGAS 300 (Zeitzone DS7)         *}
{* 03.06.2020 WW Login �ber Datenausleserschloss f�r Elster DL230, EK280      *}
{* 24.02.2020 WW Zeitsynchronisation mit optionalem UTC-Normalzeit-Offset des *}
{*               Ger�ts                                                       *}
{* 24.08.2020 WW Parametrierung Tritschler VC2, VC3, VCC erweitert f�r Stell- *}
{*               Befehle (z.B. Meldeliste l�schen)                            *}
{* 25.11.2020 WW Modbus-Abruf Prilog                                          *}
{* 18.02.2021 WW Modbus-Abruf TME400                                          *}
{* 09.07.2021 WW Reduzierung der MRG-Resourcendateizugriffe (MrgKonv.dat,     *}
{*               ParamEK.dat, ParamMeld.dat, ParamMrg.dat)                    *}
{* 06.08.2021 WW MRG-Ressourcedaten aus Liste statt Datei lesen               *}
{* 29.09.2021 WW Modbus-Abruf SICK FLOWSIC500                                 *}
{* 03.12.2021 WW Allgemeine Modbus-Routinen nach TMRGCustomAbruf verlagert    *}
{* 11.02.2022 WW Erweiterungen f�r Prilog 400 mit Modbuslisten-Variante       *}
{* 04.10.2022 WW Protokoll "Modbus TCP" nicht bei Modemtyp M in Verbindungs-  *}
{*               aufbau-Kommando; Primus/Prilog 400 Slaveadresse 1            *}
{* 05.10.2022 WW Einschr�nkung auf Passwort 1 bei Zeitsynchronisation von     *}
{*               DL 230, EK 280 entfernt                                      *}
{* 31.01.2023 WW Verfeinerte Fehlerauswertung bei Abfrage der k-Faktoren-     *}
{*               Parameter f�r MRG 905/910                                    *}
{* 14.04.2023 WW Verbindung halten                                            *}
{* 24.08.2023 WW EK280 Messwerte-Abruf: mit optionaler, erweiterter Abfrage   *}
{*               von Archiv 13                                                *}
{* 17.10.2023 WW Parametrierung Elster-Ger�te mit Ersatzzeichen               *}
{* 09.01.2024 WW Modbus-Abruf RSM200                                          *}
{* 04.04.2024 WW Erweiterung f�r RSM200: Beim Auslesen von Z�hler-Parametern  *}
{*               immer Parameter 'Z�hlerfaktor' mitauslesen;                  *}
{*               Erweiterung f�r TME400, RSM200: Auslesen der Parameter f�r   *}
{*               virtuelle Ger�tezustands-Parameter;                          *}
{*               Umstellung lesen der Parameter-Ressourcedaten-Konfiguration  *}
{*               'Ger�tezustand-Parameter' aus Feld 'Filtertyp' statt 'Para-  *}
{*               Datentyp'                                                    *}
{******************************************************************************}
Unit AbrufObjMrg;

INTERFACE

uses
  Windows, Classes, SysUtils,
  WStrUtils, WStream, Serial, SerialConst, O_Comm, SerMRGFup, SerMRGModem,
  SrvCfGIni, AbrufConst, GD_Utils, WChars, ErrPrc32, ErrConst, MValidAnswer, LGZType,
  AbrufCmd, MP_Allg, MP_Dfue, MP_Elster, MP_Tritschler, T_Tools, FupModemInit,
  MRG_ObjKonv, MResMrg, MResParam, MrgBefehl, MObjMeld, MObjPara, AbrufObj,
  MP_DSfG, T_Zeit, MDTFormt, MResUtil, WComm, WSysCon, MLGZKonvList,
  SrvModemTyp, MObjList, MP_Actaris, T_BinMask, GSMModemFkt, WicomSrvUtil,
  TCPIP_Mrg, MP_RMG, AbrufSrvIniFile, WBluetoothAdapter, MP_Kamstrup, MP_Imp,
  WResConst, ModbusMasterRes, ModbusMasterUtil, ModbusUtil, MLGZKonv, LogFile,
  UnixDT, O_ResFilesList, WResMeld, MP_SICK, AbrufObjMrgCustom;

Type

  { Stati des Zugangsschlosses (f�r IEC 1107-Ger�te, z.B. Elster DL240, EK260) }
  TSchlossStatus = (st_Schloss_bereits_offen,
                    st_Lieferantenschloss_geoeffnet,
                    st_Kundenschloss_geoeffnet,
                    st_Datenausleserschloss_geoeffnet);  // 03.06.2020, WW

  { Modi f�r Notwendigkeit eines Parameterabrufs }
  TParameterNoetig = (pn_Ja_Alle,  // es werden alle Parameter gelesen
                      pn_Ja_Menge, // es wird eine Menge von Parametern gelesen
                      pn_Nein);    // es m�ssen keine Parameter gelesen werden

  { Objekt zum MRG-Datenabruf }

  TMRGAbruf = class (TMRGCustomAbruf)
  private
    MRGTimeouts: TMRGTimeouts;
    Modem_DPS: string;  { Schnittstellen-Parameter Datenbits, Parit�t, Stopbits f�r Kommunikation mit Modem }
    MRG_DPS: string;  { Schnittstellen-Parameter Datenbits, Parit�t, Stopbits f�r Kommunikation mit Ger�t }
    KennungExt: string;  { Kennungs-Erweiterung (wird beim Kennungsvergleich ignoriert) }
    FMrgDefData: TMrgDefData;
    FMrgKonvData: TMrgKonvData;  // 09.07.2021, WW
    FMrgInfoData: TMrgInfoData;

    FParamMrgData_ZaehlerFaktor: TParamMrgData;  { Konfigurationsdaten-Record zu Parameter 'Z�hlerfaktor'; 04.04.2024, WW
                                                   -> F�r Ger�tetypen, f�r die eine Verrechnung der
                                                      Z�hler-Parameterrohwerte mit Faktor-Parameter erforderlich ist (RSM200) }
    FParaNrAllg_VerbHalten: string;  // 14.04.2023, WW
    FAbschaltBefehl: boolean;  { true: vor dem Verbindungsende wird der Abschaltbefehl (Break-Befehl) gesendet }
    IEC1107_SchlossStatus: TSchlossStatus;
    IEC1107_KanalAdressen: Array [1..4] of string;  { f�r Actaris Sparklog }
    ElsterEK_Version: string;
    ElsterEK_K_Zahl_Modus: string;
    MRGAnzKanaele: integer; { online ermittelte Anzahl von Kan�len des MRG, ben�tigt f�r:
                              -> Tritschler TTG (IEC-Protokoll): 1- oder 2-Kanalversion
                              -> Elster DS-100: 1- oder 4-Kanalversion }
    StdKanalNr: integer;    { aktueller Kanal im Stundenwert-Abruf (wenn je Kanal ein eigener Abruf n�tig ist !) }
    TagKanalNr: integer;    { aktueller Kanal im Tagessatz-Abruf (wenn je Kanal ein eigener Abruf n�tig ist !) }
    FAktKanal: integer;     { aktueller Kanal w�hrend Gesamt-Abruf }

    FParamMrgKonfigList: TParamMrgKonfigList;  // 09.07.2021, WW
    FParamEKKonfigList: TParamEKKonfigList;  // 09.07.2021, WW
    FParamMeldKonfigList: TParamMeldKonfigList;  // 06.08.2021, WW
    FMeldNrKonfigList: TMeldNrKonfigList;  // 06.08.2021, WW

    FMrgAbrufResourceList: TMrgAbrufKonfigList;  // MrgAbruf-Ressourcedatenliste; 06.08.2021, WW
    FMrgKanalBitResourceList: TMrgKanalBitKonfigList;  // MrgKanalBit-Ressourcedatenliste; 06.08.2021, WW

    FParameterBefehlListe_Alle: TBefehlList;
    // f�r Modbus-Kommunikation
    FParameterRegisterRequestListe_Alle: TRegisterRequestList;
    FMBRegisterDef_Para_Passwort: TMBRegisterDef;  // Modbus-Registerdefinition f�r Ger�tepasswort-Parameter; 11.02.2022, WW
    FMB_ID: integer;  // ID der im Ger�t hinterlegten Modbus-Registerliste; 11.02.2022, WW

    MRGAbrufRec: Array [1..C_MaxMRGDataTypes] of TAbrufRec;     { Information f�r Datenabruf }
    isDSfG_Slave: boolean;  { true, wenn auf einen DSfG-Slave umgeschaltet wurde }
    VerbAufbau_PasswortNr: integer;
    VerbAufbau_Passwort: string;
    VerbAufbau_ModemTyp: string;
    VerbAufbau_Rufnummer: string;
    ParameterListe_MRGAbruf: TParameterListe;
    Alle_Parameter_gelesen: boolean;
    LGZnormKonv_Parameter_gelesen: boolean;
    SSU_GeraeteAuswNr: byte;  { Tritschler IEC-Ger�te mit Multiplexer SSU }
    FGeraetetyp: integer;
    FTempRohFileListe: TRohFileListe;  { Liste f�r zwischengespeicherte Rohdaten-Dateien w�hrend eines Abrufs
                                         -> DS-100: Gemeinsame Rohdaten f�r Meldungen und Messwerte }
    FTempDZ_von: TDateTime;  { von-Auslesezeitpunkt zwischengespeicherter Rohdaten }
    FParametrierModus_gesetzt: boolean;  { Flag, ob Parametrier-Modus bereits erfolgreich im Ger�t
                                           gesetzt wurde -> Parametrierung DS-100, TME400,
                                           FLOWSIC500, RSM200 }

    function F_ParameterNoetig: TParameterNoetig;
    procedure CreateMeldungenAbrufRec (Modus: byte; von, bis: TDateTime);
    procedure CreateMesswerteAbrufRec (Modus: byte; von, bis: TDateTime; KanalAktivMaske: string);
    procedure CreateTagessatzAbrufRec (Modus: byte; von, bis: TDateTime; KanalAktivMaske: string);
    procedure CreatePruefSaetzeAbrufRec (Modus: byte; von, bis: TDateTime);

    procedure FillParameterBefehlListe (AParaFiltertyp: string;
      AAllgParaNrListe: TStringList; AParaBefehlListe: TBefehlList);
    procedure FillParameterBefehlListe_Geraetezustand (AParaNrAllg: string;
      AParaBefehlListe: TBefehlList);
    function UpdateParameterBefehlListe_ElsterEK (ABefehlListe: TBefehlList): boolean;
    function SetParamMrgData_ZaehlerFaktor: boolean;

    function FillMBRegisterRequestListe_Parameter (AParaFiltertyp: string;
      AAllgParaNrListe: TStringList; RRL: TRegisterRequestList): boolean;
    function FillMBRegisterRequestListe_Geraetezustand (AAllgParaNr: string;
      RRL: TRegisterRequestList): boolean;
    function FillMBRegisterRequestListe_ArchivDataRec (AMrgTyp: integer;
      AArchivtyp: TArchivtyp; iRecOffset, iMaxRecCount: word;
      RRL: TRegisterRequestList; MBAbrufData: TMBAbrufData; var iRecCount: integer;
      iMinRegisterAdr: integer = -1): boolean;
    function FillMBRegisterRequestListe_Archivheader_TME400 (AMrgTyp: integer;
      AArchivtyp: TArchivtyp; RRL: TRegisterRequestList): boolean;

    function Load_MRGResourceData_ParamMrg (AParameterUntergruppe: integer): boolean;
    function Load_MRGResourceData_ParamMeld: boolean;
    function Load_MRGResourceData_ParamEK: boolean;
    function Load_MRGResourceData_MeldNr (AGeraeteArt: string;
      AMeldGrpNr: integer): boolean;
    function Get_MRGResourceData_MBAbruf (AKommandotyp: char;
      var MBAbrufData: TMBAbrufData): boolean;

    function Init_MRGKonfigData (iGeraetetyp: integer;
      bInitParamKonfig: boolean = true): boolean;
    function Init_MRGKonfigData_Param (AParameterUntergruppe: integer): boolean;

    function SetDUEGeschwindigkeit_Fup (ModemTyp: string): boolean;
    function ModbuslisteID_Abfragen (iGeraetetyp: integer): boolean;
    function GeraeteTypAbfragen_Pruefen: boolean;
    function ElsterEK_Version_K_Zahl_ModusAbfragen: boolean;

    function KennungAbfragen (ModemAbrufgruppe: integer; Infobefehl: string;
                              InfoData_Kennung: string;
                              RufTyp: integer;
                              bCheckGeraeteTyp: boolean): boolean;
    function KennungPruefen (sKennung_Soll: string; bStop_bei_falscher_Kennung: boolean): boolean;
    function PasswortLogin_Tritschler_IEC_SSU (Passwort: string;
      cQuittungsModus: char; MeldungsListe: TMeldungsListe; sKennung_Soll: string;
      bKennung_pruefen: boolean): boolean;
    function PasswortLogin (ModemAbrufgruppe: integer; Passwort: string;
                            PasswortNr: integer): boolean;
    function Init_IEC1107_Kommunikation (sAdresse: string; cQuittungsModus: char;
      ModemAbrufgruppe: integer; bModemAutoDetect, bCheckGeraeteTyp: boolean;
      var sFilename_QuittungsAntwort: string;
      var VerbAutoDetectData: TVerbAutoDetectData): boolean;
    function Send_IEC1107_Passwort (sPasswort: string;
      bUpdateFehlerGruppeCode: boolean = true): boolean;
    function Check_IEC1107_SchlossStatus (VerbAufbauCmdData: TVerbAufbauCmdData;
      var VerbAutoDetectData: TVerbAutoDetectData): boolean;
    function Init_Corus_Kommunikation (bCheckGeraeteTyp: boolean): boolean;
    function Send_FTL_Uebertragungsbeginn: boolean;
    function Send_FTL_Uebertragungsende: boolean;
    procedure Send_Tritschler_Mux_Break;
    procedure Init_Tritschler_Mux_FTL_Kommunikation;
    procedure Logout_TME400 (AMrgTyp: integer);
    function Login_SICK (Passwort: string; PasswortNr: integer): boolean;
    procedure Logout_SICK;

    procedure Start_SendenDS100;
    procedure End_SendenDS100;
    function Check_DS100Typ (var Kanalzahl: integer; var AktKanal: integer): boolean;
    procedure Naechster_KanalDS100;
    function Set_KanalDS100 (Kanal: integer): boolean;
    function AbfragenPruefen_KennungDS100 (Kanalzahl: integer;
      VerbAufbauCmdData: TVerbAufbauCmdData): boolean;
    function DatenAuslesen_DS100 (AbrufRec: TAbrufRec; var RohFilename: string): boolean;

    function Verbindungsaufbau_Fup (VerbAufbauCmdData: TVerbAufbauCmdData;
      FUP_Reset_vor_Abruf: boolean; var VerbInfoData: TVerbInfoData): boolean;
    function Verbindungsaufbau_Modem_IP_Seriell (VerbAufbauCmdData: TVerbAufbauCmdData;
      var VerbAutoDetectData: TVerbAutoDetectData; var VerbInfoData: TVerbInfoData): boolean;
    function Verbindungsabbau_Fup: boolean;
    function Verbindungsabbau_Modem_IP_Seriell: boolean;
    function SetNextStdAbrufKanal: boolean;
    function SetNextTagAbrufKanal: boolean;
    function GetParameter_MeldKonv: boolean;
    function GetParameter_MessTagKonv (ArchivdatenTyp: integer): boolean;
    function GetTagesende (var iTagesende: integer): boolean;
    function RufEntgegennehmen_Fup (var dtVerbSteht: TDateTime): boolean;
    function RufEntgegennehmen_Modem (RufTyp: integer; var dtVerbSteht: TDateTime): boolean;

    function GetParamAllg(const sParam: string; var sVal: string;
      bReread: boolean = false): boolean; // 13.01.2011, GD
    function ZeitAbruf_Wieser(
      var dtDateTime: TDateTime; var sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
    function ZeitAbruf_EC900(
      var dtDateTime: TDateTime; var sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
    function ZeitAbruf_Elster(
      var dtDateTime: TDateTime; var sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
    function ZeitAbruf_Tritschler(
      var dtDateTime: TDateTime; var sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
    function ZeitAbruf_Corus(
      var dtDateTime: TDateTime; var sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
    function ZeitAbruf_Kamstrup_UNIGAS300(
      var dtDateTime: TDateTime; var sTimeZone, sTimeInfos: string): boolean;
    function ZeitAbruf_PrimusPrilog(
      var dtDateTime: TDateTime; var sTimeZone, sTimeInfos: string): boolean;
    function ZeitAbruf_TME400(AMrgTyp: integer; var dtDateTime: TDateTime;
      var sTimeZone, sTimeInfos: string): boolean;
    function ZeitAbruf_SICK(
      var dtDateTime: TDateTime; var sTimeZone, sTimeInfos: string): boolean;

    function ReadModbusArchiv_PrimusPrilog (AMrgTyp: integer; AArchivtyp: TArchivtyp;
      AbrufRec: TAbrufRec; dtVon, dtBis: TDateTime; MBAbrufData: TMBAbrufData;
      var sFilename_Archiv: string): boolean;
    function WriteModbusArchivZeiger_PrimusPrilog (iStartAdresse: word;
      sTyp: string; iAnzahlBytes: integer; dtZeiger: TDateTime): boolean;
    function WriteModbusParameter_PrimusPrilog (iStartAdresse: word;
      sTyp: string; iAnzahlBytes: integer; sWertNeu: string; var bOK: boolean;
      iErweiterteStatusgruppe: integer = 0): boolean;

    function ReadModbusArchiv_TME400 (AMrgTyp: integer; AArchivtyp: TArchivtyp;
      AbrufRec: TAbrufRec; dtVon, dtBis: TDateTime; var sFilename_Archiv: string): boolean;
    function ReadModbusArchivheader_TME400 (AMrgTyp: integer; AArchivtyp: TArchivtyp;
      var Archivheader: TTME400Archivheader): boolean;
    function WriteModbusParameter_TME400 (AMrgTyp: integer; iStartAdresse: word;
      sTyp: string; iAnzahlBytes: integer; sWertNeu: string; var bOK: boolean;
      iErweiterteStatusgruppe: integer = 0): boolean;
    function ReadModbusStatusregister_TME400 (AMrgTyp: integer;
      var iStatusregister: cardinal; iErweiterteStatusgruppe: integer = 0): boolean;

    function ReadModbusArchiv_SICK (AArchivtyp: TArchivtyp;
      AbrufRec: TAbrufRec; dtVon, dtBis: TDateTime;
      var sFilename_Archiv: string; var iRecType: word): boolean;
    function ReadModbusArchivInfo_SICK (AArchivtyp: TArchivtyp;
      var iRecCountAvail: word; var iRecMax: word; var iRecSize: word;
      var iRecType: word; var iRecNextPos: word): boolean;
    function WriteModbusArchivDownloadAdresse_SICK (AArchivtyp: TArchivtyp;
      iRecOffset, iMaxRecCount, iRecByteSize: word; var iRecCount: integer;
      var iDownloadAdr: word; iMinRecIndex: integer): boolean;
    function WriteModbusParameter_SICK (iStartAdresse: word;
      sTyp: string; iAnzahlBytes: integer; sWertNeu: string; var bOK: boolean;
      iErweiterteStatusgruppe: integer = 0): boolean;
    function SetKonfigurationsmodus_SICK (bOnOff: boolean;
      iErweiterteStatusgruppe: integer = 0): boolean;
    function ReadModbusAccessLevel_SICK (var iAccessLevel: cardinal;
      iErweiterteStatusgruppe: integer = 0): boolean;
    function GetFirstArchivRec_DatumZeit_SICK (sDownloadBuf: string): TDateTime;
    function Valid_SICK_DownloadPuffer (sDownloadBuf: string;
      iDownloadAdr, iRecSize: word): boolean;
    function TrimAndSwap_SICK_DownloadPuffer (sDownloadBuf: string;
      iRecSize, iRecCountMax: word; var iRecIdMax_Check: cardinal): string;
                     
    function Init_FTLintern_Kommunikation (iErweiterteStatusgruppe: integer = 0): boolean;
  public
    isFupAbruf: boolean;                   { true, wenn Abruf mit FUP erfolgt }
    Constructor Create (ACOMNr: integer; ACOMNr_Kommando: integer;
                        ACommObj: TMRGCommObj;
                        AMRGTimeouts: TMRGTimeouts; AModemname: string;
                        AModem_DPS: string; AKennungExt: string;
                        ADebugCOMProtokoll: boolean;
                        ARohdatenLoeschen: boolean;
                        AKonfigPath, AWorkPath, ANetProgPath, ALogPath: string;
                        ASignatur_freigeschaltet: boolean;
                        AXMLResponseEncodeRohdaten: integer;
                        AResourceFilesList: TResourceFilesList);
    Destructor Destroy; override;
    function VerbAufbau (VerbAufbauCmdData: TVerbAufbauCmdData;
                         FUP_Reset_vor_Abruf: boolean;
                         var StationsKennungRet: string;
                         var VerbAutoDetectData: TVerbAutoDetectData;
                         var VerbInfoData: TVerbInfoData): boolean;
    function VerbAbbau: boolean;
    function AbrufParameter (AllgParaNr: string;
                             AllgParaNrListe: TStringList;
                             ParameterListe: TParameterListe;
                             ArchivdatenTyp: integer = -1;
                             Ausgabe_Dateiname: string = '';
                             bIgnoreDeviceErrorAnswer: boolean = false): boolean;
    function AbrufMeldungen (MeldAbrufCmdData: TMessMeldPruefAbrufCmdData;
                             MeldungsListe: TMeldungsListe): boolean;
    function AbrufMessTagWerte (MessAbrufCmdData: TMessMeldPruefAbrufCmdData;
                                StaKanalKonvDataList: TStaKanalKonvDataList;
                                AnalogOrig_normiert: boolean;
                                var iTagesende: integer;
                                var KonvDateiname_MessList: TStringList;
                                var KonvDateiname_TagList: TStringList;
                                var iMaxMessKanal: integer;
                                var iMinMessKanalExt: integer;
                                var iMaxMessKanalExt: integer): boolean;
    function AbrufPruefwerte (PruefAbrufCmdData: TMessMeldPruefAbrufCmdData;
                              var KonvDateiname_Pruef: string): boolean;
    function DSfGUmschaltung (DSfGUmschaltCmdData: TDSfGUmschaltCmdData;
                              var StationsKennungRet: string): boolean;
    procedure ZeitSynchronisation (ZeitSyncCmdData: TZeitSyncCmdData;
                                   KorrekturMax: integer;
                                   var ZeitSyncInfoData: TZeitSyncInfoData);
    function ResetRundpuffer_Meld: boolean;
    function ResetRundpuffer_Mess: boolean;
    function ResetRundpuffer_Pruef: boolean;
    function UebertragungParameter (ParaEinstellCmdData: TParaEinstellCmdData;
                                    var ParaEinstellResultData: TParaEinstellResultData): boolean;
    function AbrufTransparent (TransparentCmdData: TTransparentCmdData;
                               var TransparentAntw: string): boolean;
    function RufEntgegennahme (RufTyp: integer; var StationsKennungRet: string;
                               var GeraetetypRet: integer;
                               MeldungsListe: TMeldungsListe;
                               var VerbInfoData: TVerbInfoData): boolean;
    function Rufannahme (RufannahmeCmdData: TRufannahmeCmdData;
                         var VerbInfoData: TVerbInfoData): boolean;
    function ZeitAbruf (var sUnixTime, sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
    function RufListeAbfragen (var RufListe: string): boolean;
    function SlaveRufQuittieren (Adresse: string): boolean;
    function RueckrufAusloesung (Zentrale: integer): boolean;
    function VerbHalten: boolean;
    property Geraetetyp: integer read FGeraetetyp;
  end;

IMPLEMENTATION

uses DateUtils;


{ TMRGAbruf }

{-----------------------------------------------------------------------------------}
Constructor TMRGAbruf.Create (ACOMNr: integer; ACOMNr_Kommando: integer;
                              ACommObj: TMRGCommObj;
                              AMRGTimeouts: TMRGTimeouts; AModemname: string;
                              AModem_DPS: string; AKennungExt: string;
                              ADebugCOMProtokoll: boolean;
                              ARohdatenLoeschen: boolean;
                              AKonfigPath, AWorkPath, ANetProgPath, ALogPath: string;
                              ASignatur_freigeschaltet: boolean;
                              AXMLResponseEncodeRohdaten: integer;
                              AResourceFilesList: TResourceFilesList);
{-----------------------------------------------------------------------------------}
Begin
  inherited Create (ACOMNr, ACOMNr_Kommando, ACommObj, AModemname,
                    ADebugCOMProtokoll, ARohdatenLoeschen,
                    AKonfigPath, AWorkPath, ANetProgPath, ALogPath,
                    ASignatur_freigeschaltet, AXMLResponseEncodeRohdaten,
                    AResourceFilesList);
  MRGTimeouts:=AMRGTimeouts;
  Modem_DPS:=AModem_DPS;
  KennungExt:=AKennungExt;

  FParamMrgKonfigList:=TParamMrgKonfigList.Create;  // 09.07.2021, WW

  if Assigned (FResourceFilesList) then begin  // 06.08.2021, WW
    FMrgAbrufResourceList:=TMrgAbrufKonfigList (FResourceFilesList.ListObject [rft_MrgAbruf]);
    FMrgKanalBitResourceList:=TMrgKanalbitKonfigList (FResourceFilesList.ListObject [rft_MrgKanalBit]);
  end
  else begin
    FMrgAbrufResourceList:=nil;
    FMrgKanalBitResourceList:=nil;
  end;

  // Default "nil" f�r nicht geladene Konfigurationslisten des MRG (werden nur bei
  // Bedarf im weiteren Verlauf geladen); 09.07.2021, WW
  FParamEKKonfigList:=nil;  // Elster EK-Parameter-Konfigurationsliste; 09.07.2021, WW
  FParamMeldKonfigList:=nil;  // Konfigurationsliste mit meldungsspezifischen Parameternummern; 06.08.2021, WW
  FMeldNrKonfigList:=nil;  // Meldungsnummern-Konfigurationsliste; 06.08.2021, WW

  FParameterBefehlListe_Alle:=TBefehlList.Create;
  FParameterRegisterRequestListe_Alle:=TRegisterRequestList.Create;  // 08.03.2019, WW
  ParameterListe_MRGAbruf:=TParameterListe.Create (FKonfigPath);

  isFupAbruf:=CommObj is TMRGFupCommObj;
  isDSfG_Slave:=false;
  VerbAufbau_PasswortNr:=0;
  VerbAufbau_Passwort:='';
  VerbAufbau_ModemTyp:='';
  VerbAufbau_Rufnummer:='';
  MRG_DPS:='';  // Vorbelegung: kein MRG-spezifisches Schnittstellen-Datenformat
  FGeraetetyp:=-1;

  { f�r Abruf von Tritschler IEC-Ger�ten mit Multiplexer SSU: }
  SSU_GeraeteAuswNr:=0;  { Voreinstellung: es wurde noch auf keinen Ausgang geschaltet }

  { f�r Abruf von Elster DS-100: Liste f�r zwischengespeicherte Rohdaten-Dateien }
  FTempRohFileListe:=TRohFileListe.Create;
  { Default von-Auslesezeitpunkt zwischengespeicherter Rohdaten:
    Keine zwischengespeicherten Rohdaten vorhanden }
  FTempDZ_von:=EncodeDate (2100, 1, 1);
End;

{---------------------------}
Destructor TMRGAbruf.Destroy;
{---------------------------}
Begin
  { vorhandene zwischengespeicherte Rohdatendateien l�schen: }
  FTempRohFileListe.DeleteRohFiles;
  FTempRohFileListe.Free;

  ParameterListe_MRGAbruf.Free;
  FParameterRegisterRequestListe_Alle.Free;
  FParameterBefehlListe_Alle.Free;

  FMeldNrKonfigList.Free;
  FParamEKKonfigList.Free;
  FParamMeldKonfigList.Free;
  FParamMrgKonfigList.Free;

  inherited Destroy;
End;

{-----------------------------------------------------}
function TMRGAbruf.F_ParameterNoetig: TParameterNoetig;
{-----------------------------------------------------}
{ Ergebnis: Modus f�r Notwendigkeit eines Parameterabrufs lt. MRG-Konfigurations-
            datentabelle (pn_...)
            -> z.B. f�r Messwert-Konvertierung in normierte LGZ-Werte }
begin
{$IFDEF GAS-X}
  Result:=pn_Nein;  // Abruf aller Parameter/Parametermenge f�r Messwerte nicht n�tig.
                    // Messwerte werden als Originalwerte geliefert und m�ssen
                    // daher nicht �ber Parameter in LGZ-Werte umgerechnet werden.
{$ELSE}
   if FMrgDefData.AbrufParameter = CResAbrufParaJaAlle then
     Result:=pn_Ja_Alle
   else if FMrgDefData.AbrufParameter = CResAbrufParaJaMenge then
     Result:=pn_Ja_Menge  // 08.08.2014, WW
   else
     Result:=pn_Nein;
{$ENDIF}
end;

{-----------------------------------------------------------------------------}
procedure TMRGAbruf.CreateMeldungenAbrufRec (Modus: byte; von, bis: TDateTime);
{-----------------------------------------------------------------------------}
{ erzeugt MRGAbrufRec [dt_Meldungen], welcher zum Meldungsabrufen ben�tigt wird;
  �bergabe: Modus -> d_Alles  = Es sollen alle Meldungen abgerufen werden
                     d_VonBis = Es sollen Meldungen von - bis abgerufen werden }
Var
  MAR : TAbrufRec;
  dummy: word;

Begin
  MAR:=Init_AbrufRec;  // Abruf-Record vorbelegen

  Case Modus of
    d_Alles :
      Begin
        MAR.AlleDaten := True;
        MRGAbrufRec [dt_Meldungen] := MAR;
      End;

    d_VonBis :
      Begin
        MAR.AlleDaten := false;
        with MAR do begin
          DecodeDate (von, vonJahr, vonMonat, vonTag);
          DecodeTime (von, vonStunde, vonMinute, vonSekunde, dummy);

          { Bis-Datum: aktuelles PC-Datum plus "Sicherheitsreserve", damit auch
            wirklich alle im Zeitraum angeforderten neuen Meldungen ausgelesen werden,
            wenn z.B.:
            -> PC- und Ger�tezeit nicht ganz synchron laufen (i.d.R. immer)
            -> PC- und Ger�t in verschiedenen Zeitzonen laufen
            -> Ger�te abgerufen werden, bei denen als Zeitangabe nur die bis-Stunde
               im Abfragebefehl enthalten ist (z.B. MRG 910: bis-Stunde 12
               liefert nur Meldungen bis einschlie�lich 12:00:00 !) }
          bis:=bis + 1;
          DecodeDate (bis, bisJahr, bisMonat, bisTag);
          DecodeTime (bis, bisStunde, bisMinute, bisSekunde, dummy);
        end;
        MRGAbrufRec [dt_Meldungen] := MAR;
      End;
  End;
End;

{----------------------------------------------------------------------------}
procedure TMRGAbruf.CreateMesswerteAbrufRec (Modus: byte; von, bis: TDateTime;
                                             KanalAktivMaske: string);
{----------------------------------------------------------------------------}
{ erzeugt MRGAbrufRec [dt_Messwerte], welcher zum Me�wertabrufen ben�tigt wird;
  �bergabe: Modus -> d_Alles  = Es sollen alle Me�werte abgerufen werden
                     d_VonBis = Es sollen Me�werte von - bis abgerufen werden
            von-bis-Zeitraum
            Maske aktiver Kan�le }
Var
  MAR : TAbrufRec;
  dummy: Word;
  KanalMaske: string;
  MrgKanal: integer;
  Kanal_aktiv: boolean;
  nur_2_Analogkanaele: boolean;
  GerVersion: string;
  sYYMMDD: string;

Begin
  MAR:=Init_AbrufRec;  // Abruf-Record vorbelegen

  { Kanalmaske f�r Messwert-Abrufbefehl: }
  KanalMaske:='';
  if FMrgDefData.MesswertKanaele > 0 then begin  { wenn Messwert-Kan�le definiert sind }
    for MrgKanal := 1 to FMrgDefData.MesswertKanaele do begin
      { wenn die Kanalaktiv-Maske im Messwertabruf-Clientkommando fehlt (Leer-String), werden
        alle definierten Kan�le aktiv gesetzt
        -> Kompatibilit�t zu Client-Systemen, welche keine Kanalaktiv-Maske im
           Kommando mitschicken }
      if length (KanalAktivMaske) > 0 then begin
        Kanal_aktiv:=false;
        if length (KanalAktivMaske) >= MrgKanal then
          if KanalAktivMaske [MrgKanal] = '1' then
            Kanal_aktiv:=true;
      end else
        Kanal_aktiv:=true;  { Fehlerkorrektur f�r leere KanalAktivMaske, WW 04.05.2004 }

      if Kanal_aktiv then
        KanalMaske := KanalMaske + '1'
      else
        KanalMaske := KanalMaske + '0';
    end;

    // MRG910: 4 Analogkan�le k�nnen erst ab der Nachfolgeversion vom 25.08.2004 abgefragt werden
    // 05.06.2007, WW
    if (FMrgDefData.MrgTyp = mrgtyp_MRG910) then begin
      nur_2_Analogkanaele:=true;
      if ParameterListe_MRGAbruf.GetValue (CP_ALLG_Geraeteversion, GerVersion) then begin
        { Datum aus Version rauskopieren und Stringformat-Umwandlung (DDMMYY -> YYMMDD)
          f�r Datumsvergleich: }
        sYYMMDD:=Copy (GerVersion, 13, 2) + Copy (GerVersion, 11, 2) + Copy (GerVersion, 9, 2);
        if sYYMMDD > '040825' then
          nur_2_Analogkanaele:=false;
      end;
      if nur_2_Analogkanaele then
        KanalMaske:=Copy (KanalMaske, 1, 6)  // max. 4 Impuls und 2 Analogkan�le abfragbar
    end;
  end;
  MAR.KanalMaske:=KanalMaske;

  case Modus of
    d_Alles:
      begin
        MAR.AlleDaten := True;
        MRGAbrufRec [dt_Messwerte] := MAR;
      end;

    d_VonBis :
      Begin
        MAR.AlleDaten := false;
        with MAR do begin
          DecodeDate (von, vonJahr, vonMonat, vonTag);
          DecodeTime (von, vonStunde, vonMinute, vonSekunde, dummy);
          DecodeDate (bis, bisJahr, bisMonat, bisTag);
          DecodeTime (bis, bisStunde, bisMinute, bisSekunde, dummy);
        end;
        MRGAbrufRec [dt_Messwerte] := MAR;
      End;
  end; { case }
End;

{----------------------------------------------------------------------------}
procedure TMRGAbruf.CreateTagessatzAbrufRec (Modus: byte; von, bis: TDateTime;
                                             KanalAktivMaske: string);
{----------------------------------------------------------------------------}
{ erzeugt MRGAbrufRec [dt_Tagessaetze], welcher zum Tagessatzabrufen ben�tigt wird;
  �bergabe: Modus -> d_Alles  = Es sollen alle Tagess�tze abgerufen werden
                     d_VonBis = Es sollen Tagess�tze von - bis abgerufen werden
            von-bis-Zeitraum
            Maske aktiver Kan�le }
Var
  MAR : TAbrufRec;
  dummy: Word;
  KanalMaske: string;
  MrgKanal: integer;
  Kanal_aktiv: boolean;

Begin
  MAR:=Init_AbrufRec;  // Abruf-Record vorbelegen

  { Kanalmaske aus Konfigurationsdaten lesen:
    -> es werden immer alle Kan�le gelesen ! }
  KanalMaske:='';
  if FMrgDefData.ZaehlerKanaele > 0 then begin
    for MrgKanal := 1 to FMrgDefData.ZaehlerKanaele do begin
      { wenn die Kanalaktiv-Maske im Messwertabruf-Kommando fehlt (Leer-String), werden
        alle Kan�le aktiv gesetzt
        -> Kompatibilit�t zu Client-Systemen, welche keine Kanalaktiv-Maske im
           Kommando mitschicken }
      if length (KanalAktivMaske) > 0 then begin
        Kanal_aktiv:=false;
        if length (KanalAktivMaske) >= MrgKanal then
          if (KanalAktivMaske [MrgKanal] = '1') OR
             (KanalAktivMaske [MrgKanal] = '2') then  // nur Tagess�tze lesen; 10.08.2011, WW
            Kanal_aktiv:=true;
      end else
        Kanal_aktiv:=true;  { Fehlerkorrektur f�r leere KanalAktivMaske, WW 04.05.2004 }

      if Kanal_aktiv then
        KanalMaske := KanalMaske + '1'
      else
        KanalMaske := KanalMaske + '0';
    end;
  end;
  MAR.KanalMaske:=KanalMaske;

  case Modus of
    d_Alles:
      begin
        MAR.AlleDaten := True;
        MRGAbrufRec [dt_Tagessaetze] := MAR;
      end;

    d_VonBis :
      Begin
        MAR.AlleDaten := false;
        with MAR do begin
          DecodeDate (von, vonJahr, vonMonat, vonTag);
          DecodeTime (von, vonStunde, vonMinute, vonSekunde, dummy);

          { Bis-Datum: 1 Tag mehr abrufen, sonst schickt das MRG 910 den
            letzten Tagessatz nicht; 04.03.2003, WW }
          bis:=bis + 1;
          DecodeDate (bis, bisJahr, bisMonat, bisTag);
          DecodeTime (bis, bisStunde, bisMinute, bisSekunde, dummy);
        end;
        MRGAbrufRec [dt_Tagessaetze] := MAR;
      End;
  end; { case }
End;

{-------------------------------------------------------------------------------}
procedure TMRGAbruf.CreatePruefSaetzeAbrufRec (Modus: byte; von, bis: TDateTime);
{-------------------------------------------------------------------------------}
{ erzeugt MRGAbrufRec [dt_PruefSaetze], welcher zum Pr�fs�tzeabrufen ben�tigt wird;
  �bergabe: Modus -> d_Alles  = Es sollen alle Pr�fungss�tze abgerufen werden
                     d_VonBis = Es sollen Pr�fungss�tze von - bis abgerufen werden
            von-bis-Zeitraum }
Var
  MAR : TAbrufRec;
  dummy: word;
Begin
  MAR:=Init_AbrufRec;  // Abruf-Record vorbelegen

  Case Modus of
    d_Alles :
      Begin
        MAR.AlleDaten := True;
        MRGAbrufRec [dt_PruefSaetze] := MAR;
      End;

    d_VonBis :
      Begin
        MAR.AlleDaten := false;
        with MAR do begin
          DecodeDate (von, vonJahr, vonMonat, vonTag);
          DecodeTime (von, vonStunde, vonMinute, vonSekunde, dummy);
          DecodeDate (bis, bisJahr, bisMonat, bisTag);
          DecodeTime (bis, bisStunde, bisMinute, bisSekunde, dummy);
        end;
        MRGAbrufRec [dt_Pruefsaetze] := MAR;
      End;
  End;
End;

{-------------------------------------------------------------------}
procedure TMRGAbruf.FillParameterBefehlListe (AParaFiltertyp: string;
  AAllgParaNrListe: TStringList; AParaBefehlListe: TBefehlList);
{-------------------------------------------------------------------}
{ Liste mit Befehlen zum Lesen von Parametern zusammenstellen abh�ngig von Ger�tegruppe;
  �bergabe: Filtertyp der Parameter, mit denen Befehl(e) gebildet werden soll(en)
              (C_PFT_xxx-Konstanten aus MResParam.pas)
              -> Leer = Alle Parameter (kein Filter)
            Zeiger auf Liste abzurufender Parameter (allg. Parameternummern),
              mit denen Befehl(e) gebildet werden soll(en) oder nil
              -> Wenn <> nil: �bergabe 'AParaFiltertyp' nicht relevant
  �bergabe/R�ckgabe: Parameter-Befehlliste }
var
  ParaNrMrg: string;
  i, j: integer;
  BefehlData: TBefehlData;
  BefehlDataObj: TBefehlDataObj;
  iParaNrMrg: integer;
  Code: integer;
  SL: TStringList;
  iParaDatentyp: integer;
  sParaFiltertyp: string;
  bAdd: boolean;
  slParaNrMrg: TStringList;
  iParaByteLen: integer;
  iParaAnzahl: integer;
  iSize_Answer: integer;
  ParaNrAllg: string;

  {-----------------------------------}
  procedure AddToParaBefehlListe_Corus;  // 26.09.2012, WW
  {-----------------------------------}
  { Corus-Befehl in Parameter-Befehlliste eintragen }
  var
    j: integer;

  begin
    BefehlData.sBefehl:=GetCorusSAMKommando_ParameterGruppeLesen (slParaNrMrg);
    // Zusatzinfo: Parameternummern (durch Komma getrennt) und Trennzeichen Semikolon
    BefehlData.sInfo:='';
    for j:=0 to slParaNrMrg.Count - 1 do begin
      if length (BefehlData.sInfo) > 0 then
        BefehlData.sInfo:=BefehlData.sInfo + ',';  // Komma als Trennzeichen zwischen den Parameternummern
      BefehlData.sInfo:=BefehlData.sInfo + slParaNrMrg [j];
    end;
    if length (BefehlData.sInfo) > 0 then
      BefehlData.sInfo:=BefehlData.sInfo + ';';

    BefehlDataObj:=TBefehlDataObj.Create (BefehlData);
    AParaBefehlListe.Add (BefehlDataObj);  { in Befehlsliste eintragen }
  end;

begin
  if not Assigned (AParaBefehlListe) then exit;

  AParaBefehlListe.Clear;
  case FMrgDefData.Kommandogruppe of
    101, 102, 108, 112, 113, 117, 118, 119, 5:
      begin  { Elster DL210, DL220, DL230, DL240, EK260, EK 280; Actaris Sparklog;
               Kamstrup UNIGAS 300; MRG 905, 910 }
        { Kommandogruppe 5 nur f�r Parameter mit aktuellem Ger�tezustand: }
        if (FMrgDefData.Kommandogruppe = 5) AND
           (AParaFiltertyp <> C_PFT_GERZUSTAND_AKTUELL) then exit;

        for i:=0 to FParamMrgKonfigList.Count-1 do begin
          ParaNrAllg:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.Parameternummer;
          ParaNrMrg:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.Parameternummer_im_MRG;
          sParaFiltertyp:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.Filtertyp;  // 04.04.2024, WW
          if length (ParaNrMrg) > 0 then begin  // 14.08.2012, WW
            if Assigned (AAllgParaNrListe) then begin  // Parameter aus Allg. Parameternummer-Liste; 08.08.2014, WW
              bAdd:=false;
              for j:=0 to AAllgParaNrListe.Count - 1 do begin
                if AAllgParaNrListe [j] = ParaNrAllg then begin
                  bAdd:=true;
                  Break;
                end;
              end;
            end
            else if AParaFiltertyp = '' then  // alle Parameter, kein Filter
              bAdd:=true
            else  // alle Parameter, Filter auf Parameter-Filtertyp
              bAdd:=Pos (AParaFiltertyp, sParaFiltertyp) > 0;  // 04.04.2024, WW

            if bAdd then begin
              { Befehl zur Abfrage der Parameter-Nummer bilden: }
              if (FMrgDefData.Kommandogruppe = 5) then begin  { MRG 905, 910; 12.02.2014, WW }
                BefehlData.sBefehl:=GetMRGKommando_B (ParaNrMrg);
                BefehlData.sInfo:=ParaNrMrg;  // Zusatzinfo: MRG-Parameternummer
              end
              else if (FMrgDefData.Kommandogruppe = 112) OR          { Actaris Sparklog }
                      (FMrgDefData.Kommandogruppe = 117) then begin  { Kamstrup UNIGAS 300 }
                BefehlData.sBefehl:=GetIEC1107Kommando_Lesen (ParaNrMrg);
                BefehlData.sInfo:='';  // keine Zusatzinfo
              end
              else begin  { Elster DL210, DL220, DL230, DL240, EK260, EK280 }
                BefehlData.sBefehl:=GetLIS200Kommando_Lesen (ParaNrMrg);
                // Zusatzinfo: Die allgemeine Parameternummer wird zum
                // Versions- und K-Zahl Modus-abh�ngigen Umlenken auf EK-spezifische
                // Parameternummern ben�tigt; 28.07.2014, WW
                BefehlData.sInfo:=ParaNrAllg;
              end;

              BefehlDataObj:=TBefehlDataObj.Create (BefehlData);
              AParaBefehlListe.Add (BefehlDataObj);  { in Befehlsliste eintragen }
            end;
          end;
        end;  // for i
      end;

    109:
      begin  { Tritschler TTG mit FTL-Protokoll }
        { mehrere Parameter-Lesebefehle:
            F0000: enth�lt nur kanalunabh�ngige Parameter (Abruf f�r Kanal 1)
            F0001: Parameter f�r Kanal 1
            F0011: Parameter f�r Kanal 2 }
        BefehlData.sBefehl:=GetTTG_FTLKommando_Geraeteinfo (1);
        BefehlData.sInfo:='';  // keine Zusatzinfo
        BefehlDataObj:=TBefehlDataObj.Create (BefehlData);
        AParaBefehlListe.Add (BefehlDataObj);  { in Befehlsliste eintragen }

        BefehlData.sBefehl:=GetTTG_FTLKommando_Zaehlerstaende (1);
        BefehlData.sInfo:='';  // keine Zusatzinfo
        BefehlDataObj:=TBefehlDataObj.Create (BefehlData);
        AParaBefehlListe.Add (BefehlDataObj);  { in Befehlsliste eintragen }

        BefehlData.sBefehl:=GetTTG_FTLKommando_Zaehlerstaende (2);
        BefehlData.sInfo:='';  // keine Zusatzinfo
        BefehlDataObj:=TBefehlDataObj.Create (BefehlData);
        AParaBefehlListe.Add (BefehlDataObj);  { in Befehlsliste eintragen }
      end;

    111:
      begin  { Actaris Corus }
        slParaNrMrg:=TStringList.Create;
        try
          iSize_Answer:=0;
          for i:=0 to FParamMrgKonfigList.Count-1 do begin
            ParaNrAllg:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.Parameternummer;
            ParaNrMrg:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.Parameternummer_im_MRG;
            iParaDatentyp:=StrToIntDef (TParamMrgDataObj (FParamMrgKonfigList [i]).Data.ParaDatentyp, -1);  // 08.03.2019, WW
            sParaFiltertyp:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.Filtertyp;  // 04.04.2024, WW
            iParaByteLen:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.ParaByteLen;
            iParaAnzahl:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.ParaAnzahl;
            // Wenn f�r den Parameter keine spezifische Bytel�nge konfiguriert
            // ist: Standard-Bytel�nge des Parameter-Datentyps verwenden
            if iParaByteLen <= -1 then
              iParaByteLen:=PDT_ByteLength (iParaDatentyp);

            if (length (ParaNrMrg) > 0) AND
               (iParaByteLen > -1) AND (iParaAnzahl > -1) then begin  // 14.08.2012, WW
              if Assigned (AAllgParaNrListe) then begin  // Parameter aus Allg. Parameternummer-Liste
                bAdd:=false;
                for j:=0 to AAllgParaNrListe.Count - 1 do begin
                  if AAllgParaNrListe [j] = ParaNrAllg then begin
                    bAdd:=true;
                    Break;
                  end;
                end;
              end
              else if AParaFiltertyp = '' then  // alle Parameter, kein Filter
                bAdd:=true
              else  // alle Parameter, Filter auf Parameter-Filtertyp
                bAdd:=Pos (AParaFiltertyp, sParaFiltertyp) > 0;  // 04.04.2024, WW

              if bAdd then begin
                { Befehl zur Abfrage der Parameter-Nummer bilden:
                  -> Es sind nur Parameternummern von 0..255 erlaubt.
                  -> Zusatzinfo "Parameternummer" im Listenobjekt f�r Konvertierung }
                Val (ParaNrMrg, iParaNrMrg, Code);
                if not (Code = 0) AND (iParaNrMrg >= 0) AND (iParaNrMrg <= MaxByte) then
                  Continue;
                // Parametergruppen-Befehle zusammenstellen (bisher Einzelbefehle); 26.09.2012
                //   -> Maximal 32 Parameternummern sind im Befehl erlaubt
                //   -> Antwort kann maximal 255 Zeichen im Datenteil liefern,
                //      sonst erfolgt eine NACK-Antwort
                if (slParaNrMrg.Count >= 32) OR
                   ((iSize_Answer + (iParaByteLen * iParaAnzahl)) > 255) then begin
                  AddToParaBefehlListe_Corus;

                  slParaNrMrg.Clear;
                  iSize_Answer:=0;
                end;

                slParaNrMrg.Add (IntToStr (iParaNrMrg));
                inc (iSize_Answer, iParaByteLen * iParaAnzahl);  // berechntete L�nge des Datenteils in der zu erwartenden Antwort
              end;  { if bAdd }
            end;  { if length (ParaNrMrg) > 0 }
          end;  { for i }

          if slParaNrMrg.Count > 0 then
            AddToParaBefehlListe_Corus;
        finally
          slParaNrMrg.Free;
        end;
      end;

    115:
      begin  { Elster DS-100 }
        SL:=TStringList.Create;
        try
          SL.Sorted:=true;
          SL.Duplicates:=dupIgnore;  // gleiche "Parameternummern" nicht mehrfach
          for i:=0 to FParamMrgKonfigList.Count-1 do begin
            ParaNrMrg:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.Parameternummer_im_MRG;
            if length (ParaNrMrg) > 0 then begin  // 14.08.2012, WW
              { Den Kanal-Kennzeichner in den "DS-100-Parameternummern" wegschneiden
                (letztes Zeichen), geh�rt nicht zum Befehl: }
              Delete (ParaNrMrg, length (ParaNrMrg), 1);
              SL.Add (ParaNrMrg);
            end;
          end;

          for i:=0 to SL.Count-1 do begin
            { Befehl zur Abfrage der Parameter-Nummer bilden: }
            BefehlData.sBefehl:=GetDS100_LeseBefehl (SL [i]);
            BefehlData.sInfo:='';  // keine Zusatzinfo

            BefehlDataObj:=TBefehlDataObj.Create (BefehlData);
            AParaBefehlListe.Add (BefehlDataObj);  { in Befehlsliste eintragen }
          end;
        finally
          SL.Free;
        end;
      end;
  end;  { case FMrgDefData.Kommandogruppe }
end;

{-------------------------------------------------------------------------------}
procedure TMRGAbruf.FillParameterBefehlListe_Geraetezustand (AParaNrAllg: string;
  AParaBefehlListe: TBefehlList);
{-------------------------------------------------------------------------------}
{ Liste mit Befehlen zum Lesen der Ger�tezustands-Parameter zusammenstellen
  abh�ngig von allgemeiner Parameternummer;
  �bergabe: Allgemeine Parameternummer f�r Ger�tezustand
  �bergabe/R�ckgabe: Parameter-Befehlliste }
begin
  { Aktueller Ger�tezustand EK 260, EK 280, DL 240, DL 220, DL 210, MRG 905/910: }
  if (AParaNrAllg = CP_ELS_EK260_GerZustand_aktuell) OR
     (AParaNrAllg = CP_ELS_EK280_GerZustand_aktuell) OR
     (AParaNrAllg = CP_ELS_DL240_GerZustand_aktuell) OR  // 12.02.2014, WW
     (AParaNrAllg = CP_ELS_DL230_GerZustand_aktuell) OR  // 23.05.2014, WW
     (AParaNrAllg = CP_ELS_DL220_GerZustand_aktuell) OR
     (AParaNrAllg = CP_ELS_DL210_GerZustand_aktuell) OR
     (AParaNrAllg = CP_ALLG_MRG900_GerZustand_aktuell) then
    FillParameterBefehlListe (C_PFT_GERZUSTAND_AKTUELL, nil, AParaBefehlListe)
  { Gespeicherter Ger�tezustand EK 260, EK 280, DL 240, DL 220, DL 210: }
  else if (AParaNrAllg = CP_ELS_EK260_GerZustand_gespeichert) OR
          (AParaNrAllg = CP_ELS_EK280_GerZustand_gespeichert) OR
          (AParaNrAllg = CP_ELS_DL240_GerZustand_gespeichert) OR  // 12.02.2014, WW
          (AParaNrAllg = CP_ELS_DL230_GerZustand_gespeichert) OR  // 23.05.2014, WW
          (AParaNrAllg = CP_ELS_DL220_GerZustand_gespeichert) OR
          (AParaNrAllg = CP_ELS_DL210_GerZustand_gespeichert) then
    FillParameterBefehlListe (C_PFT_GERZUSTAND_GESPEICHERT, nil, AParaBefehlListe);
end;

{------------------------------------------------------}
function TMRGAbruf.UpdateParameterBefehlListe_ElsterEK (
  ABefehlListe: TBefehlList): boolean;
{------------------------------------------------------}
{ Aktualisiert eine Parameter-Befehlsliste mit Versions- und K-Zahl Modus-
  abh�ngigen Elster EK-spezifischen Parameternummern;
  �bergabe: Parameter-Befehlliste
  Ergebnis: false, wenn Fehler beim Aktualisieren aufgetreten ist }
var
  i: integer;
  ParaNrAllg: string;
  ParaNrMrg: string;

Begin
  Result:=false;

  { EK-spezifische Parameternummern-Konfigurationsliste des MRG anlegen und aus
    Ressourcedatenliste laden, wenn noch nicht erfolgt: 06.08.2021, WW }
  if not Assigned (FParamEKKonfigList) then begin
    FParamEKKonfigList:=TParamEKKonfigList.Create;
    
    if not Load_MRGResourceData_ParamEK then exit;  // 06.08.2021, WW
  end;

  { Parameter-Befehlliste "Alle" mit EK-spezifischen Parameternummern updaten: }
  if Assigned (ABefehlListe) then begin
    for i:=ABefehlListe.Count-1 downto 0 do begin
      ParaNrAllg:=TBefehlDataObj (ABefehlListe [i]).Daten.sInfo;
      if FParamEKKonfigList.FindParaNrMrg (FMrgKonvData.ParameterGruppe, ParaNrAllg,
                                           ElsterEK_Version, ElsterEK_K_Zahl_Modus,
                                           ParaNrMrg) then begin
        if length (ParaNrMrg) > 0 then
          TBefehlDataObj (ABefehlListe [i]).Daten.sBefehl:=
            GetLIS200Kommando_Lesen (ParaNrMrg)  // Befehl updaten mit Parameternummer
        else
          ABefehlListe.Delete (i);  // Befehl aus Liste l�schen, Parameter nicht verf�gbar
      end;
    end;
  end;

  Result:=true;
end;

{--------------------------------------------------------}
function TMRGAbruf.SetParamMrgData_ZaehlerFaktor: boolean;
{--------------------------------------------------------}
{ Konfigurationsdaten-Record zu Parameter 'Z�hlerfaktor' aus Parameternummern-
  Konfigurationsliste ermitteln;
  Ergebnis: True, wenn Ermitteln der Parameter-Konfigurationsdaten erfolgreich }
var
  sParamNrAllg: string;
  ParamMrgData: TParamMrgData;

begin
  case FMrgDefData.MrgTyp of
    mrgtyp_RSM200_VCF,
    mrgtyp_RSM200_VMF: sParamNrAllg:=CP_RMG_RSM200_ZaehlerAufloesExp;
  else  // �brige Ger�tetypen
    sParamNrAllg:='';  // Nicht erforderlich
  end;

  if sParamNrAllg <> '' then begin
    { Parameter "Z�hler-Faktor" �ber allg. Parameternummer ermitteln: }
    if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                          sParamNrAllg,
                                                          ParamMrgData) then begin
      FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
      Result:=false;
      exit;
    end;

    // In FParamMrgData_ZaehlerFaktor f�r den Abruf merken:
    FParamMrgData_ZaehlerFaktor:=ParamMrgData;  // 04.04.2024, WW
  end;

  Result:=true;
end;


{----------------- Allgemeine Modbus-Register-Requestlisten -------------------}
// 08.03.2019, WW

{------------------------------------------------------------------------------}
function TMRGAbruf.FillMBRegisterRequestListe_Parameter (AParaFiltertyp: string;
  AAllgParaNrListe: TStringList; RRL: TRegisterRequestList): boolean;
{------------------------------------------------------------------------------}
{ Modbus-Register-Requestliste zum Lesen von Parametern zusammenstellen (basierend
  auf der Parameternummern-Konfigurationsliste des im Abruf befindlichen Ger�ts);
  �bergabe: Filtertyp der Parameter, aus denen die Modbus-Register-Requestliste
              zusammengestellt werden soll (C_PFT_xxx-Konstanten aus MResParam.pas)
              -> Leer = Alle Parameter (kein Filter)
            Zeiger auf Liste abzurufender Parameter (allg. Parameternummern) oder nil
              -> Wenn <> nil: �bergabe 'AParaFiltertyp' nicht relevant
  �bergabe/R�ckgabe: Register-Requestliste
  Ergebnis: True, wenn Zusammenstellen erfolgreich }

  {----------------------------------------------------------------------------}
  function AddParam_RegisterData (ParamMrgData: TParamMrgData;
                                  RegisterDataList: TRegisterDataList): boolean;
  {----------------------------------------------------------------------------}
  { Parameter der Registerdaten-Liste hinzuf�gen;
    Ergebnis: True, wenn Hinzuf�gen erfolgreich }
  var
    sParaNrMrg: string;
    sParaAdrMrg: string;
    iParaAdr: integer;
    sParaDatentyp: string;
    iParaByteLen: integer;
    bAdd: boolean;

  begin
    Result:=true;
    bAdd:=true;

    sParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;
    sParaAdrMrg:=ParamMrgData.Parameteradresse_im_MRG;
    // Wenn eine MRG-Parameteradresse definiert ist, wird diese als
    // MB-Registeradresse verwendet (Kriterium 1), ansonsten die
    // definierte MRG-Parameternummer (Kriterium 2):
    if sParaAdrMrg <> '' then
      iParaAdr:=StrToIntDef (sParaAdrMrg, -1)  // 18.02.2021, WW
    else
      iParaAdr:=StrToIntDef (sParaNrMrg, -1);
    sParaDatentyp:=ParamMrgData.ParaDatentyp;
    iParaByteLen:=ParamMrgData.ParaByteLen;

    // Nur beim Lesen aller Parameter (nicht bei Einzelparameter,
    // nicht bei AllgParaNr-Liste):
    // Parameter, welche nicht unter Login-Passwortnummern ungleich 1
    // gelesen werden k�nnen, ausfiltern (Filtertyp 'PasswortNr 1'); 29.09.2021, WW:
    if not Assigned (AAllgParaNrListe) then begin  // alle
      if (Pos (C_PFT_PASSWORTNR_1, ParamMrgData.Filtertyp) > 0) AND
         (VerbAufbau_PasswortNr <> 1) then
        bAdd:=false;  // ausfiltern
    end;

    if (iParaAdr > -1) AND bAdd then begin
      try
        // Eintrag der Registerdaten-Liste hinzuf�gen (alle Register
        // werden mit Modbus-Funktion 3 gelesen):
        if not RegisterDataList.AddRegisterData (3, iParaAdr, '',
                                                 sParaDatentyp,
                                                 iParaByteLen) then
          Result:=false;
      except
        Result:=false;
      end;
    end;
  end;

var
  i: integer;
  sParaNrAllg: string;
  RegisterDataList: TRegisterDataList;
  bAdd: boolean;
  bHasVolumePara: boolean;
  bHasVolumeFactorPara: boolean;
  sParaFiltertyp: string;

begin
  Result:=true;
  if not Assigned (RRL) then exit;
  RRL.Clear;

  RegisterDataList:=TRegisterDataList.Create;  // Zwischenliste f�r nach Startadresse sortierte Registerdaten
  try
    case FMrgDefData.Kommandogruppe of
      120,  // Primus, Prilog
      121,  // TME400, RSM200
      122:  // FLOWSIC500
        begin
          if FParamMrgKonfigList.Count > 0 then begin  // 09.07.2021, WW
            bHasVolumePara:=false;
            bHasVolumeFactorPara:=false;

            for i:=0 to FParamMrgKonfigList.Count-1 do begin
              sParaNrAllg:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.Parameternummer;
              sParaFiltertyp:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.Filtertyp;

              // alle Parameter oder Liste von Parametern mit allg. Nummer:
              bAdd:=false;
              if Assigned (AAllgParaNrListe) then begin  // Liste
                if AAllgParaNrListe.IndexOf (sParaNrAllg) >=0 then  // 29.09.2021, WW
                  bAdd:=true;
              end
              else if AParaFiltertyp = '' then  // alle Parameter, kein Filter
                bAdd:=true
              else  // alle Parameter, Filter auf Parameter-Filtertyp
                bAdd:=Pos (AParaFiltertyp, sParaFiltertyp) > 0;  // 04.04.2024, WW

              if bAdd then begin
                // Pr�fen auf Z�hler- und Z�hlerfaktor-Parameter:
                case FMrgDefData.MrgTyp of
                  mrgtyp_RSM200_VCF,
                  mrgtyp_RSM200_VMF:  // RSM200; 04.04.2024, WW
                    begin
                      if IsRSM200_VolumeParaNrAllg (sParaNrAllg) then
                        bHasVolumePara:=true;
                      if IsRSM200_VolumeFactorParaNrAllg (sParaNrAllg) then
                        bHasVolumeFactorPara:=true;
                    end;
                end;

                // Parameter der Registerdaten-Liste hinzuf�gen:
                if not AddParam_RegisterData (TParamMrgDataObj (FParamMrgKonfigList [i]).Data,
                                              RegisterDataList) then
                  Result:=false;

                if Assigned (AAllgParaNrListe) then
                  if (AAllgParaNrListe.Count = 1) then  // einzelner Parameter gefunden
                    Break;  // raus
              end;  // if bAdd
            end;  // for i

            // Den erforderlichen Z�hlerfaktor-Parameter zus�tzlich hinzuf�gen,
            // wenn Z�hler-Parameter enthalten sind; 04.04.2024, WW:
            if bHasVolumePara AND not bHasVolumeFactorPara then begin
              if FParamMrgData_ZaehlerFaktor.Parametergruppe > -1 then begin  // wenn vorab erfolgreich ermittelt
                if not AddParam_RegisterData (FParamMrgData_ZaehlerFaktor,
                                              RegisterDataList) then
                  Result:=false;
              end else
                Result:=false;
            end;
          end else
            Result:=false;
        end;
    end;  { case FMrgDefData.Kommandogruppe }

    // In Registerdaten-Liste enthaltene Einzel-Register zu geblockten Requests
    // zusammenfassen und in Register-Requestliste eintragen (Lese-Register):
    if not RRL.LoadFromRegisterDataList (RegisterDataList, false) then
      Result:=false;
  finally
    RegisterDataList.Free;
  end;
end;

// 04.04.2024, WW
{--------------------------------------------------------------------------------}
function TMRGAbruf.FillMBRegisterRequestListe_Geraetezustand (AAllgParaNr: string;
  RRL: TRegisterRequestList): boolean;
{--------------------------------------------------------------------------------}
{ Modbus-Register-Requestliste zum Lesen der Ger�tezustands-Parameter zusammen-
  stellen abh�ngig von allgemeiner Parameternummer;
  �bergabe: Allgemeine Parameternummer f�r Ger�tezustand
  �bergabe/R�ckgabe: Register-Requestliste
  Ergebnis: True, wenn Zusammenstellen erfolgreich }
begin
  { Aktueller Ger�tezustand TME400, RSM200: }
  if (AAllgParaNr = CP_RMG_TME400_GerZustand_aktuell) OR
     (AAllgParaNr = CP_RMG_RSM200_GerZustand_aktuell) then
    Result:=FillMBRegisterRequestListe_Parameter (C_PFT_GERZUSTAND_AKTUELL, nil, RRL)
  else
    Result:=false;
end;

{----------------------------------------------------------------------------}
function TMRGAbruf.FillMBRegisterRequestListe_ArchivDataRec (AMrgTyp: integer;
  AArchivtyp: TArchivtyp; iRecOffset, iMaxRecCount: word;
  RRL: TRegisterRequestList; MBAbrufData: TMBAbrufData; var iRecCount: integer;
  iMinRegisterAdr: integer = -1): boolean;
{----------------------------------------------------------------------------}
{ Modbus-Register-Requestliste zum Lesen von Archivdatens�tzen zusammenstellen
  abh�ngig von Ger�tegruppe;
  �bergaben: Ger�tetyp
             Archivtyp
             Datensatz-Offset (0 = j�ngster Datensatz, 1 = 2.-j�ngster usw.)
             Maximale Anzahl der zu lesenden Datens�tze des Archivs
             MBAbrufData-Record mit Modbusregister-Konfiguration des Archivs
             Minimale Registeradresse, optional (-1 = Keine Pr�fung auf unteres
               Ende des Archiv-Registerbereichs)
  �bergabe/R�ckgabe: Register-Requestliste
  R�ckgabe: Anzahl der Datens�tze, die per Requestliste gelesen werden
  Ergebnis: true, wenn Zusammenstellen der Requestliste ohne Fehler }

  {---------------------------------------------------------------------------}
  function GetRecCount_Request (iArchivRecSize: word; iFktCode: word): integer;
  {---------------------------------------------------------------------------}
  { Anzahl der Archivdatens�tze berechnen, die mit einem Modbus-Request maximal
    ausgelesen werden k�nnen;
    �bergabe: Register-Gr��e des Archivdatensatzes
              Funktionscode, mit dem der Modbus-Request ausgef�hrt wird
    Ergebnis: Anzahl der Archivdatens�tze }
  begin
    case iFktCode of
       3: Result:=(Get_Modbus_MaxDataBytes_03 DIV 2) DIV iArchivRecSize;
      20: Result:=(Get_Modbus_MaxDataBytes_20 DIV 2) DIV iArchivRecSize;
    else
      Result:=0;
    end;

    if (iMinRegisterAdr > -1) then begin  // 18.02.2021, WW
      // Pr�fen, ob das untere Ende des Archiv-Registerbereichs �berschritten w�re:
      if (iRecOffset < (iMinRegisterAdr + Result - 1)) then
        Result:=iRecOffset + 1 - iMinRegisterAdr;  // Anzahl Datens�tze nur bis zum unteren Ende des Archiv-Registerbereichs
    end
    else begin
      // Pr�fen, ob das obere Ende des Archiv-Registerbereichs �berschritten w�re:
      if ((iRecOffset + Result) >= iMaxRecCount) then
        Result:=iMaxRecCount - iRecOffset  // Anzahl Datens�tze nur bis zum oberen Ende des Archiv-Registerbereichs
    end;
  end;

var
  i, j: integer;
  RegisterDataList: TRegisterDataList;
  iStartAdr: word;
  iStartAdrOffset: word;
  iFileNr: word;
  MBRegisterDef: TMBRegisterDef;

begin
  Result:=true;
  iRecCount:=0;  // Vorbelegung f�r Anzahl der Datens�tze

  if not Assigned (RRL) then exit;
  RRL.Clear;

  RegisterDataList:=TRegisterDataList.Create;  // Zwischenliste f�r nach Startadresse sortierte Registerdaten
  try
    case FMrgDefData.Kommandogruppe of
      120:
        begin  { Primus, Prilog }
          case AArchivtyp of
            at_Periodenarchiv:
              begin
                case AMrgTyp of
                  mrgtyp_Primus:
                    begin
                      // Anzahl der Archivdatens�tze je Modbus-Request:
                      iRecCount:=GetRecCount_Request (C_Primus_MBRegisterSize_Mess, 3);

                      for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                        for i:=Low (Primus_MBRegisterDef_Mess) to
                               High (Primus_MBRegisterDef_Mess) do begin
                          with Primus_MBRegisterDef_Mess [i] do begin
                            // Startadresse abh�ngig vom Datensatz-Offset:
                            iStartAdr:=StartAdresse +
                              (C_Primus_MBRegisterSize_Mess * (iRecOffset + j));

                            // Eintrag der Registerdaten-Liste hinzuf�gen:
                            if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                     Typ, AnzahlBytes) then
                              Result:=false;
                          end;
                        end;
                      end;
                    end;

                  mrgtyp_Prilog:  // 25.11.2020, WW
                    begin
                      // Anzahl der Archivdatens�tze je Modbus-Request:
                      iRecCount:=GetRecCount_Request (MBAbrufData.RecSize, 3);    

                      if Assigned (MBAbrufData.MBRegisterDefList) then begin
                        for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                          for i:=0 to MBAbrufData.MBRegisterDefList.Count - 1 do begin  // 11.02.2022, WW
                            MBRegisterDef:=TMBRegisterDefObj (MBAbrufData.MBRegisterDefList [i]).Data;
                            with MBRegisterDef do begin
                              // Startadresse abh�ngig vom Datensatz-Offset:
                              iStartAdr:=StartAdresse +
                                (MBAbrufData.RecSize * (iRecOffset + j));

                              // Eintrag der Registerdaten-Liste hinzuf�gen:
                              if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                       Typ, AnzahlBytes) then
                                Result:=false;
                            end;
                          end;
                        end;                  
                      end;
                    end;
                end;  { case AMrgTyp }
              end;

            at_Ereignisarchiv:
              begin
                case AMrgTyp of
                  mrgtyp_Primus:
                    begin
                      // Anzahl der Archivdatens�tze je Modbus-Request:
                      iRecCount:=GetRecCount_Request (C_Primus_MBRegisterSize_Meld, 3);

                      for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                        for i:=Low (Primus_MBRegisterDef_Meld) to
                               High (Primus_MBRegisterDef_Meld) do begin
                          with Primus_MBRegisterDef_Meld [i] do begin
                            // Startadresse abh�ngig vom Datensatz-Offset:
                            iStartAdr:=StartAdresse +
                              (C_Primus_MBRegisterSize_Meld * (iRecOffset + j));

                            // Eintrag der Registerdaten-Liste hinzuf�gen:
                            if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                     Typ, AnzahlBytes) then
                              Result:=false;
                          end;
                        end;
                      end;
                    end;

                  mrgtyp_Prilog:  // 25.11.2020, WW
                    begin
                      // Anzahl der Archivdatens�tze je Modbus-Request:
                      iRecCount:=GetRecCount_Request (MBAbrufData.RecSize, 3);

                      if Assigned (MBAbrufData.MBRegisterDefList) then begin
                        for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                          for i:=0 to MBAbrufData.MBRegisterDefList.Count - 1 do begin
                            MBRegisterDef:=TMBRegisterDefObj (MBAbrufData.MBRegisterDefList [i]).Data;
                            with MBRegisterDef do begin
                              // Startadresse abh�ngig vom Datensatz-Offset:
                              iStartAdr:=StartAdresse +
                                (MBAbrufData.RecSize * (iRecOffset + j));

                              // Eintrag der Registerdaten-Liste hinzuf�gen:
                              if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                       Typ, AnzahlBytes) then
                                Result:=false;
                            end;
                          end;
                        end;
                      end;
                    end;
                end;  { case AMrgTyp }
              end;
          end;  { case AArchivtyp }
        end;

      121:
        begin  { TME400, RSM200 }
          case AArchivtyp of
            at_Periodenarchiv:
              begin
                // Anzahl der Archivdatens�tze je Modbus-Request:
                if AMrgTyp in [mrgtyp_TME400_VCF, mrgtyp_TME400_VMF] then
                  iRecCount:=GetRecCount_Request (C_TME400_MBRegisterSize_Mess, 20)
                else if AMrgTyp in [mrgtyp_RSM200_VCF, mrgtyp_RSM200_VMF] then  // 09.01.2024, WW
                  iRecCount:=GetRecCount_Request (C_RSM200_MBRegisterSize_Mess, 20)
                else
                  iRecCount:=0;

                // Filenummer im Modbus-Request abh�ngig vom Archivtyp:
                iFileNr:=GetMB_RequestFileNr_Archiv (AMrgTyp, AArchivtyp);

                // Startadresse im Modbus-Request abh�ngig von der Anzahl
                // der Archivdatens�tze je Modbus-Request:
                // -> z.B. Datensatz-Offset = 200, maximale Anzahl Datens�tze = 10:
                //    Registerabfrage 191..200
                iStartAdrOffset:=iRecOffset + 1 - iRecCount;

                case AMrgTyp of
                  mrgtyp_TME400_VCF:
                    begin
                      for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                        for i:=Low (TME400_VCF_MBRegisterDef_Mess) to
                               High (TME400_VCF_MBRegisterDef_Mess) do begin
                          with TME400_VCF_MBRegisterDef_Mess [i] do begin
                            // Startadresse abh�ngig vom Datensatz-Index:
                            iStartAdr:=iStartAdrOffset + StartAdresse +
                              (C_TME400_MBRegisterSize_Mess * j);

                            // Eintrag der Registerdaten-Liste hinzuf�gen:
                            if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                     Typ, AnzahlBytes,
                                                                     '', iFileNr) then
                              Result:=false;
                          end;
                        end;
                      end;
                    end;

                  mrgtyp_TME400_VMF:
                    begin
                      for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                        for i:=Low (TME400_VMF_MBRegisterDef_Mess) to
                               High (TME400_VMF_MBRegisterDef_Mess) do begin
                          with TME400_VMF_MBRegisterDef_Mess [i] do begin
                            // Startadresse abh�ngig vom Datensatz-Index:
                            iStartAdr:=iStartAdrOffset + StartAdresse +
                              (C_TME400_MBRegisterSize_Mess * j);

                            // Eintrag der Registerdaten-Liste hinzuf�gen:
                            if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                     Typ, AnzahlBytes,
                                                                     '', iFileNr) then
                              Result:=false;
                          end;
                        end;
                      end;
                    end;

                  mrgtyp_RSM200_VCF:  // 09.01.2024, WW
                    begin
                      for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                        for i:=Low (RSM200_VCF_MBRegisterDef_Mess) to
                               High (RSM200_VCF_MBRegisterDef_Mess) do begin
                          with RSM200_VCF_MBRegisterDef_Mess [i] do begin
                            // Startadresse abh�ngig vom Datensatz-Index:
                            iStartAdr:=iStartAdrOffset + StartAdresse +
                              (C_RSM200_MBRegisterSize_Mess * j);

                            // Eintrag der Registerdaten-Liste hinzuf�gen:
                            if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                     Typ, AnzahlBytes,
                                                                     '', iFileNr) then
                              Result:=false;
                          end;
                        end;
                      end;
                    end;

                  mrgtyp_RSM200_VMF:  // 09.01.2024, WW
                    begin
                      for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                        for i:=Low (RSM200_VMF_MBRegisterDef_Mess) to
                               High (RSM200_VMF_MBRegisterDef_Mess) do begin
                          with RSM200_VMF_MBRegisterDef_Mess [i] do begin
                            // Startadresse abh�ngig vom Datensatz-Index:
                            iStartAdr:=iStartAdrOffset + StartAdresse +
                              (C_RSM200_MBRegisterSize_Mess * j);

                            // Eintrag der Registerdaten-Liste hinzuf�gen:
                            if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                     Typ, AnzahlBytes,
                                                                     '', iFileNr) then
                              Result:=false;
                          end;
                        end;
                      end;
                    end;

                end;  { case AMrgTyp }
              end;

            at_Ereignisarchiv:
              begin
                case AMrgTyp of
                  mrgtyp_TME400_VCF,
                  mrgtyp_TME400_VMF: 
                    begin
                      // Anzahl der Archivdatens�tze je Modbus-Request:
                      iRecCount:=GetRecCount_Request (C_TME400_MBRegisterSize_Ereignis, 20);

                      // Filenummer im Modbus-Request abh�ngig vom Archivtyp:
                      iFileNr:=GetMB_RequestFileNr_Archiv (AMrgTyp, AArchivtyp);

                      // Startadresse im Modbus-Request abh�ngig von der Anzahl
                      // der Archivdatens�tze je Modbus-Request:
                      // -> z.B. Datensatz-Offset = 200, maximale Anzahl Datens�tze = 10:
                      //    Registerabfrage 191..200
                      iStartAdrOffset:=iRecOffset + 1 - iRecCount;

                      for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                        for i:=Low (TME400_MBRegisterDef_Ereignis) to
                               High (TME400_MBRegisterDef_Ereignis) do begin
                          with TME400_MBRegisterDef_Ereignis [i] do begin
                            // Startadresse abh�ngig vom Datensatz-Index:
                            iStartAdr:=iStartAdrOffset + StartAdresse +
                              (C_TME400_MBRegisterSize_Ereignis * j);

                            // Eintrag der Registerdaten-Liste hinzuf�gen:
                            if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                     Typ, AnzahlBytes,
                                                                     '', iFileNr) then
                              Result:=false;
                          end;
                        end;
                      end;
                    end;

                  mrgtyp_RSM200_VCF,
                  mrgtyp_RSM200_VMF:  // 09.01.2024, WW
                    begin
                      // Anzahl der Archivdatens�tze je Modbus-Request:
                      iRecCount:=GetRecCount_Request (C_RSM200_MBRegisterSize_Ereignis, 20);

                      // Filenummer im Modbus-Request abh�ngig vom Archivtyp:
                      iFileNr:=GetMB_RequestFileNr_Archiv (AMrgTyp, AArchivtyp);

                      // Startadresse im Modbus-Request abh�ngig von der Anzahl
                      // der Archivdatens�tze je Modbus-Request:
                      // -> z.B. Datensatz-Offset = 200, maximale Anzahl Datens�tze = 10:
                      //    Registerabfrage 191..200
                      iStartAdrOffset:=iRecOffset + 1 - iRecCount;

                      for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                        for i:=Low (RSM200_MBRegisterDef_Ereignis) to
                               High (RSM200_MBRegisterDef_Ereignis) do begin
                          with RSM200_MBRegisterDef_Ereignis [i] do begin
                            // Startadresse abh�ngig vom Datensatz-Index:
                            iStartAdr:=iStartAdrOffset + StartAdresse +
                              (C_RSM200_MBRegisterSize_Ereignis * j);

                            // Eintrag der Registerdaten-Liste hinzuf�gen:
                            if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                     Typ, AnzahlBytes,
                                                                     '', iFileNr) then
                              Result:=false;
                          end;
                        end;
                      end;
                    end;

                end;  { case AMrgTyp }
              end;

            at_Parameterarchiv_eichamtlich,
            at_Parameterarchiv_nichteichamtlich:
              begin
                case AMrgTyp of
                  mrgtyp_TME400_VCF,
                  mrgtyp_TME400_VMF:
                    begin
                      // Anzahl der Archivdatens�tze je Modbus-Request:
                      iRecCount:=GetRecCount_Request (C_TME400_MBRegisterSize_ParaAend, 20);

                      // Filenummer im Modbus-Request abh�ngig vom Archivtyp:
                      iFileNr:=GetMB_RequestFileNr_Archiv (AMrgTyp, AArchivtyp);

                      // Startadresse im Modbus-Request abh�ngig von der Anzahl
                      // der Archivdatens�tze je Modbus-Request:
                      // -> z.B. Datensatz-Offset = 200, maximale Anzahl Datens�tze = 10:
                      //    Registerabfrage 191..200
                      iStartAdrOffset:=iRecOffset + 1 - iRecCount;

                      for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                        for i:=Low (TME400_MBRegisterDef_ParaAend) to
                               High (TME400_MBRegisterDef_ParaAend) do begin
                          with TME400_MBRegisterDef_ParaAend [i] do begin
                            // Startadresse abh�ngig vom Datensatz-Index:
                            iStartAdr:=iStartAdrOffset + StartAdresse +
                              (C_TME400_MBRegisterSize_ParaAend * j);

                            // Eintrag der Registerdaten-Liste hinzuf�gen:
                            if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                     Typ, AnzahlBytes,
                                                                     '', iFileNr) then
                              Result:=false;
                          end;
                        end;
                      end;
                    end;

                  mrgtyp_RSM200_VCF,
                  mrgtyp_RSM200_VMF:  // 09.01.2024, WW
                    begin
                      // Anzahl der Archivdatens�tze je Modbus-Request:
                      iRecCount:=GetRecCount_Request (C_RSM200_MBRegisterSize_ParaAend, 20);

                      // Filenummer im Modbus-Request abh�ngig vom Archivtyp:
                      iFileNr:=GetMB_RequestFileNr_Archiv (AMrgTyp, AArchivtyp);

                      // Startadresse im Modbus-Request abh�ngig von der Anzahl
                      // der Archivdatens�tze je Modbus-Request:
                      // -> z.B. Datensatz-Offset = 200, maximale Anzahl Datens�tze = 10:
                      //    Registerabfrage 191..200
                      iStartAdrOffset:=iRecOffset + 1 - iRecCount;

                      for j:=0 to (iRecCount - 1) do begin  // f�r Anzahl der Archivdatens�tze
                        for i:=Low (RSM200_MBRegisterDef_ParaAend) to
                               High (RSM200_MBRegisterDef_ParaAend) do begin
                          with RSM200_MBRegisterDef_ParaAend [i] do begin
                            // Startadresse abh�ngig vom Datensatz-Index:
                            iStartAdr:=iStartAdrOffset + StartAdresse +
                              (C_RSM200_MBRegisterSize_ParaAend * j);

                            // Eintrag der Registerdaten-Liste hinzuf�gen:
                            if not RegisterDataList.AddRegisterData (FktCode, iStartAdr, '',
                                                                     Typ, AnzahlBytes,
                                                                     '', iFileNr) then
                              Result:=false;
                          end;
                        end;
                      end;
                    end;

                end;  { case AMrgTyp }
              end;

          end;  { case AArchivtyp }
        end;
    end;  { case FMrgDefData.Kommandogruppe }

    // In Registerdaten-Liste enthaltene Einzel-Register zu geblockten Requests
    // zusammenfassen und in Register-Requestliste eintragen (Lese-Register):
    if not RRL.LoadFromRegisterDataList (RegisterDataList, false) then
      Result:=false;
  finally
    RegisterDataList.Free;
  end;
end;


{---------------- Modbus-Register-Requestlisten TME400, RSM200 ----------------}

{------------------------------------------------------------------------------}
function TMRGAbruf.FillMBRegisterRequestListe_Archivheader_TME400 (
  AMrgTyp: integer; AArchivtyp: TArchivtyp; RRL: TRegisterRequestList): boolean;
{------------------------------------------------------------------------------}
{ Modbus-Register-Requestliste zum Lesen eines TME400-Archivheaders zusammenstellen;
  -> auch f�r RSM200
  �bergaben: Ger�tetyp
             Archivtyp
  �bergabe/R�ckgabe: Register-Requestliste
  Ergebnis: true, wenn Zusammenstellen der Requestliste ohne Fehler }
var
  i: integer;
  RegisterDataList: TRegisterDataList;
  iFileNr: word;

begin
  Result:=true;

  if not Assigned (RRL) then exit;
  RRL.Clear;

  RegisterDataList:=TRegisterDataList.Create;  // Zwischenliste f�r nach Startadresse sortierte Registerdaten
  try
    // Filenummer im Modbus-Request abh�ngig vom Archivtyp:
    iFileNr:=GetMB_RequestFileNr_Archivheader (AMrgTyp, AArchivtyp);

    for i:=Low (TME400_MBRegisterDef_Archivheader) to
           High (TME400_MBRegisterDef_Archivheader) do begin
      with TME400_MBRegisterDef_Archivheader [i] do begin
        // Eintrag der Registerdaten-Liste hinzuf�gen:
        if not RegisterDataList.AddRegisterData (FktCode, StartAdresse, '',
                                                 Typ, AnzahlBytes, '',
                                                 iFileNr) then
          Result:=false;
      end;
    end;

    // In Registerdaten-Liste enthaltene Einzel-Register zu geblockten Requests
    // zusammenfassen und in Register-Requestliste eintragen (Lese-Register):
    if not RRL.LoadFromRegisterDataList (RegisterDataList, false) then
      Result:=false;
  finally
    RegisterDataList.Free;
  end;
end;


{------------------------ Ressourcedaten laden --------------------------------}

// 06.08.2021, WW
{-----------------------------------------------------------------------------------------}
function TMRGAbruf.Load_MRGResourceData_ParamMrg (AParameterUntergruppe: integer): boolean;
{-----------------------------------------------------------------------------------------}
{ Liste mit den zum abzurufenden MRG geh�renden Parameter-Nummern einer
  Parameter-Untergruppe aus der Ressourcedatenliste laden;
  �bergabe: Parameter-Untergruppe
  Ergebnis: true, wenn Laden erfolgreich }
var
  pListObj: TObject;
  pParamMrgResourceList: TParamMrgKonfigList;

begin
  Result:=false;
  if Assigned (FResourceFilesList) then begin
    pListObj:=FResourceFilesList.ListObject [rft_ParamMrg];
    if Assigned (pListObj) then begin
      pParamMrgResourceList:=TParamMrgKonfigList (pListObj);

      { ParamMrgData-Eintr�ge zu Parametergruppe des MRG laden: }
      if FParamMrgKonfigList.LoadFromList_ByParaGruppe (FMrgKonvData.ParameterGruppe,
                                                        AParameterUntergruppe,
                                                        pParamMrgResourceList) then                   
        Result:=true;
    end;
  end;
  if not Result then
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
end;

{---------------------------------------------------------}
function TMRGAbruf.Load_MRGResourceData_ParamMeld: boolean;
{---------------------------------------------------------}
{ Liste mit allen zum abzurufenden MRG geh�renden meldungsspezifischen Parameter-
  Nummern aus der Ressourcedatenliste laden;
  Ergebnis: true, wenn Laden erfolgreich }
var
  pListObj: TObject;
  pParamMeldResourceList: TParamMeldKonfigList;

begin
  Result:=false;
  if Assigned (FResourceFilesList) then begin
    pListObj:=FResourceFilesList.ListObject [rft_ParamMeld];
    if Assigned (pListObj) then begin
      pParamMeldResourceList:=TParamMeldKonfigList (pListObj);

      { ParamMeldData-Eintr�ge zu Parametergruppe des MRG laden: }
      if FParamMeldKonfigList.LoadFromList_ByParaGruppe (
           FMrgKonvData.ParameterGruppe, pParamMeldResourceList) then
        Result:=true;
    end;
  end;
  if not Result then
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
end;

{-------------------------------------------------------}
function TMRGAbruf.Load_MRGResourceData_ParamEK: boolean;
{-------------------------------------------------------}
{ Liste mit allen zum abzurufenden MRG geh�renden EK-spezifischen Parameter-Nummern
  aus der Ressourcedatenliste laden;
  Ergebnis: true, wenn Laden erfolgreich }
var
  pListObj: TObject;
  pParamEKResourceList: TParamEKKonfigList;

begin
  Result:=false;
  if Assigned (FResourceFilesList) then begin
    pListObj:=FResourceFilesList.ListObject [rft_ParamEK];
    if Assigned (pListObj) then begin
      pParamEKResourceList:=TParamEKKonfigList (pListObj);

      { ParamEKData-Eintr�ge zu Parametergruppe des MRG laden: }
      if FParamEKKonfigList.LoadFromList_ByParaGruppe (FMrgKonvData.ParameterGruppe,
           pParamEKResourceList) then
        Result:=true;
    end;
  end;
  if not Result then
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
end;

{------------------------------------------------------------------}
function TMRGAbruf.Load_MRGResourceData_MeldNr (AGeraeteArt: string;
  AMeldGrpNr: integer): boolean;
{------------------------------------------------------------------}
{ Liste mit allen zu Ger�teart und Meldungsgruppe eines MRG geh�renden
  Meldungsnummern aus der Ressourcedatenliste laden;
  Ergebnis: true, wenn Laden erfolgreich }
var
  pListObj: TObject;
  pMeldNrResourceList: TMeldNrKonfigList;

begin
  Result:=false;
  if Assigned (FResourceFilesList) then begin
    pListObj:=FResourceFilesList.ListObject [rft_MeldNr];
    if Assigned (pListObj) then begin
      pMeldNrResourceList:=TMeldNrKonfigList (pListObj);

      { MeldNrData-Eintr�ge zu Ger�teart, Meldungsgruppe laden: }
      if FMeldNrKonfigList.LoadFromList_ByGerArt_MeldGrpNr (AGeraeteArt, AMeldGrpNr,
           pMeldNrResourceList) then
        Result:=true;
    end;
  end;
  if not Result then
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
end;

// 11.02.2022, WW
{-----------------------------------------------------------------}
function TMRGAbruf.Get_MRGResourceData_MBAbruf (AKommandotyp: char;
  var MBAbrufData: TMBAbrufData): boolean;
{-----------------------------------------------------------------}
{ Liest Konfigurationsdaten f�r Modbusregister-Abruf und -Konvertierung von
  Ger�tearchiven;
  �bergabe: Kommandotyp (E = Messwerte, M = Meldungen)
  R�ckgabe: MBAbrufData-Record
  Ergebnis: true, wenn Lesen erfolgreich }
begin
  if FMB_ID > -1 then begin  // Nur, wenn die Modbuslisten-ID des Ger�ts ausgelesen wurde
    Result:=false;
    if Assigned (FResourceFilesList) then
      Result:=FResourceFilesList.GetMBAbrufData (FMrgDefData.MrgTyp,
                                                 AKommandotyp, FMB_ID,
                                                 MBAbrufData);
    if not Result then begin
      if AKommandotyp = 'E' then
        FehlerGruppeCodeUpdate (EST_MWLESENERROR + ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND)
      else if AKommandotyp = 'M' then
        FehlerGruppeCodeUpdate (EST_MELESENERROR + ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND)
      else
        FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
    end;
  end else  // ansonsten nicht erforderlich, OK
    Result:=true;
end;


{------------------------------------------------------------------------------}

{----------------------------------------------------------}
function TMRGAbruf.Init_MRGKonfigData (iGeraetetyp: integer;
  bInitParamKonfig: boolean = true): boolean;
{----------------------------------------------------------}
{ Konfigurationsdaten f�r Ger�tetyp initialisieren:
  -> Default-Vorbelegungen
  -> MrgDefData, MrgKonvData ermitteln
  -> Infobefehl f�r Ger�tetyp vorhanden (MrgInfoData) ?
  -> Optional: Parameter-Konfigurationsdaten ermitteln
  �bergaben: Ger�tetyp
             Flag 'Parameter-Konfigurationsdaten initialisieren' ja/nein
  Ergebnis: true, wenn Initialisieren erfolgreich }
var
  i: integer;
  bResourceOK: boolean;

begin
  Result:=false;
  FGeraetetyp:=iGeraetetyp;  // aktuellen im Abruf befindlichen Ger�tetyp aktualisieren

  { f�r Abruf von Ger�ten mit IEC 1107-Protokoll: }
  FAbschaltBefehl:=false;  { auch f�r Actaris Corus }
  IEC1107_SchlossStatus:=st_Schloss_bereits_offen;
  for i:=Low (IEC1107_KanalAdressen) to High (IEC1107_KanalAdressen) do
    IEC1107_KanalAdressen [i]:='';

  { f�r Parametrierung von Ger�ten vom Typ Elster DS-100, RMG TME400,
    SICK FLOWSIC500: }
  FParametrierModus_gesetzt:=false;

  { f�r Abruf von Ger�ten vom Typ Elster EKnnn: }
  ElsterEK_Version:='';  // Version unbekannt
  ElsterEK_K_Zahl_Modus:='';  // K-Zahl Modus unbekannt

  { f�r Abruf von Ger�ten mit mehreren Abfragebefehlen f�r Stundenwerte/Tagess�tze: }
  StdKanalNr:=0;
  TagKanalNr:=0;
  MRGAnzKanaele:=0;
  FAktKanal:=1;  // f�r DS-100

  { f�r Abruf von Ger�ten mit Modbus-Protokoll: 11.02.2022, WW }
  with FMBRegisterDef_Para_Passwort do begin
    StartAdresse:=0;
    Typ:='';
    AnzahlBytes:=0;
    FktCode:=0;
    KanalDef:='';
  end;
  FMB_ID:=-1;

  { f�r Abruf von Ger�ten mit Verrechnung der Z�hler-Parameterrohwerte mit
    Faktor-Parameter: 04.04.2024, WW }
  with FParamMrgData_ZaehlerFaktor do begin
    Parametergruppe:=-1;
    Parameternummer:='';
    Parameternummer_im_MRG:='';
    ParaDatentyp:='';
    ParaAnzahl:=-1;
    ParaByteLen:=-1;
    Filtertyp:='';
    Parameternummer_im_MRG_Schreiben:='';
    AusgabeFormat:='';
    Parameteradresse_im_MRG:='';
    ParameterUntergruppe:=-1;
  end;

  { Konfigurationsdaten mit typspezifischen MRG-Informationen lesen: }
  bResourceOK:=false;
  if Assigned (FResourceFilesList) then
    bResourceOK:=FResourceFilesList.GetMrgDefData (iGeraetetyp, FMrgDefData);  // 06.08.2021, WW
  if not bResourceOK then begin
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
    exit;
  end;

  { Konfigurationsdaten f�r Konvertierungen lesen: 09.07.2021, WW }
  bResourceOK:=false;
  if Assigned (FResourceFilesList) then
    bResourceOK:=FResourceFilesList.GetMrgKonvData (iGeraetetyp, FMrgKonvData);  // 06.08.2021, WW
  if not bResourceOK then begin
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
    exit;
  end;

  { wenn Infobefehl f�r Ger�tetyp vorhanden, zus�tzliche Informationen aus
    MRGINFO.DAT holen: }
  if length (FMrgDefData.Infobefehl) > 0 then begin
    if Assigned (FResourceFilesList) then
      bResourceOK:=FResourceFilesList.GetMrgInfoData (iGeraetetyp, FMrgInfoData);  // 06.08.2021, WW
    if not bResourceOK then begin
      FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
      exit;
    end;
  end;

  { Parameternummer zum Halten der Verbindung lesen: }
  bResourceOK:=false;
  if Assigned (FResourceFilesList) then
    bResourceOK:=FResourceFilesList.GetParamNrMomVerb (FMrgKonvData.ParameterGruppe,
                                                       FParaNrAllg_VerbHalten);  // 14.04.2023, WW
  if not bResourceOK then begin
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
    exit;
  end;

  { Liste mit abgerufenen Ger�te-Parametern leeren: }
  ParameterListe_MRGAbruf.Clear;
  // ...und Ersatzzeichen-Flag der Ger�te-Parameter l�schen; 17.10.12023, WW
  ParameterListe_MRGAbruf.Ersatzzeichen:=false;

  Alle_Parameter_gelesen:=false;
  LGZnormKonv_Parameter_gelesen:=false;

  { Konfigurationslisten des MRG, welche nur bei Bedarf ben�tigt werden, freigeben: }
  FreeAndNil (FParamEKKonfigList);  { Liste mit Elster EK-Parametern
    -> Wird im weiteren Verlauf nur bei Abruf eines Elster EK-Ger�tetyps geladen; 09.07.2021, WW }
  FreeAndNil (FParamMeldKonfigList);  { Liste mit meldungsspezifischen Parameternummern
    -> Wird im weiteren Verlauf nur bei der Konvertierung von Meldungen geladen; 06.08.2021, WW }
  FreeAndNil (FMeldNrKonfigList);  { Liste mit Meldungsnummern
    -> Wird im weiteren Verlauf nur bei der Konvertierung von Meldungen geladen; 06.08.2021, WW }

  { Parameter-Konfigurationsdaten f�r MRG initialisieren: }
  if bInitParamKonfig then  // 11.02.2022, WW
    if not Init_MRGKonfigData_Param (FMB_ID) then exit;

  { Flag f�r Protokoll-Reinitialisierung zur�cksetzen: }
  TMRGCustomCommObj (CommObj).ReInitProtokoll:=false;  // 30.04.2019, WW

  Result:=true;
end;

{------------------------------------------------------------------------------------}
function TMRGAbruf.Init_MRGKonfigData_Param (AParameterUntergruppe: integer): boolean;
{------------------------------------------------------------------------------------}
{ Parameter-Konfigurationsdaten f�r abzurufendes MRG initialisieren:
  -> Parameternummern-Konfigurationsliste laden (abh�ngig von ggf. ausgelesener
     Modbuslisten-ID des Ger�ts)
  -> Befehlsliste zum Abruf aller Parameter laden, wenn f�r Ger�tetyp erforderlich
  -> Modbus-Registerrequestliste zum Abruf aller Parameter laden, wenn f�r
     Ger�tetyp erforderlich
  -> Konfigurationsdaten-Record zu Parameter 'Z�hlerfaktor' ermitteln, wenn f�r
     Ger�tetyp erforderlich
  �bergabe: Parameteruntergruppe
  Ergebnis: true, wenn Initialisieren erfolgreich }
begin
  Result:=false;

  { Liste mit den zum abzurufenden MRG geh�renden Parameter-Nummern der
    Parameter-Untergruppe aus der Ressourcedatenliste laden: }
  if not Load_MRGResourceData_ParamMrg (AParameterUntergruppe) then exit;  // 06.08.2021, WW

  { Laden der Befehle zum Lesen aller Parameter: }
  FillParameterBefehlListe ('', nil, FParameterBefehlListe_Alle);

  { Laden der Modbus-Requests zum Lesen aller Parameter: 08.03.2019, WW }
  FillMBRegisterRequestListe_Parameter ('', nil, FParameterRegisterRequestListe_Alle);

  { Konfigurationsdaten-Record zu Parameter 'Z�hlerfaktor' ermitteln: }
  SetParamMrgData_ZaehlerFaktor;  // 04.04.2024, WW

  Result:=true;
end;

{-----------------------------------------------------------------------}
function TMRGAbruf.SetDUEGeschwindigkeit_Fup (ModemTyp: string): boolean;
{-----------------------------------------------------------------------}
{ D�-Geschwindigkeit im FUP einstellen (Sx-Befehl); nur f�r FUP-9600 !
  �bergabe: ModemTyp
  Ergebnis: true, wenn Einstellen erfolgreich }
var
  SI: TSrvCfg32Ini;
  DUEStr: string;
  BefehlChar: char;
  Befehl: string;
  R: TRueckgabe;
  dummy: boolean;
  FupAntwort: string;
  iModemTyp: integer;

begin
  Result:=false;
  { Befehl zum Einstellen der FUP-D�-Geschwindigkeit lesen: }
  SI:=TSrvCfg32Ini.Create (FNetProgPath);
  try
    iModemTyp:=StrToIntDef (ModemTyp, -1);
    case iModemTyp of
      srv_fup_1200_HDX: DUEStr:=SI.Fup1200 [FCOMNr];   // 2 -> S2
      srv_fup_2400_DX : DUEStr:=SI.Fup2400 [FCOMNr];   // 6 -> S6
    else
      DUEStr:=SI.Fup1200 [FCOMNr];  // �brige -> S2
    end;
  finally
    SI.Free;
  end;

  if length (DUEStr) > 0 then begin
    BefehlChar:=DUEStr[1];
    if IsCharUpper (BefehlChar) then begin
      Befehl:=ESC+DUEStr+CR+ESC+LowerCase (BefehlChar)+CR;

      { D�-Geschwindigkeits-Kommando senden }
      if not CommObj.SendCommand (Befehl, [CR], 1, MRGTimeouts.FupAntwort, ad_String, R, dummy) then begin
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

{------------------------------------------------------------------------}
function TMRGAbruf.ModbuslisteID_Abfragen (iGeraetetyp: integer): boolean;
{------------------------------------------------------------------------}
{ Liest den Ger�teparameter zur Identifizierung der im Ger�t vorhandenen Modbus-
  Registerliste;
  -> Mu� vor dem Laden der versionsabh�ngigen Modbus-Parameter aufgerufen werden !
  �bergabe: Ger�tetyp
  Ergebnis: true, wenn Lesen der Modbuslisten-ID erfolgreich }
var
  sParaNrAllg_MBListeID: string;
  PL: TParameterListe;
  iValue: integer;

begin
  Result:=false;

  case iGeraetetyp of
    mrgtyp_Prilog: sParaNrAllg_MBListeID:=CP_RMG_PrimusPrilog_ModbuslisteID;
  else  // f�r �brige Modbus-Ger�tetypen nicht erforderlich (eindeutige Registerliste)
    sParaNrAllg_MBListeID:='';
  end;

  if sParaNrAllg_MBListeID <> '' then begin
    // F�r das Abfragen des Identifizierungs-Parameters m�ssen die Konfigurationsdaten
    // der fixen (variantenunabh�ngigen) Parameter des MRG geladen sein:
    if not Init_MRGKonfigData_Param (C_PUG_FIXED) then exit;  // 11.02.2022, WW

    PL:=TParameterListe.Create (FKonfigPath);
    try
      if not AbrufParameter (sParaNrAllg_MBListeID, nil, PL) then exit;
    finally
      PL.Free;
    end;
    // abgerufene Modbuslisten-ID als Ganzzahl aus Parameterliste lesen:
    if ParameterListe_MRGAbruf.GetValueInt (sParaNrAllg_MBListeID, iValue) then
      FMB_ID:=iValue;
  end;

  Result:=true;
end;

{-----------------------------------------------------}
function TMRGAbruf.GeraeteTypAbfragen_Pruefen: boolean;
{-----------------------------------------------------}
var
  PL: TParameterListe;
  sGerVersion: string;

begin
  Result:=false;
  // wenn Parameter 'Ger�teversion' noch nicht in Parameterliste verf�gbar
  // ist: Auslesen !
  if not ParameterListe_MRGAbruf.GetValue (CP_ALLG_Geraeteversion, sGerVersion) then begin
    PL:=TParameterListe.Create (FKonfigPath);
    try
      if not AbrufParameter (CP_ALLG_GeraeteVersion, nil, PL) then exit;  // Ger�teversion abrufen
    finally
      PL.Free;
    end;
    ParameterListe_MRGAbruf.GetValue (CP_ALLG_Geraeteversion, sGerVersion);
  end;

  if not CheckGeraeteTyp (FGeraetetyp, sGerVersion) then begin
    if FehlerGruppeCode_OK (Fehlergruppe, Fehlercode) then  // vorhandenen Fehler nicht �berschreiben
      FehlerGruppeCodeUpdate (EST_GERAETECHECK, GERCHECKERR_TYP_FALSCH);
  end;
  Result:=true;
end;

{----------------------------------------------------------------}
function TMRGAbruf.ElsterEK_Version_K_Zahl_ModusAbfragen: boolean;
{----------------------------------------------------------------}
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  ElsterEK_Version:='';  // Default: EK-Version unbekannt
  ElsterEK_K_Zahl_Modus:='';  // Default: K-Zahl Modus unbekannt

  TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);    { Flag setzen, es folgt Befehl mit BCC }
  TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC); { Flag setzen, es folgt Antwort mit BCC }

  { Parameter f�r K-Zahl Modus: }
  Befehl:=GetLIS200Kommando_Lesen ('8:317');
  if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  { Antwort auswerten: }
  if not ValidElster_IEC1107DatenTelegrammAntwort (R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  ElsterEK_K_Zahl_Modus:=ExtractString (R.Antwort, '(', ')', 0);  { K-Zahl Modus steht zwischen den runden Klammern }

  { Version-Test-Parameter 10:314 lesen: }
  Befehl:=GetLIS200Kommando_Lesen ('10:314');
  if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  { Antwort auswerten: }
  if ValidElster_IEC1107DatenTelegrammAntwort (R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    ElsterEK_Version:=C_EK_VERS_c;  // Version 'c'
    Result:=true;
    exit;
  end
  else begin
    if not ((R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_FEHLERTELEGRAMM)) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end;

  { Version-Test-Parameter 10:36C lesen: }
  Befehl:=GetLIS200Kommando_Lesen ('10:36C');
  if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  { Antwort auswerten: }
  if ValidElster_IEC1107DatenTelegrammAntwort (R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    ElsterEK_Version:=C_EK_VERS_b;  // Version 'b'
    Result:=true;
    exit;
  end
  else begin
    if not ((R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_FEHLERTELEGRAMM)) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end;
  ElsterEK_Version:=C_EK_VERS_a;  // Version 'a'

  Result:=true;
end;


{--------------------------------------------------------------------------------}
function TMRGAbruf.KennungAbfragen (ModemAbrufgruppe: integer; Infobefehl: string;
                                    InfoData_Kennung: string;
                                    RufTyp: integer;
                                    bCheckGeraeteTyp: boolean) : boolean;
{--------------------------------------------------------------------------------}
{ Kennung des MRG abfragen;
  �bergabe: ModemAbrufgruppe
            Infobefehl
            Information zum Ermitteln der Kennung aus dem Infobefehl
            RufTyp (> 0, wenn Kennungsabfrage im Rahmen einer Rufentgegennahme erfolgt)
            Flag 'bCheckGeraeteTyp': wenn true, wird Ger�tetyp gepr�ft
  Ergebnis: true, wenn Kennung erfolgreich abgefragt wurde }
type
  TAbfrageModus = (am_Infobefehl, am_k_Befehl, am_Bnnn_Befehl);

var
  Befehl: string;
  R: TRueckgabe;
  WithCRC_Merk: boolean;
  Left_Count, Before: word;
  Left, Right: array [0..10] of char;
  pInfo_Kennung: array [0..100] of char;
  cLeft, cRight: char;
  S: string;
  AbfrageModus: TAbfrageModus;
  dummy: integer;
  Antw: string;
  gefunden: boolean;
  Kennziffer_Geraetenummer: string;
  Kommando_Soll: string;
  bWecken: boolean;
  PosK: integer;
  PosETX: integer;
  KennungParaNrMrg: string;
  CustomParaNr_Kennung: string;
  AI: TAbrufSrvIni;
  sGerTyp: string;
  PL: TParameterListe;
  sParaNrAllg_Kennung: string;
  ParamMrgData: TParamMrgData;

begin
  Result:=false;

  if (FGeraetetyp > -1) AND (length (FMrgDefData.MrgName) > 0) then begin
    { spezielle ger�tetypabh�ngige Parameternummer f�r die Kennungsabfrage aus
      Programm-Ini lesen: 18.12.2009, WW }
    AI:=TAbrufSrvIni.Create;
    try
      CustomParaNr_Kennung:=AI.MRG_ParaNr_Kennung [FMrgDefData.MrgName];
    finally
      AI.Free;
    end;
  end else  // bei RE sind Ger�tetyp und Ger�tename hier nicht bekannt; 18.01.2010, WW
    CustomParaNr_Kennung:='';

  { Kennungsabfrage f�r Modemgruppe 5 (IEC 1107-Protokoll Elster, z.B. DL240, EK260): }
  if ModemAbrufgruppe = 5 then begin
    TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);    { Flag setzen, es folgt Kennungs-Befehl mit BCC }
    TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC); { Flag setzen, es folgt Kennungs-Antwort mit BCC }

    { Kennung (= "Stationsnummer") lesen: }
    Befehl:=GetLIS200Kommando_Lesen ('3:180');
    if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    { Antwort auswerten: }
    if not ValidElster_IEC1107DatenTelegrammAntwort (R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    R.Antwort:=ExtractString (R.Antwort, '(', ')', 0);  { Stationsnummer steht zwischen den runden Klammern }
  end

  { Kennungsabfrage f�r Modemgruppe 6 (KE-Anlagen): }
  else if ModemAbrufgruppe = 6 then begin
    if FMrgDefData.MrgTyp = mrgtyp_KE_Ruhrgas then
      Befehl:=GetKE_Kommando ('S001')
    else if FMrgDefData.MrgTyp = mrgtyp_KE_PPN then
      Befehl:=GetKE_Kommando ('T001')
    else
      Befehl:='';

    { Kennungsabfragekommando senden: }
    if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Kennung, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Antwort auf Kennungsabfrage auswerten: }
    if not ValidKEAntwort (R.Antwort, dummy, R.Fehlergruppe, R.Fehlercode) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Kennung aus Antwort auf KE-Befehl filtern: }
    R.Antwort:=ExtractString (R.Antwort, STX, ETX, 0);
    Delete (R.Antwort, 1, 4);  { Qnnn rausl�schen }
  end

  { Kennungsabfrage f�r Modemgruppe 7 (Datacon FWU): }
  else if ModemAbrufgruppe = 7 then begin
    Befehl:='/?!'+ CR+ LF;
    { Befehl senden: }
    if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Kennung, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    { Kennung aus Rohantwort extrahieren: }
    Antw:=R.Antwort;
    gefunden:=false;
    while length (Antw) > 0 do begin
      S:=F_Zerlegen (Antw, LF);
      if length (S) > 0 then begin
        if Copy (S, 1, 10) = '7-00:0.0.0' then begin      { Kennziffer f�r Stationsnummer }
          R.Antwort:=ExtractString (S, '(', ')', 0);  { Stationsnummer steht zwischen den runden Klammern }
          gefunden:=true;
          Break;
        end;
      end;
    end;  { while length (Antw) }
    if not gefunden then
      R.Antwort:='';
  end

  { Kennungsabfrage f�r Modemgruppe 8 (Tritschler-Ger�te mit IEC-Protokoll: VC2, TTG, TDS, VC3, VCC): }
  else if ModemAbrufgruppe = 8 then begin
    if (FMrgDefData.MrgTyp = mrgtyp_VC2) OR
       (FMrgDefData.MrgTyp = mrgtyp_VC3) OR
       (FMrgDefData.MrgTyp = mrgtyp_VC3_VC2komp) OR
       (FMrgDefData.MrgTyp = mrgtyp_VCC) then begin
      { "Parameter"-Befehl f�r VC2 (Abfrage aktueller Umwerterdaten): }
      if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
        Befehl:=GetTritschler_IECKommando_05_AktUmwerterDaten (SSU_GeraeteAuswNr,
          VerbAufbau_Passwort);
        bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
      end
      else begin { ohne Multiplexer SSU }
        Befehl:=GetTritschler_IECKommando_05_AktUmwerterDaten (0, '');
        bWecken:=true;
      end;
    end
    else begin
      { "Parameter"-Befehl f�r �brige Ger�tetypen TTG, TDS (Standardabfrage): }
      if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
        Befehl:=GetTritschler_IECKommando_Standardabfrage (SSU_GeraeteAuswNr,
          VerbAufbau_Passwort);
        bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
      end
      else begin
        Befehl:=GetTritschler_IECKommando_Standardabfrage (0, '');
        bWecken:=true;
      end;
    end;
    { Befehl senden: }
    if not TMRGCustomCommObj (CommObj).SendTritschlerIECCommand
      (Befehl, ad_String, MRGTimeouts.TritschlerIECProt, FMrgDefData.MrgTyp, bWecken,
       R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    // Ger�tetyp pr�fen; 15.07.2014, WW
    if bCheckGeraeteTyp then begin
      { Identifikationsantwort enth�lt :
        -> Hersteller (erste 3 Zeichen): nicht auswerten
        -> Baudratenidentifikation (4. Zeichen): nicht auswerten
        -> Ger�tetyp: Auswerten, wenn Konfiguration eingelesen wird }
      S:=ExtractString (R.Antwort, '/', CR, 0);
      sGerTyp:=Copy (S, 5, length (S));
      if not CheckGeraetetyp (FGeraetetyp, sGerTyp) then begin
        if FehlerGruppeCode_OK (Fehlergruppe, Fehlercode) then  // vorhandenen Fehler nicht �berschreiben
          FehlerGruppeCodeUpdate (EST_GERAETECHECK, GERCHECKERR_TYP_FALSCH);
      end;
    end;

    { Kennung aus Rohantwort extrahieren. Bei TTG zus�tzlich Anzahl der Kan�le
      ermitteln �ber Kennziffer 280 (aktueller ZW-Stand Kanal 2): }
    if FMrgDefData.MrgTyp = mrgtyp_TTG_IEC then begin
      Kennziffer_Geraetenummer:='000';      { Ger�tenummer-Kennziffer bei TTG (IEC) }
      MRGAnzKanaele:=1;  { Vorbelegung: abzurufendes TTG ist 1-Kanal-Ger�t }
    end else
      Kennziffer_Geraetenummer:='0.0.';      { Ger�tenummer-Kennziffer bei �brigen Ger�tetypen }

    { Falls spezielle Parameternummer f�r die Kennungsabfrage konfiguriert ist,
     diese statt der standardm��igen verwenden: 18.12.2009, WW }
    if length (CustomParaNr_Kennung) > 0 then
      Kennziffer_Geraetenummer:=CustomParaNr_Kennung;

    Antw:=R.Antwort;
    gefunden:=false;
    while length (Antw) > 0 do begin
      S:=F_Zerlegen (Antw, LF);
      if length (S) > 0 then begin
        if Copy (S, 1, length (Kennziffer_Geraetenummer)) = Kennziffer_Geraetenummer then begin  { Ger�tenummer gefunden }
          R.Antwort:=ExtractString (S, '(', ')', 0);  { Ger�tenummer steht zwischen den runden Klammern }
          gefunden:=true;  { Ger�tenummer (Kennung) gefunden }
        end;

        if FMrgDefData.MrgTyp = mrgtyp_TTG_IEC then begin
          if Copy (S, 1, 3) = '280' then  { akt. ZW-Stand Kanal 2 gefunden }
            MRGAnzKanaele:=2;  { abzurufendes TTG ist 2-Kanal-Ger�t }
        end;
      end;
    end;  { while length (Antw) }
    if not gefunden then
      R.Antwort:='';
  end

    { Kennungsabfrage f�r Modemgruppe 9 (Tritschler-Ger�te mit FTL-Protokoll: TTG): }
  else if ModemAbrufgruppe = 9 then begin
    { Ger�teinformation-Befehl f�r Kanal 1: }
    Befehl:=GetTTG_FTLKommando_Geraeteinfo (1);
    { Befehl senden: }
    if not TMRGCustomCommObj (CommObj).SendTritschlerFTLFunktionCommand
      (Befehl, ad_String, MRGTimeouts.TritschlerFTLProt, -1, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    { Kennung aus Rohantwort extrahieren: }
    Antw:=R.Antwort;
    gefunden:=false;
    PosK:=Pos ('K01', Antw);  { Position des K01-Blocks (Kundennummer, h�herwertiger Teil) }
    if PosK > 0 then begin
      R.Antwort:=Copy (Antw, PosK+6, 6);  { die h�herwertigen 6 Stellen }
      PosK:=Pos ('K41', Antw);  { Position des K41-Blocks (Kundennummer, niederwertiger Teil) }
      if PosK > 0 then begin
        R.Antwort:=R.Antwort + Copy (Antw, PosK+6, 6); { die niederwertigen 6 Stellen }
        gefunden:=true;
      end;
    end;
    if not gefunden then
      R.Antwort:='';
  end

  { Kennungsabfrage f�r Modemgruppe 10 (Actaris Corus): }
  else if ModemAbrufgruppe = 10 then begin
    TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_CRC_Corus);    { Flag setzen, es folgen Befehle mit CRC }
    TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_CRC_Corus); { Flag setzen, es folgen Antworten mit CRC }

    { Kennung lesen (= Parameter 82 "Kundendaten 1", "Customer data#1"): }
    Befehl:=GetCorusSAMKommando_ParameterLesen (82);
    if not CommObj.SendCommand (Befehl, [ETX, NAK], 1, MRGTimeouts.CorusSAMProt, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    { Antwort auswerten: }
    if not ValidCorusSAMAntwort (R.Antwort, R.Fehlergruppe, R.Fehlercode, PosETX) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    { Seriennummer aus Antwort filtern: }
    R.Antwort:=Copy (R.Antwort, 3, PosETX - 3);
    R.Antwort:=F_RightTrunc (R.Antwort, NUL);
  end

  { Kennungsabfrage f�r Modemgruppe 11 (IEC 1107-Protokoll, z.B. Actaris Sparklog,
    Kamstrup UNIGAS 300): }
  else if ModemAbrufgruppe = 11 then begin
    TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);    { Flag setzen, es folgt Kennungs-Befehl mit BCC }
    TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC); { Flag setzen, es folgt Kennungs-Antwort mit BCC }

    { Lesebefehl f�r Kennung bilden: }
    if (FMrgDefData.MrgTyp = mrgtyp_Unigas300) then  // Kamstrup UNIGAS 300; 27.03.2013, WW
      S:='C.1.0'  { Kennziffer f�r "Seriennummer" }
    else  // Actaris Sparklog
      S:='0:0.0.0';  { Kennziffer f�r "Eigentumsnummer" }

    { Falls spezielle Parameternummer f�r die Kennungsabfrage konfiguriert ist,
      diese statt der standardm��igen verwenden: 11.04.2017, WW }
    if length (CustomParaNr_Kennung) > 0 then
      S:=CustomParaNr_Kennung;

    Befehl:=GetIEC1107Kommando_Lesen (S);

    { Befehl senden: }
    if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    { Antwort auswerten: }
    if not ValidIEC1107DatenTelegrammAntwort (R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    R.Antwort:=ExtractString (R.Antwort, '(', ')', 0);  { Stationsnummer steht zwischen den runden Klammern }
  end

  { Kennungsabfrage f�r Modemgruppe 14 (Modbus-Protokoll}
  else if ModemAbrufgruppe = 14 then begin  // 08.03.2019, WW
    // Primus/Prilog:
    if (FMrgDefData.MrgTyp = mrgtyp_Primus) OR
       (FMrgDefData.MrgTyp = mrgtyp_Prilog) then  // 25.11.2020, WW
      sParaNrAllg_Kennung:=CP_RMG_PrimusPrilog_Kennung
    // TME400:
    else if (FMrgDefData.MrgTyp = mrgtyp_TME400_VCF) OR
            (FMrgDefData.MrgTyp = mrgtyp_TME400_VMF) then  // 18.02.2021, WW
      sParaNrAllg_Kennung:=CP_RMG_TME400_Kennung
    // FLOWSIC500:
    else if (FMrgDefData.MrgTyp = mrgtyp_FLOWSIC500) then  // 29.09.2021, WW
      sParaNrAllg_Kennung:=CP_SICK_FLOWSIC500_Kennung
    // RSM200:
    else if (FMrgDefData.MrgTyp = mrgtyp_RSM200_VCF) OR
            (FMrgDefData.MrgTyp = mrgtyp_RSM200_VMF) then  // 09.01.2024, WW
      sParaNrAllg_Kennung:=CP_RMG_RSM200_Kennung
    else
      sParaNrAllg_Kennung:='';

    PL:=TParameterListe.Create (FKonfigPath);
    try
      if not AbrufParameter (sParaNrAllg_Kennung, nil, PL) then begin
        FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + Fehlergruppe, Fehlercode);
        exit;
      end;
    finally
      PL.Free;
    end;
    // abgerufene Kennung aus Parameterliste lesen:
    if not ParameterListe_MRGAbruf.GetValue (sParaNrAllg_Kennung, R.Antwort) then
      R.Antwort:='';
  end

  { Kennungsabfrage f�r alle �brigen Modemgruppen (Wieser/RMG): }
  else begin
    WithCRC_Merk:=false;
    { Kommando f�r Kennungsabfrage bilden: }
    if length (Infobefehl) > 0 then begin        { Kennung abrufen �ber Infobefehl (z.B. EC694) }
      AbfrageModus:=am_Infobefehl;
      Befehl:=STX+Infobefehl+ETX;
      { Achtung: Infobefehl ohne CRC senden, sonst Antwort verst�mmelt (Fehler im EC694): }
      if not isFupAbruf then begin
        WithCRC_Merk:=TMRGCustomCommObj (CommObj).GetWithCRC;
        TMRGCustomCommObj (CommObj).SetWithCRC (false);
      end;
    end
    else if not isFupAbruf AND (RufTyp = re_MRG_Modem) then begin
      { Rufentgegennahme per Modem von Modem-Ger�ten:
        1. Kennung abrufen �ber k-Befehl
           -> mit dem k-Befehl kann bei Ger�ten mit Auslesemodus "mit Passwort"
           die Kennung ohne Kenntnis des Passworts abgerufen werden (MRG 910 ab Ende 10/2002)
        2. wenn k-Befehl nicht bekannt, Kennung mit B003-Befehl auslesen
           -> f�r Kompatibilit�t mit MRG 910-Versionen ohne k-Befehl }
      AbfrageModus:=am_k_Befehl;
      Befehl:=GetMRGKommando_k
    end
    else begin   { in allen anderen F�llen: Kennung wird mit B-Parameterbefehl abgerufen }
      AbfrageModus:=am_Bnnn_Befehl;
      if FGeraetetyp > -1 then begin
        { Parameter "Kennung" �ber allg. Parameternummer ermitteln: 28.05.2009, WW }
        if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                              CP_ALLG_MRGKennung,
                                                              ParamMrgData) then begin  // 09.07.2021, WW
          FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
          exit;
        end;
        KennungParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;  // 09.07.2021, WW
      end else  // bei RE ist der Ger�tetyp hier nicht bekannt; 18.01.2010, WW
        KennungParaNrMrg:='003';

      Befehl:=GetMRGKommando_B (KennungParaNrMrg);
    end;

    if ModemAbrufgruppe = 12 then  // EC 900 immer mit Passwort abfragen; 17.06.2009, WW
        TMRGCustomCommObj (CommObj).SetWithPasswort (true);

    { Kennungsabfragekommando senden: }
    if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Kennung, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Antwort auf Kennungsabfrage auswerten: }
    case AbfrageModus of
      am_Infobefehl : Kommando_Soll:=InfoBefehl;
      am_k_Befehl   : Kommando_Soll:='k';
      am_Bnnn_Befehl: Kommando_Soll:='B' + KennungParaNrMrg;
    else
      Kommando_Soll:='';
    end;

    if not ValidMRGAntwort (Kommando_Soll, ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
      if (AbfrageModus = am_k_Befehl) AND
         (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_AENDERUNGNICHTZULAESSIG) then begin
        { wenn auf k-Befehl STX = ETX als Antwort kommt, ist im Ger�t der Auslesemodus
          auf "mit Passwort" eingestellt (und der k-Befehl unbekannt) -> keine Rufentgegennahme m�glich }
        FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_RE_PWACTIVATED);
        exit;
      end
      else if (AbfrageModus = am_k_Befehl) AND
              (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_KOMMANDOUNBEKANNT) then begin
        { wenn k-Befehl unbekannt, nochmal mit Bnnn-Befehl probieren: }
        AbfrageModus:=am_Bnnn_Befehl;
        if FGeraetetyp > -1 then begin
          { Parameter "Kennung" �ber allg. Parameternummer ermitteln: 28.05.2009, WW }
          if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                                CP_ALLG_MRGKennung,
                                                                ParamMrgData) then begin  // 09.07.2021, WW
            FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
            exit;
          end;
          KennungParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;  // 09.07.2021, WW
        end else  // bei RE ist der Ger�tetyp hier nicht bekannt; 18.01.2010, WW
          KennungParaNrMrg:='003';

        Befehl:=GetMRGKommando_B (KennungParaNrMrg);
        if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Kennung, ad_String, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
        if not ValidMRGAntwort ('B' + KennungParaNrMrg, ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
          FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
      end
      else if (not isFupAbruf) AND ((ModemAbrufgruppe = 3) OR (ModemAbrufgruppe = 4)) AND
              (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_AENDERUNGNICHTZULAESSIG) then begin
        { Modem-Abrufgruppe 3, 4: wenn der Befehl vom MRG mit STX = ETX beantwortet wird,
          m�ssen Befehle mit Passwort geschickt werden: }
        TMRGCustomCommObj (CommObj).SetWithPasswort (true);
        if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Kennung, ad_String, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
        if not ValidMRGAntwort (Kommando_Soll, ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
          if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_AENDERUNGNICHTZULAESSIG) then begin
            { wenn jetzt wieder STX = ETX als Antwort kommt ist das Passwort falsch: }
            FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
          end else
            FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
      end
      else if (not isFupAbruf) AND (ModemAbrufgruppe = 12) AND
              (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_KEINEBERECHTIGUNG) then begin
        { Modem-Abrufgruppe 12: wenn der Befehl vom MRG mit STX = ETX beantwortet wird,
          ist das Passwort falsch: }
        FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
        exit;
      end
      else begin
        FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end;

    { Antwort auf Kennungsabfrage auswerten: }
    if AbfrageModus = am_Infobefehl then begin        { Kennung wurde �ber Infobefehl abgerufen }
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

      if not isFupAbruf then         { WithCRC wieder in urspr�nglichen Zustand versetzen }
        TMRGCustomCommObj (CommObj).SetWithCRC (WithCRC_Merk);
    end
    else if AbfrageModus = am_k_Befehl then begin     { Kennung wurde �ber k-Befehl abgerufen }
      S:=ExtractString (R.Antwort, STX, ETX, 0);
      R.Antwort:=F_Zerlegen (S, US);     { bis zum US lesen }
      Delete (R.Antwort, 1, 1);          { k rausl�schen -> R.Antwort enth�lt jetzt Kennung }
      if S = '1' then                    { S enth�lt Auslesemodus mit/ohne Passwort }
        TMRGCustomCommObj (CommObj).SetWithPasswort (true);
    end
    else if AbfrageModus = am_Bnnn_Befehl then begin  { Kennung wurde �ber Parameter abgerufen }
      { Kennung aus Antwort auf Bnnn-Befehl filtern: }
      R.Antwort:=ExtractString (R.Antwort, STX, ETX, 0);
      Delete (R.Antwort, 1, length (KennungParaNrMrg) + 1);  { Bnnn rausl�schen }
    end;
  end;  { if ModemAbrufgruppe }

  StationsKennung:=Copy (R.Antwort, 1, C_KennungLen);  { StationsKennung enth�lt jetzt die richtige Ger�tekennung }
  Result:=true;
end;

{-------------------------------------------------------}
function TMRGAbruf.KennungPruefen (sKennung_Soll: string;
  bStop_bei_falscher_Kennung: boolean): boolean;
{-------------------------------------------------------}
{ gelesene Kennung des Ger�ts mit Soll-Kennung vergleichen; bei falscher Kennung
  Fehlergruppe/-code setzen;
  �bergabe: Soll-Kennung
            Flag 'bStop_bei_falscher_Kennung'
  Ergebnis: false, wenn Kennung falsch und lt. �bergabeflag Verbindung beendet
            werden soll }
begin
  Result:=false;
  if not KennungComp (StationsKennung, sKennung_Soll, KennungExt) then begin
    { Kennungen sind nicht identisch }
    if bStop_bei_falscher_Kennung then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_KEINE_VERBINDUNG);
      exit;  { Stop bei falscher Kennung }
    end
    else if FehlerGruppeCode_OK (Fehlergruppe, Fehlercode) then  // vorhandenen Fehler nicht �berschreiben; 11.07.2014, WW    
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_VERBINDUNG);
  end;
  Result:=true;
end;

{----------------------------------------------------------------------------}
function TMRGAbruf.PasswortLogin_Tritschler_IEC_SSU (Passwort: string;
  cQuittungsModus: char; MeldungsListe: TMeldungsListe; sKennung_Soll: string;
  bKennung_pruefen: boolean): boolean;
{----------------------------------------------------------------------------}
{ Login f�r Tritschler Multiplexer SSU (Verbindungsaufbau zum SSU mit Passwort,
  Quittierungstelegramm);
  �bergabe: Passwort
            Quittungsmodus
            Zeiger auf Meldungsliste
            Soll-Kennung f�r Kennungsvergleich (leer = keinen Kennungsvergleich durchf�hren)
            Flag "Kennung pr�fen" (true = Abruf stoppen bei falscher Kennung)
  R�ckgabe: Meldungsliste mit konvertierten Roh-Meldungss�tzen (falls vorhanden)
  Ergebnis: true, wenn ok }
const
  CMaxVersuche_Login = 4;     // trial-and-error: - manche Multiplexer reagieren erst beim 3. Versuch
                              //                  - bei mehr als 4 Versuchen kappt das Modem der Gegenstelle
                              //                    die Verbindung (DCD-Modemfehler)
  CTimeout_Login     = 6000;  // ms, wie in Tritschler MoTe-Abrufprogramm

var
  Befehl: string;
  R: TRueckgabe;
  Versuch: byte;
  sAntwort: string;
  Kennziffer_Stationsbezeichnung: string;
  sStationBez: string;
  S: string;
  MeldKonv: TMeldKonv;    

begin
  Result:=false;
  TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off);  { Empfangsdaten (SSU-Kennungstelegramm) ohne BCC }
  { Befehl f�r Login an SSU senden (mit SSU-Passwort): mit Wiederholungen; 04.06.2007, WW }
  Befehl:=GetSSUKommando_Login (Passwort);
  Versuch:=0;
  while true do begin
    inc (Versuch);
    if not CommObj.SendCommand (Befehl, [LF], 1, CTimeout_Login, ad_String, R, NoCarrier) then begin
      if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) then begin
        if Versuch >= CMaxVersuche_Login then begin
          FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_TIMEOUT_WRONGPW);  // Timeout, evtl. Passwort falsch; 10.04.2008, WW
          exit;  { Timeout-Fehler nach letztem Versuch }
        end;
      end
      else begin
        FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end else
      Break;  { Login OK }
  end;

  { Antwort wird nicht ausgewertet }

  TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC);  { ab jetzt Empfangsdaten mit BCC (IEC-Standard) }
  { Quittierungs-/Optionsauswahltelegramm an SSU senden: }
  Befehl:=GetSSUKommando_Quittung (cQuittungsmodus);
  if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.TritschlerIECProt,
                                ad_File, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  case cQuittungsModus of
    '0': begin  // Anfordern SSU-Status bzw. Alarmurache
      if FMrgDefData.MrgTyp = mrgtyp_SR then begin  // Kennung des SR ermitteln
        { Antwort auswerten: }
        sAntwort:=StringFromFile (R.Antwort); { Rohfile-Inhalt in String kopieren }
        sAntwort:=ExtractString (sAntwort, STX, ETX, 0);  // Rohdatenteil zwischen STX und ETX

        Kennziffer_Stationsbezeichnung:='40.';   { Kennziffer f�r Stationsbezeichnung (Kennung des SR) }
        sStationBez:='';
        while length (sAntwort) > 0 do begin
          S:=F_Zerlegen (sAntwort, LF);
          if length (S) > 0 then begin
            if Copy (S, 1, length (Kennziffer_Stationsbezeichnung)) =
               Kennziffer_Stationsbezeichnung then begin  { Stationsbezeichnung gefunden }
              sStationBez:=ExtractString (S, '(', ')', 0);  { Stationsbezeichnung steht zwischen den runden Klammern }
              Break;
            end;
          end;
        end;  { while length (sAntwort) }

        StationsKennung:=Copy (sStationBez, 1, C_KennungLen);  { StationsKennung enth�lt jetzt die richtige Ger�tekennung }

        { wenn Soll-Kennung �bergeben wurde, Ger�te-Kennung mit Soll-Kennung vergleichen: }
        if length (sKennung_Soll) > 0 then
          if not KennungPruefen (sKennung_Soll, bKennung_pruefen) then exit;
      end;

      if Assigned (MeldungsListe) then begin
        { in Antwort enthaltene Meldungss�tze in Meldungsliste konvertieren }
        { �bergaberecord f�r Meldungskonvertierung zusammenstellen: }
        with MeldKonv do begin
          MeldKonvGruppe:=FMrgKonvData.MeldKonvGruppe;
          MNrParaStart:=FMrgKonvData.MNrParaStart;
          MNrParameter:=FMrgKonvData.MNrParameter;
          MeldungsGruppe:=FMrgKonvData.MeldungsGruppe;
          ParameterGruppe:=FMrgKonvData.ParameterGruppe;
          ParameterUnterGruppe:=FMB_ID;  // 11.02.2022, WW
          MeldGeraeteart:=FMrgKonvData.MeldGeraeteart;
          RohLoeschen:=RohdatenLoeschen;
          MrgTyp:=FMrgDefData.MrgTyp;  // 08.03.2019, WW
          ParameterListe:=ParameterListe_MRGAbruf;  // 29.09.2021, WW
        end;

        { Konfigurationsliste mit meldungsspezifischen Parameternummern des MRG anlegen
          und aus Ressourcedatenliste laden, wenn noch nicht erfolgt: 06.08.2021, WW }
        if not Assigned (FParamMeldKonfigList) then begin
          FParamMeldKonfigList:=TParamMeldKonfigList.Create;

          if not Load_MRGResourceData_ParamMeld then exit;  // 06.08.2021, WW
        end;

        { Meldungsnummern-Konfigurationsliste des MRG anlegen und aus
          Ressourcedatenliste laden, wenn noch nicht erfolgt: 06.08.2021, WW }
        if not Assigned (FMeldNrKonfigList) then begin
          FMeldNrKonfigList:=TMeldNrKonfigList.Create;

          if not Load_MRGResourceData_MeldNr (MeldKonv.MeldGeraeteart,
                   MeldKonv.Meldungsgruppe) then exit;  // 06.08.2021, WW
        end;

        // 09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG
        // 06.08.2021, WW: Mit �bergabe der Konfigurationsliste mit meldungsspezifischen
        //                 Parameternummern des MRG
        MeldungsListe.LoadFromFile (R.Antwort, MeldKonv, FParamMrgKonfigList,
                                    FParamMeldKonfigList, FMeldNrKonfigList);
        { bei ung�ltigen Meldungsrohdaten: Fehlergruppe,-code setzen }
        if not MeldungsListe.DataValid then begin
          FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_MEINVALID);
          exit;
        end;
      end else
        DeleteFile (R.Antwort);
    end;

    '1': begin  // Schalten in Programmiermode
      { Antwort wird nicht ausgewertet }
      DeleteFile (R.Antwort);

      { "�ndern"-Passwort senden (ist identisch mit Zugriffspasswort): }
      TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);  { Flag setzen, es folgt Befehl mit BCC }
      TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off);  { Flag r�cksetzen, es folgt Antwort ohne BCC }
      Befehl:=SOH+'P1'+STX+'('+Passwort+')'+ETX;
      if not CommObj.SendCommand (Befehl, [ACK], 1, MRGTimeouts.TritschlerIECProt,
                                    ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
        exit;
      end
      else begin
        if R.Antwort <> ACK then begin  { Passwort wurde nicht akzeptiert }
          FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
          exit;
        end;
      end;
    end;
  end;  { case }

  Result:=true;
end;

{----------------------------------------------------------------------------}
function TMRGAbruf.PasswortLogin (ModemAbrufgruppe: integer; Passwort: string;
                                  PasswortNr: integer): boolean;
{----------------------------------------------------------------------------}
{ Pa�wort �bertragen;
  �bergabe: Modem-Abrufgruppe
            Passwort
            Passwort-Nummer
  Ergebnis: true, wenn Passwort-Login ok }
var
  Befehl: string;
  R: TRueckgabe;
  ParaNrAllg: string;
  ParaNrMRG: string;
  dummy: integer;
  ParamMrgData: TParamMrgData;

begin
  Result:=false;
  { Login f�r Modemgruppe 6 (KE-Anlagen): }
  if ModemAbrufgruppe = 6 then begin
    if FMrgDefData.MrgTyp = mrgtyp_KE_Ruhrgas then
      Befehl:=GetKE_Kommando ('S003' + Passwort)
    else if FMrgDefData.MrgTyp = mrgtyp_KE_PPN then
      Befehl:=GetKE_Kommando ('T003' + Passwort)
    else Befehl:='';
  end
  else begin
    case PasswortNr of
      1: ParaNrAllg:=CP_DFUE_PW1;
      2: ParaNrAllg:=CP_DFUE_PW2;
      3: ParaNrAllg:=CP_DFUE_PW3;
      4: ParaNrAllg:=CP_DFUE_PW4;
    else
      ParaNrAllg:=CP_DFUE_PW1;
    end;
    { Passwort: aus der allgemeinen Parameternummer die ger�tespezifische ermitteln }
    if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                          ParaNrAllg,
                                                          ParamMrgData) then begin  // 09.07.2021, WW
      FehlerGruppeCodeUpdate (EST_LOGINERROR + ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
      exit;
    end;
    ParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;  // 09.07.2021, WW
    Befehl:=STX+'Z'+ParaNrMrg+Passwort+ETX;
  end;

  { Pa�wort�bertragungskommando senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Login, ad_String, R, NoCarrier) then begin
    { Gegenstelle bricht Verbindung ab wegen ung�ltigem Passwort: }
    if (R.Fehlergruppe = COM_FUPERROR) AND (R.Fehlercode = FUPERR_FI) OR          { FUP }
       (R.Fehlergruppe = COM_MODEMPROTERROR) AND (R.Fehlercode = CMPE_DLE_EOT) OR { Modemabrufgruppe 1 (Modem-Protokoll) }
        NoCarrier then begin                                                      { Modemabrufgruppe 2 }
      FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
    end else
      FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Pa�wort�bertragungskommando wird nur bei Modem-Abrufgruppe 6
   (KE-Anlagen) ausgewertet: }
  if ModemAbrufgruppe = 6 then begin
    if not ValidKEAntwort (R.Antwort, dummy, R.Fehlergruppe, R.Fehlercode) then begin
      FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    if length (R.Antwort) >= 6 then begin
      if R.Antwort [6] = '0' then begin
        FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
        exit;
      end;
    end;
  end
  else begin  // 31.01.2008, WW
    if not ValidMRGAntwort ('Z', ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
      FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end;
  Result:=true;
end;

{--------------------------------------------------------------}
function TMRGAbruf.Init_IEC1107_Kommunikation (sAdresse: string;
  cQuittungsModus: char; ModemAbrufgruppe: integer;
  bModemAutoDetect, bCheckGeraeteTyp: boolean;
  var sFilename_QuittungsAntwort: string;
  var VerbAutoDetectData: TVerbAutoDetectData): boolean;
{--------------------------------------------------------------}
{ einleiten der Ger�te-Kommunikation nach Norm IEC 1107;
  �bergabe: Zugangs-Adresse (Ger�te- oder Kanaladresse)
            Quittungsmodus
            Modem-Abrufgruppe
            Flag 'bModemAutoDetect': wenn true, wird Datenformat automatisch
              ermittelt
            Flag 'bCheckGeraeteTyp': wenn true, wird Ger�tetyp gepr�ft
  R�ckgabe: Rohdateiname mit Antwort auf Quittungstelegramm (leer = Rohdatei nicht vorhanden)
            Record mit automatisch ermittelten Verbindungsparametern
  Ergebnis: true, wenn erfolgreich }

  {----------------------}
  procedure Geraet_Wecken;
  {----------------------}
  var
    Befehl: string;
    i: integer;
    R: TRueckgabe;
  begin
    if ModemAbrufgruppe = 5 then begin  { Elster DL210, DL220, DL240, EK260 }
      { Weck-Befehl senden, es kommt keine Antwort ! }
      Befehl:='';
      for i:=1 to 53 do
       Befehl:=Befehl + NUL;
      CommObj.SendCommand (Befehl, [ETX], 1, 0, ad_String, R, NoCarrier);
      Sleep (1500);
    end;
  end;

const
  CMaxVersuche_Init_IEC1107 = 3;
  CTimeout_Init_IEC1107     = 7500;  // < 10 s, da Actaris Sparklog ansonsten den
                                     // Standarddatensatz unaufgefordert schickt !
                                     // F�r die �brigen Ger�tetypen sollten auch 6 s
                                     // (statt wie bisher 10 s) ausreichend lang sein.
                                     // 28.05.2009, WW
                                     // von 6s auf 7,5 s erh�ht wegen EK260, RWE (6 s
                                     // reichen offenbar doch nicht ganz); 09.11.2011, WW
var
  Befehl: string;
  R: TRueckgabe;
  S: string;
  sBaudIdent: string;
  sAntwort: string;
  Kennziffer_Kanaladresse: string;
  sBuf: string;
  iKanalNr: integer;
  Code: integer;
  Versuch: byte;
  iNewBaud: integer;
  AutoDetectVersuch: integer;
  bEnde: boolean;
  sHerstIdent: string;

begin
  Result:=false;
  sFilename_QuittungsAntwort:='';  // Vorbelegung R�ckgabe

  { Flags r�cksetzen, es folgen Befehle und Antworten ohne BCC }
  TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_Off);
  TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off);

  Geraet_Wecken;

  { Aufforderungstelegramm senden, Ger�te-/Kanaladresse optional:
    -> mit Wiederholung bei Timeout (wegen Actaris Sparklog); 28.05.2009 WW }
  Befehl:='/?' + sAdresse + '!'+CR+LF;
  Versuch:=0;
  AutoDetectVersuch:=1;
  while true do begin
    inc (Versuch);
    if not CommObj.SendCommand (Befehl, [LF], 1, CTimeout_Init_IEC1107, ad_String, R, NoCarrier) then begin
      if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) then begin
        if Versuch >= CMaxVersuche_Init_IEC1107 then begin
          bEnde:=true;
          if bModemAutoDetect then begin
            if AutoDetectVersuch = 1 then begin
              // 2. AutoDetect-Versuch mit 8N1:
              TMRGCustomCommObj (CommObj).SetAbrufgruppe (ModemAbrufgruppe, CSDataFormat_8N1);
              bEnde:=false;
            end;
          end;

          if bEnde then begin
            FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
            exit;  { Timeout-Fehler nach letztem Versuch }
          end
          else begin   // neuer AutoDetect-Versuch
            Versuch:=0;
            inc (AutoDetectVersuch);
            Geraet_Wecken;
          end;
        end;
      end
      else begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end
    else begin  { OK }
      if bModemAutoDetect then begin
        if AutoDetectVersuch = 1 then
          VerbAutoDetectData.ModemTyp:=srv_modem_standard
        else
          VerbAutoDetectData.ModemTyp:=srv_modem_8N1;
      end;

      Break;
    end;
  end;  { while }

  { Aufforderungstelegramm-Antwort enth�lt:
    -> Hersteller (erste 3 Zeichen): nicht auswerten
    -> Baudratenidentifikation (4. Zeichen): bei DF� zwar keine Baudratenumschaltung
         notwendig, aber Baudratenidentifikations-Zeichen in nachfolgendem Quittierungs-/
         Optionsauswahltelegramm mitsenden ! (26.07.2005, WW)
    -> herstellerspez. Identifikation (z.B. bei Elster: Ger�tetyp): Auswerten, wenn
         Konfiguration eingelesen wird }
  S:=ExtractString (R.Antwort, '/', CR, 0);
  sBaudIdent:=Copy (S, 4, 1);
  sHerstIdent:=Copy (S, 5, length (S));  // 15.07.2014, WW

  // Ger�tetyp pr�fen; 15.07.2014, WW
  if bCheckGeraeteTyp then begin
    if not CheckGeraeteTyp (FGeraetetyp, sHerstIdent) then begin
      if FehlerGruppeCode_OK (Fehlergruppe, Fehlercode) then  // vorhandenen Fehler nicht �berschreiben
        FehlerGruppeCodeUpdate (EST_GERAETECHECK, GERCHECKERR_TYP_FALSCH);
    end;
  end;

  { Quittierungs-/Optionsauswahltelegramm senden: }
  FAbschaltBefehl:=true;  { Flag setzen, da ab jetzt vor dem Beenden der
                            Verbindung Abschaltbefehl geschickt werden mu� }
  TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC);  { Flag setzen, es folgen Antworten mit BCC }
  Befehl:=ACK+'0'+sBaudIdent+cQuittungsModus+CR+LF;

  if CommObj is TMRGSerialCommObj then begin  // Abruf �ber serielle Schnittstelle; 18.08.2011, WW
    { mit Baudratenumschaltung: }
    iNewBaud:=0;
    if length (sBaudIdent) > 0 then begin
      case sBaudIdent [1] of
        '0': iNewBaud:=  300;  // 300 Bd
        '1': iNewBaud:=  600;  // 600 Bd
        '2': iNewBaud:= 1200;  // 1200 Bd
        '3': iNewBaud:= 2400;  // 2400 Bd
        '4': iNewBaud:= 4800;  // 4800 Bd
        '5': iNewBaud:= 9600;  // 9600 Bd
        '6': iNewBaud:=19200;  // 19200 Bd
      end;
    end;
    if iNewBaud = 0 then begin
      // Baudrate wird vom Programm nicht unterst�tzt
      // -> 7, 8, 9 von der IEC-Norm reserviert f�r sp�tere Erweiterungen
      FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_BAUD_NICHT_UNTERSTUETZT);
      exit;
    end;

    { Befehl senden: }
    if not CommObj.SendCommand (Befehl, [], 0, 0, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

 		{ vor dem Empfangen der Antwort auf Auslese-Baudrate umschalten, �brige
      Schnittstellen-Parameter bleiben gleich: }
    with TMRGSerialCommObj (CommObj).Serial do begin
      if KonvertBaudrate (iNewBaud) <> Baudrate then begin
        Delay (300);  { Warten bis alle Zeichen wirklich versendet wurden (bei 300 Bd dauert das !) }

        // Befehl zum Einstellen der Schnittstellen-Parameter im P+E K01-Bluetooth-Auslesekopf:
        Befehl:=GetKonfigKommando_PuE_K01_Blue (iNewBaud, MRG_DPS);
        // Befehl an Auslesekopf senden:
        if not CommObj.SendCommand (Befehl, [], 0, 0, ad_String, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
          exit;
        end;

        SetCommParameter (KonvertBaudrate (iNewBaud), DataBits, ParityBit, StopBits);
      end;
    end;

    { Antwort auf Befehl empfangen: }
    if not CommObj.SendCommand ('', [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end
  else begin
    if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end;

  case cQuittungsModus of
    '0': begin  // Schalten in Standard-Mode
      if FMrgDefData.MrgTyp = mrgtyp_Sparklog then begin  // Actaris Sparklog
        if length (sAdresse) = 0 then begin  // Kanaladressen aus Standard-Datensatz ermitteln
          { Antwort auswerten: }
          sAntwort:=StringFromFile (R.Antwort); { Rohfile-Inhalt in String kopieren }
          sAntwort:=ExtractString (sAntwort, STX, ETX, 0);  // Rohdatenteil zwischen STX und ETX

          Kennziffer_Kanaladresse:='0.0.0';   { Kennziffer f�r Kanaladresse (ohne Kanalnummer) }
          while length (sAntwort) > 0 do begin
            S:=F_Zerlegen (sAntwort, LF);
            if length (S) > 0 then begin
              sBuf:=ExtractString (S, ':', '(', 0);  // Kennziffer zwischen : und (
              if sBuf = Kennziffer_Kanaladresse then begin  { Kennziffer f�r Kanaladresse gefunden }
                sBuf:=ExtractString (S, NUL, ':', 0);  // Kanalnummer bis zum :
                Val (sBuf, iKanalNr, Code);
                if (Code = 0) AND (iKanalNr >= Low (IEC1107_KanalAdressen)) AND
                   (iKanalNr <= High (IEC1107_KanalAdressen)) then
                  IEC1107_KanalAdressen [iKanalNr]:=ExtractString (S, '(', ')', 0);  // Wert (Kanaladresse) zwischen ( und )
              end;
            end;
          end;  { while length (sAntwort) }
        end  { if length (sAdresse) = 0 }
        else begin
          sFilename_QuittungsAntwort:=R.Antwort;  // R�ckgabe: Rohdateiname mit Antwort auf Quittungstelegramm
        end;
      end;
    end;

    '1': begin  // Schalten in Programmiermode
           { Antwort ist uninteressant, wird nicht ausgewertet }
         end;
  end;  { case cQuittungsModus }

  // Antwort-Rohdatei l�schen, wenn Rohdateiname nicht zur�ckgegeben wird: }
  if length (sFilename_QuittungsAntwort) = 0 then
    DeleteFile (R.Antwort);

  Result:=true;
end;

{----------------------------------------------------------}
function TMRGAbruf.Send_IEC1107_Passwort (sPasswort: string;
  bUpdateFehlerGruppeCode: boolean = true): boolean;
{----------------------------------------------------------}
{ IEC 1107-Passwortpr�fung (P1);
  �bergabe: Passwort
            Optional:
              Flag 'bUpdateFehlerGruppeCode': wenn false, wird bei einem
                aufgetretenen Fehler Fehlergruppe und -code nicht gesetzt
  Ergebnis: true, wenn Passwort OK }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);  { Flag setzen, es folgen Befehle mit BCC }
  TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off);  { Flag r�cksetzen, es folgt Antwort ohne BCC }

  Befehl:=SOH+'P1'+STX+'('+sPasswort+')'+ETX;
  if not CommObj.SendCommand (Befehl, [ACK, NAK, ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier) then begin
    if bUpdateFehlerGruppeCode OR NoCarrier then  // 03.06.2020, WW
      FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
    exit;
  end
  else begin
    if R.Antwort <> ACK then begin  { Passwort wurde nicht akzeptiert }
      if bUpdateFehlerGruppeCode then
        FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
      exit;
    end;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------------}
function TMRGAbruf.Check_IEC1107_SchlossStatus (VerbAufbauCmdData: TVerbAufbauCmdData;
  var VerbAutoDetectData: TVerbAutoDetectData): boolean;
{------------------------------------------------------------------------------------}
{ IEC 1107-Zugangsschloss pr�fen, ggf. �ffnen (Elster DL210, DL220, DL240, EK260);
  �bergabe: Verbindungsaufbau-Kommandodaten
            Modem-Abrufgruppe
  R�ckgabe: Record mit automatisch ermittelten Verbindungsparametern
  Ergebnis: true, wenn erfolgreich }
var
  Befehl: string;
  R: TRueckgabe;
  cSchlossAdr: char;
  cMaxSchlossAdr: char;
  PW: string;
  bPwNrAutoDetect: boolean;
  iAutoDetectVersuch: integer;
  iMaxAutoDetectVersuche: integer;
  PwNr: integer;
  bEnde: boolean;
  bUpdateFehlerGruppeCode: boolean;

begin
  Result:=false;

  { Passwortnummer automatisch ermitteln ? 16.08.2012, WW }
  bPwNrAutoDetect:=VerbAufbauCmdData.PasswortNr = 0;

  { Welche Ger�tetypen unterst�tzen welche Schl�sser ? }
  if FMrgDefData.MrgTyp in [mrgtyp_DL210, mrgtyp_DL220, mrgtyp_DL240, mrgtyp_EK260] then begin
    iMaxAutoDetectVersuche:=2;  { 2 AutoDetect-Versuche f�r Lieferanten-, Kundenschloss }
    cMaxSchlossAdr:='4';  { Kundenschloss }
  end
  else begin
    iMaxAutoDetectVersuche:=3;  { 3 AutoDetect-Versuche f�r Lieferanten-, Kunden-, Datenausleserschloss }
    cMaxSchlossAdr:='5';  { Datenausleserschloss }
  end;

  iAutoDetectVersuch:=1;
  while true do begin
    TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);  { Flag setzen, es folgen Befehle mit BCC }
    TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC);  { Flag setzen, es folgen Antworten mit BCC }

    if bPwNrAutoDetect then begin
      if iAutoDetectVersuch = 1 then
        PwNr:=1  { erster AutoDetect-Versuch mit Lieferantenschloss }
      else if iAutoDetectVersuch = 2 then
        PwNr:=2  { zweiter AutoDetect-Versuch mit Kundenschloss }
      else
        PwNr:=3;  { dritter AutoDetect-Versuch mit Datenausleserschloss; 03.06.2020, WW }
    end else
      PwNr:=VerbAufbauCmdData.PasswortNr;

    if PwNr = 1 then
      cSchlossAdr:='3'  { "Login" erfolgt �ber Lieferantenschloss }
    else if PwNr = 2 then
      cSchlossAdr:='4'  { "Login" erfolgt �ber Kundenschloss }
    else
      cSchlossAdr:=cMaxSchlossAdr;  { "Login" erfolgt �ber Schloss der h�chsten vom Ger�tetyp
                                       unterst�tzten Passwortnummer; 03.06.2020, WW }

    { Zustand des Lieferanten- bzw. Kundenschloss lesen: }
    Befehl:=GetLIS200Kommando_Lesen (cSchlossAdr + ':170');
    if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    { Antwort auswerten: }
    IEC1107_SchlossStatus:=st_Schloss_bereits_offen;
    if Pos ('(1)', R.Antwort) = 0 then begin    { (1) nicht in der Antwort enthalten: Schloss ist zu ! }
      { Schloss �ffnen: }
      PW:=cSchlossAdr+':'+VerbAufbauCmdData.Passwort;

      { Passwort-Fehler bei erstem AutoDetect-Versuch nicht in Fehlergruppe, -code
        setzen (es erfolgt weiterer Versuch und evtl. schon gesetzte Fehlergruppe,
        -code darf nicht �berschrieben werden !) }
      bUpdateFehlerGruppeCode:=
        not (bPwNrAutoDetect AND (iAutoDetectVersuch < iMaxAutoDetectVersuche));  // 03.06.2020, WW
      if not Send_IEC1107_Passwort (PW, bUpdateFehlerGruppeCode) then begin
        bEnde:=true;
        if bPwNrAutoDetect then begin
          if iAutoDetectVersuch < iMaxAutoDetectVersuche then begin  // 03.06.2020, WW
            inc (iAutoDetectVersuch);
            bEnde:=false;  // weiter mit n�chstem AutoDetect-Versuch
          end;
        end;
        if bEnde OR NoCarrier then exit;  // 03.06.2020, WW
      end
      else begin
        { OK ! Schloss ist jetzt offen }
        if PwNr = 1 then    { bei Login �ber Lieferantenschloss }
          IEC1107_SchlossStatus:=st_Lieferantenschloss_geoeffnet
        else if PwNr = 2 then    { bei Login �ber Kundenschloss }
          IEC1107_SchlossStatus:=st_Kundenschloss_geoeffnet
        else     { bei Login �ber Datenausleserschloss; 03.06.2020, WW }
          IEC1107_SchlossStatus:=st_Datenausleserschloss_geoeffnet;

        if bPwNrAutoDetect then
          VerbAutoDetectData.PasswortNr:=PwNr;
        Break;
      end;
    end
    else begin  { Schloss ist offen }
      if bPwNrAutoDetect then begin
        VerbAutoDetectData.PasswortNr:=PwNr;
        
        // 11.07.2014, WW:
        if FehlerGruppeCode_OK (Fehlergruppe, Fehlercode) then begin  // vorhandenen Fehler nicht �berschreiben
          if PwNr = 1 then   { Lieferantenschloss ist bereits ge�ffnet }
            FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_LIEFERANTENSCHLOSS_GEOEFFNET)
          else if PwNr = 2 then    { Kundenschloss ist bereits ge�ffnet }
            FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_KUNDENSCHLOSS_GEOEFFNET)
          else    { Datenausleserschloss ist bereits ge�ffnet }
            FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_DATENAUSLESERSCHLOSS_GEOEFFNET);
        end;
      end;
      Break;  // OK, Schloss ist bereits offen
    end;
  end;  { while }
     
  Result:=true;
end;

{-------------------------------------------------------------------------------}
function TMRGAbruf.Init_Corus_Kommunikation (bCheckGeraeteTyp: boolean): boolean;
{-------------------------------------------------------------------------------}
{ einleiten der Kommunikation mit Actaris Corus;
  �bergabe: Flag 'bCheckGeraeteTyp': wenn true, wird Ger�tetyp gepr�ft
  Ergebnis: true, wenn erfolgreich }
var
  Befehl: string;
  i: integer;
  R: TRueckgabe;
  S: string;
  sBaudIdent: string;
  sPass: string;
  sHerstIdent: string;

begin
  Result:=false;
  { alle Kommunikationsflags r�cksetzen: Befehl und Antwort ohne CRC,
    Datengr��enbyte-Pr�fung aus }
  TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_Off);
  TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off);
  TMRGCustomCommObj (CommObj).SetWithDatasizeCheck (false);

  { POWER ON-Befehl senden, es kommen mind. 3 NULL-Zeichen als Antwort }
  Befehl:='';
  for i:=1 to 200 do  { 200 NUL-Zeichen lt. Info von Hrn. Hoh (Fa. Actaris) }
   Befehl:=Befehl + NUL;

  for i:=1 to 3 do  // mehrere Weckversuche
    if CommObj.SendCommand (Befehl, [NUL], 3, 1500, ad_String, R, NoCarrier) then Break;

  { SIGN ON-Telegramm senden: }
  Befehl:='/?!'+CR+LF;
  if not CommObj.SendCommand (Befehl, [LF], 1, MRGTimeouts.CorusSAMProt, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  { IDENTIFICATION-Antwort enth�lt:
    -> Hersteller (erste 3 Zeichen): nicht auswerten
    -> Baudratenidentifikation (4. Zeichen): bei DF� zwar keine Baudratenumschaltung
         notwendig, aber Baudratenidentifikations-Zeichen in nachfolgendem Quittierungs-/
         Optionsauswahltelegramm mitsenden ! )
    -> herstellerspez. Identifikation (bei Actaris Corus: MINICOR200): Auswerten,
          wenn Konfiguration eingelesen wird }
  S:=ExtractString (R.Antwort, '/', CR, 0);
  sBaudIdent:=Copy (S, 4, 1);
  sHerstIdent:=Copy (S, 5, length (S));  // 15.07.2014, WW

  // Ger�tetyp pr�fen; 15.07.2014, WW
  if bCheckGeraeteTyp then begin
    if not CheckGeraeteTyp (FGeraetetyp, sHerstIdent) then begin
      if FehlerGruppeCode_OK (Fehlergruppe, Fehlercode) then  // vorhandenen Fehler nicht �berschreiben
        FehlerGruppeCodeUpdate (EST_GERAETECHECK, GERCHECKERR_TYP_FALSCH);
    end;
  end;

  { ACKNOWLEDGEMENT-Telegramm senden: }
  Befehl:=ACK+'0'+sBaudIdent+'7'+CR+LF;  { 7 = Standard communication mode }
  if not TMRGCustomCommObj (CommObj).SendMRGCommand (Befehl, [], 0, MRGTimeouts.CorusSAMProt, ad_String, R, NoCarrier, 6) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  { feste OPERAND-Antwort: PASS CRC1 CRC2 (6 Zeichen) }
  sPass:=Copy (R.Antwort, 1, length (R.Antwort)-2);

  TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_CRC_Corus);  { Flag setzen, es folgen Befehle mit CRC }
  TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off);  { Flag setzen, es folgt Antwort ohne CRC }

  { COMMAND-Telegramm senden (Passwort): }
  Befehl:=sPass;  { es wird das empfangene PASS wieder zur�ckgeschickt }
  if not CommObj.SendCommand (Befehl, [ACK], 1, MRGTimeouts.CorusSAMProt, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  { Antwort auswerten: }
  if R.Antwort <> ACK then begin  { Passwort falsch }
    FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
    exit;
  end;

  { Flag setzen, es mu� das Datengr��en-Byte in den Empfangsdaten ausgewertet
    werden (Endezeichen kann auch im Datenteil enthalten sein !): }
  TMRGCustomCommObj (CommObj).SetWithDatasizeCheck (true);

  FAbschaltBefehl:=true;  { Flag setzen, damit ab jetzt vor dem Beenden der
                            Verbindung der BREAK-Befehl geschickt wird }
  Result:=true;
end;

{-------------------------------------------------------}
function TMRGAbruf.Send_FTL_Uebertragungsbeginn: boolean;
{-------------------------------------------------------}
{ Tritschler FTL-Protokoll: �bertragungsbeginn-Befehl senden;
  Ergebnis: true, wenn erfolgreich }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  { �bertragungsbeginn-Kommando senden: }
  Befehl:='S0000' + CR;
  if not TMRGCustomCommObj (CommObj).SendTritschlerFTLCommand (Befehl,
    MRGTimeouts.TritschlerFTLProt, true, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  Result:=true;
end;

{-----------------------------------------------------}
function TMRGAbruf.Send_FTL_Uebertragungsende: boolean;
{-----------------------------------------------------}
{ Tritschler FTL-Protokoll: �bertragungsende-Befehl senden;
  Ergebnis: true, wenn erfolgreich }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  { �bertragungsende-Befehl senden }
  Befehl:='A' + CR;
  if not TMRGCustomCommObj (CommObj).SendTritschlerFTLCommand (Befehl,
    MRGTimeouts.TritschlerFTLProt, false, R, NoCarrier) then begin
    { Timeout ist kein Fehler. Es kommt keine Antwort sofern das 225 ms-
      Zeitfenster eingehalten wurde. }
    if not ((R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT)) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end;
  Result:=true;
end;

{--------------------------------------------}
procedure TMRGAbruf.Send_Tritschler_Mux_Break;
{--------------------------------------------}
{ Tritschler Multiplexer Break-Befehl senden }
var
  Befehl: string;
  R: TRueckgabe;

begin
  { Schnittstellen-Parameter f�r den nachfolgenden Break-Befehl an den
    Multiplexer aktivieren: }
  TMRGCustomCommObj (CommObj).SetAbrufgruppe (8, MRG_DPS);
  TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);
  { Break-Befehl senden, es kommt keine Antwort ! }
  Befehl:=SOH+'B0'+ETX;
  CommObj.SendCommand (Befehl, [], 0, 0, ad_String, R, NoCarrier);
  TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_Off);
  Delay (500);
end;

{--------------------------------------------------------}
procedure TMRGAbruf.Init_Tritschler_Mux_FTL_Kommunikation;
{--------------------------------------------------------}
{ Im Tritschler Multiplexer die Kommunikation f�r FTL-Protokoll }
var
  Befehl: string;
  R: TRueckgabe;

begin
  { Schnittstellen-Parameter f�r den nachfolgenden Multiplexer-Befehl aktivieren: }
  TMRGCustomCommObj (CommObj).SetAbrufgruppe (8, MRG_DPS);
  { zum Umschalten des Ausgangs im Multiplexer wird (irgend-)ein IEC-Befehl
    geschickt: }
  Befehl:=GetTritschler_IECKommando_Standardabfrage (SSU_GeraeteAuswNr,
    VerbAufbau_Passwort);
  { Befehl senden ohne auf Antwort zu warten (die kommt als FTL-Protokoll-
    Antwort im "falschen" Schnittstellen-Datenformat und kann daher sowieso
    nicht ausgewertet werden): }
  TMRGCustomCommObj (CommObj).SendTritschlerIECCommand (Befehl, ad_String,
    0, FMrgDefData.MrgTyp, false, R, NoCarrier);

  { Schnittstellen-Parameter f�r die nachfolgenden FTL-Ger�te-Befehle aktivieren:
    -> Wartezeit, damit vorangegangener Befehl versendet wird bevor
       Schnittstellen-Parameter anschlie�end ge�ndert werden (bei von Comservern
       bereitgestellten Schnittstellen kann der Befehl noch im Sendepuffer
       stehen und er w�rde mit dem neuen Datenformat versendet werden !) }
  Delay (200);
end;

{---------------------------------------------------}
procedure TMRGAbruf.Logout_TME400 (AMrgTyp: integer);
{---------------------------------------------------}
{ Codewort-Freigabe des TME400 wieder deaktivieren, wenn sie w�hrend des Abrufs
  aktiviert wurde.
  -> auch f�r RSM200
  �bergabe: Ger�tetyp }
var
  SlaveData: TSlaveData;
  MBRegisterDef: TMBRegisterDef;
  RegisterRequestData: TRegisterRequestData;
  iPW: integer;
  Code: integer;
  bDummy: boolean;
  sDummy: string;

begin
  if FParametrierModus_gesetzt then begin
    // Zum Deaktivieren wird ein falsches Passwort gesendet, d.h.
    // ein anderes als das Login-Passwort, mit dem die Codewort-Freigabe
    // ja erfolgreich aktiviert werden konnte.
    // -> erlaubte Codewort-Werte im TME400, RSM200: 1..9999       
    Val (VerbAufbau_Passwort, iPW, Code);
    if iPW < 5000 then
      iPW:=5000
    else
      iPW:=1;

    // TME400:
    if AMrgTyp in [mrgtyp_TME400_VCF, mrgtyp_TME400_VMF] then
      MBRegisterDef:=TME400_MBRegisterDef_Para_Codewort
    // RSM200:
    else if AMrgTyp in [mrgtyp_RSM200_VCF, mrgtyp_RSM200_VMF] then  // 09.01.2024, WW
      MBRegisterDef:=RSM200_MBRegisterDef_Para_Codewort;

    SetModbusSlaveData_AdresseByteOrder (AMrgTyp, SlaveData);

    with RegisterRequestData do begin
      FktCode:=16;
      StartAdresse:=MBRegisterDef.StartAdresse;  // Registeradresse Ger�te-Codewort Freigabe
      AnzahlBytes:=MBRegisterDef.AnzahlBytes;
      AnzahlBits:=0;  // nicht verwendet
      RegisterKonvListe:=nil;  // nicht verwendet
      Typ:=MBRegisterDef.Typ;
      Wert_Einstellen:=IntToStr (iPW);  // Einstellwert Codewort
    end;

    // Modbus-Request zum Setzen des Passworts versenden, Response empfangen:
    SendModbusRequest (SlaveData, RegisterRequestData,
                       MRGTimeouts.ModbusProt, bDummy, sDummy);
  end;
end;

{-----------------------------------------------------------------------------}
function TMRGAbruf.Login_SICK (Passwort: string; PasswortNr: integer): boolean;
{-----------------------------------------------------------------------------}
{ Login-Prozedur durchf�hren f�r FLOWSIC500 (Modbus);
  �bergabe: Passwort
            Passwort-Nummer
  Ergebnis: true, wenn Login ok }
var
  SlaveData: TSlaveData;
  RegisterRequestData: TRegisterRequestData;
  bPreset: boolean;
  iID: integer;
  sWert: string;
  MBRegisterDef: TMBRegisterDef;
  sDummy: string;

begin
  // Login als Gast (4. Passwort) nicht n�tig:
  if VerbAufbau_PasswortNr <> 4 then begin
    Result:=false;

    SetModbusSlaveData_AdresseByteOrder (mrgtyp_FLOWSIC500, SlaveData);

    // Zuerst aus der Passwortnummer des Verbindungsaufbau-Kommandos die
    // User-ID des Ger�ts ermitteln:
    iID:=PasswortNrToDevicePasswortID (FMrgDefData.MrgTyp, VerbAufbau_PasswortNr);

    // Vorab-Pr�fung des Passwort:
    //   -> 1..4 Zeichen lang
    //   -> auf unerlaubte Zeichen (per Modbus nur '0'..'9' erlaubt)
    if (length (VerbAufbau_Passwort) < 1) OR
       (length (VerbAufbau_Passwort) > 4) OR
       (not isIntString (VerbAufbau_Passwort)) then begin
      FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
      exit;
    end;

    // Einstell-Wert f�r Modbus-Kommando "User-ID f�r Login schreiben" bilden
    sWert:=IntToStr (iID);  // Einstellwert

    MBRegisterDef:=FLOWSIC500_MBRegisterDef_Para_UserID_Login;

    with RegisterRequestData do begin
      FktCode:=16;
      StartAdresse:=MBRegisterDef.StartAdresse;  // Registeradresse User-ID f�r Login
      AnzahlBytes:=MBRegisterDef.AnzahlBytes;
      AnzahlBits:=0;  // nicht verwendet
      RegisterKonvListe:=nil;  // nicht verwendet
      Typ:=MBRegisterDef.Typ;
      Wert_Einstellen:=sWert;
    end;

    // Modbus-Request zum Schreiben der User-ID f�r Login versenden, Response empfangen:
    if not SendModbusRequest (SlaveData, RegisterRequestData,
                              MRGTimeouts.ModbusProt, bPreset, sDummy,
                              EST_LOGINERROR) then exit;

    { Antwort auf "User-ID f�r Login senden"-Kommando auswerten:
      -> pr�fen, ob User-ID ins Ger�t korrekt �bertragen wurde }
    if not bPreset then begin
      FehlerGruppeCodeUpdate (EST_LOGINERROR + COM_MODBUSERROR, MODBUSERR_RESP_WRONGREGISTEREADDRESS);
      exit;
    end;

    // Wenn das Schreiben der User-ID erfolgreich war, dann Passwort senden,
    // d.h. es erfolgt der Login-Versuch zu der zuvor gesendeten User-ID:
    MBRegisterDef:=FLOWSIC500_MBRegisterDef_Para_Passwort_Login;

    with RegisterRequestData do begin
      FktCode:=16;
      StartAdresse:=MBRegisterDef.StartAdresse;  // Registeradresse Passwort f�r Login
      AnzahlBytes:=MBRegisterDef.AnzahlBytes;
      AnzahlBits:=0;  // nicht verwendet
      RegisterKonvListe:=nil;  // nicht verwendet
      Typ:=MBRegisterDef.Typ;
      Wert_Einstellen:=VerbAufbau_Passwort;
    end;

    // Modbus-Request zum Schreiben des Passworts f�r Login versenden, Response empfangen:
    if not SendModbusRequest (SlaveData, RegisterRequestData,
                              MRGTimeouts.ModbusProt, bPreset, sDummy,
                              EST_LOGINERROR) then begin
        if (Fehlergruppe = EST_LOGINERROR + COM_MODBUSERROR) AND
           (Fehlercode = MODBUSERR_EXC_SLAVEDEVICEFAILURE) then
          // Modbus-Fehler durch aussagekr�ftigeren Passwort-Login-Fehler ersetzen
          FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);

      exit;
    end;

    { Antwort auf "Passwort f�r Login senden"-Kommando auswerten:
      -> pr�fen, ob Passwort ins Ger�t korrekt �bertragen wurde }
    if not bPreset then begin
      FehlerGruppeCodeUpdate (EST_LOGINERROR + COM_MODBUSERROR, MODBUSERR_RESP_WRONGREGISTEREADDRESS);
      exit;
    end;
  end;  { if VerbAufbau_PasswortNr <> 4 }

  Result:=true;
end;

{------------------------------}
procedure TMRGAbruf.Logout_SICK;
{------------------------------}
{ Logout-Prozedur durchf�hren f�r FLOWSIC500 }
var
  SlaveData: TSlaveData;
  RegisterRequestData: TRegisterRequestData;
  bDummy: boolean;
  MBRegisterDef: TMBRegisterDef;
  sDummy: string;

begin
  // Logout als Gast (4. Passwort) nicht n�tig (Anm.: W�rde mit MB-Exception 04
  // beantwortet werden)
  if VerbAufbau_PasswortNr <> 4 then begin
    SetModbusSlaveData_AdresseByteOrder (mrgtyp_FLOWSIC500, SlaveData);

    MBRegisterDef:=FLOWSIC500_MBRegisterDef_Para_Logout;

    with RegisterRequestData do begin
      FktCode:=16;
      StartAdresse:=MBRegisterDef.StartAdresse;  // Registeradresse f�r Logout
      AnzahlBytes:=MBRegisterDef.AnzahlBytes;
      AnzahlBits:=0;  // nicht verwendet
      RegisterKonvListe:=nil;  // nicht verwendet
      Typ:=MBRegisterDef.Typ;
      Wert_Einstellen:='1';
    end;

    // Modbus-Request zum Ausf�hren des Logout versenden, Response empfangen:
    SendModbusRequest (SlaveData, RegisterRequestData, MRGTimeouts.ModbusProt,
                       bDummy, sDummy);
  end;
end;  { if VerbAufbau_PasswortNr <> 4 }


{------------------------------------------------------------------------------}
function TMRGAbruf.Verbindungsaufbau_Fup (VerbAufbauCmdData: TVerbAufbauCmdData;
  FUP_Reset_vor_Abruf: boolean; var VerbInfoData: TVerbInfoData): boolean;
{------------------------------------------------------------------------------}
{ Aufbau einer FUP-Verbindung mit MRG;
  �bergabe: Verbindungsaufbau-Kommandodaten
            Flag, ob ein FUP-Reset (statt einer FUP-Initialisierung) vor dem
              Verbindungsaufbau durchgef�hrt werden soll
            Verbindungsinformationen-Record
  Ergebnis: true, wenn FUP-Verbindungsaufbau erfolgreich }
var
  Befehl: string;
  R: TRueckgabe;
  FUPAntwort: string;
  RufNr: string;

begin
  Result:=false;

  if FUP_Reset_vor_Abruf then begin   { wenn Flag gesetzt, vor dem Abruf FUP-Reset durchf�hren }
    if not Fup_Reset (TMRGFupCommObj (CommObj), FNetProgPath,
                      MRGTimeouts.FupReset, MRGTimeouts.FupAntwort, true,
                      R.Fehlergruppe, R.Fehlercode) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end
  else begin   { ansonsten Standard: FUP initialisieren }
    if not Fup_Init (TMRGFupCommObj (CommObj), FNetProgPath,
                     MRGTimeouts.FupAntwort, R.Fehlergruppe, R.Fehlercode) then begin
      if not Fup_Reset (TMRGFupCommObj (CommObj), FNetProgPath,
                        MRGTimeouts.FupReset, MRGTimeouts.FupAntwort, true,
                        R.Fehlergruppe, R.Fehlercode) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end;
  end;

  { D�-Geschwindigkeit im FUP f�r abzurufende Station einstellen (nicht beim FUP-1200): }
  if not TMRGFupCommObj (CommObj).isFUP1200 then
    if not SetDUEGeschwindigkeit_Fup (VerbAufbauCmdData.ModemTyp) then exit;

  { Rufnummer: ohne optional angeh�ngte Elster-Ger�teadresse; 21.03.2007 WW
    -> Rufnummer ist mit @ von Ger�teadresse getrennt }
  RufNr:=ExtractString (VerbAufbauCmdData.Rufnummer, NUL, '@', 0);
  Befehl:=ESC+'R'+RufNr+CR+ESC+'v'+CR;
  { Verbindungsaufbaukommando senden }
  if not CommObj.SendCommand (Befehl, [CR], 1, MRGTimeouts.Verbindungsaufbau, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  FupAntwort:=ExtractString (R.Antwort, ESC, CR, 0);
  if FupAntwort <> 'FV' then begin     { FUP-Fehlermeldung oder sonstige falsche Antwort }
    FehlerGruppeCodeUpdate (COM_FUPERROR, GetFupErrorcode (FupAntwort));
    exit;
  end;

  VerbInfoData.DZ_VerbindungSteht:=Now;  // Zeitpunkt 'Verbindung hergestellt'; 20.12.2013, WW

  { Verbindung steht jetzt: }
  NoCarrier:=false;

  { Parameter-Konfigurationsdaten f�r abzurufendes MRG initialisieren (pro forma:
    f�r ggf. ausgelesene Modbus-Listen-ID): }
  if not Init_MRGKonfigData_Param (FMB_ID) then exit;  // 11.02.2022, WW

  { Kennungsabfrage (nur Dummy-�bergaben, die werden nur f�r Modem-Abrufe ben�tigt
    bzw. keine Ger�tetyppr�fung bei FUP-Ger�ten): }
  if not KennungAbfragen (-1, '', '', 0, false) then exit;

  { Ger�te-Kennung mit Stammdaten-Kennung vergleichen: }
  if not KennungPruefen (VerbAufbauCmdData.Kennung, VerbAufbauCmdData.KennPruef) then exit;

  { Pa�wort �bertragen: }
  if not PasswortLogin (FMrgDefData.ModemAbrufgruppe, VerbAufbauCmdData.Passwort,
                        VerbAufbauCmdData.PasswortNr) then exit;
  VerbInfoData.DZ_Login:=Now;  // Zeitpunkt 'Login erfolgt'; 20.12.2013, WW

  Result:=true;
end;

{-------------------------------------------------------------------------------------------}
function TMRGAbruf.Verbindungsaufbau_Modem_IP_Seriell (VerbAufbauCmdData: TVerbAufbauCmdData;
  var VerbAutoDetectData: TVerbAutoDetectData; var VerbInfoData: TVerbInfoData): boolean;
{-------------------------------------------------------------------------------------------}
{ Aufbau einer Modem-, IP- oder seriellen Verbindung mit MRG;
  �bergabe: Verbindungsaufbau-Kommandodaten
  R�ckgabe: Record mit automatisch ermittelten Verbindungsparametern
            Verbindungsinformationen-Record
  Ergebnis: true, wenn Modem/IP/Seriell-Verbindungsaufbau erfolgreich }
var
  Befehl: string;
  R: TRueckgabe;
  MRG800PTB: boolean;
  sBuf: string;
  RufNr: string;
  sGeraeteAdresse: string;
  sDummy: string;
  AFehlergruppe: integer;
  AFehlercode: integer;
  S: string;
  IPAdr: string;
  PortId: integer;
  sBaud: string;
  iBaud: integer;
  bModemAutoDetect: boolean;
  bCheckGeraeteTyp: boolean;

begin
  Result:=false;
  if CommObj is TMRGModemCommObj then begin    { Abruf �ber Modem an serieller Schnittstelle }
    TMRGModemCommObj (CommObj).SetDCDCheck (false);      { DCD-�berwachung aus }

    { Schnittstellen-Parameter und Datenprotokoll zur Modem-Initialisierung
      aktivieren: }
    TMRGModemCommObj (CommObj).Serial.Baudrate:=
      TMRGModemCommObj (CommObj).MaxModemBaudrate; { h�chstm�gliche }
  end;
  TMRGCustomCommObj (CommObj).SetAbrufgruppe (0, Modem_DPS);

  if CommObj is TMRGModemCommObj then begin    { Abruf �ber Modem an serieller Schnittstelle }
    { Modem initialisieren: }
    MRG800PTB:=FMrgDefData.ModemAbrufgruppe = 3;
    if not Modem_Initialisieren_Wico (CommObj, MRG800PTB, ModemName, FNetProgPath,
                                      MRGTimeouts.ModemInit, MRGTimeouts.GSMModem,
                                      FCOMNr, false, nil, Fehlergruppe, Fehlercode) then exit;

    { Baudrate f�r Abruf festlegen: }
    case FMrgDefData.ModemAbrufgruppe of
      0: begin  { Versenden von SMS }
           { SMS-Format "Text" setzen: }
           if not GSM_Set_SMS_Format (TMRGCustomCommObj (CommObj), MRGTimeouts.GSMModem,
                                      smsf_Text, AFehlergruppe, AFehlercode) then begin
             FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
             exit;
           end;

           Result:=true;  { "Verbindungsaufbau" erfolgreich abgeschlossen }
           exit;
         end;

      1, 2, 3: TMRGModemCommObj (CommObj).Serial.Baudrate:=br_009600;
      4, 5, 6,
      7, 8, 9,
      10, 11, 12,
      14, 15:  TMRGModemCommObj (CommObj).Serial.Baudrate:=
                 TMRGModemCommObj (CommObj).MaxModemBaudrate; { h�chstm�gliche }     
    else
      TMRGModemCommObj (CommObj).Serial.Baudrate:=br_009600;
    end;
    { Schnittstellen-Parameter und Datenprotokoll f�r Verbindungsaufbau aktivieren: }
    TMRGCustomCommObj (CommObj).SetAbrufgruppe (0, Modem_DPS);

    { Rufnummer: ohne optional angeh�ngte Elster-Ger�teadresse; 21.03.2007 WW
      -> Rufnummer ist mit @ von Ger�teadresse getrennt }
    RufNr:=ExtractString (VerbAufbauCmdData.Rufnummer, NUL, '@', 0);
    Befehl:='atd'+RufNr+CR;
    { Verbindungsaufbaukommando senden }
    if not TMRGModemCommObj (CommObj).SendModemCommand (Befehl, MRGTimeouts.Verbindungsaufbau, R) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Antwort auf Verbindungsaufbau-Kommando auswerten: }
    if not CheckModemConnect (R.Antwort) then exit;

    VerbInfoData.DZ_VerbindungSteht:=Now;  // Zeitpunkt 'Verbindung hergestellt'; 20.12.2013, WW

    Delay (1500);  { Wartezeit nach Connect-Antwort vor nachfolgendem ersten Ger�tebefehl;
                     -> um sicherzustellen, da� Verbindung ins Ger�t durchgeschaltet ist
                     -> F�r MRG 910: 200 ms
                     -> F�r Verbindungen zu Tainy-Zentrale hochgesetzt auf 1500 ms; 05.05.2011, WW
                     Achtung: Tats�chliche Wartezeit bei Modemverbindung plus 500 ms
                              f�r Wartezeit auf Modemantwort ! }
  end
  else if CommObj is TMRGClientSocketCommObj then begin  { Abruf �ber TCP/IP, Client-Socket }
    { Kommandodaten: Rufnummer mu� IP-Adresse/Host und Port durch Leerzeichen getrennt enthalten }
    S:=VerbAufbauCmdData.Rufnummer;
    IPAdr:=F_Zerlegen (S, ' ');
    try
      PortId:=StrToInt (F_LeftTrunc (S, ' '));
    except
      PortId:=8000;
    end;

    { ClientSocket-Verbindung �ffnen: }
    if not TMRGClientSocketCommObj (CommObj).Connect (IPAdr, PortId, MRGTimeouts.Verbindungsaufbau,
                                                      AFehlergruppe, AFehlercode) then begin
      FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
      exit;
    end;

    VerbInfoData.DZ_VerbindungSteht:=Now;  // Zeitpunkt 'Verbindung hergestellt'; 20.12.2013, WW

    Delay (200);  { Wartezeit nach aufgebauter IP-Verbindung vor nachfolgendem ersten Ger�tebefehl;
                     -> um sicherzustellen, da� Verbindung ins Ger�t durchgeschaltet ist
                     -> F�r MRG 910: 200 ms }
  end
  else if CommObj is TMRGSerialCommObj then begin  // Abruf �ber serielle Schnittstelle
    VerbInfoData.DZ_VerbindungSteht:=Now;  // Zeitpunkt 'Verbindung hergestellt'; 20.12.2013, WW
  end;

  { Verbindung steht jetzt: }
  NoCarrier:=false;
  if CommObj is TMRGModemCommObj then
    TMRGModemCommObj (CommObj).SetDCDCheck (true);       { DCD-�berwachung ein }

  if CommObj is TMRGSerialCommObj then begin  // Abruf �ber serielle Schnittstelle
    { Schnittstellen-Parameter f�r serielle MRG-Auslesung sind in MrgDef.dat definiert;
      10.08.2011, WW }
    S:=FMrgDefData.Schnittstellenparameter_seriell;  // z.B '9600 8E1'
    sBaud:=F_Zerlegen (S, ' ');  // Baudrate
    iBaud:=StrToIntDef (sBaud, C_BaudMRGStandard);
    MRG_DPS:=S;  // Datenbits, Parit�t, Stopbits

    if Pos (srv_BT_PuE_K01, VerbAufbauCmdData.ModemTyp) > 0 then begin  { Auslesen �ber Bluetooth-Adapter P+E K01 }
 			// Schnittstellenparameter im Auslesekopf einstellen mit 115200 Bd, 8N1:
      TMRGSerialCommObj (CommObj).Serial.SetCommParameter (br_115200, db_8, none, sb_1);
      // Befehl zum Einstellen der Schnittstellen-Parameter im P+E K01-Bluetooth-Auslesekopf:
      Befehl:=GetKonfigKommando_PuE_K01_Blue (iBaud, MRG_DPS);
      // Befehl an Auslesekopf senden:
      if not CommObj.SendCommand (Befehl, [], 0, 0, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end;

    TMRGSerialCommObj (CommObj).Serial.Baudrate:=
      TMRGSerialCommObj (CommObj).Serial.KonvertBaudrate (iBaud);  // MRG-Auslese-Baudrate
  end;

  { Umsetzung Modemtyp (z.B. A, B) -> Bezeichnung f�r serielles Datenformat (8N1, 7E1 etc.)
    -> Serielles Datenformat aus Verbindungsaufbau-Kommando �bersteuert MRG-Typ-
       spezifisches serielles Schnittstellenformat }
  if GetSrvModemTyp_SerialDataFormatDesc (VerbAufbauCmdData.ModemTyp, sBuf) then
    MRG_DPS:=sBuf;

  { Schnittstellen-Parameter und Datenprotokoll f�r die nachfolgenden
    MRG-Befehle aktivieren:
    -> mit Modemtyp aus Verbindungsaufbau-Kommando f�r Einstellung des optionalen
       MRG-spezifischen Schnittstellen-Datenformats; 21.04.2006, WW }
  if Pos (srv_modem_SSU, VerbAufbauCmdData.ModemTyp) > 0 then  { Tritschler Multiplexer SSU K935 }
    TMRGCustomCommObj (CommObj).SetAbrufgruppe (8, MRG_DPS)
  else begin
    TMRGCustomCommObj (CommObj).SetAbrufgruppe (FMRGDefData.ModemAbrufgruppe, MRG_DPS);

    if CommObj is TMRGClientSocketCommObj then begin  { Abruf �ber TCP/IP, Client-Socket }
      { F�r Modbus-Kommunikation: Modemtyp in Kommandodaten pr�fen auf "nicht mit
        Protokoll Modbus TCP" }
      if Pos (srv_modbus_TCP_Off, VerbAufbauCmdData.ModemTyp) = 0 then
        TMRGClientSocketCommObj (CommObj).ModbusModus:=modbus_TCPIP;  // Modbus TCP; 04.10.2022, WW
    end;
  end;
  TMRGCustomCommObj (CommObj).SetPasswort (VerbAufbauCmdData.Passwort);

  { Datenformat automatisch ermitteln ? 16.08.2012, WW }
  bModemAutoDetect:=Pos (srv_modem_auto, VerbAufbauCmdData.ModemTyp) > 0;
  { Ger�tetyp pr�fen, wenn auch Datenformat automatisch ermittelt werden soll
    ("Konfiguration einlesen"); 15.07.2014, WW }
  bCheckGeraeteTyp:=bModemAutoDetect;

  case FMrgDefData.ModemAbrufgruppe of
    { bei Ger�ten der Modem-Abrufgruppe 1 mu� die Zentrale das Senderecht haben: }
    1: begin
         { ENQ senden: }
         Befehl:='*' + ENQ + CR;
         if not CommObj.SendCommand (Befehl, [CR], 1, MRGTimeouts.ACK01_ProtMeldung, ad_String,
                                       R, NoCarrier) then begin
           FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
           exit;
         end;
       end;

    { bei Ger�ten der Modem-Abrufgruppe 3, 4, 12 pr�fen, ob MRG-Kommunikation mit CRC
      erfolgen mu�: }
    3, 4, 12:
      begin
        if not TMRGCustomCommObj (CommObj).MRG_CRCKonfig_Check (R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
      end;

    { bei Ger�ten der Modem-Abrufgruppe 5 Ger�te-Kommunikation nach Norm IEC 1107
      einleiten und Zugangsschlo� pr�fen bzw. �ffnen: }
    5: begin
         { -> optionale CL-Schnittstelle (Busbetrieb, Elster): Initialisierung
              mit an Rufnummer angeh�ngter Ger�teadresse (durch @ getrennt); 21.03.2007 WW }
         sGeraeteAdresse:=ExtractString (VerbAufbauCmdData.Rufnummer, '@', NUL, 0);

         { Quittungsmodus 1: Schalten in Programmiermode }
         if not Init_IEC1107_Kommunikation (sGeraeteAdresse, '1', FMrgDefData.ModemAbrufgruppe,
                                            bModemAutoDetect, bCheckGeraeteTyp,
                                            sDummy,
                                            VerbAutoDetectData) then exit;

         if not Check_IEC1107_SchlossStatus (VerbAufbauCmdData,
                                             VerbAutoDetectData) then exit;
       end;

    { bei Ger�ten der Modem-Abrufgruppe 6 mu� die Zentrale das Senderecht haben: }
    6: begin
         { ENQ senden: }
         Befehl:=ENQ;
         if not CommObj.SendCommand (Befehl, [CR], 1, MRGTimeouts.ACK01_ProtMeldung, ad_String,
                                       R, NoCarrier) then begin
           FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
           exit;
         end;
       end;

    { bei Ger�ten der Modem-Abrufgruppe 9 (FTL) Ger�te-Kommunikation f�r Tritschler
      FTL-Protokoll einleiten: }
    9: begin
         // Nicht bei Multiplexer-Betrieb. Wird bei DSfG-Umschaltung gemacht !
         if Pos (srv_modem_SSU, VerbAufbauCmdData.ModemTyp) = 0 then
           if not Send_FTL_Uebertragungsbeginn then exit;
       end;

    { bei Ger�ten der Modem-Abrufgruppe 10 Ger�te-Kommunikation f�r Actaris Corus
      einleiten: }
   10: begin
         if not Init_Corus_Kommunikation (bCheckGeraeteTyp) then exit;
       end;

    { bei Ger�ten der Modem-Abrufgruppe 11 Ger�te-Kommunikation nach Norm IEC 1107
      einleiten: }
   11: begin
         if FMrgDefData.MrgTyp = mrgtyp_Sparklog then begin  { Actaris Sparklog }
           { Quittungsmodus 0: Schalten in Standardmode f�r Empfang des Standard-
                               Datensatzes zur Ermittlung der Kanaladressen }
           if not Init_IEC1107_Kommunikation ('', '0', FMrgDefData.ModemAbrufgruppe,
                                              false, false, sDummy,
                                              VerbAutoDetectData) then exit;
         end;

         { Quittungsmodus 1: Schalten in Programmiermode f�r Abruf }
         if not Init_IEC1107_Kommunikation ('', '1', FMrgDefData.ModemAbrufgruppe,
                                            false, bCheckGeraeteTyp, sDummy,
                                            VerbAutoDetectData) then exit;
         if not Send_IEC1107_Passwort (VerbAufbauCmdData.Passwort) then exit;
       end;

    { bei Ger�ten der Modem-Abrufgruppe 13 (Elster DS-100) Kommunikation er�ffnen: }
   13: begin
         Start_SendenDS100;

         // angeschlossenes Ger�t �berpr�fen:
         if not Check_DS100Typ (MRGAnzKanaele, FAktKanal) then exit;

         // auf 1. Kanal positionieren:
         if (MRGAnzKanaele > 1) AND (FAktKanal <> 1) then
           if not Set_KanalDS100 (1) then exit;
       end;

    { bei Ger�ten der Modem-Abrufgruppe 14 (Modbus): }
   14: begin
         { FLOWSIC500: Login-Prozedur durchf�hren VOR der Kennungsabfrage }
         if FMrgDefData.MrgTyp = mrgtyp_FLOWSIC500 then begin
           if not Login_SICK (VerbAufbauCmdData.Passwort,
                              VerbAufbauCmdData.PasswortNr) then exit;  // 29.09.2021, WW
         end;

         { ID der im Ger�t vorhandenen Modbus-Registerliste lesen: }
         if not ModbuslisteID_Abfragen (FMrgDefData.MrgTyp) then exit;  // 11.02.2022, WW
       end;
  end;

  { Parameter-Konfigurationsdaten f�r abzurufendes MRG initialisieren (f�r ggf.
    ausgelesene Modbus-Listen-ID): }
  if not Init_MRGKonfigData_Param (FMB_ID) then exit;

  { Kennungsabfrage: nicht bei Modemgruppe 8, 9 mit Modemtyp 'T' (Tritschler VC2, TTG, TDS
                     mit Multiplexer SSU K935: Wird bei DSfG-Umschaltung gemacht !) und
                     nicht bei Ger�tetyp SR; 15.05.2007, WW }
  if not (((FMrgDefData.ModemAbrufgruppe = 8) OR (FMrgDefData.ModemAbrufgruppe = 9)) AND
          ((Pos (srv_modem_SSU, VerbAufbauCmdData.ModemTyp) > 0) OR (FMrgDefData.MrgTyp = mrgtyp_SR))) then begin
    if FMrgDefData.ModemAbrufgruppe = 13 then begin  // Kennungsabfrage und -pr�fung Elster DS-100
      if not AbfragenPruefen_KennungDS100 (MRGAnzKanaele, VerbAufbauCmdData) then exit;
    end
    else begin
      if not KennungAbfragen (FMrgDefData.ModemAbrufgruppe, FMRGDefData.Infobefehl,
                              FMrgInfoData.Kennung, 0, bCheckGeraeteTyp) then exit;
      { Ger�te-Kennung mit Kennung aus Verbindungsaufbau-Kommando vergleichen: }
      if not KennungPruefen (VerbAufbauCmdData.Kennung, VerbAufbauCmdData.KennPruef) then exit;
    end;
  end;

  { Login (nach der Kennungsabfrage): }
  case FMrgDefData.ModemAbrufgruppe of
    1, 2, 6: if not PasswortLogin (FMrgDefData.ModemAbrufgruppe,
                                   VerbAufbauCmdData.Passwort,
                                   VerbAufbauCmdData.PasswortNr) then exit;  { Pa�wort �bertragen }
    3, 4, 12:
      begin
        { wenn Kennung-Befehl mit Passwort erfolgreich gesendet wurde,
          dann "Login" erfolgreich: }
        if TMRGCustomCommObj (CommObj).GetWithPasswort then begin
//
        end;
      end;
    8, 9: begin
            if (Pos (srv_modem_SSU, VerbAufbauCmdData.ModemTyp) > 0) OR  { Login nur bei Multiplexer-Betrieb }
               (FMrgDefData.MrgTyp = mrgtyp_SR) then  { bei Ger�tetyp SR }
              { Login mit Quittungsmodus 0: Anfordern des SSU-Status/Alarmursache }
              if not PasswortLogin_Tritschler_IEC_SSU (VerbAufbauCmdData.Passwort,
                '0', nil, VerbAufbauCmdData.Kennung, VerbAufbauCmdData.KennPruef) then exit;
          end;
    { ModemAbrufgruppe 5: hier kein Login ("Login" erfolgt in Methode 'Check_IEC1107_SchlossStatus'
                          mit dem �ffnen des Kunden- bzw. Lieferantenschlosses)
               7, 13, 14: kein Login erforderlich/m�glich
                      10: hier kein Login ("Login" erfolgt in Methode 'Init_Corus_Kommunikation')
                      11: hier kein Login ("Login" erfolgt in Methode 'Send_IEC1107_Passwort') }
  end;
  VerbInfoData.DZ_Login:=Now;  // Zeitpunkt 'Login erfolgt'; 20.12.2013, WW

  { Ger�tetyp abfragen und pr�fen; 15.07.2014, WW }
  if bCheckGeraeteTyp then begin
    case FMrgDefData.ModemAbrufgruppe of
      3, 4: begin  // MRG 800 PTB, 905, 910, EC 694
              if not GeraeteTypAbfragen_Pruefen then exit;
            end;
    end;
  end;

  Result:=true;
end;

{-------------------------------------------------------------------------}
function TMRGAbruf.VerbAufbau (VerbAufbauCmdData: TVerbAufbauCmdData;
                               FUP_Reset_vor_Abruf: boolean;
                               var StationsKennungRet: string;
                               var VerbAutoDetectData: TVerbAutoDetectData;
                               var VerbInfoData: TVerbInfoData): boolean;
{-------------------------------------------------------------------------}
{ Aufbau einer Verbindung mit MRG;
  �bergabe: Verbindungsaufbau-Kommandodaten
            Flag, ob ein FUP-Reset (statt einer FUP-Initialisierung) vor dem
            Verbindungsaufbau durchgef�hrt werden soll
  R�ckgabe: Kennung der MRG-Station
            Record mit automatisch ermittelten Verbindungsparametern
            Verbindungsinformationen-Record
  Ergebnis: true, wenn kompletter Verbindungsaufbau incl. Kennungsvergleich und Login erfolgreich
            -> Flag "NoCarrier" gibt bei fehlerhaftem Verbindungsaufbau Auskunft,
               ob die Verbindung noch steht oder nicht)
            -> Bei Modem-Abrufen kann eine bestehende Verbindung sicher durch
               �berwachung des DCD-Signals erkannt werden. Bei FUP-Abrufen kann
               dies nur behelfsm��ig erkannt werden. NoCarrier wird nach erfolgreichem
               Verbindungsaufbau auf true gesetzt, wenn auf einen MRG-Befehl STX..ETX
               eine FUP-Antwort ESC..CR statt einer MRG-Antwort zur�ckkommt }
Begin
  Result:=false;
  NoCarrier:=true;                 { Vorbelegung: es besteht keine Verbindung }
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }
  StationsKennungRet:='';
  { R�ckgabe-Vorbelegung f�r automatisch ermittelte Verbindungsparameter: }
  with VerbAutoDetectData do begin
    ModemTyp:='';  // Kennzeichnung, da� kein automatisch ermittelter Modemtyp vorhanden ist
    PasswortNr:=-1;  // Kennzeichnung, da� keine automatisch ermittelte Passwortnummer vorhanden ist
  end;
  { R�ckgabe-Vorbelegung f�r Verbindungsinformationen: }
  with VerbInfoData do begin  // Keine Verbindungsinformationen vorhanden
    DZ_VerbindungSteht:=0;
    DZ_Login:=0;
  end;

  try
    { Passwort-Nummer, Passwort, Modemtyp, Rufnummer  merken f�r sp�tere Kommandos
      (z.B. Zeitsynchronisation, Abfragebefehle Tritschler mit Multiplexer SSU,
      SMS versenden): }
    VerbAufbau_PasswortNr:=VerbAufbauCmdData.PasswortNr;
    VerbAufbau_Passwort:=VerbAufbauCmdData.Passwort;
    VerbAufbau_ModemTyp:=VerbAufbauCmdData.ModemTyp;
    VerbAufbau_Rufnummer:=VerbAufbauCmdData.Rufnummer;

    { Vorbelegung: Kennung aus Verbindungsaufbau-Kommando }
    StationsKennung:=VerbAufbauCmdData.Kennung;

    { allgemeine Konfigurationsdaten f�r Ger�tetyp initialisieren (ohne Parameter-
      Konfigurationsdaten: }
    if not Init_MRGKonfigData (VerbAufbauCmdData.GeraeteTyp, false) then exit;

    { Automatische Ermittlung der Passwortnummer derzeit nur f�r Modemabrufgruppe 5
      implementiert. F�r alle anderen Modemabrufgruppen wird Passwort 1 verwendet: }
    if VerbAufbauCmdData.PasswortNr = 0 then begin
      if FMrgDefData.ModemAbrufgruppe <> 5 then begin
        VerbAufbauCmdData.PasswortNr:=1;
        VerbAufbau_PasswortNr:=VerbAufbauCmdData.PasswortNr;
      end;
    end;

    if CommObj is TMRGFupCommObj then begin  // Abruf �ber FUP
      if not Verbindungsaufbau_Fup (VerbAufbauCmdData, FUP_Reset_vor_Abruf,
                                    VerbInfoData) then exit;
    end
    else begin  // Abruf �ber Modem/IP/Seriell
      if not Verbindungsaufbau_Modem_IP_Seriell (VerbAufbauCmdData,
                                                 VerbAutoDetectData,
                                                 VerbInfoData) then exit;
    end;
  finally
    StationsKennungRet:=StationsKennung;
  end;
  Result:=true;
End;

{-----------------------------------------------}
function TMRGAbruf.Verbindungsabbau_Fup: boolean;
{-----------------------------------------------}
{ FUP-Verbindung wird abgebaut
  Ergebnis: true, wenn FUP-Verbindungsabbau ok }
Var
  Befehl: string;
  R: TRueckgabe;

Begin
  Result:=false;
  Befehl:=ESC+'e'+CR;
  { Verbindungsabbaukommando senden }
  if not CommObj.SendCommand (Befehl, [CR], 1, MRGTimeouts.Verbindungsabbau, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Verbindungsabbau-Kommando wird nicht ausgewertet }

  NoCarrier:=true;
  Result:=true;
End;

{------------------------------------------------------------}
function TMRGAbruf.Verbindungsabbau_Modem_IP_Seriell: boolean;
{------------------------------------------------------------}
{ Modem-, IP- oder serielle Verbindung wird abgebaut (Modem: R�cksetzen der DTR-Leitung) }
var
  Befehl: string;
  R: TRueckgabe;
  AFehlergruppe: integer;
  AFehlercode: integer;

Begin
  Result:=false;
  case FMrgDefData.ModemAbrufgruppe of
    0: begin
         Result:=true;  { kein Verbindungsabbau (z.B. bei Versenden von SMS) }
         exit;
       end;

    5, 11:
      begin  { Ger�te mit IEC 1107-Protokoll (z.B. Elster DL210, DL220, DL240, EK260, Actaris Sparklog) }
       { BCC-Einstellungen f�r die letzten Befehle setzen: }
       TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);
       TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off);

       if FMrgDefData.ModemAbrufgruppe = 5 then begin  { Elster DL210, DL220, DL240, EK260 }
         { Das Schlo�, welches f�r den Abruf extra ge�ffnet wurde, mu� vor
           dem Beenden der Verbindung auch wieder geschlossen werden: }
         if IEC1107_SchlossStatus = st_Lieferantenschloss_geoeffnet then begin
           { Befehl senden: Lieferantenschloss schlie�en }
           Befehl:=GetLIS200Kommando_Schreiben ('3:170', '0');
           CommObj.SendCommand (Befehl, [ACK, NAK, ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier);
         end
         else if IEC1107_SchlossStatus = st_Kundenschloss_geoeffnet then begin
           { Befehl senden: Kundenschloss schlie�en }
           Befehl:=GetLIS200Kommando_Schreiben ('4:170', '0');
           CommObj.SendCommand (Befehl, [ACK, NAK, ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier);
         end
         else if IEC1107_SchlossStatus = st_Datenausleserschloss_geoeffnet then begin
           { Befehl senden: Datenausleserschloss schlie�en; 03.06.2020, WW }
           Befehl:=GetLIS200Kommando_Schreiben ('5:170', '0');
           CommObj.SendCommand (Befehl, [ACK, NAK, ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier);
         end;
       end;

       { bei gesetztem Abschaltbefehl-Flag: Abschalt-Befehl senden, es kommt keine Antwort ! }
       if FAbschaltBefehl then begin
         Befehl:=SOH+'B0'+ETX;
         CommObj.SendCommand (Befehl, [], 0, 0, ad_String, R, NoCarrier);
       end;
     end;

    6: begin   { KE-Anlagen }
         { Verbindungsende-Befehl senden, es kommt keine Antwort ! }
         if FMrgDefData.MrgTyp = mrgtyp_KE_Ruhrgas then
           Befehl:=GetKE_Kommando ('S010')
         else if FMrgDefData.MrgTyp = mrgtyp_KE_PPN then
           Befehl:=GetKE_Kommando ('T010')
         else
           Befehl:='';
         CommObj.SendCommand (Befehl, [ETX], 1, 0, ad_String, R, NoCarrier);
       end;

    9: begin  { Tritschler TTG mit FTL-Protokoll }
         Send_FTL_Uebertragungsende;  { �bertragungsende-Befehl senden }
       end;

   10: begin  { Actaris Corus }
         if FAbschaltBefehl then begin
           { CRC-Einstellungen f�r abschlie�enden BREAK-Befehl setzen: }
           TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_CRC_Corus);
           { BREAK-Befehl senden, es kommt keine Antwort ! }
           Befehl:=SOH+'B0'+ETX;
           CommObj.SendCommand (Befehl, [], 0, 0, ad_String, R, NoCarrier);
         end;
       end;

   13: begin  { Elster DS-100 }
         End_SendenDS100;  { Kommunikation mit DS-100 beenden }
       end;


   14: begin  { Modbus }
         case FMrgDefData.MrgTyp of
           mrgtyp_TME400_VCF,
           mrgtyp_TME400_VMF,  { TME400; 18.02.2021, WW }
           mrgtyp_RSM200_VCF,
           mrgtyp_RSM200_VMF:  { RSM200; 09.01.2024, WW }
             begin
               { Ggf. aktivierte Codewort-Freigabe wieder deaktivieren: }
               Logout_TME400 (FMrgDefData.MrgTyp);
             end;

           mrgtyp_FLOWSIC500:  { FLOWSIC500; 29.09.2021, WW }
             begin
               // Konfigurationsmodus im Ger�t ausschalten, wenn er w�hrend des
               // Abrufs eingeschaltet wurde:
               if FParametrierModus_gesetzt then
                 SetKonfigurationsmodus_SICK (false);                               

               { Logout-Prozedur durchf�hren: }
               Logout_SICK;
             end;
         end;
       end;
  end;

  if CommObj is TMRGModemCommObj then
    TMRGModemCommObj (CommObj).SetDCDCheck (false);      { DCD-�berwachung aus }

  if CommObj is TMRGModemCommObj then begin  { Abruf �ber Modem an serieller Schnittstelle }
    { Schnittstellen-Parameter und Datenprotokoll f�r die Modem-Antwort auf das
      R�cksetzen der DTR-Leitung einstellen (wie beim Senden des atd-Befehl): }
    TMRGCustomCommObj (CommObj).SetAbrufgruppe (0, Modem_DPS);

    TMRGModemCommObj (CommObj).Serial.ClearDTRSignal;  { DTR-Leitung r�cksetzen }
    { OK vom Modem empfangen: Verbindung beendet }
    if TMRGModemCommObj (CommObj).SendModemCommand ('', MRGTimeouts.Verbindungsabbau, R) then begin
      if Pos ('OK', AnsiUpperCase (R.Antwort)) <> 0 then begin
        NoCarrier:=true;
        Result:=true;
      end;
    end;
    TMRGModemCommObj (CommObj).Serial.SetDTRSignal;  { DTR-Leitung wieder setzen }
  end
  else if CommObj is TMRGClientSocketCommObj then begin  { Abruf �ber TCP/IP, Client-Socket }
    { ClientSocket-Verbindung schlie�en: }
    if not TMRGClientSocketCommObj (CommObj).Disconnect (MRGTimeouts.Verbindungsabbau,
                                                         AFehlergruppe, AFehlercode) then
      FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode)
    else
      Result:=true;
  end
  else if CommObj is TMRGSerialCommObj then begin  { Abruf �ber serielle Schnittstelle }
    Result:=true;
  end;
End;

{------------------------------------}
function TMRGAbruf.VerbAbbau: boolean;
{------------------------------------}
{ Verbindung wird abgebaut
  Ergebnis: true, wenn Verbindungsabbau ok }
Begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }

  if isFupAbruf then begin
    if not Verbindungsabbau_Fup then exit;
  end else
    Verbindungsabbau_Modem_IP_Seriell;
    
  Result:=true;
end;

{-----------------------------------------------}
function TMRGAbruf.SetNextStdAbrufKanal: boolean;
{-----------------------------------------------}
{ auf den n�chsten abzurufenden Stundenwert-Abrufkanal weiterschalten;
  Ergebnis: false, wenn keine weiteren Kan�le abzurufen sind }
begin
  Result:=false;
  case FMrgDefData.Kommandogruppe of
    102, 119:
         begin        { Elster DL240, DL230 }
           inc (StdKanalNr);
           Result:=StdKanalNr < 4;  { Messperiodenarchive 1 bis 4 }
         end;

    104: begin        { Datacon FWU }
           inc (StdKanalNr);   { wird als Index f�r den Zugriff auf das Feld
                                 'FWU_MessAbruf_Kennzahlen' verwendet
                                 -> entspricht NICHT der Wieser-Kanalnummer }
           Result:=StdKanalNr < High (FWU_MessAbruf_Kennzahlen);
         end;

    107: begin        { Tritschler TTG mit IEC-Protokoll }
           inc (StdKanalNr);
           Result:=StdKanalNr < MRGAnzKanaele;  { Anzahl der Kan�le online ermittelt }
         end;

    108: begin        { Elster DL220 }
           inc (StdKanalNr);
           Result:=StdKanalNr < 2;  { Messperiodenarchive 1 und 2 }
         end;

    109: begin        { Tritschler TTG mit FTL-Protokoll }
           inc (StdKanalNr);
           Result:=StdKanalNr < 2;  { Datentelegramm 2 (Messperiodenwerte) f�r Kan�le 1 und 2 }
           { -> Anzahl der Kan�le wird nicht wie beim TTG mit IEC-Protokoll online
                ermittelt, da nur Ger�te ab Version x6.00 die Kanalinfo (K18-Block) liefern }
         end;

    112: begin        { Actaris Sparklog }
           inc (StdKanalNr);
           Result:=StdKanalNr < 4;  { Messperiodenarchive 1 bis 4 }
         end;

    113: begin        { Elster DL210 }
           inc (StdKanalNr);
           Result:=StdKanalNr < 1;  { nur Messperiodenarchiv 1 }
         end;

    115: begin        { Elster DS-100 }
           inc (StdKanalNr);
           Result:=StdKanalNr < MRGAnzKanaele;  { Anzahl der Kan�le online ermittelt }
         end;
  end;  { case }
end;

{-----------------------------------------------}
function TMRGAbruf.SetNextTagAbrufKanal: boolean;
{-----------------------------------------------}
{ auf den n�chsten abzurufenden Tagessatz-Abrufkanal weiterschalten;
  Ergebnis: false, wenn keine weiteren Kan�le abzurufen sind }
begin
  Result:=false;
  case FMrgDefData.Kommandogruppe of
    104: begin        { Datacon FWU }
           inc (TagKanalNr);         { wird als Index f�r den Zugriff auf das Feld
                                       'FWU_TagAbruf_Kennzahlen' verwendet }
           Result:=TagKanalNr < High (FWU_TagAbruf_Kennzahlen);
         end;

    109: begin        { Tritschler TTG mit FTL-Protokoll }
           inc (TagKanalNr);
           Result:=TagKanalNr < 2;  { Datentelegramm 1 (Z�hlerst�nde) f�r Kan�le 1 und 2 }
           { -> Anzahl der Kan�le wird nicht wie beim TTG mit IEC-Protokoll online
                ermittelt, da nur Ger�te ab Version x6.00 die Kanalinfo (K18-Block) liefern }
         end;
  end;  { case }
end;

{------------------------------------------------}
function TMRGAbruf.GetParameter_MeldKonv: boolean;
{------------------------------------------------}
{ alle f�r die Konvertierung von Meldungen ben�tigten Parameter auslesen;
  Ergebnis: true, wenn Auslesen der Parameter OK }
var
  PL: TParameterListe;
  sdummy: string;

begin
  Result:=false;

  { bei SICK FLOWSIC500 mu� die Ger�tezeitzone (Minuten) abgefragt werden, damit in der
    ME-Konvertierung die UTC-Zeitstempel der Meldungs-Rohdaten in Zeitstempel
    mit lokaler Zeit umgerechnet werden k�nnen; 29.09.2021, WW }
  if (FMrgDefData.MrgTyp = mrgtyp_FLOWSIC500) then begin
    // wenn Parameter 'Zeitzone in Miunten' noch nicht in Parameterliste
    // verf�gbar ist: Auslesen !
    if not ParameterListe_MRGAbruf.GetValue (CP_SICK_FLOWSIC500_Zeitzone_Minuten, sdummy) then begin
      PL:=TParameterListe.Create (FKonfigPath);
      try
        if not AbrufParameter (CP_SICK_FLOWSIC500_Zeitzone_Minuten, nil, PL) then exit;
      finally
        PL.Free;
      end;
    end;
  end;

  Result:=true;
end;

{-----------------------------------------------------------------------------}
function TMRGAbruf.GetParameter_MessTagKonv (ArchivdatenTyp: integer): boolean;
{-----------------------------------------------------------------------------}
{ alle f�r die Konvertierung von Messwerten/Tages�tzen ben�tigten Parameter auslesen;
  �bergabe: Archivdatentyp
  Ergebnis: true, wenn Auslesen der Parameter OK }
var
  PL: TParameterListe;
  sdummy: string;
  slParaAllg: TStringList;
  sParaNrAllg: string;
  sFiltertyp: string;
  i: integer;

begin
  Result:=false;

  if (ArchivdatenTyp = C_CmdArchDatenTyp_LGZnorm) AND  // 19.07.2005, WW
     (F_ParameterNoetig = pn_Ja_Alle) then begin
    { zur Konvertierung der Messwerte in normierte LGZ-Werte sind bei manchen
      Ger�tetypen Ger�te-Parameter notwendig. Variante: Es werden alle Parameter
      gelesen }
    if not Alle_Parameter_gelesen then begin  // alle Parameter nur einmal je Abruf lesen
      PL:=TParameterListe.Create (FKonfigPath);
      try
        if not AbrufParameter ('0', nil, PL) then exit;  // alle Parameter abrufen
      finally
        PL.Free;
      end;
    end;
  end
  else begin
    { zur Konvertierung der Messwerte in normierte LGZ-Werte sind bei manchen
      Ger�tetypen Ger�te-Parameter notwendig. Variante: Es werden nur die
      erforderlichen Parameter gelesen; 08.08.2014, WW }
    if (ArchivdatenTyp = C_CmdArchDatenTyp_LGZnorm) AND
       (F_ParameterNoetig = pn_Ja_Menge) then begin
      if not LGZnormKonv_Parameter_gelesen then begin  // LGZnormKonv-Parameter nur einmal je Abruf lesen
        slParaAllg:=TStringList.Create;  // Liste f�r allg. Parameternummern der LGZnormKonv-Parameter
        try
          if not (FParamMrgKonfigList.Count > 0) then begin  // 09.07.2021, WW
            FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
            exit;
          end;

          for i:=0 to FParamMrgKonfigList.Count-1 do begin
            sParaNrAllg:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.Parameternummer;
            sFiltertyp:=TParamMrgDataObj (FParamMrgKonfigList [i]).Data.Filtertyp;
            // nur Parameter, welche f�r Messwerte-Konvertierung "LGZ-normiert"
            // ben�tigt werden, in Parameternummern-Liste laden (Filtertyp 'N'):
            if (Pos (C_PFT_LGZNORMKONV, sFiltertyp) > 0) AND (length (sParaNrAllg) > 0) then
              slParaAllg.Add (sParaNrAllg);
          end;

          if slParaAllg.Count = 0 then begin  // keine LGZnormKonv-Parameter definiert
            FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
            exit;
          end;

          PL:=TParameterListe.Create (FKonfigPath);
          try
            // Definierte LGZnormKonv-Parameter abrufen:
            if not AbrufParameter ('', slParaAllg, PL) then exit;
          finally
            PL.Free;
          end;
        finally
          slParaAllg.Free;
        end;
        LGZnormKonv_Parameter_gelesen:=true;
      end;
    end;  { if (ArchivdatenTyp = C_CmdArchDatenTyp_LGZnorm) AND ... }

    { bei MRG 800PTB mu� die Ger�teversion abgefragt werden.
      -> Pr�fung erfolgt in der MW/TA-Konvertierung (MRG910 kann als
         MRG800PTB abgefragt werden. Konvertierung w�rde falsche Werte liefern !)
         05.06.2007, WW }
    if (FMrgDefData.MrgTyp = mrgtyp_MRG800PTB) then begin
      // wenn Parameter 'Ger�teversion' noch nicht in Parameterliste verf�gbar
      // ist: Auslesen !
      if not ParameterListe_MRGAbruf.GetValue (CP_ALLG_Geraeteversion, sdummy) then begin
        PL:=TParameterListe.Create (FKonfigPath);
        try
          if not AbrufParameter (CP_ALLG_GeraeteVersion, nil, PL) then exit;  // Ger�teversion abrufen
        finally
          PL.Free;
        end;
      end;
    end

    { bei MRG 910 m�ssen die Ger�teversion und die k-Faktoren abgefragt werden.
      - Ger�teversion gibt Aufschlu�, ob max. 2 oder 4 Analogkan�le mit
         dem E-Befehl abgefragt werden k�nnen; 05.06.2007, WW
      - k-Faktoren m�ssen in der MW/TA-Konvertierung in die Messwerte und
        Tagess�tze eingerechnet werden; 28.05.2013, WW }
    else if (FMrgDefData.MrgTyp = mrgtyp_MRG910) then begin
      // wenn Parameter 'Ger�teversion' noch nicht in Parameterliste verf�gbar
      // ist: Auslesen !
      if not ParameterListe_MRGAbruf.GetValue (CP_ALLG_Geraeteversion, sdummy) then begin
        PL:=TParameterListe.Create (FKonfigPath);
        try
          if not AbrufParameter (CP_ALLG_GeraeteVersion, nil, PL) then exit;  // Ger�teversion abrufen
        finally
          PL.Free;
        end;
      end;

      // wenn Parameter 'k-Faktor, Impulskanal 1..4' noch nicht in Parameterliste verf�gbar
      // sind: optionale k-Faktoren auslesen (in Ger�teversionen ab 01/2011) !
      if not ParameterListe_MRGAbruf.GetValue (CP_IMP_k_Faktor [1], sdummy) OR
         not ParameterListe_MRGAbruf.GetValue (CP_IMP_k_Faktor [2], sdummy) OR
         not ParameterListe_MRGAbruf.GetValue (CP_IMP_k_Faktor [3], sdummy) OR
         not ParameterListe_MRGAbruf.GetValue (CP_IMP_k_Faktor [4], sdummy) then begin
        PL:=TParameterListe.Create (FKonfigPath);
        try
          if not AbrufParameter (CP_IMP_k_Faktor [1], nil, PL, -1, '', true) then exit;  // k-Faktor, Imp 1 abrufen
          if not AbrufParameter (CP_IMP_k_Faktor [2], nil, PL, -1, '', true) then exit;  // k-Faktor, Imp 2 abrufen
          if not AbrufParameter (CP_IMP_k_Faktor [3], nil, PL, -1, '', true) then exit;  // k-Faktor, Imp 3 abrufen
          if not AbrufParameter (CP_IMP_k_Faktor [4], nil, PL, -1, '', true) then exit;  // k-Faktor, Imp 4 abrufen
        finally
          PL.Free;
        end;
      end;
    end                                                

    { bei MRG 905 m�ssen die k-Faktoren abgefragt werden.
      - k-Faktoren m�ssen in der MW/TA-Konvertierung in die Messwerte und
        Tagess�tze eingerechnet werden; 28.05.2013, WW }
    else if (FMrgDefData.MrgTyp = mrgtyp_MRG905) then begin
      // wenn Parameter 'k-Faktor, Impulskanal 1..2' noch nicht in Parameterliste verf�gbar
      // sind: optionale k-Faktoren auslesen (in Ger�teversionen ab 01/2011) !
      if not ParameterListe_MRGAbruf.GetValue (CP_IMP_k_Faktor [1], sdummy) OR
         not ParameterListe_MRGAbruf.GetValue (CP_IMP_k_Faktor [2], sdummy) then begin
        PL:=TParameterListe.Create (FKonfigPath);
        try
          if not AbrufParameter (CP_IMP_k_Faktor [1], nil, PL, -1, '', true) then exit;  // k-Faktor, Imp 1 abrufen
          if not AbrufParameter (CP_IMP_k_Faktor [2], nil, PL, -1, '', true) then exit;  // k-Faktor, Imp 2 abrufen
        finally
          PL.Free;
        end;
      end;
    end

    { bei Tritschler VC2 mu� Ger�tedatum und -zeit abgefragt werden, damit in der
      MW-Konvertierung der Lastprofildaten 0-Werte von fehlenden Werten sicher
      unterschieden werden k�nnen; 06.05.2009, WW }
    else if (FMrgDefData.MrgTyp = mrgtyp_VC2) OR
            (FMrgDefData.MrgTyp = mrgtyp_VC3_VC2komp) then begin
      // wenn Parameter 'Ger�tedatum' und 'Ger�tezeit' noch nicht in Parameterliste
      // verf�gbar sind: Auslesen !
      if not ParameterListe_MRGAbruf.GetValue (CP_FTL_IEC_Zeit, sdummy) OR
         not ParameterListe_MRGAbruf.GetValue (CP_FTL_IEC_Datum, sdummy) then begin
        PL:=TParameterListe.Create (FKonfigPath);
        try
          if not AbrufParameter ('0', nil, PL) then exit;  // alle Parameter abrufen
        finally
          PL.Free;
        end;
      end;
    end

    { bei SICK FLOWSIC500 mu� die Ger�tezeitzone (Minuten) abgefragt werden, damit in der
      MW-Konvertierung die UTC-Zeitstempel der Messperioden-Rohdaten in Zeitstempel
      mit lokaler Zeit umgerechnet werden k�nnen; 29.09.2021, WW }
    else if (FMrgDefData.MrgTyp = mrgtyp_FLOWSIC500) then begin
      // wenn Parameter 'Zeitzone in Miunten' noch nicht in Parameterliste
      // verf�gbar ist: Auslesen !
      if not ParameterListe_MRGAbruf.GetValue (CP_SICK_FLOWSIC500_Zeitzone_Minuten, sdummy) then begin
        PL:=TParameterListe.Create (FKonfigPath);
        try
          if not AbrufParameter (CP_SICK_FLOWSIC500_Zeitzone_Minuten, nil, PL) then exit;
        finally
          PL.Free;
        end;
      end;
    end

{$IFDEF GAS-X}
    { Gas-X-Abruf: Bei Actaris Corus m�ssen die Ger�tezeit und Z�hlerst�nde
      abgefragt werden, damit aktuelle Z�hlerst�nde in der MW/TA-Konvertierung
      mitverarbeitet werden k�nnen; 09.10.2013, WW }
    else if (FMrgDefData.MrgTyp = mrgtyp_Corus) then begin
      // wenn Parameter 'Ger�tezeit, Z�hlerstand Vb, Vn, Vb st�r, Vn ges' noch
      // nicht in Parameterliste verf�gbar sind: Auslesen !
      if not ParameterListe_MRGAbruf.GetValue (CP_ACT_CORUS_DateTime, sdummy) OR
         not ParameterListe_MRGAbruf.GetValue (CP_ACT_CORUS_Vb, sdummy) OR
         not ParameterListe_MRGAbruf.GetValue (CP_ACT_CORUS_Vn, sdummy) OR
         not ParameterListe_MRGAbruf.GetValue (CP_ACT_CORUS_Vb_stoer, sdummy) OR
         not ParameterListe_MRGAbruf.GetValue (CP_ACT_CORUS_Vn_ges, sdummy) then begin
        PL:=TParameterListe.Create (FKonfigPath);
        try
          slParaAllg:=TStringList.Create;  // Liste f�r abzurufende allg. Parameternummern
          try
            slParaAllg.Add (CP_ACT_CORUS_DateTime);
            slParaAllg.Add (CP_ACT_CORUS_Vb);
            slParaAllg.Add (CP_ACT_CORUS_Vn);
            slParaAllg.Add (CP_ACT_CORUS_Vb_stoer);
            slParaAllg.Add (CP_ACT_CORUS_Vn_ges);
            if not AbrufParameter ('', slParaAllg, PL) then exit;
          finally
            slParaAllg.Free;
          end;
        finally
          PL.Free;
        end;
      end;
    end;
{$ENDIF}
  end;
  Result:=true;
end;

{-----------------------------------------------------------------}
function TMRGAbruf.GetTagesende (var iTagesende: integer): boolean;
{-----------------------------------------------------------------}
{ im Ger�t eingestelltes Tagesende ermitteln;
  R�ckgabe: Tagesende
  Ergebnis: true, wenn Ermitteln des Tagesendes OK }
var
  PL: TParameterListe;
  PNrAllg_Tagesende: string;
  sBuf: string;
  iBuf: integer;
  Code: integer;

begin
  Result:=false;

  iTagesende:=CTagesende;  // Default-Tagesende, wenn aus Ger�t nicht auslesbar

  { Tagesende �ber Ger�te-Parameter auslesen, wenn f�r den Abruf
    noch nicht erfolgt (wird f�r MW/TA-Konvertierung mancher MRG-Typen
    und f�r XML-Antwort der Tagess�tze ben�tigt):
    -> nicht bei Ger�tetypen "KE" (Tagesende kann nicht gelesen werden; wird aber
       auch nicht ben�tigt, da Tagess�tze nicht konvertiert/weitergegeben werden)
    -> nicht bei Ger�tetyp "Datacon FWU" (Tagesende kann nicht gelesen werden; ist
       im Ger�t fest auf 6 Uhr gesetzt, Defaultwert kann daher verwendet werden; Stand 07.07.2003)
    -> nicht bei EC694 (Tagesende nicht n�tig, da keine Tagess�tze im Ger�t vorhanden)
    -> nicht bei Tritschler TTG mit IEC-/FTL-Protokoll (Tagesende kommt in Antwort auf
       "Parameter"-Lesebefehl nicht mit; Ger�t liefert nur regul�re Monats-Tagess�tze,
       keine "Sonder-Tagess�tze", Defaultwert kann daher verwendet werden; Stand 07.07.2003)
    -> Actaris Sparklog (es gibt nur kanalspezifische Tagesende-Parameter,
       keinen allgemeing�ltigen, der f�r alle Kan�le gemeinsam gilt. Kanalspezifische
       Tagesende-Einstellungen werden vom Abrufsystem aber nicht unterst�tzt.)
    -> nicht bei Elster DS-100 (es gibt keinen Parameter "Tagesende")
    -> nicht bei TME400, RSM200 (es gibt keinen Parameter "Tagesende"; ist im Ger�t fest         
       auf 0 Uhr gesetzt; Defaultwert 6 kann trotzdem verwendet werden, da nicht
       ben�tigt (Tagess�tze werden nicht konvertiert/weitergegeben) }
  if (FMrgDefData.MrgTyp <> mrgtyp_KE_Ruhrgas) AND
     (FMrgDefData.MrgTyp <> mrgtyp_KE_PPN) AND
     (FMrgDefData.MrgTyp <> mrgtyp_FWU) AND
     (FMrgDefData.MrgTyp <> mrgtyp_EC694) AND
     (FMrgDefData.MrgTyp <> mrgtyp_TTG_IEC) AND
     (FMrgDefData.MrgTyp <> mrgtyp_TTG_FTL) AND
     (FMrgDefData.MrgTyp <> mrgtyp_Sparklog) AND
     (FMrgDefData.MrgTyp <> mrgtyp_DS100) AND
     (FMrgDefData.MrgTyp <> mrgtyp_TME400_VCF) AND                                     
     (FMrgDefData.MrgTyp <> mrgtyp_TME400_VMF) AND
     (FMrgDefData.MrgTyp <> mrgtyp_RSM200_VCF) AND
     (FMrgDefData.MrgTyp <> mrgtyp_RSM200_VMF) then begin
    // wenn Parameter 'Tagesende' noch nicht in Parameterliste verf�gbar
    // ist: Auslesen ! 14.07.2005, WW
    case FMrgDefData.MrgTyp of
      mrgtyp_DL210,
      mrgtyp_DL220,
      mrgtyp_DL230,
      mrgtyp_DL240: PNrAllg_Tagesende:=CP_ELS_Tagesgrenze [1];  // 23.05.2014, WW
      mrgtyp_EK260,
      mrgtyp_EK280: PNrAllg_Tagesende:=CP_ELS_Tagesgrenze2;
      mrgtyp_VC2,
      mrgtyp_VC3,
      mrgtyp_VC3_VC2komp,
      mrgtyp_VCC,
      mrgtyp_TDS,
      mrgtyp_MCO,
      mrgtyp_MC2: PNrAllg_Tagesende:=CP_FTL_IEC_Tageswechsel;
      mrgtyp_Corus: PNrAllg_Tagesende:=CP_ACT_CORUS_Tageswechsel;
      mrgtyp_Unigas300: PNrAllg_Tagesende:=CP_KAM_UNIGAS300_Gastag;
      mrgtyp_Primus,
      mrgtyp_Prilog: PNrAllg_Tagesende:=CP_RMG_PrimusPrilog_Tagesende;  // 25.11.2020, WW
      mrgtyp_FLOWSIC500: PNrAllg_Tagesende:=CP_SICK_FLOWSIC500_Tagesende;  // 29.09.2021, WW
    else   { alle Wieser-MRG }
      PNrAllg_Tagesende:=CP_ALLG_Tagesende;
    end;
    // pr�fen, ob Tagesende bereits in der Parameterliste enthalten ist:
    if not ParameterListe_MRGAbruf.GetValue (PNrAllg_Tagesende, sBuf) then begin
      // wenn nicht, aus Ger�t auslesen:
      PL:=TParameterListe.Create (FKonfigPath);
      try
        if not AbrufParameter (PNrAllg_Tagesende, nil, PL) then exit;  // Tagesende abrufen
      finally
        PL.Free;
      end;
      // abgerufenes Tagesende aus Parameterliste lesen:
      if not ParameterListe_MRGAbruf.GetValue (PNrAllg_Tagesende, sBuf) then
        sBuf:='';
    end;

    if length (sBuf) > 0 then begin
      { in Tagesende-Rohparameterwert die Einheit abschneiden bei:
        - Elster DL210, DL220, DL230, DL240, EK260, EK280
        - Kamstrup UNIGAS 300 }
      if (FMrgDefData.MrgTyp = mrgtyp_DL210) OR (FMrgDefData.MrgTyp = mrgtyp_DL220) OR
         (FMrgDefData.MrgTyp = mrgtyp_DL230) OR (FMrgDefData.MrgTyp = mrgtyp_DL240) OR
         (FMrgDefData.MrgTyp = mrgtyp_EK260) OR (FMrgDefData.MrgTyp = mrgtyp_EK280) OR
         (FMrgDefData.MrgTyp = mrgtyp_Unigas300) then
        sBuf:=F_Zerlegen (sBuf, '*');

      Val (sBuf, iBuf, Code);
      if Code = 0 then begin
        { aus Tagesende-Rohparameterwert die Stunde extrahieren (Minuten ignorieren,
          werden nicht unterst�tzt):
          - SICK FLOWSIC500 }
        if (FMrgDefData.MrgTyp = mrgtyp_FLOWSIC500) then  // 29.09.2021, WW
          iBuf:=iBuf DIV 100;  // hhmm: z.B. 600 ist 06:00

        iTagesende:=iBuf;
      end;
    end;
  end;
  Result:=true;
end;

{-------------------------------------------------------------------------------------}
function TMRGAbruf.AbrufParameter (AllgParaNr: string;
                                   AllgParaNrListe: TStringList;
                                   ParameterListe: TParameterListe;
                                   ArchivdatenTyp: integer = -1;
                                   Ausgabe_Dateiname: string = '';
                                   bIgnoreDeviceErrorAnswer: boolean = false): boolean;
{-------------------------------------------------------------------------------------}
{ Ruft einen einzelnen, mehrere (Liste) oder alle Parameter im MRG ab;
  �bergabe: allgemeine Parameter-Nummer (f�r Abfrage eines einzelnen Parameters)
              oder '0...0' bzw. Leer-String (f�r Abfrage aller Parameter)
            Zeiger auf Liste abzurufender Parameter (allg. Parameternummern) oder nil
              -> Wenn <> nil: �bergabe 'AllgParaNr' nicht relevant
            Zeiger auf Parameterliste (abgerufene Parameter) oder nil
            Archivdatentyp zur Steuerung der Parameter-Ausgabe (-1 = Standard-Ausgabe
              in Parameterliste)
            Dateiname zur Ausgabe der gelesenen Parameter
            Flag 'bIgnoreDeviceErrorAnswer': wenn true, wird bei einer Fehlerantwort
              des Ger�ts keine Fehlergruppe/-code gesetzt
  R�ckgabe: Parameterliste mit konvertierten Rohdaten, wenn nicht nil
  Ergebnis: true, wenn Parameterabruf erfolgreich }

  {---------------------------------------------------}
  procedure DeleteParameterRohfile (sFilename: string);
  {---------------------------------------------------}
  begin
    if RohdatenLoeschen then begin
      { Aus dem Rohfilenamen evtl. vorangestellte Kanalnummer incl. Strichpunkt-
        Trenner l�schen: }
      if Pos (';', sFilename) > 0 then
        F_Zerlegen (sFilename, ';');
      DeleteFile (sFileName);
    end;
  end;

Var
  R: TRueckgabe;
  Befehl: string;
  AnswerDest: TAnswerDest;
  ParaNrMrg: string;
  AlleParameter: boolean;
  ParaKonv: TParaKonv;
  bWecken: boolean;
  i: integer;
  RFL: TTextListe;
  iParaNrMrg: integer;
  Code: integer;
  BefehlData: TBefehlData;
  BefehlDataObj: TBefehlDataObj;
  APBL: TBefehlList;
  bNochmal: boolean;
  iVersuch: integer;
  sDummy: string;
  iDummy: integer;
  StartIndex_PBL: integer;
  AktIndex_PBL: integer;
  AktIndex_PBL_Merk: integer;
  sExt: string;
  iIndex_Rohfile: integer;
  iAktKanal: integer;
  vDummy: TVerbAutoDetectData;
  bLoadGerZustand: boolean;
  bOK: boolean;
  sParamEK: string;
  SlaveData: TSlaveData;
  RegisterRequestData: TRegisterRequestData;
  RRL: TRegisterRequestList;
  bDummy: boolean;
  ParamMrgData: TParamMrgData;
  slParaAllg: TStringList;

Begin
  Result:=false;

  try
    { Pr�fen, ob vom MRG Parameter abgerufen werden k�nnen:
      -> EC694 Parameterabruf freigegeben; 25.04.2014 WW }
    if (FMrgDefData.MrgTyp = mrgtyp_KE_Ruhrgas) OR
       (FMrgDefData.MrgTyp = mrgtyp_KE_PPN) OR
       (FMrgDefData.MrgTyp = mrgtyp_SR) OR
       (FMrgDefData.MrgTyp = mrgtyp_Veribox_Mini) then begin
      FehlerGruppeCodeUpdate (SYS_ABRUFERROR, SYSABRFERR_KOMMANDOUNGUELTIG);
      exit;
    end;

    { Pr�fen, ob vom MRG Einzel-Parameter abgerufen werden k�nnen: }
    if (FMrgDefData.MrgTyp = mrgtyp_FWU) OR
       (FMrgDefData.MrgTyp = mrgtyp_VC2) OR
       (FMrgDefData.MrgTyp = mrgtyp_VC3) OR
       (FMrgDefData.MrgTyp = mrgtyp_VC3_VC2komp) OR
       (FMrgDefData.MrgTyp = mrgtyp_VCC) OR
       (FMrgDefData.MrgTyp = mrgtyp_TTG_IEC) OR
       (FMrgDefData.MrgTyp = mrgtyp_TTG_FTL) OR
       (FMrgDefData.MrgTyp = mrgtyp_TDS) OR
       (FMrgDefData.MrgTyp = mrgtyp_MCO) OR
       (FMrgDefData.MrgTyp = mrgtyp_MC2) OR
       (FMrgDefData.MrgTyp = mrgtyp_DS100) then
      { Datacon FWU, Tritschler VC2, TTG, TDS, MCO (IEC): kein Einzelparameter-Abruf
        m�glich oder implementiert, stattdessen immer alle Parameter holen }
      AlleParameter:=true
    else
      { alle Parameter werden mit allg. Parameternummer '0..0' (Stellenzahl-unabh�ngig)
        oder "leer" gelesen: }
      AlleParameter:=F_LeftTrunc (AllgParaNr, '0') = '';

    if AlleParameter OR Assigned (AllgParaNrListe) then
      AnswerDest:=ad_File
    else begin               { einzelner Parameter }
      { aus der allgemeinen Parameternummer die ger�tespezifische ermitteln: }
      if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                            AllgParaNr,
                                                            ParamMrgData) then begin  // 09.07.2021, WW
        FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
        exit;
      end;
      ParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;  // 09.07.2021, WW
  //    AnswerDest:=ad_String;  Rohdaten-Konvertierung aus String vorerst nur f�r
  //                            ParameterKonvGruppe 1 verf�gbar, daher wird auch bei
  //                            Einzelparameter-Abruf die Antwort in eine Datei geschrieben.
      AnswerDest:=ad_File;
    end;

    { �bergaberecord f�r Parameterkonvertierung zusammenstellen: }
    with ParaKonv do begin
      ParaKonvGruppe:=FMrgKonvData.ParaKonvGruppe;
      ParaGruppe:=FMrgKonvData.ParameterGruppe;
      ParaUnterGruppe:=FMB_ID;  // 11.02.2022, WW
      RohLoeschen:=RohdatenLoeschen;
    end;

    bNochmal:=true;
    iVersuch:=1;
    while bNochmal do begin
      bNochmal:=false;  { Standard: nur einmal }

      FehlerGruppeCodeUpdate (0, 0, true);         { Vorbelegung Fehlergruppe/-code: OK }

      if NoCarrier then begin                  { es besteht keine Verbindung mehr }
        FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
        exit;
      end;

      if FMrgDefData.Kommandogruppe < 100 then begin
        { Kommando standardm��ig zusammensetzen und abrufen (Wieser-Ger�te): }
        bLoadGerZustand:=false;
        APBL:=TBefehlList.Create;
        try
          if AlleParameter then begin
            BefehlData.sBefehl:=GetMRGKommando_B ('');
            BefehlData.sInfo:='';  // keine Zusatzinfo
            BefehlDataObj:=TBefehlDataObj.Create (BefehlData);
            APBL.Add (BefehlDataObj);  { in Befehlsliste eintragen }
          end
          else begin                  { einzelner Parameter }
            if length (ParaNrMrg) > 0 then begin
              BefehlData.sBefehl:=GetMRGKommando_B (ParaNrMrg);
              BefehlData.sInfo:=ParaNrMrg;  // Zusatzinfo: MRG-Parameternummer
              BefehlDataObj:=TBefehlDataObj.Create (BefehlData);
              APBL.Add (BefehlDataObj);  { in Befehlsliste eintragen }
            end
            else begin  { virtueller Parameter (Ger�tezustand); 12.12.2014, WW }
              { Liste mit Befehlen zum Lesen der einzelnen Ger�tezustands-
                Parameter zusammenstellen: }
              FillParameterBefehlListe_Geraetezustand (AllgParaNr, APBL);

              AnswerDest:=ad_String;
              bLoadGerZustand:=true;
            end;
          end;

          for i:=0 to APBL.Count-1 do begin
            Befehl:=TBefehlDataObj (APBL [i]).Daten.sBefehl;
            ParaNrMrg:=TBefehlDataObj (APBL [i]).Daten.sInfo;
            { Parameterabruf-Kommando senden }
            if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Parameter, AnswerDest, R, NoCarrier) then begin
              FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
              exit;
            end;

            { Parameter korrekt gelesen }
            if AnswerDest = ad_File then begin     { Rohfile mit allen MRG-Parametern ist entstanden }
              if not ValidMRGAntwortfile ('B', FMrgDefData.ModemAbrufgruppe, R.Antwort,
                                          R.Fehlergruppe, R.Fehlercode, RohdatenLoeschen) then begin
                // 31.01.2023, WW: Verfeinerte Fehlerauswertung, nur Antworten mit MRG-Fehlerzeichen ignorieren
                if bIgnoreDeviceErrorAnswer AND  // 28.05.2013, WW
                   not ((R.Fehlergruppe = COM_MRGERROR) AND
                        (R.Fehlercode in [MRGERR_ANTWORTUNVOLLSTAENDIG,
                                          MRGERR_ANTWORTUNERWARTET])) AND
                   not (R.Fehlergruppe = ST_FILEERROR) then  // 31.01.2023, WW
                  Result:=true
                else
                  FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                exit;
              end;

              { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
              if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then
                CopyDatei (R.Antwort, Ausgabe_Dateiname);  { Rohdaten-Zieldatei schreiben }

              { Rohfile in Parameterliste konvertieren:
                09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
              if ParameterListe <> nil then
                ParameterListe.LoadFromFile (R.Antwort, ParaKonv, FParamMrgKonfigList)
              else
                DeleteParameterRohfile (R.Antwort);  // 24.08.2023, WW
            end
            else begin                             { String mit einzelnem MRG-Parameter ist entstanden }
              if not ValidMRGAntwort ('B' + ParaNrMrg, FMrgDefData.ModemAbrufgruppe,
                                      R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
                // 31.01.2023, WW: Verfeinerte Fehlerauswertung, nur Antworten mit MRG-Fehlerzeichen ignorieren
                if bIgnoreDeviceErrorAnswer AND  //  28.05.2013, WW
                   not ((R.Fehlergruppe = COM_MRGERROR) AND
                        (R.Fehlercode in [MRGERR_ANTWORTUNVOLLSTAENDIG,
                                          MRGERR_ANTWORTUNERWARTET])) then  // 31.01.2023, WW
                  Result:=true
                else
                  FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                exit;
              end;
              { Einzelparameter-Rohstring in Parameterliste konvertieren:
                -> geht vorerst nur f�r Ger�te der Parametergruppe 1 !
                09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
              if ParameterListe <> nil then
                ParameterListe.LoadFromHeapA (R.Antwort, ParaKonv, FParamMrgKonfigList);
            end;
          end;  { for i }
        finally
          APBL.Free;
        end;

        { Wert f�r virtuellen Ger�tezustand-Parameter bilden und in
          Parameterliste eintragen: 12.02.2014, WW }
        if bLoadGerZustand AND (ParameterListe <> nil) then  // 14.04.2023, WW
          ParameterListe.LoadGeraeteZustand (FMrgDefData.MrgTyp, AllgParaNr);  
      end
      else begin
        { Fremdger�te: }
        case FMrgDefData.Kommandogruppe of
          101, 102, 108, 113, 118, 119:  { Elster DL210, DL220, DL230, DL240, EK260, EK280 }
            begin
              { Bei EK260 und EK280 einmalig Version und K-Zahl Modus ermitteln
                zur Abfrage der richtigen Parameter f�r Gasqualit�tsdaten: 28.07.2014, WW }
              if (FMrgDefData.Kommandogruppe = 101) OR
                 (FMrgDefData.Kommandogruppe = 118) then begin
                if (length (ElsterEK_Version) = 0) OR
                   (length (ElsterEK_K_Zahl_Modus) = 0) then begin
                  if not ElsterEK_Version_K_Zahl_ModusAbfragen then exit;

                  { Parameter-Befehlsliste "Alle" mit Versions- und K-Zahl Modus-
                    abh�ngigen Elster EK-spezifischen Parameternummern aktualisieren: }
                  if not UpdateParameterBefehlListe_ElsterEK (FParameterBefehlListe_Alle) then exit;
                end;
              end;

              { Flags setzen, es folgen Befehl(e) mit BCC; 14.07.2005, WW }
              TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);
              TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC);

              bLoadGerZustand:=false;
              if AlleParameter OR Assigned (AllgParaNrListe) then begin
                if Assigned (AllgParaNrListe) then begin
                  APBL:=TBefehlList.Create;
                  try
                    { Befehls-Liste zum Lesen der allgemeinen Parameter zusammenstellen: }
                    FillParameterBefehlListe ('', AllgParaNrListe, APBL);

                    { Parameter-Befehlsliste mit Versions- und K-Zahl Modus-
                      abh�ngigen Elster EK-spezifischen Parameternummern aktualisieren: }
                    if not UpdateParameterBefehlListe_ElsterEK (APBL) then exit;

                    { In der Befehlsliste enthaltene Befehle senden und Daten empfangen: }
                    if not CommObj.SendCommandList (
                      APBL, 0, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File,
                      R, NoCarrier, iDummy) then begin
                      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                      exit;
                    end;
                  finally
                    APBL.Free;
                  end;
                end
                else begin
                  { in der Parameter-Befehlsliste "Alle" enthaltene Befehle senden und
                    Daten empfangen (die Befehlsliste wird in Init_MRGKonfigData geladen): }
                  if not CommObj.SendCommandList (
                    FParameterBefehlListe_Alle, 0, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File,
                    R, NoCarrier, iDummy) then begin
                    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                    exit;
                  end;
                end;
              end
              else begin                  { einzelner Parameter }
                if length (ParaNrMrg) > 0 then begin
                  { Bei EK260 und EK280 pr�fen, ob zu allgemeiner Parameternummer,
                    Version und K-Zahl Modus eine Elster EK-spezifische Parameternummer
                    definiert ist. Falls ja, wird diese f�r die Parameterabfrage
                    verwendet: 28.07.2014, WW }
                  if (FMrgDefData.Kommandogruppe = 101) OR
                     (FMrgDefData.Kommandogruppe = 118) then begin
                    { EK-spezifische Parameternummern-Konfigurationsliste des MRG anlegen und aus
                      Ressourcendatenliste laden, wenn noch nicht erfolgt: 06.08.2021, WW }
                    if not Assigned (FParamEKKonfigList) then begin
                      FParamEKKonfigList:=TParamEKKonfigList.Create;

                      if not Load_MRGResourceData_ParamEK then exit;  // 06.08.2021, WW
                    end;

                    if FParamEKKonfigList.FindParaNrMrg (
                         FMrgKonvData.ParameterGruppe, AllgParaNr,
                         ElsterEK_Version, ElsterEK_K_Zahl_Modus, sParamEK) then begin
                      // EK-spezifische Parameternummer gefunden
                      ParaNrMrg:=sParamEK;
                      if length (ParaNrMrg) = 0 then begin
                        // Parameter nicht verf�gbar, nicht abrufen, OK
                        Result:=true;
                        exit;
                      end;
                    end;
                  end;

                  { Befehl bilden: }
                  Befehl:=GetLIS200Kommando_Lesen (ParaNrMrg);
                  { Parameterabruf-Kommando senden }
                  if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File, R, NoCarrier) then begin
                    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                    exit;
                  end;
                end
                else begin  { virtueller Parameter (Ger�tezustand); 16.08.2012, WW }
                  APBL:=TBefehlList.Create;
                  try
                    { Liste mit Befehlen zum Lesen der einzelnen Ger�tezustands-
                      Parameter zusammenstellen: }
                    FillParameterBefehlListe_Geraetezustand (AllgParaNr, APBL);
                    { In der Liste enthaltene Befehle senden und Daten empfangen: }
                    if not CommObj.SendCommandList (
                      APBL, 0, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File,
                      R, NoCarrier, iDummy) then begin
                      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                      exit;
                    end;
                  finally
                    APBL.Free;
                  end;
                  bLoadGerZustand:=true;
                end;
              end;

              { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
              if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then
                CopyDatei (R.Antwort, Ausgabe_Dateiname);  { Rohdaten-Zieldatei schreiben }

              { Rohfile in Parameterliste konvertieren:
                09.07.2021, WW: Mit �bergabe der allgemeinen und der Elster EK-spezifischen
                                Parameternummern-Konfigurationsliste des MRG }
              if ParameterListe <> nil then
                ParameterListe.LoadFromFile (R.Antwort, ParaKonv, FParamMrgKonfigList,
                                             FParamEKKonfigList)
              else
                DeleteParameterRohfile (R.Antwort);  // 24.08.2023, WW

              { Wert f�r virtuellen Ger�tezustand-Parameter bilden und in
                Parameterliste eintragen: 16.08.2012, WW }
              if bLoadGerZustand AND (ParameterListe <> nil) then  // 14.04.2023, WW
                ParameterListe.LoadGeraeteZustand (FMrgDefData.MrgTyp, AllgParaNr);
            end;

          104:
            begin  { Datacon FWU }
              Befehl:='/?!'+ CR+ LF;  { Befehl zum Abruf allgemeiner Stationsdaten }
              { Befehl senden: }
              if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Parameter, ad_File, R, NoCarrier) then begin
                FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                exit;
              end;

              { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
              if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then
                CopyDatei (R.Antwort, Ausgabe_Dateiname);  { Rohdaten-Zieldatei schreiben }

              { Rohfile in Parameterliste konvertieren:
                09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
              if ParameterListe <> nil then
                ParameterListe.LoadFromFile (R.Antwort, ParaKonv, FParamMrgKonfigList)
              else
                DeleteParameterRohfile (R.Antwort);  // 24.08.2023, WW
          end;

          106, 116:
            begin  { Tritschler VC2, VC3, VCC }
              { -> es wird der Befehl zur Auslesung aktueller Umwerterdaten aufgerufen,
                   da er beim VC2 interessantere Parameter liefert als der IEC-
                   Standardabfrage-Befehl }
              if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
                Befehl:=GetTritschler_IECKommando_05_AktUmwerterDaten (SSU_GeraeteAuswNr,
                  VerbAufbau_Passwort);
                bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
              end
              else begin { ohne Multiplexer SSU }
                Befehl:=GetTritschler_IECKommando_05_AktUmwerterDaten (0, '');
                bWecken:=true;
              end;
              { Befehl senden: }
              if not TMRGCustomCommObj (CommObj).SendTritschlerIECCommand
                (Befehl, ad_File, MRGTimeouts.TritschlerIECProt, FMrgDefData.MrgTyp,
                 bWecken, R, NoCarrier) then begin
                FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                exit;
              end;

              { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
              if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then
                CopyDatei (R.Antwort, Ausgabe_Dateiname);  { Rohdaten-Zieldatei schreiben }

              { Rohfile in Parameterliste konvertieren:
                09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
              if ParameterListe <> nil then
                ParameterListe.LoadFromFile (R.Antwort, ParaKonv, FParamMrgKonfigList)
              else
                DeleteParameterRohfile (R.Antwort);  // 24.08.2023, WW
            end;

          107, 110:
            begin  { Tritschler TTG mit IEC-Protokoll, TDS }
              if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
                Befehl:=GetTritschler_IECKommando_Standardabfrage (SSU_GeraeteAuswNr,
                  VerbAufbau_Passwort);
                bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
              end
              else begin
                Befehl:=GetTritschler_IECKommando_Standardabfrage (0, '');
                bWecken:=true;
              end;
              { Befehl senden: }
              if not TMRGCustomCommObj (CommObj).SendTritschlerIECCommand
                (Befehl, ad_File, MRGTimeouts.TritschlerIECProt, FMrgDefData.MrgTyp,
                 bWecken, R, NoCarrier) then begin
                FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                exit;
              end;

              { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
              if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then
                CopyDatei (R.Antwort, Ausgabe_Dateiname);  { Rohdaten-Zieldatei schreiben }

              { Rohfile in Parameterliste konvertieren:
                09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
              if ParameterListe <> nil then
                ParameterListe.LoadFromFile (R.Antwort, ParaKonv, FParamMrgKonfigList)
              else
                DeleteParameterRohfile (R.Antwort);  // 24.08.2023, WW
            end;

          109:
            begin  { Tritschler TTG mit FTL-Protokoll }
              RFL:=TTextListe.Create;
              try
                // mehrere Parameter-Lesebefehle:
                for i:=0 to FParameterBefehlListe_Alle.Count - 1 do begin
                  Befehl:=TBefehlDataObj (FParameterBefehlListe_Alle [i]).Daten.sBefehl;
                  { Befehl senden: }
                  if not TMRGCustomCommObj (CommObj).SendTritschlerFTLFunktionCommand
                    (Befehl, ad_File, MRGTimeouts.TritschlerFTLProt, -1, R, NoCarrier) then begin
                    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                    exit;
                  end;

                  { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
                  if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then begin
                    if FParameterBefehlListe_Alle.Count > 1 then  // bei mehr als 1 Datei
                      sExt:='_' + IntToStr (i + 1)  // Extension der Zieldateien mit fortlaufender Nummer
                    else
                      sExt:='';
                    CopyDatei (R.Antwort, Ausgabe_Dateiname + sExt);  { Rohdaten-Zieldatei schreiben }
                  end;

                  { Rohfile in Rohfileliste eintragen: }
                  RFL.Add (R.Antwort);
                end;  { for }

                { Rohfiles in Parameterliste konvertieren:
                  09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
                if ParameterListe <> nil then
                  ParameterListe.LoadFromFileList (RFL, ParaKonv, FParamMrgKonfigList)
                else begin
                  for i:=0 to RFL.Count - 1 do
                    DeleteParameterRohfile (RFL [i]);  // 24.08.2023, WW
                end;
              finally
                RFL.Free;
              end;
            end;

          111:
            begin  { Actaris Corus }
              { Flags setzen, es folgen Befehl(e) mit CRC }
              TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_CRC_Corus);
              TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_CRC_Corus);

              { -> Zum Lesen der Corus-Parameter wird SendCommandList
                   verwendet, da als Befehlszusatzinfo die Parameternummer mit�bergeben
                   werden mu� ! }
              if AlleParameter OR Assigned (AllgParaNrListe) then begin
                if Assigned (AllgParaNrListe) then begin
                  APBL:=TBefehlList.Create;
                  try
                    { Befehls-Liste zum Lesen der allgemeinen Parameter zusammenstellen: }
                    FillParameterBefehlListe ('', AllgParaNrListe, APBL);

                    { In der Befehlsliste enthaltene Befehle senden und Daten empfangen: }
                    bOK:=CommObj.SendCommandList (
                      APBL, 0, [ETX, NAK], 1, MRGTimeouts.CorusSAMProt, ad_File,
                      R, NoCarrier, iDummy);
                  finally
                    APBL.Free;
                  end;
                end
                else begin
                  { in der Parameter-Befehlsliste "Alle" enthaltenen Befehle senden und
                    Daten empfangen (die Befehlsliste wird in Init_MRGKonfigData geladen): }
                  bOK:=CommObj.SendCommandList (
                    FParameterBefehlListe_Alle, 0, [ETX, NAK], 1, MRGTimeouts.CorusSAMProt, ad_File,
                    R, NoCarrier, iDummy);
                end;
                if bOK then begin
                  { Hier kein ValidCorusSAMAntwort, Validierung erfolgt in Konvertierungsroutine }

                  { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
                  if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then
                    CopyDatei (R.Antwort, Ausgabe_Dateiname);  { Rohdaten-Zieldatei schreiben }

                  { Rohfile in Parameterliste konvertieren:
                    09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
                  if ParameterListe <> nil then
                    ParameterListe.LoadFromFile (R.Antwort, ParaKonv, FParamMrgKonfigList)
                  else
                    DeleteParameterRohfile (R.Antwort);  // 24.08.2023, WW
                end
                else begin
                  { Bei Timeout Kommunikation mit Actaris Corus neu einleiten (Ger�t
                    schl�ft nach ca. 5 s ein !) und Parameter nochmal abrufen: }
                  if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) AND
                     (iVersuch <= 1) then begin
                    bNochmal:=true;
                    inc (iVersuch);
                    if not Init_Corus_Kommunikation (false) then exit;
                  end
                  else begin
                    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                    exit;
                  end;
                end;
              end
              else begin                  { einzelner Parameter }
                { Befehl bilden (es sind nur Parameternummern von 0..255 erlaubt): }
                Val (ParaNrMrg, iParaNrMrg, Code);
                if (Code = 0) AND (iParaNrMrg >= 0) AND (iParaNrMrg <= MaxByte) then begin
                  APBL:=TBefehlList.Create;
                  try
                    BefehlData.sBefehl:=GetCorusSAMKommando_ParameterLesen (iParaNrMrg);
                    BefehlData.sInfo:=IntToStr (iParaNrMrg) + ';';
                    BefehlDataObj:=TBefehlDataObj.Create (BefehlData);
                    APBL.Add (BefehlDataObj);
                    { Parameterabruf-Kommando senden }
                    if CommObj.SendCommandList (
                      APBL, 0, [ETX, NAK], 1, MRGTimeouts.CorusSAMProt, ad_File,
                      R, NoCarrier, iDummy) then begin
                      { Hier kein ValidCorusSAMAntwort, Validierung erfolgt in Konvertierungsroutine }

                      { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
                      if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then
                        CopyDatei (R.Antwort, Ausgabe_Dateiname);  { Rohdaten-Zieldatei schreiben }

                      { Rohfile in Parameterliste konvertieren:
                        09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
                      if ParameterListe <> nil then
                        ParameterListe.LoadFromFile (R.Antwort, ParaKonv, FParamMrgKonfigList)
                      else
                        DeleteParameterRohfile (R.Antwort);  // 24.08.2023, WW
                    end
                    else begin
                      { Bei Timeout Kommunikation mit Actaris Corus neu einleiten (Ger�t
                        schl�ft nach ca. 5 s ein !) und Parameter nochmal abrufen: }
                      if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) AND
                         (iVersuch <= 1) then begin
                        bNochmal:=true;
                        inc (iVersuch);
                        if not Init_Corus_Kommunikation (false) then exit;
                      end
                      else begin
                        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                        exit;
                      end;
                    end;
                  finally
                    APBL.Free;
                  end;
                end;
              end;
            end;

          112, 117:  { Actaris Sparklog, Kamstrup UNIGAS 300 }
            begin
              if AlleParameter OR Assigned (AllgParaNrListe) then begin
                APBL:=nil;
                try
                  if Assigned (AllgParaNrListe) then begin
                    APBL:=TBefehlList.Create;
                    { Befehls-Liste zum Lesen der allgemeinen Parameter zusammenstellen: }
                    FillParameterBefehlListe ('', AllgParaNrListe, APBL);
                  end;

                  RFL:=TTextListe.Create;
                  try
                    { in der Parameter-Befehlsliste "Alle" bzw. in Befehlsliste mit �bergebenen
                      allg. Parameternummern enthaltene Befehle senden und
                      Daten empfangen (die Befehlsliste "Alle" wird in Init_MRGKonfigData geladen): }
                    StartIndex_PBL:=0;  // es soll mit dem ersten Befehl in der Parameterbefehlsliste begonnen werden
                    AktIndex_PBL_Merk:=-1;  // Defaultwert f�r Merkindex, bei dem zuletzt Timeout aufgetreten ist
                    iIndex_Rohfile:=0;  // Defaultindex f�r Rohfilename
                    while true do begin
                      { Flags setzen, es folgen Befehl(e) mit BCC }
                      TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);
                      TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC);

                      if Assigned (APBL) then
                        bOK:=CommObj.SendCommandList (
                          APBL, StartIndex_PBL, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File,
                          R, NoCarrier, AktIndex_PBL, true)  // Rohfile bei Fehler NICHT l�schen
                      else  // alle Parameter
                        bOK:=CommObj.SendCommandList (
                          FParameterBefehlListe_Alle, StartIndex_PBL, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File,
                          R, NoCarrier, AktIndex_PBL, true);  // Rohfile bei Fehler NICHT l�schen
                      if not bOK then begin
                        { Bei Timeout: Kommunikation mit Ger�t neu einleiten und Parameterabruf
                          mit letztem Parameter-Befehl fortsetzen (Grund ? Actaris Sparklog-Ger�tefehler ! Vorangegangene
                          Ger�teantwort mit BCC = '/' wird wegen Echo auf der CL0-Schnittstelle von Sparklog
                          f�lschlicherweise als neue Kommunikationser�ffnung interpretiert; 29.04.2009, WW }
                        if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) AND
                           (AktIndex_PBL <> AktIndex_PBL_Merk) then begin  // nicht, wenn gleicher Befehl nochmal Timeout liefert !

                          { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
                          if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then begin
                            sExt:='_' + IntToStr (iIndex_Rohfile + 1);  // Extension der Zieldateien mit fortlaufender Nummer
                            CopyDatei (R.Antwort, Ausgabe_Dateiname + sExt);  { Rohdaten-Zieldatei schreiben }
                            inc (iIndex_Rohfile);
                          end;

                          { Rohfile mit allen bislang erfolgreich gelesenen Antworten in Rohfileliste eintragen: }
                          RFL.Add (R.Antwort);

                          { Merkindex, bei dem zuletzt Timeout aufgetreten ist, aktualisieren: }
                          AktIndex_PBL_Merk:=AktIndex_PBL;
                          { mit aktuellem Parameterbefehl (bei dem der Timeout aufgetreten ist) weitermachen: }
                          StartIndex_PBL:=AktIndex_PBL;
                          { Quittungsmodus 1: Schalten in Programmiermode f�r Fortsetzung des Parameterabrufs }
                          if not Init_IEC1107_Kommunikation ('', '1', FMrgDefData.ModemAbrufgruppe,
                                                             false, false, sDummy, vDummy) then exit;
                          if not Send_IEC1107_Passwort (VerbAufbau_Passwort) then exit;
                        end
                        else begin
                          DeleteFile (R.Antwort);  // Rohfile l�schen
                          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                          exit;
                        end;
                      end
                      else begin  // OK
                        { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
                        if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then begin
                          sExt:='_' + IntToStr (iIndex_Rohfile + 1);  // Extension der Zieldateien mit fortlaufender Nummer
                          CopyDatei (R.Antwort, Ausgabe_Dateiname + sExt);  { Rohdaten-Zieldatei schreiben }
                        end;

                        { Rohfile in Rohfileliste eintragen: }
                        RFL.Add (R.Antwort);
                        Break;
                      end;
                    end;  { while true }

                    { Rohfiles in Parameterliste konvertieren:
                      09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
                    if ParameterListe <> nil then
                      ParameterListe.LoadFromFileList (RFL, ParaKonv, FParamMrgKonfigList)
                    else begin
                      for i:=0 to RFL.Count - 1 do
                        DeleteParameterRohfile (RFL [i]);  // 24.08.2023, WW
                    end;
                  finally
                    RFL.Free;
                  end;
                finally
                  APBL.Free;
                end;
              end
              else begin                  { einzelner Parameter }
                { Flags setzen, es folgen Befehl(e) mit BCC }
                TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);
                TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC);

                { Befehl bilden: }
                Befehl:=GetIEC1107Kommando_Lesen (ParaNrMrg);
                { Parameterabruf-Kommando senden }
                if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File, R, NoCarrier) then begin
                  FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                  exit;
                end;

                { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
                if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then
                  CopyDatei (R.Antwort, Ausgabe_Dateiname);  { Rohdaten-Zieldatei schreiben }

                { Rohfile in Parameterliste konvertieren:
                  09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
                if ParameterListe <> nil then
                  ParameterListe.LoadFromFile (R.Antwort, ParaKonv, FParamMrgKonfigList)
                else
                  DeleteParameterRohfile (R.Antwort);  // 24.08.2023, WW
              end;
            end;

          115:  // Elster DS-100
            begin
              { alle in der Parameter-Befehlsliste enthaltene Befehle f�r jeden
                Kanal senden und Daten empfangen (die Befehlsliste wird in
                Init_MRGKonfigData geladen): }
              RFL:=TTextListe.Create;
              try
                { Auf ersten Kanal schalten, wenn erforderlich: }
                if FAktKanal <> 1 then
                  Set_KanalDS100 (1);
                for iAktKanal:=1 to MRGAnzKanaele do begin
                  if not CommObj.SendCommandList (
                    FParameterBefehlListe_Alle, 0, [], 0, MRGTimeouts.ElsterDS100Prot, ad_File,
                    R, NoCarrier, iDummy) then begin
                    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                    exit;
                  end;

                  { Ausgabe der gelesenen Parameter �ber Archivdatentyp gesteuert: 10.08.2011, WW }
                  if ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then begin
                    sExt:='_' + IntToStr (iAktKanal);  // Extension der Zieldateien mit Kanal-Nummer
                    CopyDatei (R.Antwort, Ausgabe_Dateiname + sExt);  { Rohdaten-Zieldatei schreiben }
                  end;

                  { Rohfile in Rohfileliste eintragen:
                    -> Kanalnummer durch Strichpunkt getrennt dem Rohfilenamen
                       voranstellen, damit die Konvertierung wei�, zu welchem Logbuch
                       die Rohdaten geh�ren ! }
                  RFL.Add (IntToStr (iAktKanal) + ';' + R.Antwort);

                  // auf n�chsten Kanal weiterschalten, nicht beim letzten:
                  if (MRGAnzKanaele > 1) AND (iAktKanal < MRGAnzKanaele) then
                    Naechster_KanalDS100;
                end;  { for iAktKanal }

                { Rohfiles in Parameterliste konvertieren:
                  09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
                if ParameterListe <> nil then
                  ParameterListe.LoadFromFileList (RFL, ParaKonv, FParamMrgKonfigList)
                else begin
                  for i:=0 to RFL.Count - 1 do
                    DeleteParameterRohfile (RFL [i]);  // 24.08.2023, WW
                end;
              finally
                RFL.Free;
              end;
            end;

          120,  // Primus, Prilog
          121,  // TME400, RSM200
          122:  // FLOWSIC500
            begin
              SetModbusSlaveData_AdresseByteOrder (FMrgDefData.MrgTyp, SlaveData);

              if AlleParameter AND not Assigned (AllgParaNrListe) then begin  // alle
                // alle Parameter-Register abfragen:
                for i:=0 to FParameterRegisterRequestListe_Alle.Count - 1 do begin
                  RegisterRequestData:=
                    TRegisterRequestDataObj (FParameterRegisterRequestListe_Alle [i]).Data;
                  // Modbus-Request versenden, Response empfangen:
                  if not SendModbusRequest (SlaveData, RegisterRequestData,
                                            MRGTimeouts.ModbusProt,
                                            bDummy, sDummy) then exit;
                end;  { for i }

                { Die in der Register-Requestliste enthaltenen Konvertierungslisten
                  in Parameterliste konvertieren:
                  09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
                if ParameterListe <> nil then
                  ParameterListe.LoadFromModbusRegisterRequestList (
                    FParameterRegisterRequestListe_Alle, ParaKonv, FParamMrgKonfigList);
              end
              else begin                  { einzelner Parameter, Liste }
                RRL:=TRegisterRequestList.Create;
                try
                  bLoadGerZustand:=false;
                  if Assigned (AllgParaNrListe) then
                    { Laden der Modbus-Requests zum Lesen einer Liste von Parametern: }
                    FillMBRegisterRequestListe_Parameter ('', AllgParaNrListe, RRL)  // 29.09.2021, WW
                  else if (length (ParaNrMrg) > 0) then begin
                    { Laden der Modbus-Requests zum Lesen eines einzelnen Parameters: }
                    slParaAllg:=TStringList.Create;  // Temp-Liste f�r allg. Parameternummer
                    try
                      slParaAllg.Add (AllgParaNr);  // 04.04.2024, WW
                      FillMBRegisterRequestListe_Parameter ('', slParaAllg, RRL)  // 29.09.2021, WW
                    finally
                      slParaAllg.Free;
                    end;
                  end
                  else begin  { virtueller Parameter (Ger�tezustand); 04.04.2024, WW }
                    { Laden der Modbus-Requests zum Lesen der einzelnen
                      Ger�tezustands-Parameter: }
                    FillMBRegisterRequestListe_Geraetezustand (AllgParaNr, RRL);
                    bLoadGerZustand:=true;
                  end;

                  // Parameter-Register abfragen:
                  for i:=0 to RRL.Count - 1 do begin
                    RegisterRequestData:=
                      TRegisterRequestDataObj (RRL [i]).Data;
                    // Modbus-Request versenden, Response empfangen:
                    if not SendModbusRequest (SlaveData, RegisterRequestData,
                                              MRGTimeouts.ModbusProt,
                                              bDummy, sDummy) then exit;
                  end;  { for i }

                  if ParameterListe <> nil then begin
                    { Die in der Register-Requestliste enthaltenen Konvertierungslisten
                      in Parameterliste konvertieren:
                      09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG }
                    ParameterListe.LoadFromModbusRegisterRequestList (RRL, ParaKonv,
                                                                      FParamMrgKonfigList);

                    { Wert f�r virtuellen Ger�tezustand-Parameter bilden und in
                      Parameterliste eintragen: 04.04.2024, WW }
                    if bLoadGerZustand then
                      ParameterListe.LoadGeraeteZustand (FMrgDefData.MrgTyp, AllgParaNr);
                  end;
                finally
                  RRL.Free;
                end;
              end;
            end;
        end;  { case }
      end;
    end;  { while bNochmal }

    // Inhalt der Parameterliste in die klasseninterne Parameterliste des Abrufs
    // kopieren:
    if ParameterListe <> nil then begin  // 14.04.2023, WW
      if AlleParameter AND not Assigned (AllgParaNrListe) then begin
        Alle_Parameter_gelesen:=true;
        ParameterListe_MRGAbruf.LoadFromParameterlist (ParameterListe, true);  // Ziel-Liste vorher leeren
      end else                              
        ParameterListe_MRGAbruf.LoadFromParameterlist (ParameterListe, false); // Ziel-Liste updaten
      // Gesetztes Ersatzzeichen-Flag �bernehmen; 17.10.2023, WW
      if ParameterListe.Ersatzzeichen then
        ParameterListe_MRGAbruf.Ersatzzeichen:=ParameterListe.Ersatzzeichen;
    end;
  finally
    { Wenn die gelesenen Parameter in Rohdaten-Zieldatei geschrieben werden, keine
      Standard-Ausgabe in XML-Response: 10.08.2011, WW }
    if (ArchivdatenTyp = C_CmdArchDatenTyp_RohFile) AND (ParameterListe <> nil) then  // 14.04.2023, WW
      Parameterliste.Clear;
  end;
  Result:=true;       
End;

{------------------------------------------------------------------------------}
function TMRGAbruf.AbrufMeldungen (MeldAbrufCmdData: TMessMeldPruefAbrufCmdData;
                                   MeldungsListe: TMeldungsListe): boolean;
{------------------------------------------------------------------------------}
{ Abrufen der Meldungen; MRGAbrufRec [dt_meldungen] wird je nach Modus erzeugt
  �bergabe: Meldungsabruf-Kommandodaten
            Zeiger auf Meldungsliste
  R�ckgabe: Meldungsliste mit konvertierten Rohdaten
  Ergebnis: true, wenn Meldungabruf ok }
Var
  R: TRueckgabe;
  Befehl: string;
  MAR: TAbrufRec;
  Modus: byte;
  MrgAbrufData: TMrgAbrufData;
  MeldKonv: TMeldKonv;
  Kommando_Soll: string;
  bWecken: boolean;
  bNochmal: boolean;
  iVersuch: integer;
  DatenspeicherID: integer;
  RFL: TRohFileListe;
  sDummy: string;
  MrgAbrufKonfigList: TMrgAbrufKonfigList;
  i: integer;
  sExt: string;
  sInfo: string;
  S: string;
  S2: string;
  iAktKanal: integer;
  FNRohKopie: string;
  bOK: boolean;
  vDummy: TVerbAutoDetectData;
  iArchivNr_EichLogB: integer;
  iArchivNr_Aend: integer;
  iArchivNr_LogB: integer;
  bSortieren: boolean;
  ME_vonDZ: TDateTime;
  ME_bisDZ: TDateTime;
  sFileName_Archiv: string;
  ME_Archivtyp: TArchivtyp;
  iRecType: word;
  MBAbrufData_Meld: TMBAbrufData;

Begin
  Result:=false;

  { Konfigurationsdaten f�r Modbusregister-Abruf und -Konvertierung von Meldungen
    lesen: }
  if not Get_MRGResourceData_MBAbruf ('M', MBAbrufData_Meld) then exit;  // 11.02.2022, WW

  { alle f�r die Konvertierung von Meldungen ben�tigten Parameter auslesen; 29.09.2021, WW }
  if not GetParameter_MeldKonv then exit;

  RFL:=TRohFileListe.Create;  { Liste f�r Rohdatendateien }
  try
    bSortieren:=false;  // Default: Abgerufene Meldungen in Meldungsliste nicht
                        // sortieren

    bNochmal:=true;
    iVersuch:=1;
    while bNochmal do begin
      bNochmal:=false;  { Standard: nur einmal }

      { In der Rohfileliste evtl. enthaltene Rohdateien und Liste l�schen: }
      RFL.DeleteRohFiles;
      RFL.Clear;

      FehlerGruppeCodeUpdate (0, 0, true);         { Vorbelegung Fehlergruppe/-code: OK }

      if NoCarrier then begin                  { es besteht keine Verbindung mehr }
        FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
        exit;
      end;

      { Pr�fen, ob lt. MrgDef-Eintrag vom MRG Meldungen abgerufen werden k�nnen: }
      if not FMrgDefData.AbrufMeldung then begin
        FehlerGruppeCodeUpdate (SYS_ABRUFERROR, SYSABRFERR_KOMMANDOUNGUELTIG);
        exit;
      end;

      if MeldAbrufCmdData.vonDZ > 0 then
        Modus:=d_VonBis
      else
        Modus:=d_Alles;

      CreateMeldungenAbrufRec (Modus, MeldAbrufCmdData.vonDZ, MeldAbrufCmdData.bisDZ);

      if FMrgDefData.Kommandogruppe < 100 then begin
        { Kommando standardm��ig tabellengesteuert zusammensetzen und abrufen (Wieser-Ger�te): }
        MrgAbrufKonfigList:=TMrgAbrufKonfigList.Create;
        try
          if not MrgAbrufKonfigList.LoadFromList_ByKommandoTypGruppe ('M',
                   FMrgDefData.Kommandogruppe, FMrgAbrufResourceList) then begin  // 06.08.2021, WW
            FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
            exit;
          end;

          for i:=0 to MrgAbrufKonfigList.Count - 1 do begin
            MrgAbrufData:=TMrgAbrufDataObj (MrgAbrufKonfigList [i]).Data;
            GetMRGKommando_Archiv (MRGAbrufRec [dt_Meldungen], MrgAbrufData.Kommandotyp,
                                   MrgAbrufData.AbrufKommando, MrgAbrufData.AbrufKommando_alle_Daten,
                                   FMrgDefData.MrgTyp, FMrgKanalBitResourceList,  // 06.08.2021, WW
                                   Befehl);

            { Meldungsabruf-Kommando senden }
            if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Meldungen, ad_File, R, NoCarrier) then begin
              { In der Rohfileliste evtl. enthaltene Rohdateien aus
                erfolgreichen Teilabrufen l�schen: }
              RFL.DeleteRohFiles;
              FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
              exit;
            end;

            { Antwort auf Meldungsabruf-Kommando auswerten: }
            if length (Befehl) > 0 then
              Kommando_Soll:=Befehl[2]  // Zeichen nach dem STX
            else
              Kommando_Soll:='';
            if not ValidMRGAntwortfile (Kommando_Soll, FMrgDefData.ModemAbrufgruppe, R.Antwort,
                                        R.Fehlergruppe, R.Fehlercode, RohdatenLoeschen) then begin
              { In der Rohfileliste evtl. enthaltene Rohdateien aus
                erfolgreichen Teilabrufen l�schen: }
              RFL.DeleteRohFiles;
              FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
              exit;
            end;

            RFL.Add (R.Antwort);  { Rohfile in Rohfileliste eintragen }
          end;  { for i }

          if MrgAbrufKonfigList.Count > 1 then
            bSortieren:=true;  // 23.05.2014, WW
        finally
          MrgAbrufKonfigList.Free;
        end;
      end
      else begin
        { Kommando per Quellcode zusammensetzen ohne zugrundeliegende Kommando-Definition aus Tabelle
          (Fremdger�te, deren Kommando-Definitionen nicht in die Tabellenstruktur passen): }
        MAR:=MRGAbrufRec [dt_Meldungen];

        with MAR do begin  // 08.03.2019, WW
          if MAR.AlleDaten then begin
            ME_vonDZ:=MeldAbrufCmdData.vonDZ;
            ME_bisDZ:=MeldAbrufCmdData.bisDZ;
          end
          else begin
            ME_vonDZ:=EncodeDateTime (vonJahr, vonMonat, vonTag, vonStunde, vonMinute, vonSekunde, 0);
            ME_bisDZ:=EncodeDateTime (bisJahr, bisMonat, bisTag, bisStunde, bisMinute, bisSekunde, 0);
          end;
        end;

        case FMrgDefData.Kommandogruppe of
          101: begin    { Elster EK260 }
                 Befehl:=GetLIS200Kommando_Archiv (MAR, 4);            { Archiv 4 }
                 { Befehl senden und Daten empfangen: }
                 bOK:=TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                   [ETX, EOT], ad_File, MRGTimeouts.IEC1107Telegr, cs_BCC, R,
                   NoCarrier, true);
                 { Fehlergruppe/-code aktualisieren: }
                 if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then
                   FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                 if not bOK then exit;

                 RFL.Add (R.Antwort);  { Rohfile in Rohfileliste eintragen }
               end;

          102, 108, 113:
            begin    { Elster DL210, DL220, DL240 }
              Befehl:=GetLIS200Kommando_Archiv (MAR, 10);          { Archiv 10 }
              { Befehl senden und Daten empfangen: }
              bOK:=TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                  [ETX, EOT], ad_File, MRGTimeouts.IEC1107Telegr, cs_BCC, R,
                  NoCarrier, true);
              { Fehlergruppe/-code aktualisieren: }
              if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then
                FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
              if not bOK then exit;

              RFL.Add (R.Antwort);  { Rohfile in Rohfileliste eintragen }
            end;

          106: begin  { Tritschler VC2 }
                 { -> Der VC2 besitzt kein Meldungsarchiv. Ersatzweise wird die Kennziffer F()
                      mit den aktuellen Fehlerzust�nden abgefragt und ausgewertet. F() ist
                      z.B in der Antwort auf den Befehl f�r aktuelle Umwerterdaten enthalten. }
                 if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
                   Befehl:=GetTritschler_IECKommando_05_AktUmwerterDaten (
                     SSU_GeraeteAuswNr, VerbAufbau_Passwort);
                   bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
                 end
                 else begin
                   Befehl:=GetTritschler_IECKommando_05_AktUmwerterDaten (0, '');
                   bWecken:=true;
                 end;
                 { Befehl senden: }
                 if not TMRGCustomCommObj (CommObj).SendTritschlerIECCommand
                   (Befehl, ad_File, MRGTimeouts.TritschlerIECProt, FMrgDefData.MrgTyp,
                    bWecken, R, NoCarrier) then begin
                   FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                   exit;
                 end;

                 RFL.Add (R.Antwort);  { Rohfile mit eichtechnischen Meldungen in Rohfileliste eintragen }
               end;

          110: begin  { Tritschler TDS }
                 if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
                   Befehl:=GetTritschler_IECKommando_26_Logbuecher (MAR,
                     SSU_GeraeteAuswNr, VerbAufbau_Passwort);
                   bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
                 end
                 else begin
                   Befehl:=GetTritschler_IECKommando_26_Logbuecher (MAR, 0, '');
                   bWecken:=true;
                 end;
                 { Befehl senden: }
                 if not TMRGCustomCommObj (CommObj).SendTritschlerIECCommand
                   (Befehl, ad_File, MRGTimeouts.TritschlerIECProt, FMrgDefData.MrgTyp,
                    bWecken, R, NoCarrier) then begin
                   FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                   exit;
                 end;

                 RFL.Add (R.Antwort);  { Rohfile in Rohfileliste eintragen }
               end;

          111: begin  // Actaris Corus
                 { Flags setzen, es folgt Befehl und Antwort mit CRC }
                 TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_CRC_Corus);
                 TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_CRC_Corus);

                 { Es werden das Ereignis-Logbuch (Datenspeicher-ID 4), das
                   Parameter-Logbuch (5) und das eichtechnische Logbuch (6) ausgelesen: }
                 for DatenspeicherID:=4 to 6 do begin
                   Befehl:=GetCorusSAMKommando_DatenspeicherLesen (MAR, DatenspeicherID);
                   { Befehl senden und Daten empfangen: }
                   if not TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                       [ETX], ad_File, MRGTimeouts.CorusSAMProt, cs_CRC_Corus, R,
                       NoCarrier, false) then begin
                     { Bei Timeout Kommunikation mit Actaris Corus neu einleiten (Ger�t
                       schl�ft nach ca. 5 s ein !) und Meldungen nochmal abrufen: }
                     if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) AND
                        (iVersuch <= 1) then begin
                       bNochmal:=true;
                       inc (iVersuch);
                       if not Init_Corus_Kommunikation (false) then begin
                         { In der Rohfileliste evtl. enthaltene Rohdateien aus
                           erfolgreichen Teilabrufen l�schen: }
                         RFL.DeleteRohFiles;
                         exit;
                       end;
                       Break;
                     end
                     else begin
                       { In der Rohfileliste evtl. enthaltene Rohdateien aus
                         erfolgreichen Teilabrufen l�schen: }
                       RFL.DeleteRohFiles;
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                   end;

                   { Rohfile in Rohfileliste eintragen:
                     -> Datenspeicher-ID durch Strichpunkt getrennt dem Rohfilenamen
                        voranstellen, damit die Konvertierung wei�, zu welchem Logbuch
                        die Rohdaten geh�ren ! }
                   RFL.Add (IntToStr (DatenspeicherID) + ';' + R.Antwort);
                 end;  { for DatenspeicherID }

                 bSortieren:=true;  // 23.05.2014, WW
               end;

          112:  begin    { Actaris Sparklog }
                 { Es werden das Betriebslogbuch (Kennzahl P.98) und das Logbuch
                   f�r eichrechtlich relevante Daten (P.99) ausgelesen:
                   -> trial-and-error: erst Eichlogbuch lesen, sonst evtl. Timeout
                      beim Lesen des Eichlogbuchs (Testger�t GV Rhein-Erft) }
                 bNochmal:=true;
                 iVersuch:=1;
                 while bNochmal do begin
                   bNochmal:=false;  { Standard: nur einmal }

                   TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);    { Flag setzen, es folgt Befehl mit BCC }
                   TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC); { Flag setzen, es folgt Antwort mit BCC }
                   { Befehl zum Lesen des Eichlogbuchs: }
                   Befehl:=GetActaris_IEC1107Kommando_Archiv (MAR, 'P.99');

                   { Befehl senden und Daten empfangen: }
                   if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File, R, NoCarrier) then begin  // 21.10.2009, WW
                     { Bei Timeout: Kommunikation mit Ger�t neu einleiten und Abruf des Meldungsbefehls
                       wiederholen (Grund ? Actaris Sparklog-Ger�tefehler ! Vorangegangene Ger�teantwort mit BCC = '/' wird wegen
                       Echo auf der CL0-Schnittstelle von Sparklog f�lschlicherweise als neue Kommunikationser�ffnung
                       interpretiert; 29.04.2009, WW }
                     if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) AND
                        (iVersuch <= 1) then begin
                       bNochmal:=true;
                       inc (iVersuch);
                       { Quittungsmodus 1: Schalten in Programmiermode f�r Fortsetzung des Parameterabrufs }
                       if not Init_IEC1107_Kommunikation ('', '1', FMrgDefData.ModemAbrufgruppe,
                                                          false, false, sDummy, vDummy) then exit;
                       if not Send_IEC1107_Passwort (VerbAufbau_Passwort) then exit;
                     end
                     else begin
                       { In der Rohfileliste evtl. enthaltene Rohdateien aus
                         erfolgreichen Teilabrufen l�schen: }
                       RFL.DeleteRohFiles;
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                   end else
                     RFL.Add (R.Antwort);  { Rohfile in Rohfileliste eintragen }
                 end;  { while bNochmal }

                 bNochmal:=true;
                 iVersuch:=1;
                 while bNochmal do begin
                   bNochmal:=false;  { Standard: nur einmal }

                   TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);    { Flag setzen, es folgt Befehl mit BCC }
                   TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC); { Flag setzen, es folgt Antwort mit BCC }
                   { Befehl zum Lesen des Betriebslogbuchs: }
                   Befehl:=GetActaris_IEC1107Kommando_Archiv (MAR, 'P.98');

                   { Befehl senden und Daten empfangen: }
                   if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File, R, NoCarrier) then begin  // 21.10.2009, WW
                     { Bei Timeout: Kommunikation mit Ger�t neu einleiten und Abruf des Meldungsbefehls
                       wiederholen (Grund ? Actaris Sparklog-Ger�tefehler ! Vorangegangene Ger�teantwort mit BCC = '/' wird wegen
                       Echo auf der CL0-Schnittstelle von Sparklog f�lschlicherweise als neue Kommunikationser�ffnung
                       interpretiert; 29.04.2009, WW }
                     if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) AND
                        (iVersuch <= 1) then begin
                       bNochmal:=true;
                       inc (iVersuch);
                       { Quittungsmodus 1: Schalten in Programmiermode f�r Fortsetzung des Parameterabrufs }
                       if not Init_IEC1107_Kommunikation ('', '1', FMrgDefData.ModemAbrufgruppe,
                                                          false, false, sDummy, vDummy) then exit;
                       if not Send_IEC1107_Passwort (VerbAufbau_Passwort) then exit;
                     end
                     else begin
                       { In der Rohfileliste evtl. enthaltene Rohdateien aus
                         erfolgreichen Teilabrufen l�schen: }
                       RFL.DeleteRohFiles;
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                   end else
                     RFL.Add (R.Antwort);  { Rohfile in Rohfileliste eintragen }
                 end;  { while bNochmal }

                 bSortieren:=true;  // 23.05.2014, WW
               end;

          115: begin    { Elster DS-100 }
                 { Pr�fen, ob DS-100-Datenspeicher f�r geforderten Auslesezeitraum
                   bereits ausgelesen wurde und in Form tempor�rer Rohdatendateien
                   kanalweise vorliegt: }
                 if not (MeldAbrufCmdData.vonDZ < FTempDZ_von) then begin
                   for i:=0 to FTempRohFileListe.Count - 1 do
                     RFL.Add (FTempRohFileListe [i]);  { Tempor�res Rohfile in Rohfileliste eintragen }
                 end
                 else begin  { Datenspeicher wurde f�r Auslesezeitraum noch nicht ausgelesen }
                   { evtl. vorhandene zwischengespeicherte Rohdatendateien l�schen
                     und Liste leeren: }
                   FTempRohFileListe.DeleteRohFiles;
                   FTempRohFileListe.Clear;

                    { Auf ersten Kanal schalten, wenn erforderlich: }
                   if FAktKanal <> 1 then
                     Set_KanalDS100 (1);
                   // f�r alle Kan�le:
                   for iAktKanal:=1 to MRGAnzKanaele do begin
                     { DS-100-Datenspeicher f�r aktuellen Kanal auslesen: }
                     if not DatenAuslesen_DS100 (MAR, R.Antwort) then exit;

                     { Rohfile-Kopie anlegen f�r evtl. folgenden Messwerte-Abruf
                       (Rohdaten enthalten auch Messwertdaten): }
                     FNRohKopie:=ChangeFileExt (R.Antwort, ext_MRG_TempRoh);
                     CopyDatei (R.Antwort, FNRohKopie);
                     { Dateiname der Rohfile-Kopie in tempor�rer Rohfileliste
                       zwischenspeichern: }
                     FNRohKopie:=IntToStr (iAktKanal) + ';' + FNRohKopie;
                     FTempRohFileListe.Add (FNRohKopie);

                     { Kanalnummer durch Strichpunkt getrennt dem Rohfilenamen voranstellen, damit
                       die Konvertierung wei�, zu welchem Kanal die Rohdaten geh�ren ! }
                     R.Antwort:=IntToStr (iAktKanal) + ';' + R.Antwort;
                     RFL.Add (R.Antwort);  { Rohfile in Rohfileliste eintragen }

                     // Bei 4-Kanal-Ger�t auf n�chsten Kanal weiterschalten, nicht beim letzten.
                     // Auch bei 1-Kanal-Ger�t, damit Datenspeicher erneut von
                     // Beginn an ausgelesen werden kann !
                     if ((MRGAnzKanaele > 1) AND (iAktKanal < MRGAnzKanaele)) OR  // 4-Kanal
                        (MRGAnzKanaele = 1) then
                       Naechster_KanalDS100;
                   end;  { for iAktKanal }

                   { Merker setzen mit von-Auslesezeitpunkt: Datenspeicher gelesen }
                   FTempDZ_von:=MeldAbrufCmdData.vonDZ;
                 end;
               end;

          116: begin  { Tritschler VC3, VCC }
                 { Nicht eichtechnisches Logbuch auslesen: }
                 if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
                   Befehl:=GetTritschler_IECKommando_27_Logbuecher_nicht_eichtechnisch (MAR,
                     SSU_GeraeteAuswNr, VerbAufbau_Passwort);
                   bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
                 end
                 else begin
                   Befehl:=GetTritschler_IECKommando_27_Logbuecher_nicht_eichtechnisch (MAR, 0, '');
                   bWecken:=true;
                 end;
                 { Befehl senden: }
                 if not TMRGCustomCommObj (CommObj).SendTritschlerIECCommand
                   (Befehl, ad_File, MRGTimeouts.TritschlerIECProt, FMrgDefData.MrgTyp,
                    bWecken, R, NoCarrier) then begin
                   FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                   exit;
                 end;
                 S:=R.Antwort;  { Rohfile merken }

                 { Eichtechnisches Logbuch auslesen: }
                 if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
                   Befehl:=GetTritschler_IECKommando_28_Logbuecher_eichtechnisch (MAR,
                     SSU_GeraeteAuswNr, VerbAufbau_Passwort);
                   bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
                 end
                 else begin
                   Befehl:=GetTritschler_IECKommando_28_Logbuecher_eichtechnisch (MAR, 0, '');
                   bWecken:=true;
                 end;
                 { Befehl senden: }
                 if not TMRGCustomCommObj (CommObj).SendTritschlerIECCommand
                   (Befehl, ad_File, MRGTimeouts.TritschlerIECProt, FMrgDefData.MrgTyp,
                    bWecken, R, NoCarrier) then begin
                   FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                   exit;
                 end;

                 RFL.Add (S);  { Rohfile mit nicht eichtechnischen Meldungen in Rohfileliste eintragen }
                 RFL.Add (R.Antwort);  { Rohfile mit eichtechnischen Meldungen in Rohfileliste eintragen }

                 bSortieren:=true;  // 23.05.2014, WW
               end;

          117: begin    { Kamstrup UNIGAS 300 }
                 TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);    { Flag setzen, es folgt Befehl mit BCC }
                 TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC); { Flag setzen, es folgt Antwort mit BCC }
                 { Befehl zum Lesen des Eichlogbuchs: }
                 Befehl:=GetKamstrup_IEC1107Kommando_Logbuch ('P.99');

                 { Befehl senden und Daten empfangen: }
                 if not TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                   [ETX, EOT], ad_File, MRGTimeouts.IEC1107Telegr, cs_BCC, R,
                   NoCarrier, false) then begin
                   FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                   exit;
                 end;
                 S:=R.Antwort;  { Rohfile merken }

                 { Befehl zum Lesen des Betriebslogbuchs: }
                 Befehl:=GetKamstrup_IEC1107Kommando_Logbuch ('P.98');

                 { Befehl senden und Daten empfangen: }
                 if not TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                   [ETX, EOT], ad_File, MRGTimeouts.IEC1107Telegr, cs_BCC, R,
                   NoCarrier, false) then begin
                   FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                   exit;
                 end;

                 RFL.Add (S);  { Rohfile mit Eichlogbuch-Meldungen in Rohfileliste eintragen }
                 RFL.Add (R.Antwort);  { Rohfile mit Betriebslogbuch-Meldungen in Rohfileliste eintragen }

                 bSortieren:=true;  // 23.05.2014, WW
               end;

          118, 119:
               begin    { Elster EK280, DL230 }
                 if FMrgDefData.Kommandogruppe = 118 then begin  // EK280
                   iArchivNr_EichLogB:=9;  // Archiv 9
                   iArchivNr_Aend:=5;  // Archiv 5
                   iArchivNr_LogB:=4;  // Archiv 4
                 end
                 else begin  // DL230
                   iArchivNr_EichLogB:=12;  // Archiv 12
                   iArchivNr_Aend:=11;  // Archiv 11
                   iArchivNr_LogB:=10;  // Archiv 10
                 end;

                 { Befehl zum Lesen des eichtechnischen Logbuchs: }
                 Befehl:=GetLIS200Kommando_Archiv (MAR, iArchivNr_EichLogB);
                 { Befehl senden und Daten empfangen: }
                 bOK:=TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                   [ETX, EOT], ad_File, MRGTimeouts.IEC1107Telegr, cs_BCC, R,
                   NoCarrier, true);
                 { Fehlergruppe/-code aktualisieren: }
                 if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then
                   FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                 if not bOK then exit;
                 S:=R.Antwort;  { Rohfile merken }

                 { Befehl zum Lesen des �nderungsarchivs: }
                 Befehl:=GetLIS200Kommando_Archiv (MAR, iArchivNr_Aend);
                 { Befehl senden und Daten empfangen: }
                 bOK:=TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                   [ETX, EOT], ad_File, MRGTimeouts.IEC1107Telegr, cs_BCC, R,
                   NoCarrier, true);
                 { Fehlergruppe/-code aktualisieren: }
                 if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then
                   FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                 if not bOK then exit;
                 S2:=R.Antwort;  { Rohfile merken }

                 { Befehl zum Lesen des Logbuchs: }
                 Befehl:=GetLIS200Kommando_Archiv (MAR, iArchivNr_LogB);
                 { Befehl senden und Daten empfangen: }
                 bOK:=TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                   [ETX, EOT], ad_File, MRGTimeouts.IEC1107Telegr, cs_BCC, R,
                   NoCarrier, true);
                 { Fehlergruppe/-code aktualisieren: }
                 if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then
                   FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                 if not bOK then exit;

                 { Rohfile mit Eichlogbuch-Meldungen in Rohfileliste eintragen:
                     -> Meldungsarchiv-Kennzeichen durch Strichpunkt getrennt dem
                        Rohfilenamen voranstellen, damit die Konvertierung wei�,
                        da� die Rohdaten zum eichtechnischen Logbuch geh�ren ! }
                 RFL.Add (CMeldNr_EichParameterVeraendert + ';' + S);
                 { Rohfile mit �nderungsarchiv-Meldungen in Rohfileliste eintragen:
                     -> Meldungsarchiv-Kennzeichen durch Strichpunkt getrennt dem
                        Rohfilenamen voranstellen, damit die Konvertierung wei�,
                        da� die Rohdaten zum �nderungs-Archiv geh�ren ! }
                 RFL.Add (CMeldNr_ParameterVeraendert + ';' + S2);
                 { Rohfile mit Logbuch-Meldungen in Rohfileliste eintragen }
                 RFL.Add (R.Antwort);

                 bSortieren:=true;  // 23.05.2014, WW
               end;

          120:
            begin  { Primus, Prilog }
              // Status-Archiv per Modbus abrufen (Meldungen):
              if not ReadModbusArchiv_PrimusPrilog (FMrgDefData.MrgTyp,
                                                    at_Ereignisarchiv, MAR,
                                                    ME_vonDZ, ME_bisDZ,
                                                    MBAbrufData_Meld,
                                                    sFilename_Archiv) then exit;
              RFL.Add (sFileName_Archiv);
            end;

          121:
            begin  { TME400, RSM200 }
              { Es werden das Ereignisarchiv (Meldungen), das eichamtliche und das
                nicht-eichamtliche Parameterarchiv (Parameter�nderungen) ausgelesen: }
              for i:=1 to 3 do begin
                case i of
                  1: ME_Archivtyp:=at_Ereignisarchiv;
                  2: ME_Archivtyp:=at_Parameterarchiv_eichamtlich;
                  3: ME_Archivtyp:=at_Parameterarchiv_nichteichamtlich;
                else
                   ME_Archivtyp:=at_Parameterarchiv_nichteichamtlich;
                end;

                // Archiv per Modbus abrufen:
                if not ReadModbusArchiv_TME400 (FMrgDefData.MrgTyp,
                                                ME_Archivtyp, MAR,
                                                ME_vonDZ, ME_bisDZ,
                                                sFilename_Archiv) then begin
                  if (Fehlergruppe <> ST_DATACHECK) then begin  // nicht bei ung�ltigen Archivheadern
                    { In der Rohfileliste evtl. enthaltene Rohdateien aus
                      erfolgreichen Teilabrufen l�schen: }
                    RFL.DeleteRohFiles;
                    exit;
                  end;
                end
                else begin
                  { Rohfile in Rohfileliste eintragen:
                    -> Archivtyp-ID durch Strichpunkt getrennt dem Rohfilenamen
                       voranstellen, damit die Konvertierung wei�, zu welchem Archiv
                       die Rohdaten geh�ren ! }
                  RFL.Add (IntToStr (integer (ME_Archivtyp)) + ';' + sFileName_Archiv);
                end;
              end;  // for i

              bSortieren:=true;  // 23.05.2014, WW
            end;

          122:
            begin  { FLOWSIC500 }
              { Es werden das Ereignislogbuch (Meldungen) sowie das Parameterlogbuch,
                eichtechnische und das Gasparameter-Logbuch (jeweils Parameter-
                �nderungen) ausgelesen: }
              for i:=1 to 4 do begin
                case i of
                  1: ME_Archivtyp:=at_Ereignisarchiv;
                  2: ME_Archivtyp:=at_Parameterarchiv_nichteichamtlich;
                  3: ME_Archivtyp:=at_Parameterarchiv_eichamtlich;
                  4: ME_Archivtyp:=at_Parameterarchiv_Gas;
                else
                   ME_Archivtyp:=at_Parameterarchiv_Gas;
                end;

                // Logbuch per Modbus abrufen:
                if not ReadModbusArchiv_SICK (ME_Archivtyp, MAR,
                                              ME_vonDZ, ME_bisDZ,
                                              sFilename_Archiv, iRecType) then begin
                  if (Fehlergruppe <> ST_DATACHECK) then begin  // nicht bei ung�ltigen Downloadpuffer-Rohdaten
                    { In der Rohfileliste evtl. enthaltene Rohdateien aus
                      erfolgreichen Teilabrufen l�schen: }
                    RFL.DeleteRohFiles;
                    exit;
                  end;
                end
                else begin
                  { Rohfile in Rohfileliste eintragen:
                    -> Datensatz-Typnummer durch Strichpunkt getrennt dem Rohfilenamen
                       voranstellen, damit die Konvertierung wei�, welche Struktur
                       die Rohdaten haben ! }
                  RFL.Add (IntToStr (iRecType) + ';' + sFileName_Archiv);
                end;
              end;  // for i

              bSortieren:=true;  // 23.05.2014, WW
            end;
        end;  { case FMrgDefData.Kommandogruppe }
      end;  { if FMrgDefData.Kommandogruppe }
    end;  { while bNochmal }

    { Meldungen korrekt gelesen, jetzt Rohfile in Meldungsliste konvertieren }
    if (MeldungsListe <> nil) then begin
      { Wenn die gelesenen Meldungen in Rohdaten-Zieldatei(en) geschrieben werden,
        keine Standard-Ausgabe in XML-Response: 10.08.2011, WW }
      if MeldAbrufCmdData.ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then begin
        for i:=0 to RFL.Count - 1 do begin
          S:=RFL [i];
          { Pr�fen, ob dem Antwort-Filenamen eine Zusatzinformation vorangestellt ist: }
          if Pos (';', S) > 0 then begin
            sInfo:=F_Zerlegen (S, ';');
            sExt:='_' + sInfo;  // Extension der Zieldateien mit Zusatzinfo
          end
          else if RFL.Count > 1 then
            sExt:='_' + IntToStr (i)  // Extension der Zieldateien mit fortlaufender Nummer
          else
            sExt:='';
          CopyDatei (S, MeldAbrufCmdData.Dateiname + sExt);  { Rohdaten-Zieldatei schreiben }
        end;  { for i }

        RFL.DeleteRohFiles;  // Rohdaten l�schen
      end
      else begin
        { �bergaberecord f�r Meldungskonvertierung zusammenstellen: }
        with MeldKonv do begin
          MeldKonvGruppe:=FMrgKonvData.MeldKonvGruppe;
          MNrParaStart:=FMrgKonvData.MNrParaStart;
          MNrParameter:=FMrgKonvData.MNrParameter;
          MeldungsGruppe:=FMrgKonvData.MeldungsGruppe;
          ParameterGruppe:=FMrgKonvData.ParameterGruppe;
          ParameterUnterGruppe:=FMB_ID;  // 11.02.2022, WW
          MeldGeraeteart:=FMrgKonvData.MeldGeraeteart;
          RohLoeschen:=RohdatenLoeschen;
          MrgTyp:=FMrgDefData.MrgTyp;  // f�r Modbus-Daten (Primus/Prilog, TME400, FLOWSIC500, RSM200); 08.03.2019, WW
          ParameterListe:=ParameterListe_MRGAbruf;  // 29.09.2021, WW
          MBAbrufData:=MBAbrufData_Meld;  // f�r Modbus-Daten; 11.02.2022, WW
        end;

        { Konfigurationsliste mit meldungsspezifischen Parameternummern des MRG anlegen
          und aus Ressourcedatenliste laden, wenn noch nicht erfolgt: 06.08.2021, WW }
        if not Assigned (FParamMeldKonfigList) then begin
          FParamMeldKonfigList:=TParamMeldKonfigList.Create;

          if not Load_MRGResourceData_ParamMeld then exit;  // 06.08.2021, WW
        end;
        
        { Meldungsnummern-Konfigurationsliste des MRG anlegen und aus
          Ressourcedatenliste laden, wenn noch nicht erfolgt: 06.08.2021, WW }
        if not Assigned (FMeldNrKonfigList) then begin
          FMeldNrKonfigList:=TMeldNrKonfigList.Create;

          if not Load_MRGResourceData_MeldNr (MeldKonv.MeldGeraeteart,
                   MeldKonv.Meldungsgruppe) then exit;  // 06.08.2021, WW
        end;

        { Rohfiles in Meldungsliste konvertieren:
          09.07.2021, WW: Mit �bergabe der Parameternummern-Konfigurationsliste des MRG
          06.08.2021, WW: Mit �bergabe der Konfigurationsliste mit meldungsspezifischen
                          Parameternummern des MRG }
        MeldungsListe.LoadFromFileList (RFL, MeldKonv, bSortieren, FParamMrgKonfigList,
                                        FParamMeldKonfigList, FMeldNrKonfigList);
        { bei ung�ltigen Meldungsrohdaten: Fehlergruppe,-code setzen }
        if not MeldungsListe.DataValid then begin
          FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_MEINVALID);
          exit;
        end;
      end;
    end;  // if (MeldungsListe <> nil)
  finally
    RFL.Free;
  end;

  Result:=true;
End;

{---------------------------------------------------------------------------------}
Function TMRGAbruf.AbrufMessTagWerte (MessAbrufCmdData: TMessMeldPruefAbrufCmdData;
                                      StaKanalKonvDataList: TStaKanalKonvDataList;
                                      AnalogOrig_normiert: boolean;
                                      var iTagesende: integer;
                                      var KonvDateiname_MessList: TStringList;
                                      var KonvDateiname_TagList: TStringList;
                                      var iMaxMessKanal: integer;
                                      var iMinMessKanalExt: integer;
                                      var iMaxMessKanalExt: integer): boolean;
{---------------------------------------------------------------------------------}
{ Abrufen der Stundens�tze und Tagess�tze; MRGAbrufRec [dt_messwerte] wird je nach Modus erzeugt
  �bergabe: Messwertabruf-Kommandodaten
            Liste mit Sta-Kanaldaten
            Schalter f�r R�ckgabe von normierten Analog-Messwerten
  R�ckgaben: Tagesende (prozedurintern f�r Konvertierung und Abrufzeitpunkt-Korrektur)
             Listen mit Namen der von der Konvertierung erzeugten Dateien mit
               Messwerten und Tagess�tzen
             Anzahl der aus den abgerufenen Rohdaten konvertierten Messwert-
               Kan�le (-1, wenn unbekannt)
             Niedrigste und h�chste Kanalnummer des aus den abgerufenen Rohdaten
               konvertierten, erweiterten Messwert-Archivs (-1, wenn unbekannt)
  Ergebnis: true, wenn Stundenwerte- und Tagess�tze-Abruf ok }

  {-----------------------------------------------------------------------------}
  procedure Ausgabe_Messwerte (sAntwortFilename: string; iIndex_Rohfile: integer;
    MRGKonvert: TMRGKonvert);
  {-----------------------------------------------------------------------------}
  { Ausgabe der gelesenen Stundenwerte �ber Archivdatentyp gesteuert;
    �bergaben: Antwort-Filename
               Index f�r Zieldatei
               Zeiger auf MRGKonvert-Objekt }
  var
    S: string;
    sInfo: string;
    sExt: string;

  begin
    if MessAbrufCmdData.ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then begin
      S:=sAntwortFilename;
      { Pr�fen, ob dem Antwort-Filenamen eine Zusatzinformation vorangestellt ist: }
      if Pos (';', S) > 0 then begin
        sInfo:=F_Zerlegen (S, ';');
        sExt:='_' + sInfo;  // Extension der Zieldateien mit Zusatzinfo
      end else
        sExt:='_' + IntToStr (iIndex_Rohfile);  // Extension der Zieldateien mit fortlaufender Nummer
      CopyDatei (S, MessAbrufCmdData.Dateiname + sExt);  { Rohdaten-Zieldatei schreiben }
      DeleteFile (S);
    end
    else if Assigned (MRGKonvert) then
      MRGKonvert.AddFileName (sAntwortFilename, dt_Messwerte);
  end;


Var
  R: TRueckgabe;
  Befehl: string;
  MAR: TAbrufRec;
  ArchivNr: integer;
  Modus: byte;
  WeitererAbruf: boolean;
  MRGKonvert: TMRGKonvert;
  MrgAbrufData: TMrgAbrufData;
  MW_vonDZ: TDateTime;
  MW_bisDZ: TDateTime;
  KennzifferStr: string;
  vonStundeTmp: word;
  vonJahrTmp, bisJahrTmp: word;
  wdummy: word;
  sdummy: string;
  Kommando_Soll: string;
  bWecken: boolean;
  dtBuf: TDateTime;
  iVersuch: integer;
  sKanalAdresse: string;
  sRohfilename_ZS: string;
  AFehlergruppe: integer;
  AFehlercode: integer;
  iStundenProSMS: integer;
  MrgAbrufKonfigList: TMrgAbrufKonfigList;
  iIndex_Rohfile: integer;
  sExt: string;
  sInfo: string;
  S: string;
  FNRohKopie: string;
  bOK: boolean;
  vDummy: TVerbAutoDetectData;
  sFileName_Archiv: string;
  iRecType: word;
  MBAbrufData_Mess: TMBAbrufData;
  AI: TAbrufSrvIni;
  sArchivExt: string;

Begin
  Result:=false;
  // Vorbelegungen: Kanalzahlen aus Konvertierung unbekannt
  iMaxMessKanal:=-1;
  iMinMessKanalExt:=-1;
  iMaxMessKanalExt:=-1;

  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }

  { Pr�fen, ob lt. MrgDef-Eintrag vom MRG Messwerte abgerufen werden k�nnen: }
  if FMrgDefData.Kommandogruppe <= 0 then begin
    FehlerGruppeCodeUpdate (SYS_ABRUFERROR, SYSABRFERR_KOMMANDOUNGUELTIG);
    exit;
  end;

  { Konfigurationsdaten f�r Modbusregister-Abruf und -Konvertierung von Messwerten
    lesen: }
  if not Get_MRGResourceData_MBAbruf ('E', MBAbrufData_Mess) then exit;  // 11.02.2022, WW

  if MessAbrufCmdData.vonDZ > 0 then
    Modus:=d_VonBis
  else
    Modus:=d_Alles;

  MW_vonDZ:=MessAbrufCmdData.vonDZ;
  MW_bisDZ:=MessAbrufCmdData.bisDZ;

  case FMrgDefData.Kommandogruppe of
    114: begin  // Veraut Veribox-Mini
           if Modus = d_Alles then begin
             MW_vonDZ:=EncodeDateTime (2000, 1, 1, 0, 0, 0, 0);  { fr�hestm�glicher Zeitpunkt bei Veribox }
             MW_bisDZ:=Now;
           end;

           { Hochrechnung: Max. m�gliche Anzahl der Messperiodenwerte je Daten-SMS
             Annahme: MP = 60 min, Kan�le in Antwort-SMS wie aktive Kan�le in Kanalmaske }
           iStundenProSMS:=Get_Veribox_MPSaetzeProSMS (60, MessAbrufCmdData.KanalaktivMaske,
                                                       FMrgDefData.AnzahlKanaele);

           { Mehrere SMS mit Teil-Zeitbereichen versendet, da evtl. nicht alle
             angeforderten Messwerte in 1 Antwort-SMS Platz haben: }
           while CmpDateTime (MW_vonDZ, MW_bisDZ) <= 0 do begin
             { Korrektur des von-Abrufzeitpunkts: }
             { Veribox liefert Daten, welche �lter als der angeforderte von-Abrufpunkt sind: }
             dtBuf:=IncSecond (MW_vonDZ, -1);  { eine Sekunde fr�her }
             { 1 Stunde mehr abrufen, da die Stundenmengen aus Z�hlerstandsdifferenzen
               gebildet werden und sonst der erste Wert in der ersten Antwort-SMS fehlend w�re: }
             dtBuf:=IncHour (dtBuf, -1);

             Befehl:=GetVeriboxMini_SMSKommando_Daten (dtBuf);

             { Befehl mit SMS versenden: }
             if not GSM_Send_SMS (TMRGCustomCommObj (CommObj), MRGTimeouts.GSMModem,
                                  VerbAufbau_Rufnummer, Befehl,
                                  AFehlergruppe, AFehlercode) then begin
               FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
               exit;
             end;

             // Es m�ssen mind. 2 S�tze enthalten sein, sonst kann allein aus den
             // SMS-Antwort-Rohdaten keine ZS-Differenz gebildet werden:
             if iStundenProSMS > 1 then
               MW_vonDZ:=IncHour (MW_vonDZ, iStundenProSMS - 1)
             else
               Break;
           end;
         end;

  else  { Standard: Kommandogruppen f�r aktive Abrufe bei bestehender Verbindung }
    { alle f�r die Konvertierung von Messwerten/Tages�tzen ben�tigten Parameter auslesen: }
    if not GetParameter_MessTagKonv (MessAbrufCmdData.ArchivdatenTyp) then exit;

    { im Ger�t eingestelltes Tagesende ermitteln: }
    if not GetTagesende (iTagesende) then exit;

    { jetzt folgt der eigentliche Messwert-Abruf: }
    MRGKonvert:=TMRGKonvert.Create (FMrgDefData.MrgTyp, RohdatenLoeschen);
    try
      try
        {-------------------- Stundenwerte abrufen --------------------------------}

        { Korrektur des von-Abrufzeitpunkts f�r Messwerte bei von-Stunde zwischen 0..Tagesende:
          -> damit f�r alle Ger�tetypen der angeforderte Zeitbereich sicher abgerufen wird !
          -> Ab 22.07.2019, WW: Nur noch f�r die Wieser-Ger�te (f�r die die Korrektur
             ursp�nglich erforderlich war (MRG 2xxx, 3xxx) bzw. sich die Abrufzeit
             nicht merklich verl�ngert (MRG 900, EC900) }
        if Modus = d_VonBis then begin
          if FMrgDefData.Kommandogruppe < 100 then begin  // 22.07.2019, WW
            DecodeTime (MW_vonDZ, vonStundeTmp, wdummy, wdummy, wdummy);
            if vonStundeTmp <= iTagesEnde then
              MW_vonDZ:=MW_vonDZ - 1;  { einen Tag fr�her }
          end;
        end;

        WeitererAbruf:=true;
        iVersuch:=1;
        iIndex_Rohfile:=0;
        while WeitererAbruf do begin
          inc (iIndex_Rohfile);

          if NoCarrier then begin              { es besteht keine Verbindung mehr }
            FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
            exit;
          end;

          if isDSfG_Slave then begin
            { bei DSfG-Slaves werden die Stundenwerte in 1-Tagesbl�cken geholt:
              -> Anm.: Die E-Befehlsyntax der Ger�tetypen mit DSfG-Umleitung beinhaltet
                       nur Datumsinformation, keine Zeitinformation. Daher kann bei der
                       Aufteilung des Abrufs in 1-Tagesbl�cken die von/bis-Zeit
                       unber�cksichtigt bleiben. }
            MW_bisDZ:=MW_vonDZ;           { nur 1 Tag holen }
            CreateMesswerteAbrufRec (Modus, MW_vonDZ, MW_bisDZ, MessAbrufCmdData.KanalaktivMaske);
            MW_vonDZ:=MW_vonDZ + 1;       { f�r Folgeabruf: n�chster Tag }
            WeitererAbruf:=Trunc (MW_vonDZ) <= Trunc (MessAbrufCmdData.bisDZ);
          end
          else begin
            DecodeDate (MW_vonDZ, vonJahrTmp, wdummy, wdummy);
            DecodeDate (MW_bisDZ, bisJahrTmp, wdummy, wdummy);
            { MRG, welche �ber einen Jahreswechsel abgerufen werden sollen, dies aber
              nicht unterst�tzen: 2 Teilabrufe (bis Jahresende und ab dem neuen Jahr) }
            if not FMrgDefData.Jahreswechsel AND (vonJahrTmp < bisJahrTmp) then begin
              MW_bisDZ:=EncodeDate (vonJahrTmp, 12, 31) + EncodeTime (23, 59, 59, 0);  { bis Jahresende }
              CreateMesswerteAbrufRec (Modus, MW_vonDZ, MW_bisDZ, MessAbrufCmdData.KanalaktivMaske);
              { f�r Folgeabruf: ab Beginn des Folgejahres bis zum angeforderten bis-Zeitpunkt }
              MW_vonDZ:=EncodeDate (bisJahrTmp, 1, 1);
              MW_bisDZ:=MessAbrufCmdData.bisDZ;
              WeitererAbruf:=true;
            end
            else begin
              CreateMesswerteAbrufRec (Modus, MW_vonDZ, MW_bisDZ, MessAbrufCmdData.KanalaktivMaske);
              WeitererAbruf:=false;
            end;
          end;

          if FMrgDefData.Kommandogruppe < 100 then begin
            { Kommando standardm��ig tabellengesteuert zusammensetzen und abrufen (Wieser-Ger�te): }
            MrgAbrufKonfigList:=TMrgAbrufKonfigList.Create;
            try
              if not MrgAbrufKonfigList.LoadFromList_ByKommandoTypGruppe ('E',
                       FMrgDefData.Kommandogruppe, FMrgAbrufResourceList) then begin  // 06.08.2021, WW
                FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
                exit;
              end;
              if MrgAbrufKonfigList.Count = 0 then begin
                FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
                exit;
              end;

              MrgAbrufData:=TMrgAbrufDataObj (MrgAbrufKonfigList [0]).Data;
              GetMRGKommando_Archiv (MRGAbrufRec [dt_Messwerte], MrgAbrufData.Kommandotyp,
                                     MrgAbrufData.AbrufKommando, MrgAbrufData.AbrufKommando_alle_Daten,
                                     FMrgDefData.MrgTyp, FMrgKanalBitResourceList,  // 06.08.2021, WW
                                     Befehl);

              { Me�wertabruf-Kommando senden }
              if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Messwerte, ad_File, R, NoCarrier) then begin
                FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                exit;
              end;

              { Antwort auf Me�wertabruf-Kommando auswerten: }
              if length (Befehl) > 0 then
                Kommando_Soll:=Befehl[2]  // Zeichen nach dem STX
              else
                Kommando_Soll:='';
              if not ValidMRGAntwortfile (Kommando_Soll, FMrgDefData.ModemAbrufgruppe, R.Antwort,
                                          R.Fehlergruppe, R.Fehlercode, RohdatenLoeschen) then begin
                FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                exit;
              end;
            finally
              MrgAbrufKonfigList.Free;
            end;
          end
          else begin
            { bei Ger�ten, bei denen f�r jeden Kanal ein eigener Abrufbefehl n�tig ist, wird
              auf den n�chsten Kanal weitergeschaltet: }
            WeitererAbruf:=SetNextStdAbrufKanal;

            { Kommando per Quellcode zusammensetzen ohne zugrundeliegende Kommando-Definition aus Tabelle
              (Fremdger�te, deren Kommando-Definitionen nicht in die Tabellenstruktur passen): }
            MAR:=MRGAbrufRec [dt_Messwerte];
            case FMrgDefData.Kommandogruppe of
              101, 118:
                   begin    { Elster EK260, EK280 }
                     Befehl:=GetLIS200Kommando_Archiv (MAR, 3);    { Archiv 3 }
                     { Befehl senden und Daten empfangen: }
                     bOK:=TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                       [ETX, EOT], ad_File, MRGTimeouts.IEC1107Telegr, cs_BCC, R,
                       NoCarrier, true);
                     { Fehlergruppe/-code aktualisieren: }
                     if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                     if not bOK then exit;

                     { Nur EK280: Konfiguration f�r ger�tetypabh�ngige, erweiterte
                       Abfrage von Archiv 13 aus Programm-Ini lesen: 24.08.2023, WW }
                     if (FMrgDefData.Kommandogruppe = 118) AND
                        (length (FMrgDefData.MrgName) > 0) then begin
                       AI:=TAbrufSrvIni.Create;
                       try
                         sArchivExt:=AI.MRG_ArchivExt [FMrgDefData.MrgName, 13];
                       finally
                         AI.Free;
                       end;

                       if length (sArchivExt) > 0 then begin
                         { Ausgabe der gelesenen Stundenwerte von Archiv 3: }
                         Ausgabe_Messwerte (R.Antwort, iIndex_Rohfile, MRGKonvert);

                         Befehl:=GetLIS200Kommando_Archiv (MAR, 13);  { Archiv 13 }
                         { Befehl senden und Daten empfangen: }
                         bOK:=TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                           [ETX, EOT], ad_File, MRGTimeouts.IEC1107Telegr, cs_BCC, R,
                           NoCarrier, true);
                         { Fehlergruppe/-code aktualisieren: }
                         if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then
                           FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                         if not bOK then exit;

                         { Kanalnummern der INI-Konfiguration durch Strichpunkt getrennt
                           dem Rohfilenamen voranstellen, damit die Konvertierung wei�,
                           welche Kan�le aus den Rohdaten konvertiert werden sollen ! }
                         R.Antwort:=sArchivExt + ';' + R.Antwort;
                       end;
                     end;
                   end;

              102, 108, 113, 119:
                   begin    { Elster DL210, DL220, DL230, DL240 }
                     { Befehl bilden: es ist f�r jeden Kanal ein eigener Abruf n�tig ! }
                     ArchivNr:=StdKanalNr * 2; { Kan�le 1, 2, 3, 4 -> Archive 2, 4, 6, 8 }
                     Befehl:=GetLIS200Kommando_Archiv (MAR, ArchivNr);
                     { Befehl senden und Daten empfangen: }
                     bOK:=TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                         [ETX, EOT], ad_File, MRGTimeouts.IEC1107Telegr, cs_BCC, R,
                         NoCarrier, true);
                     { Fehlergruppe/-code aktualisieren: }
                     if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                     if not bOK then exit;

                     { Kanalnummer durch Strichpunkt getrennt dem Rohfilenamen voranstellen, damit
                       die Konvertierung wei�, zu welchem Kanal die Rohdaten geh�ren ! }
                     R.Antwort:=IntToStr (StdKanalNr) + ';' + R.Antwort;
                   end;

              103: begin  { KE-Anlage (Ruhrgas-Typ) }
                     { Befehl f�r von-Auslese-Zeitpunkt bilden: }
                     Befehl:=GetKE_Kommando_Zeitbereich_von (MAR);
                     { Befehl senden: }
                     if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.ACK01_ProtMeldung, ad_String, R, NoCarrier) then begin
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                     { Antwort wird nicht ausgewertet }

                     { Befehl f�r bis-Auslese-Zeitpunkt bilden: }
                     Befehl:=GetKE_Kommando_Zeitbereich_bis (MAR);
                     { Befehl senden: }
                     if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.ACK01_ProtMeldung, ad_String, R, NoCarrier) then begin
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                     { Antwort wird nicht ausgewertet }

                     { Befehl zum Auslesen der Daten bilden: }
                     Befehl:=GetKE_Kommando ('S026');   { Auslesen Ruhrgas-Typ }
                     { Befehl senden und Daten empfangen: }
                     if not TMRGCustomCommObj (CommObj).SendKEDatentelegramm_S (Befehl, ad_File,
                                                                                MRGTimeouts.ACK01_ProtMeldung, R, NoCarrier) then begin
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                   end;

              104: begin  { Datacon FWU }
                     { Befehl bilden: es ist f�r jeden Kanal ein eigener Abruf n�tig ! }
                     if (StdKanalNr >= Low (FWU_MessAbruf_Kennzahlen)) AND (StdKanalNr <= High (FWU_MessAbruf_Kennzahlen)) then begin
                       KennzifferStr:=FWU_MessAbruf_Kennzahlen [StdKanalNr];
                       Befehl:=GetFWU_Kommando_Archiv (MAR, KennzifferStr);
                       { Befehl senden und Daten empfangen: }
                       if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Messwerte, ad_File, R, NoCarrier) then begin
                         FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                         exit;
                       end;
                     end else
                       Continue;
                   end;

              105: begin  { KE-Anlage (PPN-Typ) }
                     { Befehl f�r von-Auslese-Zeitpunkt bilden: }
                     Befehl:=GetKE_Kommando_Zeitbereich_von (MAR);
                     { Befehl senden: }
                     if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.ACK01_ProtMeldung, ad_String, R, NoCarrier) then begin
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                     { Antwort wird nicht ausgewertet }

                     { Befehl f�r bis-Auslese-Zeitpunkt bilden: }
                     Befehl:=GetKE_Kommando_Zeitbereich_bis (MAR);
                     { Befehl senden: }
                     if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.ACK01_ProtMeldung, ad_String, R, NoCarrier) then begin
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                     { Antwort wird nicht ausgewertet }

                     Befehl:=GetKE_Kommando ('T090');   { Auslesen PPN-Typ }
                     { Befehl senden und Daten empfangen: }
                     if not TMRGCustomCommObj (CommObj).SendKEDatentelegramm_T (Befehl, ad_File,
                                                                                MRGTimeouts.ACK01_ProtMeldung, R, NoCarrier) then begin
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                   end;

              106: begin  { Tritschler VC2 }
                     if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
                       Befehl:=GetTritschler_IECKommando_99_Komplettauslesung (MAR,
                         SSU_GeraeteAuswNr, VerbAufbau_Passwort);
                       bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
                     end
                     else begin
                       Befehl:=GetTritschler_IECKommando_99_Komplettauslesung (MAR, 0, '');
                       bWecken:=true;
                     end;
                     { Befehl senden: }
                     if not TMRGCustomCommObj (CommObj).SendTritschlerIECCommand
                       (Befehl, ad_File, MRGTimeouts.TritschlerIECProt, FMrgDefData.MrgTyp,
                        bWecken, R, NoCarrier) then begin
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                   end;

              107: begin  { Tritschler TTG mit IEC-Protokoll }
                     { Befehl bilden: es ist f�r jeden Kanal ein eigener Abruf n�tig ! }
                     if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
                       Befehl:=GetTTG_IECKommando_Lastprofil (MAR, StdKanalNr,
                         SSU_GeraeteAuswNr, VerbAufbau_Passwort);
                       bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
                     end
                     else begin
                       Befehl:=GetTTG_IECKommando_Lastprofil (MAR, StdKanalNr, 0, '');
                       bWecken:=true;
                     end;
                     { Befehl senden: }
                     if not TMRGCustomCommObj (CommObj).SendTritschlerIECCommand
                       (Befehl, ad_File, MRGTimeouts.TritschlerIECProt, FMrgDefData.MrgTyp,
                        bWecken, R, NoCarrier) then begin
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                   end;

              109: begin  { Tritschler TTG mit FTL-Protokoll }
                     { Befehl bilden: es ist f�r jeden Kanal ein eigener Abruf n�tig ! }
                     Befehl:=GetTTG_FTLKommando_Messperiodenwerte (StdKanalNr);
                     { von-Auslesezeitpunkt: }
                     if MAR.AlleDaten then
                       dtBuf:=-1
                     else
                       dtBuf:=MW_vonDZ;
                     { Befehl senden: }
                     if not TMRGCustomCommObj (CommObj).SendTritschlerFTLFunktionCommand
                       (Befehl, ad_File, MRGTimeouts.TritschlerFTLProt, dtBuf, R, NoCarrier) then begin
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                   end;

              110, 116: begin  { Tritschler TDS, MCO, VC3, VCC }
                     if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
                       Befehl:=GetTritschler_IECKommando_29_Zaehlerstandsprofil (MAR,
                         SSU_GeraeteAuswNr, VerbAufbau_Passwort);
                       bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
                     end
                     else begin
                       Befehl:=GetTritschler_IECKommando_29_Zaehlerstandsprofil (MAR, 0, '');
                       bWecken:=true;
                     end;
                     { Befehl senden: }
                     if not TMRGCustomCommObj (CommObj).SendTritschlerIECCommand
                       (Befehl, ad_File, MRGTimeouts.TritschlerIECProt, FMrgDefData.MrgTyp,
                        bWecken, R, NoCarrier) then begin
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                   end;

              111: begin  // Actaris Corus
                     { Flags setzen, es folgt Befehl und Antwort mit CRC }
                     TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_CRC_Corus);
                     TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_CRC_Corus);

                     Befehl:=GetCorusSAMKommando_DatenspeicherLesen (MAR, 0);  { Hauptdatenspeicher (Interval log) }
                     { Befehl senden und Daten empfangen: }
                     if not TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                         [ETX], ad_File, MRGTimeouts.CorusSAMProt, cs_CRC_Corus, R,
                         NoCarrier, false) then begin
                       { Bei Timeout Kommunikation mit Actaris Corus neu einleiten (Ger�t
                         schl�ft nach ca. 5 s ein !) und Meldungen nochmal abrufen: }
                       if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) AND
                          (iVersuch <= 1) then begin
                         WeitererAbruf:=true;
                         inc (iVersuch);
                         if not Init_Corus_Kommunikation (false) then exit;
                         Continue;
                       end
                       else begin
                         FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                         exit;
                       end;
                     end;
                   end;

              112: begin    { Actaris Sparklog }
                     { es ist f�r jeden Kanal eine neue Initialisierung der Kommunikation
                       mit der entsprechenden Kanaladresse und ein eigener Abruf n�tig !
                       -> Kanal 1: Adresse 11; Kanal 2: Adresse 22 usw. }
                     if (StdKanalNr >= Low (IEC1107_KanalAdressen)) AND
                        (StdKanalNr <=  High (IEC1107_KanalAdressen)) then begin
                       sKanalAdresse:=IEC1107_KanalAdressen [StdKanalNr];
                       if length (sKanalAdresse) > 0 then begin
                         { Quittungsmodus 0: Schalten in Standardmode f�r Empfang des
                           Standard-Datensatzes mit Z�hlerst�nden des Kanals }
                         if not Init_IEC1107_Kommunikation (sKanalAdresse, '0',
                           FMrgDefData.ModemAbrufgruppe, false, false,
                           sRohfilename_ZS, vDummy) then exit;

                         if length (sRohfilename_ZS) > 0 then begin  // Z�hlerst�nde korrekt empfangen
                           { Kanalnummer durch Strichpunkt getrennt dem Rohfilenamen voranstellen, damit
                             die Konvertierung wei�, zu welchem Kanal die Rohdaten geh�ren ! }
                           sRohfilename_ZS:=IntToStr (StdKanalNr) + ';' + sRohfilename_ZS;

                           { Ausgabe der gelesenen Tagess�tze �ber Archivdatentyp gesteuert: 10.08.2011, WW }
                           if MessAbrufCmdData.ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then begin
                             sInfo:=F_Zerlegen (sRohfilename_ZS, ';');
                             sExt:='_' + sInfo;  // Extension der Zieldateien mit Zusatzinfo
                             CopyDatei (sRohfilename_ZS, MessAbrufCmdData.Dateiname_TA + sExt);  { Rohdaten-Zieldatei schreiben }
                             DeleteFile (sRohfilename_ZS);
                           end else
                             MRGKonvert.AddFileName (sRohfilename_ZS, dt_Tagessaetze);
                         end;

                         { Quittungsmodus 1: Schalten in Programmiermode f�r Abruf des Lastgangs }
                         if not Init_IEC1107_Kommunikation (sKanalAdresse, '1',
                           FMrgDefData.ModemAbrufgruppe, false, false,
                           sDummy, vDummy) then exit;

                         TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);    { Flag setzen, es folgt Befehl mit BCC }
                         TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC); { Flag setzen, es folgt Antwort mit BCC }
                         Befehl:=GetActaris_IEC1107Kommando_Archiv (MAR, 'P.01');  { Lastgang }
                         { Befehl senden und Daten empfangen: }
                         if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_File, R, NoCarrier) then begin  // 21.10.2009, WW
                           FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                           exit;
                         end;
                         { Kanalnummer durch Strichpunkt getrennt dem Rohfilenamen voranstellen, damit
                           die Konvertierung wei�, zu welchem Kanal die Rohdaten geh�ren ! }
                         R.Antwort:=IntToStr (StdKanalNr) + ';' + R.Antwort;
                       end;
                     end;
                   end;

              115: begin    { Elster DS-100 }
                     { Pr�fen, ob DS-100-Datenspeicher f�r geforderten Auslesezeitraum
                       bereits ausgelesen wurde und in Form tempor�rer Rohdatendateien
                       kanalweise vorliegt: }
                     if not (MessAbrufCmdData.vonDZ < FTempDZ_von) then begin
                       if FTempRohFileListe.Count >= StdKanalNr then
                         R.Antwort:=FTempRohFileListe [StdKanalNr-1]  { Tempor�res Rohfile f�r Kanal zuweisen }
                       else
                         R.Antwort:='';
                     end
                     else begin  { Datenspeicher wurde f�r Auslesezeitraum noch nicht ausgelesen }
                       { vor dem Lesen des ersten Kanals evtl. vorhandene zwischengespeicherte
                         Rohdatendateien l�schen und Liste leeren: }
                       if StdKanalNr = 1 then begin
                         FTempRohFileListe.DeleteRohFiles;
                         FTempRohFileListe.Clear;
                       end;
 
                       { Auf ersten Kanal schalten, wenn erforderlich: }
                       if (StdKanalNr = 1) AND (FAktKanal <> 1) then
                         Set_KanalDS100 (1);

                       { DS-100-Datenspeicher f�r aktuellen Kanal auslesen: }
                       if not DatenAuslesen_DS100 (MAR, R.Antwort) then exit;

                       { Rohfile-Kopie anlegen f�r evtl. folgenden Meldungen-Abruf
                         (Rohdaten enthalten auch Meldungsdaten): }
                       FNRohKopie:=ChangeFileExt (R.Antwort, ext_MRG_TempRoh);
                       CopyDatei (R.Antwort, FNRohKopie);
                       { Dateiname der Rohfile-Kopie in tempor�rer Rohfileliste
                         zwischenspeichern: }
                       FNRohKopie:=IntToStr (StdKanalNr) + ';' + FNRohKopie;
                       FTempRohFileListe.Add (FNRohKopie);

                       { Kanalnummer durch Strichpunkt getrennt dem Rohfilenamen voranstellen, damit
                         die Konvertierung wei�, zu welchem Kanal die Rohdaten geh�ren ! }
                       R.Antwort:=IntToStr (StdKanalNr) + ';' + R.Antwort;

                       // Bei 4-Kanal-Ger�t auf n�chsten Kanal weiterschalten, nicht beim letzten.
                       // Auch bei 1-Kanal-Ger�t, damit Datenspeicher erneut von
                       // Beginn an ausgelesen werden kann !
                       if ((MRGAnzKanaele > 1) AND (StdKanalNr < MRGAnzKanaele)) OR  // 4-Kanal
                          (MRGAnzKanaele = 1) then
                         Naechster_KanalDS100;

                       { Nach letztem Kanal Merker setzen mit von-Auslesezeitpunkt:
                         Datenspeicher gelesen }
                       if StdKanalNr >= MRGAnzKanaele then
                         FTempDZ_von:=MessAbrufCmdData.vonDZ;
                     end;
                   end;

              117: begin    { Kamstrup UNIGAS 300 }
                     TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);    { Flag setzen, es folgt Befehl mit BCC }
                     TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC); { Flag setzen, es folgt Antwort mit BCC }
                     Befehl:=GetKamstrup_IEC1107Kommando_Archiv (MAR, 'P.01');  { Messperiodenarchiv }
                     { Befehl senden und Daten empfangen: }
                     if not TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                       [ETX, EOT], ad_File, MRGTimeouts.IEC1107Telegr, cs_BCC, R,
                       NoCarrier, false) then begin
                       FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                       exit;
                     end;
                   end;

              120:
                begin  { Primus, Prilog }
                  // Daten-Archiv per Modbus abrufen (Messwerte):
                  if not ReadModbusArchiv_PrimusPrilog (FMrgDefData.MrgTyp,
                                                        at_Periodenarchiv, MAR,
                                                        MW_vonDZ, MW_bisDZ,
                                                        MBAbrufData_Mess,
                                                        sFilename_Archiv) then exit;
                  R.Antwort:=sFileName_Archiv;
                end;

              121:
                begin  { TME400, RSM200 }
                  // Periodenarchiv per Modbus abrufen (Messwerte):
                  if not ReadModbusArchiv_TME400 (FMrgDefData.MrgTyp,
                                                  at_Periodenarchiv, MAR,
                                                  MW_vonDZ, MW_bisDZ,
                                                  sFilename_Archiv) then exit;
                  R.Antwort:=sFileName_Archiv;
                end;

              122:
                begin  { FLOWSIC500 }
                  // Messperiodenarchiv per Modbus abrufen (Messwerte):
                  if not ReadModbusArchiv_SICK (at_Periodenarchiv, MAR,
                                                MW_vonDZ, MW_bisDZ,
                                                sFilename_Archiv, iRecType) then exit;
                  { Datensatz-Typnummer durch Strichpunkt getrennt dem Rohfilenamen voranstellen, damit
                    die Konvertierung wei�, welche Struktur die Rohdaten haben ! }
                  R.Antwort:=IntToStr (iRecType) + ';' + sFileName_Archiv;
                end;
            end;  { case }
          end;

          { Me�werte korrekt gelesen }

          { Ausgabe der gelesenen Stundenwerte �ber Archivdatentyp gesteuert: 10.08.2011, WW }
          Ausgabe_Messwerte (R.Antwort, iIndex_Rohfile, MRGKonvert);
        end;  { while WeitererAbruf }

        {---------------------- Tagess�tze abrufen --------------------------------}
        if FMrgDefData.AbrufTagessatz then begin     { Pr�fen, ob vom MRG Tagess�tze separat abgerufen werden }
          WeitererAbruf:=true;
          iIndex_Rohfile:=0;
          while WeitererAbruf do begin
            inc (iIndex_Rohfile);

            if NoCarrier then begin                { es besteht keine Verbindung mehr }
              FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
              exit;
            end;

            CreateTagessatzAbrufRec (Modus, MessAbrufCmdData.vonDZ, MessAbrufCmdData.bisDZ,
                                     MessAbrufCmdData.KanalaktivMaske);
            WeitererAbruf:=false;         { Standard: nur 1 Tagessatz-Abrufbefehl }

            if FMrgDefData.Kommandogruppe < 100 then begin
              { Kommando standardm��ig tabellengesteuert zusammensetzen und abrufen (Wieser-Ger�te): }
              MrgAbrufKonfigList:=TMrgAbrufKonfigList.Create;
              try
                if not MrgAbrufKonfigList.LoadFromList_ByKommandoTypGruppe ('F',
                         FMrgDefData.Kommandogruppe, FMrgAbrufResourceList) then begin  // 06.08.2021, WW
                  FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
                  exit;
                end;
                if MrgAbrufKonfigList.Count = 0 then begin
                  FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
                  exit;
                end;

                MrgAbrufData:=TMrgAbrufDataObj (MrgAbrufKonfigList [0]).Data;
                GetMRGKommando_Archiv (MRGAbrufRec [dt_Tagessaetze], MrgAbrufData.Kommandotyp,
                                       MrgAbrufData.AbrufKommando, MrgAbrufData.AbrufKommando_alle_Daten,
                                       FMrgDefData.MrgTyp, FMrgKanalBitResourceList,  // 06.08.2021, WW
                                       Befehl);

                { Tagessatzabruf-Kommando senden }
                if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Tagessaetze, ad_File, R, NoCarrier) then begin
                  FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                  exit;
                end;

                { Antwort auf Tagessatzabruf-Kommando auswerten: }
                if length (Befehl) > 0 then
                  Kommando_Soll:=Befehl[2]  // Zeichen nach dem STX
                else
                  Kommando_Soll:='';
                if not ValidMRGAntwortfile (Kommando_Soll, FMrgDefData.ModemAbrufgruppe, R.Antwort,
                                            R.Fehlergruppe, R.Fehlercode, RohdatenLoeschen) then begin
                  FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                  exit;
                end;
              finally
                MrgAbrufKonfigList.Free;
              end;
            end
            else begin
              { bei Ger�ten, bei denen f�r jeden Kanal ein eigener Abrufbefehl n�tig ist, wird
                auf den n�chsten Kanal weitergeschaltet: }
              WeitererAbruf:=SetNextTagAbrufKanal;

              { Kommando per Quellcode zusammensetzen ohne zugrundeliegende Kommando-Definition aus Tabelle
                (Fremdger�te, deren Kommando-Definitionen nicht in die Tabellenstruktur passen): }
              MAR:=MRGAbrufRec [dt_Tagessaetze];
              case FMrgDefData.Kommandogruppe of
                104: begin  { Datacon FWU }
                       { Befehl bilden: es ist f�r jeden Kanal ein eigener Abruf n�tig ! }
                       if (TagKanalNr >= Low (FWU_TagAbruf_Kennzahlen)) AND (TagKanalNr <= High (FWU_TagAbruf_Kennzahlen)) then begin
                         KennzifferStr:=FWU_TagAbruf_Kennzahlen [TagKanalNr];
                         Befehl:=GetFWU_Kommando_Archiv (MAR, KennzifferStr);
                         { Befehl senden und Daten empfangen: }
                         if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Tagessaetze, ad_File, R, NoCarrier) then begin
                           FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                           exit;
                         end;
                       end else
                         Continue;
                     end;

                109: begin  { Tritschler TTG mit FTL-Protokoll }
                       { Befehl bilden: es ist f�r jeden Kanal ein eigener Abruf n�tig ! }
                       Befehl:=GetTTG_FTLKommando_Zaehlerstaende (TagKanalNr);
                       { Befehl senden: }
                       if not TMRGCustomCommObj (CommObj).SendTritschlerFTLFunktionCommand
                         (Befehl, ad_File, MRGTimeouts.TritschlerFTLProt, -1, R, NoCarrier) then begin
                         FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                         exit;
                       end;
                     end;

                111: begin  // Actaris Corus
                       { Flags setzen, es folgt Befehl und Antwort mit CRC }
                       TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_CRC_Corus);
                       TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_CRC_Corus);

                       Befehl:=GetCorusSAMKommando_DatenspeicherLesen (MAR, 3);  { Monatsspeicher (Monthly log) }
                       { Befehl senden und Daten empfangen: }
                       if not TMRGCustomCommObj (CommObj).SendIEC1107DatenTelegramm (Befehl,
                           [ETX], ad_File, MRGTimeouts.CorusSAMProt, cs_CRC_Corus, R,
                           NoCarrier, false) then begin
                         FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
                         exit;
                       end;
                     end;
              end;  { case }
            end;

            { Tagess�tze korrekt gelesen }

            { Ausgabe der gelesenen Tagess�tze �ber Archivdatentyp gesteuert: 10.08.2011, WW }
            if MessAbrufCmdData.ArchivdatenTyp = C_CmdArchDatenTyp_RohFile then begin
              S:=R.Antwort;
              { Pr�fen, ob dem Antwort-Filenamen eine Zusatzinformation vorangestellt ist: }
              if Pos (';', S) > 0 then begin
                sInfo:=F_Zerlegen (S, ';');
                sExt:='_' + sInfo;  // Extension der Zieldateien mit Zusatzinfo
              end else
                sExt:='_' + IntToStr (iIndex_Rohfile);  // Extension der Zieldateien mit fortlaufender Nummer
              CopyDatei (S, MessAbrufCmdData.Dateiname_TA + sExt);  { Rohdaten-Zieldatei schreiben }
              DeleteFile (S);
            end else
              MRGKonvert.AddFileName (R.Antwort, dt_Tagessaetze);
          end; { while WeitererAbruf }
        end;  { if FMrgDefData.AbrufTagessatz }

        Result:=true;
      finally
        { Wenn die gelesenen Stundenwerte/Tagess�tze in Rohdaten-Zieldatei(en)
          geschrieben werden, keine Standard-Ausgabe in XML-Response: 10.08.2011, WW }
        if MessAbrufCmdData.ArchivdatenTyp <> C_CmdArchDatenTyp_RohFile then begin
          { immer konvertieren, auch wenn ein Fehler aufgetreten ist: }
          if not MRGKonvert.MessTagKonvert (FMrgDefData, FMrgKonvData,  // 06.08.2021, WW
                                            MBAbrufData_Mess,  // 11.02.2022, WW
                                            FMrgKanalBitResourceList,  // 06.08.2021, WW
                                            FWZ_SZResourceList,  // 06.08.2021, WW
                                            iTagesende,
                                            MessAbrufCmdData.KanalaktivMaske,
                                            StaKanalKonvDataList,
                                            ParameterListe_MRGAbruf,
                                            AnalogOrig_normiert,
                                            KonvDateiname_MessList,
                                            KonvDateiname_TagList,
                                            iMaxMessKanal,
                                            iMinMessKanalExt,
                                            iMaxMessKanalExt) then begin
            FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
            Result:=false;
          end;
        end;
      end;
    finally
      MRGKonvert.Free;
    end;
  end;  { case FMrgDefData.Kommandogruppe }
End;

{--------------------------------------------------------------------------------}
function TMRGAbruf.AbrufPruefwerte (PruefAbrufCmdData: TMessMeldPruefAbrufCmdData;
                                    var KonvDateiname_Pruef: string): boolean;
{--------------------------------------------------------------------------------}
{ Abrufen der Pr�fungss�tze; MRGAbrufRec [dt_pruefsaetze] wird je nach Modus erzeugt
  �bergabe: Pr�fsatzabruf-Kommandodaten
  R�ckgabe: Name der von der Konvertierung erzeugten Dateien mit Pr�fungss�tzen
  Ergebnis: true, wenn Pr�fsatz-Abruf ok }
Var
  R: TRueckgabe;
  Befehl: string;
  MRGKonvert: TMRGKonvert;
  MrgAbrufData: TMrgAbrufData;
  Modus: byte;
  Kommando_Soll: string;
  MrgAbrufKonfigList: TMrgAbrufKonfigList;

Begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);         { Vorbelegung Fehlergruppe/-code: OK }

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  { Vorbelegung der R�ckgabe: keine Pr�fsatz-Datendatei vorhanden }
  KonvDateiname_Pruef:='';

  { Pr�fen, ob lt. MrgDef-Eintrag bei vorliegendem MRG-Typ Pr�fungss�tze
    abgerufen werden k�nnen: }
  if not FMrgDefData.AbrufPruefsatz then begin
    FehlerGruppeCodeUpdate (SYS_ABRUFERROR, SYSABRFERR_KOMMANDOUNGUELTIG);
    exit;
  end;

  if PruefAbrufCmdData.vonDZ > 0 then
    Modus:=d_VonBis
  else
    Modus:=d_Alles;

  MRGKonvert:=TMRGKonvert.Create (FMrgDefData.MrgTyp, RohdatenLoeschen);
  try
    try
      CreatePruefSaetzeAbrufRec (Modus, PruefAbrufCmdData.vonDZ, PruefAbrufCmdData.bisDZ);

      MrgAbrufKonfigList:=TMrgAbrufKonfigList.Create;
      try
        if not MrgAbrufKonfigList.LoadFromList_ByKommandoTypGruppe ('X',
                 FMrgDefData.Kommandogruppe, FMrgAbrufResourceList) then begin  // 06.08.2021, WW
          FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
          exit;
        end;
        if MrgAbrufKonfigList.Count = 0 then begin
          FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
          exit;
        end;

        MrgAbrufData:=TMrgAbrufDataObj (MrgAbrufKonfigList [0]).Data;
        GetMRGKommando_Archiv (MRGAbrufRec [dt_Pruefsaetze], MrgAbrufData.Kommandotyp,
                               MrgAbrufData.AbrufKommando, MrgAbrufData.AbrufKommando_alle_Daten,
                               FMrgDefData.MrgTyp, FMrgKanalBitResourceList,  // 06.08.2021, WW
                               Befehl);

        { Pr�fsatzabruf-Kommando senden }
        if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Pruefsaetze, ad_File, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
          exit;
        end;

        { Antwort auf Pr�fsatzabruf-Kommando auswerten: }
        if length (Befehl) > 0 then
          Kommando_Soll:=Befehl[2]  // Zeichen nach dem STX
        else
          Kommando_Soll:='';
        if not ValidMRGAntwortfile (Kommando_Soll, FMrgDefData.ModemAbrufgruppe, R.Antwort,
                                    R.Fehlergruppe, R.Fehlercode, RohdatenLoeschen) then begin
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
      finally
        MrgAbrufKonfigList.Free;
      end;

      { Pr�fungss�tze korrekt gelesen }
      MRGKonvert.AddFileName (R.Antwort, dt_Pruefsaetze);

      Result:=true;
    finally
      { immer konvertieren, auch wenn ein Fehler aufgetreten ist: }
      MRGKonvert.PruefKonvert (KonvDateiname_Pruef);
    end;
  finally
    MRGKonvert.Free;
  end;
End;

{----------------------------------------------------------------------------}
function TMRGAbruf.DSfGUmschaltung (DSfGUmschaltCmdData: TDSfGUmschaltCmdData;
                                    var StationsKennungRet: string): boolean;
{----------------------------------------------------------------------------}
{ Auf DSfG-Teilnehmer (Slave) umschalten;
  �bergabe: Umschalt-Kommandodaten
  R�ckgabe: Kennung der MRG-Station, auf die umgeschaltet wurde
  Ergebnis: true, wenn Umschaltung ok }
var
  R: TRueckgabe;
  Befehl: string;
  Adresse_alt: string;
  PL: TParameterListe;
  ParaWert: string;
  IBuf: integer;

begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);         { Vorbelegung Fehlergruppe/-code: OK }
  StationsKennungRet:='';
  try
    { Vorbelegung: neue Kennung aus Umschalt-Kommando; 26.03.2008 WW }
    StationsKennung:=DSfGUmschaltCmdData.Kennung_neu;

    if NoCarrier then begin                  { es besteht keine Verbindung mehr }
      FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
      exit;
    end;

    { bei Tritschler IEC- und FTL-Ger�ten (VC2, TTG, TDS), wenn bereits auf einen
      Multiplexer-Ausgang geschaltet wurde: }
    if ((FMrgDefData.ModemAbrufgruppe = 8) OR (FMrgDefData.ModemAbrufgruppe = 9)) AND
       (SSU_GeraeteAuswNr > 0) then begin
      if FMrgDefData.ModemAbrufgruppe = 9 then  { Tritschler TTG mit FTL-Protokoll }
        if not Send_FTL_Uebertragungsende then exit;  { FTL-�bertragungsende-Befehl senden }

      Send_Tritschler_Mux_Break;  { Tritschler Multiplexer Break-Befehl senden }
    end;

    { allgemeine Konfigurationsdaten initialisieren f�r den Ger�tetyp, auf den
      umgeschaltet werden soll: }
    if not Init_MRGKonfigData (DSfGUmschaltCmdData.GeraeteTyp) then exit;

    if (FMrgDefData.ModemAbrufgruppe = 8) OR
       (FMrgDefData.ModemAbrufgruppe = 9) then begin  { Tritschler IEC- und FTL-Ger�te (VC2, TTG, TDS) }
      if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin   { mit Multiplexer SSU }
        { SSU-Ger�teauswahlnummer setzen (wird in Umschalt-Adresse �bergeben): }
        IBuf:=StrToIntDef (DSfGUmschaltCmdData.Adresse, -1);
        if IBuf < 0 then begin  // Umschalt-Adresse enth�lt keine g�ltige SSU-Ger�teauswahlnummer
          FehlerGruppeCodeUpdate (SYS_ABRUFERROR, SYSABRFERR_KOMMANDONICHTPLAUSIBEL);
          exit;
        end;
        SSU_GeraeteAuswNr:=IBuf;

        if FMrgDefData.ModemAbrufgruppe = 9 then begin  { Tritschler FTL-Ger�te }
          { Im Tritschler Multiplexer die Kommunikation f�r FTL-Protokoll  }
          Init_Tritschler_Mux_FTL_Kommunikation;

          { Schnittstellen-Parameter f�r die nachfolgenden FTL-Ger�te-Befehle aktivieren: }
          TMRGCustomCommObj (CommObj).SetAbrufgruppe (FMrgDefData.ModemAbrufgruppe, MRG_DPS);

          { Kommunikation f�r Tritschler FTL-Protokoll einleiten: }
          if not Send_FTL_Uebertragungsbeginn then exit;
        end else  { Tritschler IEC-Ger�te }
          { Schnittstellen-Parameter f�r die nachfolgenden Ger�te-Befehle aktivieren: }
          TMRGCustomCommObj (CommObj).SetAbrufgruppe (FMrgDefData.ModemAbrufgruppe, MRG_DPS);

        { Kennungsabfrage f�r an SSU-Ger�teauswahlnummer angeschlossenes Ger�t, 15.05.2007, WW }
        if not KennungAbfragen (FMrgDefData.ModemAbrufgruppe, FMrgDefData.Infobefehl,
                                FMrgInfoData.Kennung, 0, false) then exit;

        { Ger�te-Kennung mit neuer Kennung aus Umschalt-Kommando vergleichen: }
        if not KennungComp (StationsKennung, DSfGUmschaltCmdData.Kennung_neu, KennungExt) then begin
          { Kennungen sind nicht identisch }
          if DSfGUmschaltCmdData.KennPruef then begin         { mit Kennungspr�fung }
            FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_INVKENNUNG_KEINE_UMSCHALTUNG);
            exit;
          end else                                           { ohne Kennungspr�fung }
            FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_INVKENNUNG_UMSCHALTUNG);
        end;
      end
      else begin
        FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_UMSCHALTUNG);
        exit;
      end;
    end

    else begin  // "echte" Wieser DSfG-Umleitung
      { zuerst Adresse des aktuellen DSfG-Teilnehmers auslesen (f�r evtl. notwendige R�ckschaltung): }
      PL:=TParameterListe.Create (FKonfigPath);
      try
        if not AbrufParameter (CP_DSfG_SSK_Adresse, nil, PL) then begin
          FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_UMSCHALTUNG);
          exit;
        end;
        if PL.GetValue (CP_DSfG_SSK_Adresse, ParaWert) then begin
          if ParaWert = '_' then
            Adresse_alt:='0'
          else
            Adresse_alt:=ParaWert;
        end
        else begin
          FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_UMSCHALTUNG);
          exit;
        end;
      finally
        PL.Free;
      end;

      { Befehl f�r Umschaltung }
      Befehl:=STX + '}' + DSfGUmschaltCmdData.Adresse + ETX;

      { Umschalt-Kommando senden }
      if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.DSfGUmschaltung, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;

      { Antwort auf Umschalt-Kommando auswerten: }
      if not ValidMRGAntwort ('}', FMrgDefData.ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;

      if length (DSfGUmschaltCmdData.Adresse) > 0 then begin
        if not ValidDSfGUmschaltAntwort (R.Antwort, DSfGUmschaltCmdData.Adresse [1]) then begin
          FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_UMSCHALTUNG);
          exit;
        end;
      end;

      { Umschaltung erfolgreich, jetzt Kennung abfragen }
      isDSfG_Slave:=true;
      Befehl:=GetMRGKommando_B ('003');

      { Abfrage-Kommando f�r Kennung senden }
      if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Kennung, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;

      { Antwort auf Abfrage-Kommando f�r Kennung auswerten: }
      if not ValidMRGAntwort ('B003', FMrgDefData.ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;

      if not ValidSlaveKennung (R.Antwort, DSfGUmschaltCmdData.Kennung_neu,
                                StationsKennung) then begin
        { Kennungen sind nicht identisch }
        if DSfGUmschaltCmdData.KennPruef then begin         { mit Kennungspr�fung }
          { auf bisheriges MRG wieder zur�ckschalten: }
          Befehl:=STX + '}' + Adresse_alt + ETX;

          { R�ckschalt-Kommando senden }
          if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.DSfGUmschaltung, ad_String, R, NoCarrier) then begin
            FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
            exit;
          end;
          FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_INVKENNUNG_KEINE_UMSCHALTUNG);
          exit;
        end else                                           { ohne Kennungspr�fung }
          FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_INVKENNUNG_UMSCHALTUNG);
      end;
    end;
  finally
    StationsKennungRet:=StationsKennung;
  end;
  Result:=true;
end;

{--------------------------------------------------------------------------------}
procedure TMRGAbruf.ZeitSynchronisation (ZeitSyncCmdData: TZeitSyncCmdData;
                                         KorrekturMax: integer;
                                         var ZeitSyncInfoData: TZeitSyncInfoData);
{--------------------------------------------------------------------------------}
{ Zeit im MRG auf PC-Zeit setzen;
  �bergabe: Zeitsynchronisations-Kommandodaten
            Max. Korrekturzeit (  0: es wird synchronisiert
                                > 0: es wird maximal um die in 'KorrekturMax'
                                     �bergebene Anzahl an Sekunden korrigiert)
  R�ckgabe: Infodaten zur Zeitsynchronisation }
Const
  C_Sicherheitsabstand = 30; { s }  { keine Sync in diesem Zeitraum vor einer vollen Stunde }
  C_KorrGetMRGZeit = 250; { ms }    { ungef�hrer Korrekturwert f�r ausgelesene MRG-Zeit  }
  C_KorrSetMRGZeit = 750; { ms }    { ungef�hrer Korrekturwert f�r �bertragung der neuen MRG-Zeit }

var
  R: TRueckgabe;
  Befehl: string;
  PCDateTime: TDateTime;
  MRGDateTime: TDateTime;
  NewMRGDateTime: TDateTime;
  MRGHour, PCHour: word;
  NewMRGHour: word;
  dummy1, dummy2, dummy3: word;
  Naechste_volle_Stunde_DateTime: TDateTime;
  DiffSec: longint;
  DiffSec_StdWechsel: longint;
  MRGTimeStr, MRGDateStr: string;
  NextSZToWZ: TDateTime;
  NextSZToWZHour: word;
  AltWert, NeuWert: string;
  DatumParaNrMRG: string;
  DatumParaNrAllg: string;
  ZeitParaNrMRG: string;
  ZeitParaNrMrg_Einst: string;
  ZeitParaNrAllg: string;
  ZeitSync_erfolgreich: boolean;
  AktSZ_WZ_Flag: integer;
  SZ_WZ_Umstellung_im_Geraet: boolean;
  bWecken: boolean;
  AFehlergruppe: integer;
  AFehlercode: integer;
  PosETX: integer;
  bNochmal: boolean;
  iVersuch: integer;
  cBefehl: char;
  MRGTZStr: string;
  dtBuf: TDateTime;
  PL: TParameterListe;
  bOK: boolean;
  sUtcOffsetDev: string;
  MBRegisterDef: TMBRegisterDef;
  ParamMrgData: TParamMrgData;
  bResourceOK: boolean;
  iKorrSeconds: integer;
  slParaAllg: TStringList;
  dtWZ_SZ_Offset: TDateTime;
  dtDevicePC_TimezoneDiff: TDateTime;
  dtPCDateTimeOffsetUTC: TDateTime;
  UTCZeitParaNrAllg: string;
  MRGTimeStr_UTC: string;
  MRGDateTime_UTC: TDateTime;
  NewMRGDateTime_UTC: TDateTime;

begin
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: Abweichung < min. Abweichung (OK) }
  with ZeitSyncInfoData do begin   { Vorbelegung R�ckgabe: Keine ZeitSync-Infodaten vorhanden }
    DZ_Server:=0;
    DZ_Geraet:=0;
  end;

  { Pr�fen, ob lt. MrgDef-Eintrag bei vorliegendem MRG-Typ eine Zeitsynchronisation
    durchgef�hrt werden kann: }
  if (length (FMrgDefData.Datumformat) = 0) AND (length (FMrgDefData.Zeitformat) = 0) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+SYS_ABRUFERROR, SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT);
    exit;
  end;

  { Vorbelegung: Zus�tzliche MRG-UTC-Zeit nicht vorhanden (wird nur f�r Zeit-Korrektur
    von Ger�ten verwendet, bei denen per UTC-Zeitstempel eine Zeitverstellung erfolgt) }
  MRGDateTime_UTC:=0;

  bNochmal:=true;
  iVersuch:=1;
  while bNochmal do begin
    bNochmal:=false;  { Standard: nur einmal }

    if NoCarrier then begin                  { es besteht keine Verbindung mehr }
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
      exit;
    end;

    { Vorbelegung: MRG-Datum/Zeit und MRG-Zeitzone nicht vorhanden }
    MRGTimeStr:='';
    MRGDateStr:='';
    MRGTZStr:='';

    { Vorbelegung: kein eigener Parameter zum Einstellen der neuen Zeit
                  (Lese-Parameter = Einstell-Parameter; Standard) }
    ZeitParaNrMrg_Einst:='';

    // Datum und Zeit aus Ger�t auslesen (lokale Ger�te-Zeit):
    if FMrgDefData.Kommandogruppe < 100 then begin  { standardm��ige ZeitSync (Wieser-Ger�te) }
      if FMrgDefData.Kommandogruppe = 6 then begin  { EC 900 }
        { ger�tespezifische Parameternummer f�r das Einstellen der neuen Ger�tezeit
          �ber allg. Parameternummer ermitteln: }
        if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                              CP_RMG_EC900_SetzEing_DZSync,
                                                              ParamMrgData) then begin  // 09.07.2021, WW
          FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
          exit;
        end;
        ZeitParaNrMrg_Einst:=ParamMrgData.Parameternummer_im_MRG;  // 09.07.2021, WW

        ZeitParaNrAllg:=CP_RMG_EC900_Systemzeit;  // allg. Parameternummer f�r Lesen von Datum/Zeit
      end
      else begin
        { Parameter "Datum" �ber allg. Parameternummer abrufen: }
        if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                              CP_ALLG_DATUM,
                                                              ParamMrgData) then begin  // 09.07.2021, WW
          FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
          exit;
        end;
        DatumParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;  // 09.07.2021, WW

        Befehl:=GetMRGKommando_B (DatumParaNrMrg);

        { Kommando f�r "Datum abrufen" senden }
        if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Parameter, ad_String, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
          exit;
        end;

        { Antwort auf Kommando f�r "Datum abrufen" auswerten: }
        if not ValidMRGAntwort ('B' + DatumParaNrMRG, FMrgDefData.ModemAbrufgruppe, R.Antwort,
                                R.Fehlergruppe, R.Fehlercode) then begin
          FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
          exit;
        end;

        MRGDateStr:=ExtractString (R.Antwort, STX, ETX, 0);  // Rohdatenteil zwischen STX und ETX
        MRGDateStr:=ExtractString (MRGDateStr, NUL, US, 0);  // wegen EC694: US abschneiden
        MRGDateStr:=Copy (MRGDateStr, length (DatumParaNrMRG) + 2, length (MRGDateStr));

        ZeitParaNrAllg:=CP_ALLG_ZEIT;
      end;

      { Parameter "Zeit" �ber allg. Parameternummer abrufen: }
      if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                            ZeitParaNrAllg,
                                                            ParamMrgData) then begin  // 09.07.2021, WW
        FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
        exit;
      end;
      ZeitParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;  // 09.07.2021, WW
      Befehl:=GetMRGKommando_B (ZeitParaNrMrg);

      { Kommando f�r "Zeit abrufen" senden }
      if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Parameter, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end
    else begin
      { Fremdger�te: }
      case FMrgDefData.Kommandogruppe of
        101, 102, 108, 113, 118, 119:
          begin    { Elster DL210, DL220, DL230, DL240, EK260, EK280 }
            { Parameter "Datum/Zeit" abrufen: }
            Befehl:=GetLIS200Kommando_Lesen ('1:400');

            { Flags setzen, es folgt Befehl und Antwort mit BCC }
            TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);
            TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC);
            { Kommando f�r "Datum/Zeit abrufen" senden }
            if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier) then begin
              FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
              exit;
            end;
          end;

        { Kommandogruppen 106, 110 (Tritschler VC2, TDS): kein Lesen der aktuellen Ger�te-Zeit m�glich }


        111:
          begin    { Actaris Corus }
            { Flags setzen, es folgt Befehl und Antwort mit CRC }
            TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_CRC_Corus);
            TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_CRC_Corus);

            { Parameter "Aktuelles Datum/Uhrzeit" abrufen (= Parameter 106): }
            Befehl:=GetCorusSAMKommando_ParameterLesen (106);
            if not CommObj.SendCommand (Befehl, [ETX, NAK], 1, MRGTimeouts.CorusSAMProt, ad_String, R, NoCarrier) then begin
              { Bei Timeout Kommunikation mit Actaris Corus neu einleiten (Ger�t
                schl�ft nach ca. 5 s ein !) und Datum/Uhrzeit nochmal abrufen: }
              if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) AND
                 (iVersuch <= 1) then begin
                bNochmal:=true;
                inc (iVersuch);
                if not Init_Corus_Kommunikation (false) then exit;
              end
              else begin
                FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + R.Fehlergruppe, R.Fehlercode);
                exit;
              end;
            end;
          end;

        112, 117:
          begin    { Actaris Sparklog, Kamstrup UNIGAS 300 }
            { Flags setzen, es folgen Befehl(e) mit BCC }
            TMRGCustomCommObj (CommObj).SetSendWithChecksum (cs_BCC);
            TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_BCC);

            { Parameter "Aktuelles Datum" abrufen: }
            Befehl:=GetIEC1107Kommando_Lesen ('0.9.2');
            if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier) then begin
              FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + R.Fehlergruppe, R.Fehlercode);
              exit;
            end;

            { Antwort auf Kommando f�r "Aktuelles Datum abrufen" auswerten: }
            if not ValidIEC1107DatenTelegrammAntwort (R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
              FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + R.Fehlergruppe, R.Fehlercode);
              exit;
            end;
            MRGDateStr:=ExtractString (R.Antwort, '(', ')', 0);  { Datum steht zwischen den runden Klammern }
            { Aktuelles Datum kann im Format DS7 oder DS6 vorliegen. Wir verwenden DS6: }
            if length (MRGDateStr) > length (FMrgDefData.Datumformat) then begin
              // Bugfix: Zeitzone merken VOR dem K�rzen auf DS6; 10.12.2019, WW
              MRGTZStr:=Copy (MRGDateStr, 1, 1);  // bei DS7 ist 1. Zeichen die Zeitzone
              MRGDateStr:=Copy (MRGDateStr, length (MRGDateStr) - length (FMrgDefData.Datumformat) + 1,
                                length (FMrgDefData.Datumformat));  // DS6
            end;

            { Parameter "Aktuelle Zeit" abrufen: }
            Befehl:=GetIEC1107Kommando_Lesen ('0.9.1');
            if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier) then begin
              FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + R.Fehlergruppe, R.Fehlercode);
              exit;
            end;
          end;

        115:
          begin    { Elster DS-100 }
            { Mit z-Befehl aktuelle Datum/Zeit abrufen: }
            cBefehl:='z';
            Befehl:=GetDS100_LeseBefehl (cBefehl);
            if not TMRGCustomCommObj (CommObj).SendElsterDS100Command (Befehl, ad_String,
              MRGTimeouts.ElsterDS100Prot, R, NoCarrier) then begin
              FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + R.Fehlergruppe, R.Fehlercode);
              exit;
            end;
          end;

        120,  { Primus, Prilog }
        121:  { TME400, RSM200 }
          begin  { Modbus, 1 Parameter mit Datum/Zeit-Information wird gelesen }
            if (FMrgDefData.MrgTyp = mrgtyp_Primus) OR
               (FMrgDefData.MrgTyp = mrgtyp_Prilog) then  // 25.11.2020, WW
              ZeitParaNrAllg:=CP_RMG_PrimusPrilog_Sommerzeit
            else if (FMrgDefData.MrgTyp = mrgtyp_TME400_VCF) OR
                    (FMrgDefData.MrgTyp = mrgtyp_TME400_VMF) then  // 18.02.2021, WW
              ZeitParaNrAllg:=CP_RMG_TME400_Unixzeit
            else if (FMrgDefData.MrgTyp = mrgtyp_RSM200_VCF) OR
                    (FMrgDefData.MrgTyp = mrgtyp_RSM200_VMF) then  // 09.01.2024, WW
              ZeitParaNrAllg:=CP_RMG_RSM200_Unixzeit
            else
              ZeitParaNrAllg:='';

            PL:=TParameterListe.Create (FKonfigPath);
            try
              if not AbrufParameter (ZeitParaNrAllg, nil, PL) then begin
                FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + Fehlergruppe, Fehlercode);
                exit;
              end;
            finally
              PL.Free;
            end;

            // abgerufene aktuelle Ger�tezeit/-datum aus Parameterliste lesen:
            if not ParameterListe_MRGAbruf.GetValue (ZeitParaNrAllg, MRGTimeStr) then
              MRGTimeStr:='';
            MRGDateStr:=MRGTimeStr;
          end;

        122:  { FLOWSIC500; 29.09.2021, WW }
          begin  { Modbus, Parameter "Datum" und "Zeit" werden gelesen
                   -> Bei Zeitkorrektur zus�tzlich die UTC-Zeit lesen }
            if (FMrgDefData.MrgTyp = mrgtyp_FLOWSIC500) then begin
              DatumParaNrAllg:=CP_SICK_FLOWSIC500_Datum;
              ZeitParaNrAllg:=CP_SICK_FLOWSIC500_Zeit;
              UTCZeitParaNrAllg:=CP_SICK_FLOWSIC500_Zeitstempel_UTC;
            end
            else begin
              DatumParaNrAllg:='';
              ZeitParaNrAllg:='';
              UTCZeitParaNrAllg:=''
            end;

            slParaAllg:=TStringList.Create;  // Liste f�r abzurufende allg. Parameternummern
            try
              slParaAllg.Add (DatumParaNrAllg);
              slParaAllg.Add (ZeitParaNrAllg);
              if (KorrekturMax > 0) then
                slParaAllg.Add (UTCZeitParaNrAllg);

              PL:=TParameterListe.Create (FKonfigPath);
              try
                if not AbrufParameter ('', slParaAllg, PL) then begin
                  FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + Fehlergruppe, Fehlercode);
                  exit;
                end;
              finally
                PL.Free;
              end;
            finally
              slParaAllg.Free;
            end;

            // abgerufenes aktuelles Ger�tedatum aus Parameterliste lesen:
            if not ParameterListe_MRGAbruf.GetValue (DatumParaNrAllg, MRGDateStr) then
              MRGDateStr:='';

            // abgerufene aktuelle Ger�tezeit aus Parameterliste lesen:
            if not ParameterListe_MRGAbruf.GetValue (ZeitParaNrAllg, MRGTimeStr) then
              MRGTimeStr:='';

            // Nur bei Zeit-Korrektur: Abgerufene aktuelle UTC-Ger�tezeit aus Parameterliste lesen
            if (KorrekturMax > 0) then
              if ParameterListe_MRGAbruf.GetValue (UTCZeitParaNrAllg, MRGTimeStr_UTC) then
                MRGDateTime_UTC:=MRGStrToDateTime('MB', '', 'MB', MRGTimeStr_UTC);
          end;

      end;  { case }
    end;
  end;  { while bNochmal }

  { PC-Zeit ermitteln:
    -> um sie mit der MRG-Zeit vergleichen zu k�nnen, wird die PC-Zeit gleich auf
       die im MRG herrschende Zeitzone umgerechnet }
  bResourceOK:=false;
  if Assigned (FWZ_SZResourceList) then
    { Zeitzoneninfo f�r PC-Zeit ermitteln: }
    bResourceOK:=FWZ_SZResourceList.FindSZ_WZ_Info (AktSZ_WZ_Flag, NextSZToWZ);  // 06.08.2021, WW
  if not bResourceOK then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
    exit;
  end;
  
  SZ_WZ_Umstellung_im_Geraet:=Copy (ZeitSyncCmdData.Zeitbasis_Geraet, 1, 1) = C_CmdZSyncBasis_S;
  dtWZ_SZ_Offset:=GetWZ_SZ_Offset (AktSZ_WZ_Flag, SZ_WZ_Umstellung_im_Geraet);

  // UTC-Offset der aktuellen PC-Zeit:
  dtPCDateTimeOffsetUTC:=GetTimeZoneBias (true);

  // Aktuelle lokale PC-Zeit mit f�r MRG eingerechnetem SZ/WZ-Offset:
  PCDateTime:=Now + dtWZ_SZ_Offset;

  // ab 24.02.2020, WW: mit optionalem UTC-Normalzeit-Offset des MRG (Zeitzone)
  sUtcOffsetDev:=Copy (ZeitSyncCmdData.Zeitbasis_Geraet, 2, length (ZeitSyncCmdData.Zeitbasis_Geraet));
  dtDevicePC_TimezoneDiff:=Get_DevicePC_TimezoneDiff (sUtcOffsetDev);

  PCDateTime:=PCDateTime + dtDevicePC_TimezoneDiff;  // Zeitzonen-Differenz MRG/PC in PC-Zeit einrechnen

  // ausgelesene Roh-Zeit/Datum konvertieren, MRGDateTime bilden nach Formatangaben:
  if FMrgDefData.Kommandogruppe < 100 then begin  { standardm��ige ZeitSync (Wieser-Ger�te) }
    { Antwort auf Kommando f�r "Zeit abrufen" auswerten: }
    if not ValidMRGAntwort ('B' + ZeitParaNrMRG, FMrgDefData.ModemAbrufgruppe, R.Antwort,
                            R.Fehlergruppe, R.Fehlercode) then begin
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    MRGTimeStr:=ExtractString (R.Antwort, STX, ETX, 0);  // Rohdatenteil zwischen STX und ETX
    MRGTimeStr:=ExtractString (MRGTimeStr, NUL, US, 0);  // wegen EC694: US abschneiden
    MRGTimeStr:=Copy (MRGTimeStr, length (ZeitParaNrMRG) + 2, length (MRGTimeStr));

    if FMrgDefData.Kommandogruppe = 6 then  { EC 900 }
      MRGDateStr:=MRGTimeStr;
  end
  else begin
    { Fremdger�te: }
    case FMrgDefData.Kommandogruppe of
      101, 102, 108, 113, 118, 119:
        begin    { Elster DL210, DL220, DL230, DL240, EK260, EK280 }
          { Antwort auf Kommando f�r "Datum/Zeit abrufen" auswerten: }
          if not ValidElster_IEC1107DatenTelegrammAntwort (R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
            exit;
          end;
          { Datum/Zeit steht zwischen den runden Klammern durch Komma getrennt }
          MRGTimeStr:=ExtractString (R.Antwort, '(', ')', 0);
          MRGDateStr:=F_Zerlegen (MRGTimeStr, ',');
        end;

      { Kommandogruppen 106, 110 (Tritschler VC2, TDS): entf�llt, da kein Lesen der aktuellen Ger�te-Zeit erfolgt }    

      111:
        begin    { Actaris Corus }
          { Antwort auf Kommando f�r "Aktuelles Datum/Uhrzeit abrufen" auswerten: }
          if not ValidCorusSAMAntwort (R.Antwort, R.Fehlergruppe, R.Fehlercode, PosETX) then begin
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + R.Fehlergruppe, R.Fehlercode);
            exit;
          end;
          { Datum/Zeit bin�rcodiert im Corus-DATE-Format: }
          MRGTimeStr:=Copy (R.Antwort, 3, PosETX - 3);  // Datum und Zeit enthalten
          MRGDateStr:=MRGTimeStr;
        end;

      112, 117:
        begin    { Actaris Sparklog, Kamstrup UNIGAS 300 }
          { Antwort auf Kommando f�r "Aktuelle Zeit abrufen" auswerten: }
          if not ValidIEC1107DatenTelegrammAntwort (R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + R.Fehlergruppe, R.Fehlercode);
            exit;
          end;
          MRGTimeStr:=ExtractString (R.Antwort, '(', ')', 0);  { Zeit steht zwischen den runden Klammern }
          { Aktuelles Zeit kann im Format ZS7 oder Z6 vorliegen. Wir verwenden Z6: }
          if length (MRGTimeStr) > length (FMrgDefData.Zeitformat) then
            MRGTimeStr:=Copy (MRGTimeStr, length (MRGTimeStr) - length (FMrgDefData.Zeitformat) + 1,
                              length (FMrgDefData.Zeitformat));
        end;

      115:
        begin    { Elster DS-100 }
            { Antwort auf z-Befehl auswerten: }
            if not KonvDS100Param (R.Antwort, 'z', MRGDateStr) then begin
              FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + ST_KONVERROR, SKERR_PARAMKONV);
              exit;
            end;
            MRGTimeStr:=Copy (MRGDateStr, 7, 6);
            MRGDateStr:=Copy (MRGDateStr, 1, 6);
        end;

    end;  { case }
  end;

  { aktuelle MRG-Zeit bereitstellen, wenn vorhanden:
    -> Datum- und Zeit-String in DateTime konvertieren (ger�teabh�ngig !)
    -> Korrektur wegen �bertragungslaufzeit }
  if (length (MRGTimeStr) > 0) AND (length (MRGDateStr) > 0) then
    MRGDateTime:=MRGStrToDateTime(FMrgDefData.Datumformat, MRGDateStr, FMrgDefData.Zeitformat, MRGTimeStr) +
                 EncodeTime (0, 0, 0, C_KorrGetMRGZeit)
  else
    MRGDateTime:=0;  // MRG-Datum/Zeit nicht vorhanden

  { MRG-Zeit und PC-Zeit: R�ckgabe in ZeitSyncInfoData }
  with ZeitSyncInfoData do begin
    DZ_Server:=PCDateTime;  // PC-Zeit mit f�r MRG eingerechneten Offsets (SZ/WZ, Zeitzone)
    DZ_Geraet:=MRGDateTime;
  end;

  if MRGDateTime > 0 then begin  // nur, wenn MRG-Datum/Zeit vorhanden
    { Abweichung zwischen MRG-Zeit und PC-Zeit ermitteln: }
    F_TimeDiff(PCDateTime, MRGDateTime, DiffSec);

    { ZeitSync nur, wenn Betrag der Abweichung >= ZeitSyncMin, ansonsten alles OK
      (innerhalb Toleranz) und fertig: }
    if Abs(DiffSec) < ZeitSyncCmdData.Abweichung_min then exit;

    { ZeitSync nur, wenn Betrag der Abweichung <= ZeitSyncMax: }
    if Abs(DiffSec) > ZeitSyncCmdData.Abweichung_max then begin
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_HIGHERMAX);
      exit;
    end;

    { Abweichung liegt innerhalb des Zeitfensters -> es mu� synchronisiert werden }
  end;

  { Synchronisieren geht nur mit Pa�wort 1:
    -> Einschr�nkung gilt nicht bei:
       - MRG 800, 800 PTB, 905, 910
       - RMG EC 900
       - Tritschler VC2, TDS, VC3, VCC
       - Actaris Corus, Sparklog
       - RMG EC 900
       - Elster DS-100
       - Elster DL 230, EK 280 (Mitteilung, Test Sopra: auch mit Datenausleserschloss m�glich); 05.10.2022, WW
       - Kamstrup UNIGAS 300
       - RMG Primus 400, Prilog 400
       - RMG TME400, RSM200
       - SICK FLOWSIC500 }
  if (FMrgDefData.Kommandogruppe <> 5) AND
     (FMrgDefData.Kommandogruppe <> 6) AND
     (FMrgDefData.Kommandogruppe <> 106) AND
     (FMrgDefData.Kommandogruppe <> 110) AND
     (FMrgDefData.Kommandogruppe <> 111) AND
     (FMrgDefData.Kommandogruppe <> 112) AND
     (FMrgDefData.Kommandogruppe <> 115) AND
     (FMrgDefData.Kommandogruppe <> 116) AND
     (FMrgDefData.Kommandogruppe <> 117) AND
     (FMrgDefData.Kommandogruppe <> 118) AND  // 05.10.2022, WW
     (FMrgDefData.Kommandogruppe <> 119) AND  // 05.10.2022, WW
     (FMrgDefData.Kommandogruppe <> 120) AND
     (FMrgDefData.Kommandogruppe <> 121) AND
     (FMrgDefData.Kommandogruppe <> 122) then begin
    if VerbAufbau_PasswortNr <> 1 then begin
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_WRONGPASSWORDNR);
      exit;
    end;
  end;

  { keine ZeitSynch zum Zeitpunkt der Umstellung von Sommer- auf Winterzeit wegen
    doppelt vorhandener Stunde: }
  DecodeTime (Now, PCHour, dummy1, dummy2, dummy3);
  DecodeTime (NextSZToWZ, NextSZToWZHour, dummy1, dummy2, dummy3);
  if (Int (PCDateTime) = Int (NextSZToWZ)) AND (PCHour = (NextSZToWZHour-1)) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_SZTOWZ);
    exit;
  end;

  { neue MRG-Zeit bereitstellen:
    -> Anpassung der zu sendenden PC-Zeit wegen �bertragungslaufzeit
    -> in NewMRGDateTime ist Stundenoffset ber�cksichtigt �ber PCDateTime (SZ/WZ, Zeitzone)
    -> Zeit-Korrektur nur m�glich, wenn aktuelle Ger�tezeit vorhanden ist, ansonsten
       wird synchronisiert; 21.11.2008, WW }
  if (KorrekturMax > 0) AND (MRGDateTime > 0) then begin
    if Abs(DiffSec) <= KorrekturMax then begin  { Zeitdifferenz Ger�t/PC ist kleiner als die Korrekturweite }
      NewMRGDateTime:=PCDateTime + EncodeTime (0, 0, 0, C_KorrSetMRGZeit);  { PC-Zeit zum Synchronisieren }

      // F�r Ger�te, welche per UTC-Zeit synchronisiert werden:
      // Synchronisations-Zeit auf UTC r�ckgerechnet und ohne die f�r die lokale
      // MRG-Zeit eingerechneten Offsets (SZ/WZ, Zeitzone); 29.09.2021, WW
      NewMRGDateTime_UTC:=NewMRGDateTime - dtPCDateTimeOffsetUTC -
                          dtWZ_SZ_Offset - dtDevicePC_TimezoneDiff;
    end
    else begin
      { mit Korrekturweite beaufschlagte Ger�tezeit zum Korrigieren: }
      if DiffSec > 0 then  { PCDateTime < MRGDateTime }
        iKorrSeconds:=KorrekturMax * (-1)
      else  { PCDateTime > MRGDateTime }
        iKorrSeconds:=KorrekturMax;
      NewMRGDateTime:=IncSecond (MRGDateTime, iKorrSeconds);

      // F�r Ger�te, welche per UTC-Zeit korrigiert werden:
      // Mit Korrekturweite beaufschlagte UTC-Ger�tezeit zum Korrigieren; 29.09.2021, WW
      NewMRGDateTime_UTC:=IncSecond (MRGDateTime_UTC, iKorrSeconds);

      // Bugfix ZeitSync-Info f�r Server-Zeit (Soll-Zeit der Korrektur); 05.10.2022, WW
      ZeitSyncInfoData.DZ_Server:=NewMRGDateTime;
    end;
  end
  else begin
    NewMRGDateTime:=PCDateTime + EncodeTime (0, 0, 0, C_KorrSetMRGZeit);  { PC-Zeit zum Synchronisieren }

    // F�r Ger�te, welche per UTC-Zeit synchronisiert werden:
    // Synchronisations-Zeit auf UTC r�ckgerechnet und ohne die f�r die lokale
    // MRG-Zeit eingerechneten Offsets (SZ/WZ, Zeitzone); 29.09.2021, WW
    NewMRGDateTime_UTC:=NewMRGDateTime - dtPCDateTimeOffsetUTC -
                        dtWZ_SZ_Offset - dtDevicePC_TimezoneDiff;
  end;

  if MRGDateTime > 0 then begin  // nur, wenn MRG-Datum/Zeit vorhanden
    { ZeitSync/Korrektur nur bei gleichem Datum und gleicher Stunde von alter und
      neuer MRG-Zeit: }
    DecodeTime(MRGDateTime, MRGHour, dummy1, dummy2, dummy3);
    DecodeTime(NewMRGDateTime, NewMRGHour, dummy1, dummy2, dummy3);
    if not ((int (MRGDateTime) = int (NewMRGDateTime)) AND (MRGHour = NewMRGHour)) then begin
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_DIFFERENTHOURS);
      exit;
    end;

    { alte und neue MRG-Zeit m�ssen den Sicherheitsabstand zum n�chsten
      MRG-Stundenwechsel einhalten: }
    Naechste_volle_Stunde_DateTime:=int (MRGDateTime) +
                                    EncodeTime(MRGHour, 0, 0, 0) +
                                    EncodeTime(1, 0, 0, 0);
    if MRGDateTime < NewMRGDateTime then
      F_TimeDiff(NewMRGDateTime, Naechste_volle_Stunde_DateTime, DiffSec_StdWechsel)
    else
      F_TimeDiff(MRGDateTime, Naechste_volle_Stunde_DateTime, DiffSec_StdWechsel);

    { ZeitSync nur bei gen�gend Sicherheitsabstand zum n�chsten Stundenwechsel: }
    if DiffSec_StdWechsel <= C_Sicherheitsabstand then begin
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_PERIODEND);
      exit;
    end;
  end;

  // Befehlsstring f�r Parametrierung der neuen Zeit bilden; neue Zeit in Ger�t �bertragen
  // und Erfolg der Parametrierung pr�fen:
  ZeitSync_erfolgreich:=true;
  if FMrgDefData.Kommandogruppe < 100 then begin  { standardm��ige ZeitSync (Wieser-Ger�te) }
    MRGTimeStr:=FormatMRGTime(FMrgDefData.Zeitformat, NewMRGDateTime);   { ger�teabh�ngig ! }

    if length (ZeitParaNrMrg_Einst) > 0 then  // �ndern der Ger�tezeit �ber eigenen Einstell-Parameter
      ZeitParaNrMRG:=ZeitParaNrMrg_Einst;

    Befehl:=GetMRGKommando_C (ZeitParaNrMRG, MRGTimeStr);
    { Kommando f�r "Zeit �bertragen" senden }
    if CommObj is TMRGCustomCommObj then
      TMRGCustomCommObj (CommObj).SetLaengeParaNr (length (ZeitParaNrMRG));  // L�nge der Parameternummer setzen; 29.02.2012, WW
    if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Parametrieren,
                                ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Antwort auf "Zeit �bertragen"-Kommando auswerten: }
    if not ValidMRGAntwort ('C' + ZeitParaNrMRG, FMrgDefData.ModemAbrufgruppe, R.Antwort,
                            R.Fehlergruppe, R.Fehlercode) then begin
      if (R.Fehlergruppe = COM_MRGERROR) AND
         ((R.Fehlercode = MRGERR_AENDERUNGNICHTZULAESSIG) OR (R.Fehlercode = MRGERR_KEINEBERECHTIGUNG)) then
        FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_NOSUCCESS)
      else
        FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Pr�fen, ob MRG-Zeit tats�chlich ge�ndert wurde: }
    NeuWert:=MRGTimeStr;  // 27.03.2009, WW
    if not MRGParameter_geaendert (R.Antwort, AltWert, NeuWert) then begin
      if Abs(DiffSec) > 1 then
        ZeitSync_erfolgreich:=false;
      { bei DiffSec = 1 liefert das Ger�t evtl. Altwert = Neuwert. Das ist in diesem Fall
        aber kein Fehler, da wegen unvermeidbarer Ungenauigkeiten evtl. versucht wurde,
        die aktuelle Ger�tezeit wieder zu parametrieren. }
    end;
  end
  else begin
    { Fremdger�te: }
    case FMrgDefData.Kommandogruppe of
      101, 102, 108, 113, 118, 119:
        begin    { Elster DL210, DL220, DL230, DL240, EK260, EK280 }
          MRGTimeStr:=FormatMRGDate(FMrgDefData.Datumformat, NewMRGDateTime) + ',' +
                      FormatMRGTime(FMrgDefData.Zeitformat, NewMRGDateTime);

          Befehl:=GetLIS200Kommando_Schreiben ('1:400', MRGTimeStr);
          { Flag setzen, es folgt Antwort ohne BCC }
          TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off);
          { Kommando f�r "Datum/Zeit �bertragen" senden }
          if not CommObj.SendCommand (Befehl, [ACK, NAK, ETX], 1, MRGTimeouts.IEC1107Telegr,
                                      ad_String, R, NoCarrier) then begin
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
            exit;
          end;

          { Antwort auf "Datum/Zeit �bertragen"-Kommando auswerten:
            -> pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
          if R.Antwort <> ACK then
            ZeitSync_erfolgreich:=false;
        end;

      106, 110, 116:
        begin     { Tritschler VC2, TDS, VC3, VCC }
          MRGTimeStr:=DateTimeToUTCStr (NewMRGDateTime, SZ_WZ_Umstellung_im_Geraet);

          if Pos (srv_modem_SSU, VerbAufbau_ModemTyp) > 0 then begin  { mit Multiplexer SSU }
            Befehl:=GetTritschler_IECKommando_20_ZeitSync (MRGTimeStr,
              SSU_GeraeteAuswNr, VerbAufbau_Passwort);
            bWecken:=false;  { mit Multiplexer SSU kein Wecken des Ger�ts erforderlich }
          end
          else begin
            Befehl:=GetTritschler_IECKommando_20_ZeitSync (MRGTimeStr, 0, '');
            bWecken:=true;
          end;
          { Zeitsynchronisations-Befehl senden: }
          if not TMRGCustomCommObj (CommObj).SendTritschlerIECCommand
            (Befehl, ad_String, MRGTimeouts.TritschlerIECProt, FMrgDefData.MrgTyp,
             bWecken, R, NoCarrier) then begin
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
            exit;
          end;

          { Antwort auf Zeitsynchronisations-Befehl auswerten:
            -> pr�fen, ob Zeit im Ger�t tats�chlich ge�ndert wurde }
          ValidTritschler_IECAntwort_20_ZeitSync (R.Antwort, AFehlergruppe,
            AFehlercode, dtBuf);
          { R�ckgabe Ger�tezeit vor der Synchronisation, wenn vorhanden; 28.06.2018, WW }
          if dtBuf > 0 then
            ZeitSyncInfoData.DZ_Geraet:=dtBuf;

          // "Bereits synchron" nicht als Fehlergruppe/-code zur�ckgeben (stattdessen
          // Default 0, 0 wie bei "Abweichung < min. Abweichung"):
          if not ((AFehlergruppe = EST_ZEITSYNCERROR) AND
                  (AFehlercode = ZSYNCERR_BEREITSSYNCHRON)) then
            FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);

          exit;  // Raus, ohne die Standard-Auswertung des "ZeitSync_erfolgreich"-Flags am Prozedur-Ende
        end;

      111:
        begin    { Actaris Corus }
          MRGTimeStr:=Date2Bin_Corus (NewMRGDateTime);

          Befehl:=GetCorusSAMKommando_ParameterSchreiben (106, MRGTimeStr);
          { Flag setzen, es folgt Antwort ohne CRC }
          TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off);
          { Kommando f�r "Datum/Zeit �bertragen" senden }
          if not CommObj.SendCommand (Befehl, [ACK, NAK], 1, MRGTimeouts.CorusSAMProt,
                                      ad_String, R, NoCarrier) then begin
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
            exit;
          end;

          { Antwort auf "Datum/Zeit �bertragen"-Kommando auswerten:
            -> pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
          if R.Antwort <> ACK then
            ZeitSync_erfolgreich:=false;
        end;

      112, 117:
        begin    { Actaris Sparklog, Kamstrup UNIGAS 300 }
          { Zeitformat Z6: }
          MRGTimeStr:=FormatMRGTime(FMrgDefData.Zeitformat, NewMRGDateTime);
          if FMrgDefData.Kommandogruppe = 117 then
            MRGTimeStr:=MRGTZStr + MRGTimeStr;  // Kamstrup UNIGAS 300 fest mit DS7/ZS7-Format

          Befehl:=GetIEC1107Kommando_Schreiben ('0.9.1', MRGTimeStr, VerbAufbau_Passwort);
          { Flag setzen, es folgt Antwort ohne BCC }
          TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off);
          { Kommando f�r "Aktuelle Zeit �bertragen" senden }
          if not CommObj.SendCommand (Befehl, [ACK, NAK, ETX], 1, MRGTimeouts.IEC1107Telegr,
                                      ad_String, R, NoCarrier) then begin
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
            exit;
          end;

          { Antwort auf "Aktuelle Zeit �bertragen"-Kommando auswerten:
            -> pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
          if R.Antwort <> ACK then
            ZeitSync_erfolgreich:=false;
        end;

      115:
        begin    { Elster DS-100 }
          // Zutrittscode mu� gesendet werden
          if not TMRGCustomCommObj (CommObj).SetDS100Parameter ('c', VerbAufbau_Passwort,
            MRGTimeouts.ElsterDS100Prot, ZeitSync_erfolgreich, R, NoCarrier) then begin
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
            exit;
          end;

          if ZeitSync_erfolgreich then begin
            // Ger�te-Zeit setzen:
            MRGTimeStr:=FormatMRGDate(FMrgDefData.Datumformat, NewMRGDateTime) +
                        FormatMRGTime(FMrgDefData.Zeitformat, NewMRGDateTime);
            if not TMRGCustomCommObj (CommObj).SetDS100Parameter ('{', MRGTimeStr,
              MRGTimeouts.ElsterDS100Prot, ZeitSync_erfolgreich, R, NoCarrier) then begin
              FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
              exit;
            end;
          end;
        end;

      120:
        begin    { Primus, Prilog }
          // Modbus-Parameter "Ger�tezeit" einstellen:

          { aus der allgemeinen Parameternummer die ger�tespezifische ermitteln: }
          if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                                CP_RMG_PrimusPrilog_Sommerzeit,
                                                                ParamMrgData) then begin  // 11.02.2022, WW
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR + ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
            exit;
          end;

          with MBRegisterDef do begin  // 11.02.2022, WW
            StartAdresse:=StrToIntDef (ParamMrgData.Parameternummer_im_MRG, 0);
            Typ:=ParamMrgData.ParaDatentyp;
            AnzahlBytes:=ParamMrgData.ParaByteLen;
          end;

          MRGTimeStr:=DateTimeToStr (NewMRGDateTime);

          if not WriteModbusParameter_PrimusPrilog (MBRegisterDef.StartAdresse,
                                                    MBRegisterDef.Typ,
                                                    MBRegisterDef.AnzahlBytes,
                                                    MRGTimeStr,   
                                                    bOK, EST_ZEITSYNCERROR) then exit;

          { Pr�fen, ob Ger�tezeit im Ger�t tats�chlich ge�ndert wurde }
          if not bOK then
            ZeitSync_erfolgreich:=false;
        end;

      121:
        begin    { TME400, RSM200 }
          // Modbus-Parameter "Ger�te-Uhrzeit" einstellen:
          // TME400:
          if FMrgDefData.MrgTyp in [mrgtyp_TME400_VCF, mrgtyp_TME400_VMF] then
            MBRegisterDef:=TME400_MBRegisterDef_Para_Uhrzeit
          // RSM200:
          else if FMrgDefData.MrgTyp in [mrgtyp_RSM200_VCF, mrgtyp_RSM200_VMF] then  // 09.01.2024, WW
            MBRegisterDef:=RSM200_MBRegisterDef_Para_Uhrzeit;

          MRGTimeStr:=FormatMRGTime ('HHMMSS', NewMRGDateTime);

          if not WriteModbusParameter_TME400 (FMrgDefData.MrgTyp,
                                              MBRegisterDef.StartAdresse,
                                              MBRegisterDef.Typ,
                                              MBRegisterDef.AnzahlBytes,
                                              MRGTimeStr,
                                              bOK, EST_ZEITSYNCERROR) then exit;

          { Pr�fen, ob Ger�tezeit im Ger�t tats�chlich ge�ndert wurde }
          if not bOK then
            ZeitSync_erfolgreich:=false;

          { RSM200: Die ge�nderte Ger�tezeit mu� zus�tzlich best�tigt werden
            (sonst schreibt das Ger�t keine Geht-Meldung 'Uhr ung�ltig' ins
            Ereignisarchiv); 09.01.2024, WW }
          if FMrgDefData.MrgTyp in [mrgtyp_RSM200_VCF, mrgtyp_RSM200_VMF] then begin
            MBRegisterDef:=RSM200_MBRegisterDef_Para_ZeitDatumBestaetigen;

            if not WriteModbusParameter_TME400 (FMrgDefData.MrgTyp,
                                                MBRegisterDef.StartAdresse,
                                                MBRegisterDef.Typ,
                                                MBRegisterDef.AnzahlBytes,
                                                '1',
                                                bOK, EST_ZEITSYNCERROR) then exit;

            { Pr�fen, ob die Best�tigung der Ger�tezeit im Ger�t erfolgreich war }
            if not bOK then begin
              FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);    
              exit;
            end;
          end;
        end;

      122:
        begin    { FLOWSIC500 }
          // Modbus-Parameter "UTC-Referenzzeit f�r Synchronisation" einstellen:
          MBRegisterDef:=FLOWSIC500_MBRegisterDef_Para_SyncReferenzZeit;
          MRGTimeStr:=DateTimeToStr (NewMRGDateTime_UTC);

          if not WriteModbusParameter_SICK (MBRegisterDef.StartAdresse,
                                            MBRegisterDef.Typ,
                                            MBRegisterDef.AnzahlBytes,
                                            MRGTimeStr,
                                            bOK, EST_ZEITSYNCERROR) then exit;

          { Pr�fen, ob Zeit im Ger�t tats�chlich ge�ndert wurde }
          if not bOK then
            ZeitSync_erfolgreich:=false;
        end;

    end;  { case }
  end;

  if ZeitSync_erfolgreich then begin
    if KorrekturMax > 0 then  { es wurde nur korrigiert: Warnung 'Korrektur' }
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_CORRMAX)
    else  { es wurde synchronisiert: OK }
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_SUCCESS);
  end else
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_NOSUCCESS);
end;

{-----------------------------------------------}
function TMRGAbruf.ResetRundpuffer_Meld: boolean;
{-----------------------------------------------}
{ Rundpuffer f�r Meldungen im MRG zur�cksetzen;
  Ergebnis: true, wenn Rundpufferreset f�r Meldungen erfolgreich }
var
  R: TRueckgabe;
  Befehl: string;

Begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (EST_RPRESET_MEERROR+COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  { Pr�fen, ob lt. MrgDef-Eintrag bei vorliegendem MRG-Typ ein Rundpufferreset
    durchgef�hrt werden kann: }
  if not FMrgDefData.RpReset then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_MEERROR+SYS_ABRUFERROR, SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT);
    exit;
  end;

  { Rundpuffer zur�cksetzen nur mit Pa�wort 1: }
  if VerbAufbau_PasswortNr <> 1 then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_MEERROR, RPRESETERR_WRONGPASSWORDNR);
    exit;
  end;

  Befehl := STX + 'W' + ETX;

  { Kommando f�r Rundpufferreset senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.RundpufferReset, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_MEERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Kommando f�r Rundpufferreset auswerten: }
  if not ValidMRGAntwort ('W', FMrgDefData.ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_ANTWORTUNERWARTET) then
      FehlerGruppeCodeUpdate (EST_RPRESET_MEERROR, RPRESETERR_NOSUCCESS)
    else
      FehlerGruppeCodeUpdate (EST_RPRESET_MEERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  Result:=true;
End;

{-----------------------------------------------}
function TMRGAbruf.ResetRundpuffer_Mess: boolean;
{-----------------------------------------------}
{ Rundpuffer f�r Me�werte im MRG zur�cksetzen;
  Ergebnis: true, wenn Rundpufferreset f�r Messwerte erfolgreich }
var
  R: TRueckgabe;
  Befehl: string;

Begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (EST_RPRESET_MWERROR+COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  { Pr�fen, ob lt. MrgDef-Eintrag bei vorliegendem MRG-Typ ein Rundpufferreset
    durchgef�hrt werden kann: }
  if not FMrgDefData.RpReset then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_MWERROR+SYS_ABRUFERROR, SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT);
    exit;
  end;

  { Rundpuffer zur�cksetzen nur mit Pa�wort 1: }
  if VerbAufbau_PasswortNr <> 1 then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_MWERROR, RPRESETERR_WRONGPASSWORDNR);
    exit;
  end;

  Befehl := STX + 'T' + ETX;

  { Kommando f�r Rundpufferreset senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.RundpufferReset, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_MWERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Kommando f�r Rundpufferreset auswerten: }
  if not ValidMRGAntwort ('T', FMrgDefData.ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_ANTWORTUNERWARTET) then
      FehlerGruppeCodeUpdate (EST_RPRESET_MWERROR, RPRESETERR_NOSUCCESS)
    else
      FehlerGruppeCodeUpdate (EST_RPRESET_MWERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  Result:=true;
End;

{------------------------------------------------}
function TMRGAbruf.ResetRundpuffer_Pruef: boolean;
{------------------------------------------------}
{ Rundpuffer f�r Pr�fungss�tze im MRG zur�cksetzen;
  Ergebnis: true, wenn Rundpufferreset f�r Pr�fungss�tze erfolgreich }
var
  R: TRueckgabe;
  Befehl: string;

Begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (EST_RPRESET_PRERROR+COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  { Pr�fen, ob lt. MrgDef-Eintrag bei vorliegendem MRG-Typ ein Rundpufferreset
    durchgef�hrt werden kann: }
  if not FMrgDefData.RpReset then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_PRERROR+SYS_ABRUFERROR, SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT);
    exit;
  end;

  { Rundpuffer zur�cksetzen nur mit Pa�wort 1: }
  if VerbAufbau_PasswortNr <> 1 then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_PRERROR, RPRESETERR_WRONGPASSWORDNR);
    exit;
  end;

  Befehl := STX + 'P' + ETX;

  { Kommando f�r Rundpufferreset senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.RundpufferReset, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_PRERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Kommando f�r Rundpufferreset auswerten: }
  if not ValidMRGAntwort ('P', FMrgDefData.ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_ANTWORTUNERWARTET) then
      FehlerGruppeCodeUpdate (EST_RPRESET_PRERROR, RPRESETERR_NOSUCCESS)
    else
      FehlerGruppeCodeUpdate (EST_RPRESET_PRERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  Result:=true;
End;

{------------------------------------------------------------------------------------------------------}
function TMRGAbruf.UebertragungParameter (ParaEinstellCmdData: TParaEinstellCmdData;
                                          var ParaEinstellResultData: TParaEinstellResultData): boolean;
{------------------------------------------------------------------------------------------------------}
{ Parameter ins MRG �bertragen;
  �bergabe: Parametrier-Kommandodaten
  R�ckgabe: Parametrier-Ergebnisdaten
  Ergebnis: true, wenn Parametrierung erfolgreich }
Var
  Befehl: string;
  R: TRueckgabe;
  AWertAlt, AWertNeu: string;
  AWertNeu_Roh: string;
  AParaNrAllg: string;
  ParaNrMrg: string;
  csMerk: TChecksum;
  OK: boolean;
  PL: TParameterListe;
  bOK: boolean;
  iKanalNr: integer;
  cBefehlcodeLesen: char;
  cBefehlcodeSchreiben: char;
  iParaNrMrg: integer;
  Code: integer;
  iParaDatentyp: integer;
  iParaAnzahl: integer;
  iParaByteLen: integer;
  ParamMrgData: TParamMrgData;
  bNochmal: boolean;
  iVersuch: integer;
  sWertNeu_Befehl: string;
  iAbrufGrpMerk: integer;
  AParaPasswort: string;
  AParaPasswortNr: integer;
  sParamEK: string;
  ParaAdrMrg: string;

Begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }
  with ParaEinstellResultData do begin  { Vorbelegung R�ckgabe }
    ParaTyp:=ParaEinstellCmdData.ParaTyp;  // 03.01.2022, WW
    BAdr:='';  // nicht verwendet bei MRG
    ParaAdr:='';
    WertAlt:='';
    WertNeu:='';
  end;

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (EST_PARAMERROR+COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  { Pr�fen, ob MRG �berhaupt parametriert werden kann:
    -> VC2, VC3, VCC erst ab 30.04.2019; lt. Nutzungsvertrag nur f�r Wico22/GM-T,
       nicht f�r Gas-X-Version ! }
  if (FMrgDefData.MrgTyp = mrgtyp_KE_Ruhrgas) OR
     (FMrgDefData.MrgTyp = mrgtyp_KE_PPN) OR
     (FMrgDefData.MrgTyp = mrgtyp_FWU) OR
     (FMrgDefData.MrgTyp = mrgtyp_TTG_IEC) OR
     (FMrgDefData.MrgTyp = mrgtyp_TTG_FTL) OR
     (FMrgDefData.MrgTyp = mrgtyp_TDS) OR
     (FMrgDefData.MrgTyp = mrgtyp_MCO) OR
     (FMrgDefData.MrgTyp = mrgtyp_MC2) OR
     (FMrgDefData.MrgTyp = mrgtyp_SR) OR
     (FMrgDefData.MrgTyp = mrgtyp_EC694) OR  // m�glich, aber nicht realisiert
     (FMrgDefData.MrgTyp = mrgtyp_Veribox_Mini)
{$IFDEF GAS-X}
     OR
     (FMrgDefData.MrgTyp = mrgtyp_VC2) OR
     (FMrgDefData.MrgTyp = mrgtyp_VC3_VC2komp) OR
     (FMrgDefData.MrgTyp = mrgtyp_VC3) OR
     (FMrgDefData.MrgTyp = mrgtyp_VCC)
{$ENDIF}
  then begin
    FehlerGruppeCodeUpdate (SYS_ABRUFERROR, SYSABRFERR_KOMMANDOUNGUELTIG);
    exit;
  end;

  AParaNrAllg:=ParaEinstellCmdData.ParaAdr;
  AWertNeu:=ParaEinstellCmdData.ParaWertNeu;
  { Optional erforderlich f�r bestimmte Ger�tetypen (derzeit VC2, VC3, VCC); 30.04.2019, WW }
  AParaPasswort:=ParaEinstellCmdData.ZCode1;  // ZCode1 -> (Parametrier-)Passwort
  AParaPasswortNr:=StrToIntDef (ParaEinstellCmdData.ZCode2, -1);  // ZCode2 -> (Parametrier-)Passwortnummer

  { Parameternummern-Konfigurationsdaten zur allgemeinen Parameternummer ermitteln
    (ger�tespezifische Parameternummer, Parameter-Datentyp etc.) }
  if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                        AParaNrAllg,
                                                        ParamMrgData) then begin  // 09.07.2021, WW
    FehlerGruppeCodeUpdate (EST_PARAMERROR+ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
    exit;
  end;

  ParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;
  ParaAdrMrg:=ParamMrgData.Parameteradresse_im_MRG;  // 18.02.2021, WW
  iParaDatentyp:=StrToIntDef (ParamMrgData.ParaDatentyp, -1);  // 08.03.2019, WW
  iParaAnzahl:=ParamMrgData.ParaAnzahl;
  iParaByteLen:=ParamMrgData.ParaByteLen;

  if FMrgDefData.Kommandogruppe < 100 then begin  { standardm��ige Parametrierung (Wieser-Ger�te) }
    sWertNeu_Befehl:=AWertNeu;

    bNochmal:=true;
    iVersuch:=1;
    while bNochmal do begin
      bNochmal:=false;  { Standard: nur einmal }

      Befehl:=GetMRGKommando_C (ParaNrMrg, sWertNeu_Befehl);
      { Kommando f�r "Parameter �bertragen" senden }
      if CommObj is TMRGCustomCommObj then
        TMRGCustomCommObj (CommObj).SetLaengeParaNr (length (ParaNrMRG));  // L�nge der Parameternummer setzen; 29.02.2012, WW
      if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.Parametrieren,
                                    ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);
        exit;
      end;

      { Antwort auf "Parameter �bertragen"-Kommando auswerten: }
      if not ValidMRGAntwort ('C' + ParaNrMrg, FMrgDefData.ModemAbrufgruppe, R.Antwort,
                              R.Fehlergruppe, R.Fehlercode) then begin
        FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);
        exit;
      end;

      { Pr�fen, ob Parameter im MRG tats�chlich ge�ndert wurde:
        -> Funktion liefert aus der Rohantwort den alten und neuen Wert zur�ck }
      if not MRGParameter_geaendert (R.Antwort, AWertAlt, AWertNeu) then begin
        if (length (sWertNeu_Befehl) < length (AWertAlt)) AND
           (iVersuch <= 1) then begin
          // Nochmal probieren mit modifiziertem neuen Parameterwert: mit
          // abschlie�enden Spaces (f�r Parameterwerte mit fester L�nge); 16.05.2014, WW
          sWertNeu_Befehl:=F_RightPad (sWertNeu_Befehl, ' ', length (AWertAlt));

          bNochmal:=true;
          inc (iVersuch);
        end else
          FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
      end else
        Result:=true;
    end;  { while bNochmal }
  end
  else begin
    { Fremdger�te: }
    case FMrgDefData.Kommandogruppe of
      101, 102, 108, 113, 118, 119,   { Elster DL210, DL220, DL230, DL240, EK260, EK280 }
      112, 117:               { Actaris Sparklog, Kamstrup UNIGAS 300 }
        begin
          { zuerst aktuellen Parameterwert auslesen: }
          PL:=TParameterListe.Create (FKonfigPath);
          try
            if not AbrufParameter (AParaNrAllg, nil, PL) then exit;
            PL.GetValue (AParaNrAllg, AWertAlt);
          finally
            PL.Free;
          end;

          { jetzt neuen Wert parametrieren: }
          if (FMrgDefData.Kommandogruppe = 112) OR      { Actaris Sparklog }
             (FMrgDefData.Kommandogruppe = 117) then    { Kamstrup UNIGAS 300 }
            Befehl:=GetIEC1107Kommando_Schreiben (ParaNrMrg, AWertNeu, VerbAufbau_Passwort)
          else begin  { Elster DL210, DL220, DL230, DL240, EK260, EK280 }
            { Bei EK260 und EK280 pr�fen, ob zu allgemeiner Parameternummer,
              Version und K-Zahl Modus eine Elster EK-spezifische Parameternummer
              definiert ist. Falls ja, wird diese f�r die Parameterabfrage
              verwendet: 30.04.2019, WW }
            if (FMrgDefData.Kommandogruppe = 101) OR
               (FMrgDefData.Kommandogruppe = 118) then begin
              { EK-spezifische Parameternummern-Konfigurationsliste des MRG anlegen und aus
                Ressourcedatenliste laden, wenn noch nicht erfolgt: 06.08.2021, WW }
              if not Assigned (FParamEKKonfigList) then begin
                FParamEKKonfigList:=TParamEKKonfigList.Create;

                if not Load_MRGResourceData_ParamEK then exit;  // 06.08.2021, WW
              end;

              if FParamEKKonfigList.FindParaNrMrg (
                   FMrgKonvData.ParameterGruppe, AParaNrAllg,
                   ElsterEK_Version, ElsterEK_K_Zahl_Modus, sParamEK) then begin
                // EK-spezifische Parameternummer gefunden
                ParaNrMrg:=sParamEK;
                if length (ParaNrMrg) = 0 then begin
                  // Parameter nicht verf�gbar
                  FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOTAVAILABLE);
                  exit;
                end;
              end;
            end;

            // Ggf. Wandlung Hex-kodierter Elster-Ersatzzeichen im neuen Wert; 17.10.2023, WW
            AWertNeu_Roh:=AWertNeu;
            if ParameterListe_MRGAbruf.Ersatzzeichen then
              WAsciiToElsterHex (AWertNeu_Roh);

            Befehl:=GetLIS200Kommando_Schreiben (ParaNrMrg, AWertNeu_Roh);
          end;

          csMerk:=TMRGCustomCommObj (CommObj).GetReceiveWithChecksum;  { Receice-Flag merken }
          TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off); { Flag setzen, es folgt Antwort ohne BCC }

          { Kommando f�r "Parameter �bertragen" senden }
          OK:=CommObj.SendCommand (Befehl, [ACK, NAK, ETX], 1, MRGTimeouts.IEC1107Telegr, ad_String, R, NoCarrier);
          TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (csMerk); { Receive-Flag auf urspr�nglichen Wert wieder zur�cksetzen }
          if not OK then begin
            FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);
            exit;
          end;

          { Antwort auf "Parameter �bertragen"-Kommando auswerten:
            -> pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
          if R.Antwort <> ACK then begin
            FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
            AWertNeu:=AWertAlt;  // R�ckgabe "neuer Wert = alter Wert" bei erfolgloser Parametrierung
          end else
            Result:=true;
        end;

      106, 116:
        begin  { Tritschler VC2, VC3, VCC; 30.04.2019, WW }
          { Neuen Wert parametrieren oder Stell-Befehl ausf�hren: }
          { -> Es wird das FTL-interne Protokoll mit eigenen ger�tespezifischen
               Parameternummern (Blocknummern) zum Schreiben von Parametern verwendet }
          if (length (ParamMrgData.Parameternummer_im_MRG_Schreiben) > 0) then begin
            { Es sind nur Blocknummern von 0..999 erlaubt (3-stellig): }
            Val (ParamMrgData.Parameternummer_im_MRG_Schreiben, iParaNrMrg, Code);
            if (Code = 0) AND (iParaNrMrg >= 0) AND (iParaNrMrg <= 999) then begin
              { Aktuell gesetztes Protokoll merken �ber Abrufgruppe: }
              iAbrufGrpMerk:=TMRGCustomCommObj (CommObj).Abrufgruppe;

              { Neues Protokoll setzen �ber Abrufgruppe: FTL-intern }
              TMRGCustomCommObj (CommObj).SetAbrufgruppe (15);
              try
                { Ger�te-Kommunikation f�r internes FTL-Protokoll einleiten: }
                if not Init_FTLintern_Kommunikation (EST_PARAMERROR) then exit;

                { Passwort-Befehl f�r Parametrierberechtigung senden: }
                Befehl:=GetTritschler_FTLinternKommando_WritePasswort (
                  AParaPasswort, AParaPasswortNr);
                if not TMRGCustomCommObj (CommObj).SendTritschlerFTL_internCommand
                  (Befehl, MRGTimeouts.TritschlerFTLProt, R, NoCarrier) then begin
                  FehlerGruppeCodeUpdate (EST_PARAMERROR + R.Fehlergruppe, R.Fehlercode);
                  exit;
                end;

                if (ParamMrgData.ParaDatentyp = '') then  // 24.08.2020, WW
                  AWertNeu_Roh:=''  // f�r Stell-Befehle ohne Daten (z.B. Meldeliste l�schen, Blocknr. 007)
                else begin
                  { Neuen einzustellenden Wert (Anzeigeformat) in FTL-intern-Rohformat wandeln: }
                  if not FormatPara_FTLintern_Roh (AWertNeu, ParamMrgData.ParaDatentyp,
                                                   AWertNeu_Roh) then begin
                    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_FORMATWERT);
                    exit;
                  end;
                end;

                { Befehl f�r "Parameter �bertragen" senden }
                Befehl:=GetTritschler_FTLinternKommando (iParaNrMrg, AWertNeu_Roh, 'S');
                if not TMRGCustomCommObj (CommObj).SendTritschlerFTL_internCommand
                  (Befehl, MRGTimeouts.TritschlerFTLProt, R, NoCarrier) then begin
                  FehlerGruppeCodeUpdate (EST_PARAMERROR + R.Fehlergruppe, R.Fehlercode);
                  exit;
                end;

                if (ParamMrgData.ParaDatentyp = '') then  // 24.08.2020, WW
                  AWertNeu:='OK';  // Kennzeichnung, da� Stell-Befehl erfolgreich ausgef�hrt wurde 

                Result:=true;
              finally
                { Bisheriges Protokoll wieder setzen �ber gemerkte Abrufgruppe: }
                TMRGCustomCommObj (CommObj).SetAbrufgruppe (iAbrufGrpMerk, MRG_DPS);
                { Flag f�r Start der Protokoll-Reinitialisierungsphase setzen: }
                TMRGCustomCommObj (CommObj).ReInitProtokoll:=true;
              end;
            end
            else begin
              FehlerGruppeCodeUpdate (EST_PARAMERROR+ST_KONFIGERROR, KFERR_KONFIGDATAINVALID);
              exit;
            end;
          end
          else begin
            FehlerGruppeCodeUpdate (EST_PARAMERROR+ST_KONFIGERROR, KFERR_KONFIGDATAINVALID);
            exit;
          end;
        end;

      111:
        begin    { Actaris Corus }
          { zuerst aktuellen Parameterwert auslesen: }
          PL:=TParameterListe.Create (FKonfigPath);
          try
            if not AbrufParameter (AParaNrAllg, nil, PL) then exit;
            PL.GetValue (AParaNrAllg, AWertAlt);
          finally
            PL.Free;
          end;

          { jetzt neuen Wert parametrieren: }
          // Wenn f�r den Parameter keine spezifische Bytel�nge konfiguriert
          // ist: Standard-Bytel�nge des Parameter-Datentyps verwenden
          if iParaByteLen <= -1 then
            iParaByteLen:=PDT_ByteLength (iParaDatentyp);

          if (length (ParaNrMrg) > 0) AND
             (iParaByteLen > -1) AND (iParaAnzahl > -1) then begin  // 14.08.2012, WW
            { Befehl bilden (es sind nur Parameternummern von 0..255 erlaubt): }
            Val (ParaNrMrg, iParaNrMrg, Code);
            if (Code = 0) AND (iParaNrMrg >= 0) AND (iParaNrMrg <= MaxByte) then begin
              // Neuen Wert (Anzeigeformat) in Corus-Rohformat wandeln:
              if not FormatPara_Corus_Roh (AWertNeu, iParaDatentyp, iParaAnzahl,
                                           iParaByteLen, AWertNeu_Roh) then begin
                FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_FORMATWERT);
                exit;
              end;

              Befehl:=GetCorusSAMKommando_ParameterSchreiben (iParaNrMrg, AWertNeu_Roh);
              csMerk:=TMRGCustomCommObj (CommObj).GetReceiveWithChecksum;  { Receice-Flag merken }
              TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (cs_Off); { Flag setzen, es folgt Antwort ohne CRC }

              { Kommando f�r "Parameter �bertragen" senden }
              OK:=CommObj.SendCommand (Befehl, [ACK, NAK], 1, MRGTimeouts.CorusSAMProt,
                                       ad_String, R, NoCarrier);
              TMRGCustomCommObj (CommObj).SetReceiveWithChecksum (csMerk); { Receive-Flag auf urspr�nglichen Wert wieder zur�cksetzen }
              if not OK then begin
                FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);
                exit;
              end;

              { Antwort auf "Parameter �bertragen"-Kommando auswerten:
                -> pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
              if R.Antwort <> ACK then begin
                FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
                AWertNeu:=AWertAlt;  // R�ckgabe "neuer Wert = alter Wert" bei erfolgloser Parametrierung
              end else
                Result:=true;
            end
            else begin
              FehlerGruppeCodeUpdate (EST_PARAMERROR+ST_KONFIGERROR, KFERR_KONFIGDATAINVALID);
              exit;
            end;
          end
          else begin
            FehlerGruppeCodeUpdate (EST_PARAMERROR+ST_KONFIGERROR, KFERR_KONFIGDATAINVALID);
            exit;
          end;
        end;

      115:
        begin    { Elster DS-100 }
          { MRG-spezifische Parameternummer beim DS-100:
              1.Zeichen = Lese-Befehlscode des Parameters
              2. Zeichen = Kanalnummer }
          if length (ParaNrMrg) < 2 then begin
            FehlerGruppeCodeUpdate (EST_PARAMERROR+ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
            exit;
          end;

          cBefehlcodeLesen:=ParaNrMrg [1];
          case cBefehlcodeLesen of
            'l': cBefehlcodeSchreiben:='m'; // Ger�tenummer
            'h': cBefehlcodeSchreiben:='i'; // Me�stellennumer
            'j': cBefehlcodeSchreiben:='k'; // Z�hlernummer
            'p': cBefehlcodeSchreiben:='q'; // Intervall
            'n': cBefehlcodeSchreiben:='o'; // Impulsfaktor (cp-Wert)
            't': cBefehlcodeSchreiben:='u'; // aktueller setzbarer Z�hlerstand
            'z': cBefehlcodeSchreiben:='{'; // Datum/Zeit
          else
            cBefehlcodeSchreiben:=NUL;
          end;

          if cBefehlcodeSchreiben = NUL then begin  // Parameter kann nicht ge�ndert werden
            FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
            exit;
          end;

          iKanalNr:=StrToIntDef (ParaNrMrg [2], -1);
          if (iKanalNr < 1) OR (iKanalNr > MRGAnzKanaele) then begin
            FehlerGruppeCodeUpdate (EST_PARAMERROR+ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
            exit;
          end;

          { Auf Kanal schalten, wenn erforderlich: }
          if iKanalNr <> FAktKanal then begin
            Set_KanalDS100 (iKanalNr);
            FParametrierModus_gesetzt:=false;  // bei Kanalwechsel mu� Zutrittscode neu gesendet werden
          end;

          // Zutrittscode mu� je Kanal einmal gesendet werden
          if not FParametrierModus_gesetzt then begin
            if not TMRGCustomCommObj (CommObj).SetDS100Parameter ('c', VerbAufbau_Passwort,
              MRGTimeouts.ElsterDS100Prot, bOK, R, NoCarrier) then begin
              FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);
              exit;
            end;
          end else
            bOK:=true;

          if bOK then begin
            FParametrierModus_gesetzt:=true;  // Flag setzen: Zutrittscode erfolgreich gesendet

            // Parameter setzen:
            if not TMRGCustomCommObj (CommObj).SetDS100Parameter (cBefehlcodeSchreiben, AWertNeu,
              MRGTimeouts.ElsterDS100Prot, bOK, R, NoCarrier) then begin
              FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);  // Fehlergruppe korrigiert; 01.02.2012, WW
              exit;
            end;
          end;

          if not bOK then begin
            FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
            AWertNeu:=AWertAlt;  // R�ckgabe "neuer Wert = alter Wert" bei erfolgloser Parametrierung
          end else
            Result:=true;
        end;

      120,  { Primus, Prilog }
      121,  { TME400, RSM200 }
      122:  { FLOWSIC500 }
        begin  { Modbus }
          { zuerst aktuellen Parameterwert auslesen: }
          PL:=TParameterListe.Create (FKonfigPath);
          try
            if not AbrufParameter (AParaNrAllg, nil, PL) then exit;
            PL.GetValue (AParaNrAllg, AWertAlt);
          finally
            PL.Free;
          end;

          { jetzt neuen Wert parametrieren: }
          if (length (ParaNrMrg) > 0) AND
             (iParaByteLen > -1) AND (length (ParamMrgData.ParaDatentyp) > 0) then begin
            // Wenn eine MRG-Parameteradresse definiert ist, wird diese als
            // MB-Registeradresse verwendet (Kriterium 1), ansonsten die
            // definierte MRG-Parameternummer (Kriterium 2):
            if ParaAdrMrg <> '' then
              ParaNrMrg:=ParaAdrMrg;  // 18.02.2021, WW

            { Parameternummer mu� Word-Ganzzahl sein (Modbus-Registeradresse): }
            Val (ParaNrMrg, iParaNrMrg, Code);
            if (Code = 0) AND (iParaNrMrg >= 0) AND (iParaNrMrg <= MaxWord) then begin
              // Modbus-Parameter einstellen:
              case FMrgDefData.MrgTyp of
                mrgtyp_Primus,
                mrgtyp_Prilog:
                  begin
                    if not WriteModbusParameter_PrimusPrilog (
                      iParaNrMrg, ParamMrgData.ParaDatentyp, iParaByteLen, AWertNeu,
                      bOK, EST_PARAMERROR) then exit;
                  end;

                mrgtyp_TME400_VCF,
                mrgtyp_TME400_VMF,
                mrgtyp_RSM200_VCF,  // 09.01.2024, WW
                mrgtyp_RSM200_VMF:   
                  begin
                    if not WriteModbusParameter_TME400 (FMrgDefData.MrgTyp,
                      iParaNrMrg, ParamMrgData.ParaDatentyp, iParaByteLen, AWertNeu,
                      bOK, EST_PARAMERROR) then exit;
                  end;

                mrgtyp_FLOWSIC500:  // 29.09.2021, WW
                  begin
                    if not WriteModbusParameter_SICK (
                      iParaNrMrg, ParamMrgData.ParaDatentyp, iParaByteLen, AWertNeu,
                      bOK, EST_PARAMERROR) then exit;
                  end;
              else
                bOK:=false;
              end;

              { Pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
              if not bOK then begin
                FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
                AWertNeu:=AWertAlt;  // R�ckgabe "neuer Wert = alter Wert" bei erfolgloser Parametrierung
              end else
                Result:=true;
            end
            else begin
              FehlerGruppeCodeUpdate (EST_PARAMERROR+ST_KONFIGERROR, KFERR_KONFIGDATAINVALID);
              exit;
            end;
          end
          else begin
            FehlerGruppeCodeUpdate (EST_PARAMERROR+ST_KONFIGERROR, KFERR_KONFIGDATAINVALID);
            exit;
          end;
        end; 

    end;  { case }
  end;

  { Ergebnis der erfolgreichen Parametrierung: }
  with ParaEinstellResultData do begin
    ParaAdr:=AParaNrAllg;
    WertAlt:=AWertAlt;
    WertNeu:=AWertNeu;
  end;
End;

{---------------------------------------------------------------------------}
function TMRGAbruf.AbrufTransparent (TransparentCmdData: TTransparentCmdData;
                                     var TransparentAntw: string): boolean;
{---------------------------------------------------------------------------}
{ Senden eines beliebigen Befehls an MRG, Empfangen der Antwort;
  �bergabe: Transparentbefehl
  R�ckgabe: Rohantwort auf Transparentbefehl
  Ergebnis: true, wenn Transparentbefehl-Abruf ok }
Var
  R: TRueckgabe;
  TO_Transparent: integer;

begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }
  TransparentAntw:='';             { Vorbelegung R�ckgabe }

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  { Timeout f�r Transparent-Kommando: }
  if TransparentCmdData.Timeout >= 0 then
    TO_Transparent:=TransparentCmdData.Timeout  // aus Transparent-Kommandodaten
  else
    TO_Transparent:=MRGTimeouts.Binaerdatei;  // INI-Einstellung

  if (FMrgDefData.Kommandogruppe > 0) AND
     (FMrgDefData.Kommandogruppe < 100) then begin   { Transparent-Abruf f�r Wieser-Ger�te }
    { Transparent-Kommando senden }
    if not CommObj.SendCommand (TransparentCmdData.Befehl, [ETX], 1, TO_Transparent,
                                  ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Antwort auf Transparent-Kommando auswerten: }
    if not ValidMRGAntwort ('', FMrgDefData.ModemAbrufgruppe, R.Antwort,
                            R.Fehlergruppe, R.Fehlercode) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    TransparentAntw:=R.Antwort;   { R�ckgabe: Rohantwort }
  end
  else begin  { Fremdger�te -> Transparent-Abruf bislang nicht implementiert }
    FehlerGruppeCodeUpdate (SYS_ABRUFERROR, SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT);
    exit;
  end;
  Result:=true;
end;

{-----------------------------------------------------------------------------}
function TMRGAbruf.RufEntgegennehmen_Fup (var dtVerbSteht: TDateTime): boolean;
{-----------------------------------------------------------------------------}
{ angekommenen Ruf per FUP entgegennehmen;
  R�ckgabe: Zeitpunkt 'Verbindung steht'
  Ergebnis: true, wenn Rufentgegennahme ok }
var
  Befehl: string;
  R: TRueckgabe;

Begin
  Result:=false;

  Befehl:=ESC+'z'+CR;
  { Rufannahmekommando senden, es kommt keine Antwort ! }
  if not CommObj.SendCommand (Befehl, [CR], 1, 0, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Ruf wurde entgegengenommen, Verbindung steht jetzt: }
  dtVerbSteht:=Now;  // Zeitpunkt 'Verbindung hergestellt'; 20.12.2013, WW
  NoCarrier:=false;
  Result:=true;
end;

{-------------------------------------------------------------------------------}
function TMRGAbruf.RufEntgegennehmen_Modem (RufTyp: integer;
                                            var dtVerbSteht: TDateTime): boolean;
{-------------------------------------------------------------------------------}
{ angekommenen Ruf per Modem entgegennehmen;
  �bergabe: RufTyp (MRG-Modem-Ger�te oder MRG-FUP-Ger�te)
  R�ckgabe: Zeitpunkt 'Verbindung steht'
  Ergebnis: true, wenn Rufannahme ok }
var
  Befehl: string;
  R: TRueckgabe;

Begin
  if CommObj is TMRGModemCommObj then begin   { logischerweise nur bei serieller DF�-Kommunikation }
    Result:=false;
    TMRGModemCommObj (CommObj).SetDCDCheck (false);      { DCD-�berwachung aus }

    { Schnittstellen-Parameter und Datenprotokoll f�r Rufannahme-Befehl sind bereits durch
      die letzte Modeminitialisierung festgelegt }

    Befehl:='ata' + CR;
    { Rufannahmekommando senden: }
    if not TMRGModemCommObj (CommObj).SendModemCommand (Befehl, MRGTimeouts.RufAnnahmeModem, R) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Antwort auf Rufannahme-Kommando auswerten: }
    if not CheckModemConnect (R.Antwort) then exit;

    { Ruf wurde entgegengenommen, Verbindung steht jetzt: }
    dtVerbSteht:=Now;  // Zeitpunkt 'Verbindung hergestellt'; 20.12.2013, WW
    NoCarrier:=false;
    TMRGModemCommObj (CommObj).SetDCDCheck (true);       { DCD-�berwachung ein }
    Delay (200);       { Wartezeit (generell empfehlenswert, n�tig f�r MRG 910) }

    if RufTyp = re_MRG_Modem then begin  { Anruf eines Modem-Ger�ts annehmen }
      { Schnittstellen-Parameter und Datenprotokoll f�r die nachfolgenden
        MRG-Befehle f�r Abrufgruppe 4 (MRG 910 !) aktivieren: }
      TMRGCustomCommObj (CommObj).SetAbrufgruppe (4);
      {  pr�fen, ob MRG-Kommunikation mit CRC erfolgen mu�: }
      if not TMRGCustomCommObj (CommObj).MRG_CRCKonfig_Check (R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end
    else if RufTyp = re_MRG_FTL_SR then begin  { Anruf eines Tritschler Stationsrechners annehmen }
      { Schnittstellen-Parameter und Datenprotokoll f�r die nachfolgenden
        Ger�te-Befehle f�r Abrufgruppe 8 (Tritschler IEC) aktivieren: }
      TMRGCustomCommObj (CommObj).SetAbrufgruppe (8);
    end
    else begin  { Anruf eines FUP-Ger�ts annehmen }
      { Schnittstellen-Parameter und Datenprotokoll f�r die nachfolgenden
        MRG-Befehle nach ACK01-Protokoll aktivieren: }
     TMRGCustomCommObj (CommObj).SetAbrufgruppe (1);

      { vom Ger�t gesendetes ENQ entgegennehmen: }
      if not CommObj.SendCommand ('', [CR], 1, MRGTimeouts.ACK01_ProtMeldung, ad_String,
                                    R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;

      { bei Ger�ten der Modem-Abrufgruppe 1 mu� die Zentrale das Senderecht haben: }
      { ENQ senden: }
      Befehl:='*' + ENQ + CR;
      if not CommObj.SendCommand (Befehl, [CR], 1, MRGTimeouts.ACK01_ProtMeldung, ad_String,
                                    R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end;
  end;
  Result:=true;
end;

{-----------------------------------------------------------------------------------}
function TMRGAbruf.RufEntgegennahme (RufTyp: integer; var StationsKennungRet: string;
                                     var GeraetetypRet: integer;
                                     MeldungsListe: TMeldungsliste;
                                     var VerbInfoData: TVerbInfoData): boolean;
{-----------------------------------------------------------------------------------}
{ angekommenen Ruf entgegennehmen, Kennung abfragen;
  �bergabe: RufTyp f�r Modem-Rufannahme
            Zeiger auf Meldungsliste
  R�ckgabe: Kennung der rufenden Station
            Ger�tetyp der rufenden Station (nur bei Anruf eines Tritschler Stationsrechners)
            Verbindungsinformationen-Record
  Ergebnis: true, wenn Rufentgegennahme ok }
var
  OK: boolean;

Begin
  Result:=false;
  { R�ckgabe-Vorbelegung f�r Verbindungsinformationen: }
  with VerbInfoData do begin  // Keine Verbindungsinformationen vorhanden
    DZ_VerbindungSteht:=0;
    DZ_Login:=0;
  end;

  NoCarrier:=true;                 { Vorbelegung: es besteht keine Verbindung }
  FehlerGruppeCodeUpdate (0, 0, true);         { Vorbelegung Fehlergruppe/-code: OK }
  GeraetetypRet:=-1;  // Vorbelegung: unbekannt
  StationsKennungRet:='';
  try
    if isFupAbruf then
      OK:=RufEntgegennehmen_Fup (VerbInfoData.DZ_VerbindungSteht)
    else
      OK:=RufEntgegennehmen_Modem (RufTyp, VerbInfoData.DZ_VerbindungSteht);
    if not OK then exit;

    if RufTyp = re_MRG_FTL_SR then begin  { Anruf eines Tritschler Stationsrechners }
      { -> Lizenzfreischaltung: global �ber Programm-Funktion "Rufentgegennahme-MRG",
           keine ger�tetypspezifische Freischaltung (notwendig) }
      GeraetetypRet:=mrgtyp_SR;  // Ger�tetyp fest zugewiesen (SSU kompatibel zu SR)

      { allgemeine Konfigurationsdaten f�r Ger�tetyp initialisieren: }
      if not Init_MRGKonfigData (GeraeteTypRet) then exit;

      { Login mit Quittungsmodus 0: Anfordern des SSU-Status/Alarmursache
        -> bei ankommendem Ruf mit Standardpasswort 000000 m�glich
        -> Meldungss�tze werden in Meldungsliste konvertiert
        -> kein Kennungsvergleich/-bewertung (wird in Abruf-Client gemacht }
      if not PasswortLogin_Tritschler_IEC_SSU ('000000', '0', MeldungsListe,
        '', false) then exit;

      { Verbindung beenden: }
      if not VerbAbbau then exit;
    end
    else begin  { Anruf aller �brigen Ger�te (Wieser) }
      if not KennungAbfragen (-1, '', '', RufTyp, false) then exit;  { �bergaben f�r Rufentgegennahme per FUP und Modem }
    end;
  finally
    StationsKennungRet:=StationsKennung;
  end;
  Result:=true;
End;

{-----------------------------------------------------------------------}
function TMRGAbruf.Rufannahme (RufannahmeCmdData: TRufannahmeCmdData;
                               var VerbInfoData: TVerbInfoData): boolean;
{-----------------------------------------------------------------------}
{ eingegangenen MRG-Ruf annehmen (Login durchf�hren);
  �bergabe: Rufannahme-Kommandodaten
  R�ckgabe: Verbindungsinformationen-Record
  Ergebnis: true, wenn Rufannahme erfolgreich }
begin
  Result:=false;

  { R�ckgabe-Vorbelegung f�r Verbindungsinformationen: }
  with VerbInfoData do begin  // Keine Verbindungsinformationen vorhanden
    DZ_VerbindungSteht:=0;
    DZ_Login:=0;
  end;

  NoCarrier:=false;                { Vorbelegung: Verbindung besteht bereits }
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }

  { Passwort-Nummer und Passwort merken f�r sp�tere Kommandos (z.B. Zeitsynchronisation,
    Abfragebefehle Tritschler mit Multiplexer SSU): }
  VerbAufbau_PasswortNr:=RufannahmeCmdData.PasswortNr;
  VerbAufbau_Passwort:=RufannahmeCmdData.Passwort;

  { allgemeine Konfigurationsdaten f�r Ger�tetyp initialisieren: }
  if not Init_MRGKonfigData (RufannahmeCmdData.GeraeteTyp) then exit;

  { Login: }
  if isFupAbruf then begin
    if not PasswortLogin (FMrgDefData.ModemAbrufgruppe, RufannahmeCmdData.Passwort,
                          RufannahmeCmdData.PasswortNr) then exit;  { Pa�wort �bertragen }
  end
  else begin
    TMRGCustomCommObj (CommObj).SetPasswort (RufannahmeCmdData.Passwort);
    case FMrgDefData.ModemAbrufgruppe of
      1, 2: if not PasswortLogin (FMrgDefData.ModemAbrufgruppe, RufannahmeCmdData.Passwort,
                                  RufannahmeCmdData.PasswortNr) then exit; { Pa�wort �bertragen }
      3, 4, 12:
        begin
          { wenn Kennung-Befehl mit Passwort erfolgreich gesendet wurde,
            dann "Login" erfolgreich: }
//              if TMRGCustomCommObj (CommObj).GetWithPasswort then begin
//                ZustandMessage (CommObj.COMPort, RufStammDaten.MrgId, z_LoginErfolgt, '', WithZustandTabelle);
//                UpDateMRGJournal (JournalId, C_WJournal_DZLoggedIn);
//              end;
        end;
      { ModemAbrufgruppe 5: hier kein Login ("Login" erfolgt in Methode 'Init_IEC1107_Kommunikation'
                            mit dem �ffnen des Kunden- bzw. Lieferantenschlosses)
                      7, 9: kein Login erforderlich/m�glich }
    end;
  end;
  VerbInfoData.DZ_Login:=Now;  // Zeitpunkt 'Login erfolgt'; 20.12.2013, WW
  Result:=true;
end;

{ Wert eines Parameters ermitteln                   }
{ �bergabe: Allgemeine Parameternummer              }
{           Neu auslesen aus Ger�t (optional)       }
{ R�ckgabe: Parameterwert                           }
{ Ergebnis: Erfolg ja / nein                        }  // 13.01.2011, GD
{---------------------------------------------------}
function TMRGAbruf.GetParamAllg(const sParam: string; var sVal: string;
  bReread: boolean = false): boolean;
{---------------------------------------------------}
var
  pPL  : TParameterListe;
begin
  if bReread then begin  // 18.02.2021, WW
    // es soll neu gelesen werden                       
    sVal:='';
    Result:=false;
  end else
    // pr�fen, ob Wert bereits in der Parameterliste enthalten ist:
    Result := ParameterListe_MRGAbruf.GetValue(sParam, sVal);

  if (not Result) then begin
    // wenn nicht, aus Ger�t auslesen:
    pPL := TParameterListe.Create(FKonfigPath);
    try
      if (not AbrufParameter(sParam, nil, pPL))
      then
      Exit;  // Wert abrufen
    finally
      pPL.Free;
    end;
    // abgerufenen Parameter aus Parameterliste lesen:
    Result := ParameterListe_MRGAbruf.GetValue(sParam, sVal);
  end;
end;

{ Zeitinformationen eines Wieser-Ger�tes abrufen    }
{ R�ckgaben: Datum/Zeit, Zeitzone (W,S),            }
{            zus�tzliche UTC-Infos                  }
{ Ergebnis: Erfolg ja / nein                        }
{---------------------------------------------------}
function TMRGAbruf.ZeitAbruf_Wieser(var dtDateTime: TDateTime;
  var sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
{---------------------------------------------------}
var
  s, sTime, sDate : string;
begin
  try
    if (GetParamAllg(CP_ALLG_Zeit, sTime)) and
       (GetParamAllg(CP_ALLG_Datum, sDate)) then
    begin
      // Datum / Zeit ist Pflicht, der Rest optional
      dtDateTime := MRGStrToDateTime(
        FMrgDefData.Datumformat, sDate, FMrgDefData.Zeitformat, sTime);
      sTimeZone := '';
      sTimeInfos := '';

      // Zeitzoneneinstellung abrufbar
      if (GetParamAllg(CP_ALLG_SZWZ_Modus, s)) then begin
        // Zeitzoneneinstellung aktiv?
        if (Trim(s) = '1') then begin
          // Zeitpunkt der letzten Umstellung holen
          if (GetParamAllg(CP_ALLG_SZWZ_LastChange, s)) then begin
            s := Trim(s);
            // G�ltige L�nge?
            if (Length(s) >= 10) then begin
              s := Copy(s, 3, 2);  // Format ddmmyyhhnn... => Monat
              if (StrToIntDef(s, 0) = 3)
              then sTimeZone := 'S'
              else if (StrToIntDef(s, 0) = 10) then sTimeZone := 'W';
            end;
          end;
        end
        else if (Trim(s) = '0') then sTimeZone := 'W';
      end;

      // Wenn bis hierher nix passierte, ist das Ergebnis OK;
      Result := True;
    end
    else Result := False;  // Ohne Datum / Zeit ist das Ergebnis immer falsch
  except
    Fehlergruppe := EST_ZEITBEFEHLERROR;  // 13.02.2012, WW
    Fehlercode := ZEITBEFERR_ZEITLESEN;
    Result := False;
  end;
end;

{ Zeitinformationen eines EC900-Ger�tes abrufen     }
{ R�ckgaben: Datum/Zeit, Zeitzone (W,S),            }
{            zus�tzliche UTC-Infos                  }
{ Ergebnis: Erfolg ja / nein                        }
{---------------------------------------------------}
function TMRGAbruf.ZeitAbruf_EC900(var dtDateTime: TDateTime;
  var sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
{---------------------------------------------------}
var
  s, sDate : string;
begin
  try
    if (GetParamAllg(CP_RMG_EC900_Systemzeit, sDate)) then begin
      // Datum / Zeit ist Pflicht, der Rest optional
      dtDateTime := MRGStrToDateTime(
        FMrgDefData.Datumformat, sDate, FMrgDefData.Zeitformat, sDate);
      sTimeZone := '';
      sTimeInfos := '';

      // Zeitzoneneinstellung abrufbar
      if (GetParamAllg(CP_RMG_EC900_TimeZone, s)) then begin
        sTimeZone := Trim(s);
        if (UpperCase(sTimeZone) = 'M') then sTimeZone := 'W';  // Gax-X-Forderung
      end;
      // UTC-Differenz abrufbar?
      if ((GetParamAllg(CP_RMG_EC900_DiffToUTC, s)) or
          (GetParamAllg(CP_RMG_EC900_DiffToUTC2, s))) and
        (StrToIntDef(Trim(s), -9999) > -9000) then
      begin
        sTimeInfos := 'UTC+' + FormatFloat('00.00', (StrToInt(Trim(s)) / 60));
      end;

      // Wenn bis hierher nix passierte, ist das Ergebnis OK;
      Result := True;
    end
    else Result := False;  // Ohne Datum / Zeit ist das Ergebnis immer falsch
  except
    Fehlergruppe := EST_ZEITBEFEHLERROR;  // 13.02.2012, WW
    Fehlercode := ZEITBEFERR_ZEITLESEN;
    Result := False;
  end;
end;

{ Zeitinformationen eines Elster-Ger�tes abrufen    }
{ R�ckgaben: Datum/Zeit, Zeitzone (W,S),            }
{            zus�tzliche UTC-Infos                  }
{ Ergebnis: Erfolg ja / nein                        }
{---------------------------------------------------}
function TMRGAbruf.ZeitAbruf_Elster(var dtDateTime: TDateTime;
  var sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
{---------------------------------------------------}
var
  sDate, sTime : string;
begin
  try
    if (GetParamAllg(CP_ELS_DatumZeit, sTime)) then begin
      // Datum / Zeit ist Pflicht, der Rest optional
      sDate := F_Zerlegen(sTime, ',');
      dtDateTime := MRGStrToDateTime(
        FMrgDefData.Datumformat, sDate, FMrgDefData.Zeitformat, sTime);

      // Zeitzoneneinstellung / UTC-Differenz nicht bekannt
      sTimeZone := '';
      sTimeInfos := '';

      // Wenn bis hierher nix passierte, ist das Ergebnis OK;
      Result := True;
    end
    else Result := False;  // Ohne Datum / Zeit ist das Ergebnis immer falsch
  except
    Fehlergruppe := EST_ZEITBEFEHLERROR;  // 13.02.2012, WW
    Fehlercode := ZEITBEFERR_ZEITLESEN;
    Result := False;
  end;
end;

{ Zeitinformationen eines VC2, VC3, VCC-Ger�tes abrufen }
{ R�ckgaben: Datum/Zeit, Zeitzone (W,S),            }
{            zus�tzliche UTC-Infos                  }
{ Ergebnis: Erfolg ja / nein                        }
{---------------------------------------------------}
function TMRGAbruf.ZeitAbruf_Tritschler(var dtDateTime: TDateTime;
  var sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
{---------------------------------------------------}
var
  s, sDate, sTime : string;
begin
  try
    if (GetParamAllg(CP_FTL_IEC_Zeit, sTime)) and
       (GetParamAllg(CP_FTL_IEC_Datum, sDate)) then
    begin
      // Datum / Zeit ist Pflicht, der Rest optional
      dtDateTime := MRGStrToDateTime('JJ/MM/TT', sDate, 'HH:MM:SS', sTime);
      sTimeZone := '';
      sTimeInfos := '';

      // Zeitzoneneinstellung abrufbar
      if (GetParamAllg(CP_FTL_TimeZone, s)) then begin
        if (Trim(s) = '0') then sTimeZone := 'W'
        else if (Trim(s) = '1') then sTimeZone := 'S';
      end;
      // UTC-Differenz abrufbar?
      if (GetParamAllg(CP_FTL_TimeUTC, s)) then begin
        sTimeInfos := 'UTC' + Format('%.0d', [StrToIntDef(Trim(s), 0) div 60]);
      end;

      // Wenn bis hierher nix passierte, ist das Ergebnis OK;
      Result := True;
    end
    else Result := False;  // Ohne Datum / Zeit ist das Ergebnis immer falsch
  except
    Fehlergruppe := EST_ZEITBEFEHLERROR;  // 13.02.2012, WW
    Fehlercode := ZEITBEFERR_ZEITLESEN;
    Result := False;
  end;
end;

{ Zeitinformationen eines EC900-Ger�tes abrufen     }
{ R�ckgaben: Datum/Zeit, Zeitzone (W,S),            }
{            zus�tzliche UTC-Infos                  }
{ Ergebnis: Erfolg ja / nein                        }
{---------------------------------------------------}
function TMRGAbruf.ZeitAbruf_Corus(var dtDateTime:
  TDateTime; var sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
{---------------------------------------------------}
var
  s, sDate, sTime : string;
  dtSZ, dtWZ      : TDateTime;
begin
  try
    if (GetParamAllg(CP_ACT_CORUS_DateTime, s)) then begin
      sDate := Trim(GetStringPart(s, 1, ' '));
      sTime := Trim(GetStringPart(s, 2, ' '));
      // Datum / Zeit ist Pflicht, der Rest optional
      dtDateTime := MRGStrToDateTime('TT.MM.JJJJ', sDate, 'HH:MM:SS', sTime);
      sTimeZone := '';
      sTimeInfos := '';

      // Zeitzoneneinstellung abrufbar
      if (GetParamAllg(CP_ACT_CORUS_SZWZChangeMode, s)) then begin
        if (Trim(s) = '1') or (Trim(s) = '2') then begin  // Automatische Umschaltung
          // N�chste Umschaltungen abrufbar?
          if (GetParamAllg(CP_ACT_CORUS_NextSZChange, s)) and
            (GetParamAllg(CP_ACT_CORUS_NextWZChange, sDate)) then
          begin
            dtSZ := MRGStrToDateTime(
              'TT.MM.JJJJ', Trim(GetStringPart(s, 1, ' ')),
              'HH:MM:SS', Trim(GetStringPart(s, 2, ' ')));
            dtWZ := MRGStrToDateTime(
              'TT.MM.JJJJ', Trim(GetStringPart(sDate, 1, ' ')),
              'HH:MM:SS', Trim(GetStringPart(sDate, 2, ' ')));
            if (dtSZ > Now) and (dtSZ < Now + 365) and (dtWZ > Now) and
              (dtWZ < Now + 365) then
            begin
              if (dtSZ < dtWZ) then sTimeZone := 'W' else sTimeZone := 'S';  // Bugfix fehlende Sommerzeit; 24.02.2020, WW
            end;
          end;
        end
        else sTimeZone := 'W';  // Kein Umschaltmodus
      end;

      // Wenn bis hierher nix passierte, ist das Ergebnis OK;
      Result := True;
    end
    else Result := False;  // Ohne Datum / Zeit ist das Ergebnis immer falsch
  except
    Fehlergruppe := EST_ZEITBEFEHLERROR;  // 13.02.2012, WW
    Fehlercode := ZEITBEFERR_ZEITLESEN;
    Result := False;
  end;
end;

{ Zeitinformationen eines Kamstrup UNIGAS 300-Ger�tes abrufen }
{ R�ckgaben: Datum/Zeit, Zeitzone (W,S),            }
{            zus�tzliche UTC-Infos                  }
{ Ergebnis: Erfolg ja / nein                        }
{---------------------------------------------------}
function TMRGAbruf.ZeitAbruf_Kamstrup_UNIGAS300(var dtDateTime: TDateTime;
  var sTimeZone, sTimeInfos: string): boolean;  // 27.03.2013, WW
{---------------------------------------------------}
var
  sDate, sTime : string;
  s: string;

begin
  try
    if (GetParamAllg(CP_KAM_UNIGAS300_Zeit, sTime)) and
       (GetParamAllg(CP_KAM_UNIGAS300_Datum, sDate)) then begin
      // Datum / Zeit ist Pflicht, der Rest optional
      sTimeZone := '';
      sTimeInfos := '';

      // Aktuelle Zeit und Datum liegen im Format ZS7 bzw. DS7 vor (1. Stelle ist
      // aktuelle Zeitzone):
      s:=Copy (sTime, 1, 1);
      if (Trim(s) = '0') then sTimeZone := 'W'
      else if (Trim(s) = '1') then sTimeZone := 'S';

      Delete (sTime, 1, 1);
      Delete (sDate, 1, 1);
      dtDateTime := MRGStrToDateTime(
        FMrgDefData.Datumformat, sDate, FMrgDefData.Zeitformat, sTime);

      // UTC-Differenz nicht bekannt

      // Wenn bis hierher nix passierte, ist das Ergebnis OK;
      Result := True;
    end
    else Result := False;  // Ohne Datum / Zeit ist das Ergebnis immer falsch
  except
    Fehlergruppe := EST_ZEITBEFEHLERROR;
    Fehlercode := ZEITBEFERR_ZEITLESEN;
    Result := False;
  end;
end;

{ Zeitinformationen eines Primus/Prilog 400 abrufen }
{ R�ckgaben: Datum/Zeit, Zeitzone (W,S),            }
{            zus�tzliche UTC-Infos                  }
{ Ergebnis: Erfolg ja / nein                        }
{---------------------------------------------------}
function TMRGAbruf.ZeitAbruf_PrimusPrilog(var dtDateTime: TDateTime;
  var sTimeZone, sTimeInfos: string): boolean;  // 22.07.2019, WW
{---------------------------------------------------}
var
  sSystemZeit, sSommerZeit: string;
  dtSystemZeit: TDateTime;
  iDiff_sec: Int64;
  PL: TParameterListe;

begin
  try
    PL:=TParameterListe.Create (FKonfigPath);
    try
      // Alle Parameter neu abrufen. Wir brauchen die Werte f�r die beiden Parameter
      //  "Systemzeit" und "Sommerzeit" m�glichst zeitgleich !
      if AbrufParameter ('0', nil, PL) then begin
        if (FMrgDefData.MrgTyp = mrgtyp_Primus) OR
           (FMrgDefData.MrgTyp = mrgtyp_Prilog) then begin  // 25.11.2020, WW
          PL.GetValue (CP_RMG_PrimusPrilog_Systemzeit, sSystemZeit);
          PL.GetValue (CP_RMG_PrimusPrilog_Sommerzeit, sSommerZeit);
        end
        else begin
          Result:=false;
          exit;
        end;

        // Datum / Zeit ist Pflicht, der Rest optional
        sTimeZone := '';
        sTimeInfos := '';

        // Aktuelle Ger�tezeit mit Einberechnung eines evtl. vorliegenden Sommerzeit-
        // Offsets ist im Parameter "Sommerzeit" enthalten:
        dtDateTime := MRGStrToDateTime(
          FMrgDefData.Datumformat, '', FMrgDefData.Zeitformat, sSommerZeit);

        // Zeitzone:
        // Aktuelle Ger�tezeit OHNE Einberechnung eines evtl. vorliegenden Sommerzeit-
        // Offsets ist im Parameter "Systemzeit" enthalten:
        dtSystemZeit := MRGStrToDateTime(
          FMrgDefData.Datumformat, '', FMrgDefData.Zeitformat, sSystemZeit);

        iDiff_sec := SecondsBetween (dtSystemZeit, dtDateTime);
        // Toleranz 30s bei Vergleich der Zeitstempel:
        if (iDiff_sec >= -30) AND (iDiff_sec <= 30) then  // ca. gleiche Zeit
          sTimeZone := 'W'
        else if (iDiff_sec >= 3570) AND (iDiff_sec <= 3630) then  // ca. +1h
          sTimeZone := 'S';

        // UTC-Differenz nicht bekannt

        // Wenn bis hierher nix passierte, ist das Ergebnis OK;
        Result := True;
      end
      else Result := False;  // Ohne Datum / Zeit ist das Ergebnis immer falsch
    finally
      PL.Free;
    end;
  except
    Fehlergruppe := EST_ZEITBEFEHLERROR;
    Fehlercode := ZEITBEFERR_ZEITLESEN;
    Result := False;
  end;
end;

{ Zeitinformationen eines TME400 abrufen            }
{ -> auch f�r RSM200                                }
{ �bergabe: Ger�tetyp                               }
{ R�ckgaben: Datum/Zeit, Zeitzone (W,S),            }
{            zus�tzliche UTC-Infos                  }
{ Ergebnis: Erfolg ja / nein                        }
{---------------------------------------------------}
function TMRGAbruf.ZeitAbruf_TME400(AMrgTyp: integer; var dtDateTime: TDateTime;
  var sTimeZone, sTimeInfos: string): boolean;  // 18.02.2021, WW
{---------------------------------------------------}
var
  sParaNrAllg: string;
  sDateTime : string;

begin
  try
    { TME400: }
    if AMrgTyp in [mrgtyp_TME400_VCF, mrgtyp_TME400_VMF] then
      sParaNrAllg:=CP_RMG_TME400_Unixzeit
    { RSM200: }
    else if AMrgTyp in [mrgtyp_RSM200_VCF, mrgtyp_RSM200_VMF] then  // 09.01.2024, WW
      sParaNrAllg:=CP_RMG_RSM200_Unixzeit
    else begin
      Result := false;
      exit;
    end;

    if (GetParamAllg(sParaNrAllg, sDateTime, true)) then begin  // neu abrufen
      // Datum / Zeit ist Pflicht, der Rest optional

      // Aktuelle Zeit und Datum im Unix-Format:
      UnixTimeStrToDateTime (sDateTime, dtDateTime);

      // Zeitzoneneinstellung / UTC-Differenz nicht bekannt
      sTimeZone := '';
      sTimeInfos := '';

      // Wenn bis hierher nix passierte, ist das Ergebnis OK;
      Result := True;
    end
    else Result := False;  // Ohne Datum / Zeit ist das Ergebnis immer falsch
  except
    Fehlergruppe := EST_ZEITBEFEHLERROR;
    Fehlercode := ZEITBEFERR_ZEITLESEN;
    Result := False;
  end;
end;

{ Zeitinformationen eines FLOWSIC500 abrufen        }
{ R�ckgaben: Datum/Zeit, Zeitzone (W,S),            }
{            zus�tzliche UTC-Infos                  }
{ Ergebnis: Erfolg ja / nein                        }
{---------------------------------------------------}
function TMRGAbruf.ZeitAbruf_SICK(var dtDateTime: TDateTime;
  var sTimeZone, sTimeInfos: string): boolean;  // 29.09.2021, WW
{---------------------------------------------------}
var
  s, sDate, sTime : string;
  iStatus: cardinal;
  iZeitzone_Minuten: integer;

begin
  try
    if (GetParamAllg(CP_SICK_FLOWSIC500_Zeit, sTime, true)) and
       (GetParamAllg(CP_SICK_FLOWSIC500_Datum, sDate, true)) then  // neu abrufen
    begin
      // Datum / Zeit ist Pflicht, der Rest optional
      dtDateTime := MRGStrToDateTime(
        FMrgDefData.Datumformat, sDate, FMrgDefData.Zeitformat, sTime);
      sTimeZone := '';
      sTimeInfos := '';

      // Zeitzoneneinstellung abrufbar �ber Ger�testatus
      if (GetParamAllg(CP_SICK_FLOWSIC500_Status, s, true)) then begin  // neu abrufen
        iStatus:=StrToInt64Def ('$' + s, 0);  // Rohwert ist in Hex

        // Bit 4 des Ger�testatus: Sommerzeit aktiv }
        if (iStatus AND $10) = 0 then
          sTimeZone := 'W'
        else
          sTimeZone := 'S';
      end;

      // UTC-Differenz aus Parametern f�r Zeitzone und DST berechnen
      if (GetParamAllg(CP_SICK_FLOWSIC500_Zeitzone_Minuten, s)) then begin  // neu abrufen
        iZeitzone_Minuten:=StrToIntDef (s, 0);  // Zeitzone in minuten
        if sTimeZone = 'S' then
          inc (iZeitzone_Minuten, 60);  // SZ-Offset

        if iZeitzone_Minuten < 0 then
          sTimeInfos:='UTC-'
        else
          sTimeInfos:='UTC+';
        iZeitzone_Minuten:=Abs (iZeitzone_Minuten);
        sTimeInfos := sTimeInfos + Format ('%.2d', [iZeitzone_Minuten DIV 60]) +
          Format ('%.2d', [iZeitzone_Minuten MOD 60]);  // +-hhmm
      end;

      // Wenn bis hierher nix passierte, ist das Ergebnis OK;
      Result := True;
    end
    else Result := False;  // Ohne Datum / Zeit ist das Ergebnis immer falsch
  except
    Fehlergruppe := EST_ZEITBEFEHLERROR;
    Fehlercode := ZEITBEFERR_ZEITLESEN;
    Result := False;
  end;
end;

{ Zeitinformationen des Ger�tes abrufen             }
{ R�ckgaben: Unixzeit, Zeitzone (W,S),              }
{            zus�tzliche UTC-Infos                  }
{ Ergebnis: Erfolg ja / nein                        }
{---------------------------------------------------}
function TMRGAbruf.ZeitAbruf (var sUnixTime, sTimeZone, sTimeInfos: string): boolean;  // 13.01.2011, GD
{---------------------------------------------------}
var
  dtDateTime: TDateTime;
begin
  // Hinweis:
  // Die Zeitinformationen werden f�r die Ger�tetypfamilien Wieser, EC900, Elster,
  // Tritschler, Corus und UNIGAS300 in den entsprechenden Unterfunktionen je
  // Verbindung nur 1 Mal aus dem Ger�t ausgelesen. Weitere ZeitAbruf-Aufrufe
  // liefern dann nur noch die gepufferten Zeitinformationen ohne erneuten
  // Ger�teabruf. Das ist eigentlich nicht wirklich korrekt. Bislang wird darauf
  // verzichtet dies zu korrigieren, da in der Praxis der GAS-X-Client je Verbindung
  // offenbar nur einmal die Zeitinformationen abfr�gt und die Einschr�nkung daher
  // keine Auswirkung hat.

  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK; 13.03.2018, WW }

  if (FMrgDefData.Kommandogruppe < 100) and (FMrgDefData.Kommandogruppe <> 6) then  // Alle Wiesers
    Result := ZeitAbruf_Wieser(dtDateTime, sTimeZone, sTimeInfos)  // Wieser-Ger�te
  else if (FMrgDefData.Kommandogruppe = 6) then  // EC900
    Result := ZeitAbruf_EC900(dtDateTime, sTimeZone, sTimeInfos)
  else if (FMrgDefData.Kommandogruppe in [101, 102, 108, 113, 118, 119]) then  // Elster DL210, DL220, DL230, DL240, EK260, EK280
    Result := ZeitAbruf_Elster(dtDateTime, sTimeZone, sTimeInfos)
  else if (FMrgDefData.Kommandogruppe in [106, 116]) then  // VC2, VC3, VCC
    Result := ZeitAbruf_Tritschler(dtDateTime, sTimeZone, sTimeInfos)
  else if (FMrgDefData.Kommandogruppe in [111]) then  // Corus
    Result := ZeitAbruf_Corus(dtDateTime, sTimeZone, sTimeInfos)
  else if (FMrgDefData.Kommandogruppe in [117]) then  // Kamstrup UNIGAS 300
    Result := ZeitAbruf_Kamstrup_UNIGAS300(dtDateTime, sTimeZone, sTimeInfos)
  else if (FMrgDefData.Kommandogruppe = 120) then  // Primus 400, Prilog 400
    Result := ZeitAbruf_PrimusPrilog(dtDateTime, sTimeZone, sTimeInfos)  // 22.07.2019, WW
  else if (FMrgDefData.Kommandogruppe = 121) then  // TME400, RSM200
    Result := ZeitAbruf_TME400(FMrgDefData.MrgTyp, dtDateTime, sTimeZone, sTimeInfos)  // 18.02.2021, WW
  else if (FMrgDefData.Kommandogruppe = 122) then  // FLOWSIC500
    Result := ZeitAbruf_SICK(dtDateTime, sTimeZone, sTimeInfos)  // 29.09.2021, WW
  else begin
    Fehlergruppe := SYS_ABRUFERROR;
    Fehlercode := SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT;
    Result := False;
  end;

  if (Result) then sUnixTime := IntToHex(DateTimeToUnix(dtDateTime), 8);
end;

{------------------------------------------------------------------}
function TMRGAbruf.RufListeAbfragen (var RufListe: string): boolean;
{------------------------------------------------------------------}
{ Rufanregungsliste abfragen; dieser String (L�nge 14) enth�lt f�r jeden Slave, der
  einen Ruf ausgel�st hat, dessen DSfG-Adresse bzw. 0, wenn der Slave keinen Ruf
  ausgel�st hat;
  R�ckgabe: Ruflisten-String
  Ergebnis: true, wenn Ruflistenabfrage ok }
Var
  R: TRueckgabe;
  Befehl: string;

begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }
  Rufliste:='';                    { Vorbelegung R�ckgabe }

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  Befehl:=STX + '#' + ETX;

  { Ruflistenabfrage-Kommando senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.DSfGRufliste, ad_String,
                                R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Ruflistenabfrage-Kommando auswerten: }
  if not ValidMRGAntwort ('#', FMrgDefData.ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
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

{---------------------------------------------------------------}
function TMRGAbruf.SlaveRufQuittieren (Adresse: string): boolean;
{---------------------------------------------------------------}
{ Slave-Ruf in Rufliste quittieren;
  �bergabe: Slave-Adresse
  Ergebnis: true, wenn Slave-Rufquittierung ok }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  Befehl:=STX + '#' + Adresse + ETX;

  { Rufquittierungs-Kommando senden }
  if not CommObj.SendCommand (Befehl, [ETX], 1, MRGTimeouts.DSfGRufQuittung, ad_String,
                                R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Rufquittierungs-Kommando auswerten: }
  if not ValidMRGAntwort ('#', FMrgDefData.ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_ANTWORTUNERWARTET) then
      FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_RUFQUITTIEREN)
    else
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  Result:=true;
end;

{-----------------------------------------------------------------}
function TMRGAbruf.RueckrufAusloesung (Zentrale: integer): boolean;
{-----------------------------------------------------------------}
{ R�ckruf im MRG ausl�sen;
  �bergabe: Nr. der Zentrale (1 oder 2 bei Wieser MRG)
  Ergebnis: true, wenn R�ckrufausl�sung erfolgreich }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);   { Vorbelegung Fehlergruppe/-code: OK }

  if NoCarrier then begin                  { es besteht keine Verbindung mehr }
    FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_VERB_UNTERBROCHEN);
    exit;
  end;

  if FMrgDefData.ModemAbrufgruppe = 8 then begin  { Tritschler-Ger�te mit IEC-Protokoll: SR }
    { Login mit Quittungsmodus 1: Schalten in Programmiermode }
    if not PasswortLogin_Tritschler_IEC_SSU (VerbAufbau_Passwort, '1', nil, '', false) then exit;

    { Probealarm ausl�sen: }
    Befehl:=SOH+'W2'+STX+'10(5)'+ETX;
    if not CommObj.SendCommand (Befehl, [ACK], 1, MRGTimeouts.TritschlerIECProt,
                                  ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (ST_RUECKRUFERROR + R.Fehlergruppe, R.Fehlercode);
      exit;
    end
    else begin
      if R.Antwort <> ACK then begin  { Probealarm konnte nicht ausgel�st werden }
        FehlerGruppeCodeUpdate (ST_RUECKRUFERROR, RUECKRUFERR_AUSLOESUNG);
        exit;
      end;
    end;

    { Break-Befehl senden, es kommt keine Antwort ! }
    Befehl:=SOH+'B0'+ETX;
    CommObj.SendCommand (Befehl, [], 0, 0, ad_String, R, NoCarrier);
  end
  else begin  { alle �brigen Ger�te (Wieser) }
    if Zentrale = 1 then
      Befehl:='A'
    else
      Befehl:='a';

    { Kommando f�r Rufausl�sung senden }
    if not CommObj.SendCommand (STX + Befehl + ETX, [ETX], 1, MRGTimeouts.Rufausloesung, ad_String,
                                  R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Antwort auf Kommando f�r Rufausl�sung auswerten: }
    if not ValidMRGAntwort (Befehl, FMrgDefData.ModemAbrufgruppe, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
      if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_ANTWORTUNERWARTET) then
        FehlerGruppeCodeUpdate (ST_RUECKRUFERROR, RUECKRUFERR_AUSLOESUNG)
      else
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end;

  Result:=true;
end;

// 14.04.2023, WW
{-------------------------------------}
function TMRGAbruf.VerbHalten: boolean;
{-------------------------------------}
{ Befehl senden, um Verbindung zur Station zu halten;
  Ergebnis: true, wenn Senden des Haltebefehls ok }
Begin
  Result:=false;
  FehlerGruppeCodeUpdate (0, 0, true);         { Vorbelegung Fehlergruppe/-code: OK }

  if FParaNrAllg_VerbHalten <> '' then
    if not AbrufParameter (FParaNrAllg_VerbHalten, nil, nil) then exit;

  Result:=true;
end;


{------------------------- Routinen f�r DS-100 --------------------------------}

{------------------------------------}
procedure TMRGAbruf.Start_SendenDS100;
{------------------------------------}
{ Kommunikation mit DS-100 er�ffnen }
var
  R: TRueckgabe;

begin
  CommObj.SendCommand (STX, [], 0, 0, ad_String, R, NoCarrier);
	Delay (50); // kurz warten, sonst klappt der anschlie�ende erste Befehl nicht
end;

{----------------------------------}
procedure TMRGAbruf.End_SendenDS100;
{----------------------------------}
{ Kommunikation mit DS-100 beenden }
var
  R: TRueckgabe;

begin
  CommObj.SendCommand (ETX, [], 0, 0, ad_String, R, NoCarrier);
	Delay (50); // kurz warten, sonst klappt der anschlie�ende erste Befehl nicht
end;

{--------------------------------------------------------}
function TMRGAbruf.Check_DS100Typ (var Kanalzahl: integer;
  var AktKanal: integer): boolean;
{--------------------------------------------------------}
{ Pr�fen, ob ein Elster DS-100 dranh�ngt und R�ckgabe der Kanalzahl und des
  aktuellen Kanals;
  R�ckgabe: Kanalzahl
            Aktueller Kanal
  Ergebnis: true, wenn DS-100 dranh�ngt }
const
  CTimeout_CheckDS100 = 1000;  // Timeout in ms f�r DS-100 identifizieren

var
	Befehl: string;
  S: string;
  cBefehl: char;
  R: TRueckgabe;

begin
  Result:=false;
  { Vorbelegung R�ckgaben: }
  Kanalzahl:=1;
  AktKanal:=1;

  // Pr�fen, ob DS-100 dranh�ngt:
  Befehl:='?a';  // ?a-Befehl als Testbefehl
  if not TMRGCustomCommObj (CommObj).SendElsterDS100Command (Befehl, ad_String,
    CTimeout_CheckDS100, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  // Anzahl der Kan�le und aktuellen Kanal bestimmen:
  cBefehl:='Y';
  Befehl:=GetDS100_LeseBefehl (cBefehl);
  if TMRGCustomCommObj (CommObj).SendElsterDS100Command (Befehl, ad_String,
    MRGTimeouts.ElsterDS100Prot, R, NoCarrier) then begin
    // wenn das Ger�t den Y-Befehl unterst�tzt ist es ein 4-Kanal-Ger�t !
    Kanalzahl:=4;
    // aktueller Kanal im Ger�t:
    S:=ExtractString (R.Antwort, NUL, '%', 0);
    S:=Copy (S, 2, length (S));
    AktKanal:=StrToInt (S);
    AktKanal:=AktKanal + 1;	// Kanalnummerierung im Ger�t: 0..3
  end;
  Result:=true;
end;

{---------------------------------------}
procedure TMRGAbruf.Naechster_KanalDS100;
{---------------------------------------}
{ Auf n�chsten Kanal im DS-100 weiterschalten	}
begin
  End_SendenDS100;
  Start_SendenDS100;

  { Aktuellen Kanal merken: }
  if MRGAnzKanaele > 1 then begin
    if FAktKanal < MRGAnzKanaele then
      inc (FAktKanal)
    else
      FAktKanal:=1;
  end;
end;

{----------------------------------------------------------}
function TMRGAbruf.Set_KanalDS100 (Kanal: integer): boolean;
{----------------------------------------------------------}
{ Auf bestimmten Kanal im DS-100 schalten;
  �bergabe: Kanal, auf den geschaltet werden soll }
var
  KanalSoll: integer;
  sKanal: string;
  sAktKanal: string;
  cBefehl: char;
  Befehl: string;
  R: TRueckgabe;
  S: string;

begin
  Result:=false;

  KanalSoll:=Kanal;
  if KanalSoll > 4 then
    KanalSoll:=4;	// maximal sind 4 Kan�le m�glich
  sKanal:=Format ('%.2d', [KanalSoll-1]);
  sAktKanal:='';
  while sAktKanal <> sKanal do begin
    Naechster_KanalDS100;

		cBefehl:='Y';
    Befehl:=GetDS100_LeseBefehl (cBefehl);
    if not TMRGCustomCommObj (CommObj).SendElsterDS100Command (Befehl, ad_String,
      MRGTimeouts.ElsterDS100Prot, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    S:=ExtractString (R.Antwort, NUL, '%', 0);
    sAktKanal:=Copy (S, 2, length (S));

    FAktKanal:=StrToInt (sAktKanal) + 1;
	end;  { while sAktKanal <> sKanal }
  Result:=true;
end;

{------------------------------------------------------------------}
function TMRGAbruf.AbfragenPruefen_KennungDS100 (Kanalzahl: integer;
  VerbAufbauCmdData: TVerbAufbauCmdData): boolean;
{------------------------------------------------------------------}
{	DS-100-Kennung lesen und pr�fen;
  �bergabe:	Anzahl der im Ger�t vorhandenen Kan�le
            Verbindungsaufbau-Kommandodaten
  Ergebnis:	true, wenn Kennung lesen und pr�fen OK }
var
  cBefehl: char;
  Befehl: string;
  R: TRueckgabe;
  Wert: string;
  sVglKennung: string;
  l: integer;
  iAktKanal: integer;

begin
  Result:=false;

  Delay (2000);  // damit keine FFC-Bl�cke mit gleichem Zeitstempel am Daten-
                 // anfang entstehen -> Abbruchkriterium "Datenwiederholung"

  // f�r alle Kan�le:
  for iAktKanal:=1 to Kanalzahl do begin
    cBefehl:='l';
    Befehl:=GetDS100_LeseBefehl (cBefehl);
    if not TMRGCustomCommObj (CommObj).SendElsterDS100Command (Befehl, ad_String,
      MRGTimeouts.ElsterDS100Prot, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    // Antwort auswerten
    if not KonvDS100Param (R.Antwort, 'l', Wert) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + ST_KONVERROR, SKERR_PARAMKONV);
      exit;
    end;

    StationsKennung:=Copy (Wert, 1, C_KennungLen);  // nur die ersten 14 Stellen
    // F�r Kennungsvergleich Kennung aus Verbindungsaufbau-Kommando auf
    // Stellenanzahl der Ger�tekennung mit f�hrenden '0' auff�llen:
    sVglKennung:=F_LeftPad (VerbAufbauCmdData.Kennung, '0', length (StationsKennung));

    // Konfig-Kennung mit Platzhalter-Zeichen ('?') bei 4-Kanal-Ger�t
    // Die Ger�tenummern der 4 Kan�le unterscheiden sich nur an der
    // Zehntausender-Stelle, dort steht die Kanalnummer:
    if Kanalzahl > 1 then begin
      l:=length (sVglKennung);
      if l >= 5 then begin
        if sVglKennung [l-4] <> '?' then begin
          if VerbAufbauCmdData.KennPruef then
            FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_KEINE_VERBINDUNG)
          else begin
            FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_VERBINDUNG);
            Result:=true;
          end;
          exit;
        end;
        sVglKennung [l-4]:=Chr (iAktKanal + 48);	// Kanalnummer an der Zehntausender-Stelle f�r Vergleich einf�gen
      end;
    end;

    // Kennungsvergleich
    if not KennungPruefen (sVglKennung, VerbAufbauCmdData.KennPruef) then exit;

    // auf n�chsten Kanal weiterschalten:
    if Kanalzahl > 1 then
      Naechster_KanalDS100;
  end;  { for iAktKanal }

  if Kanalzahl > 1 then begin
    l:=length (StationsKennung);
    if l >= 5 then begin
      StationsKennung [l-4]:='?';  // In der Stationskennung Platzhalter an der Zehntausender-Stelle einf�gen
    end;
  end;

  Result:=true;
end;

{----------------------------------------------------------}
function TMRGAbruf.DatenAuslesen_DS100 (AbrufRec: TAbrufRec;
  var RohFilename: string): boolean;
{----------------------------------------------------------}
{	DS-100-Datenspeicher auslesen;
  �bergabe:	AbrufRec
  R�ckgabe: Rohdaten-Dateiname
  Ergebnis:	true, wenn Daten auslesen OK }
const
  Datum_Init: DateRec = (year: 2100; month: 1; day: 1);
  Zeit_Init: TimeRec = (hour: 0; min: 0; sec: 0; hsec: 0);

var
	Stop: boolean;
  Befehl: string;
	cBefehl: char;
  ABDatum: DateRec;
  ABZeit: TimeRec;
	DatumBuf: DateRec;
	ZeitBuf: TimeRec;
	JahrBuf: SmallInt;
	MonatBuf: SmallInt;
	TagBuf: SmallInt;
	StundeBuf: SmallInt;
	MinuteBuf: SmallInt;
	First: boolean;
  PosFF: integer;
	FFxSatz: string;
	Daten: string;
  Steuercode: string;
  c: char;
  Datum: DateRec;
  Zeit: TimeRec;
	VonDatum_MW: DateRec;
	VonZeit_MW: TimeRec;
  R: TRueckgabe;
  S: string;

begin
  Result:=false;
  // Daten-Rohfile anlegen, Dateiname f�r R�ckgabe:
  RohFileName:=CreateTempRohFile (FWorkPath, prefix_MRG_Roh);

  // von-Datum/Zeit in DateRec, TimeRec wandeln:
  with AbrufRec do begin
    if AlleDaten then begin
      { Ersatz-von-Zeitpunkt f�r "alle Daten" bilden: ab 1.1.2000 }
      with VonDatum_MW do begin
        Year:=2000;
        Month:=1;
        Day:=1;
      end;
      with VonZeit_MW do begin
        Hour:=0;
        Min:=0;
        Sec:=0;
        HSec:=0;
      end;
    end
    else begin
      with VonDatum_MW do begin
        Year:=vonJahr;
        Month:=vonMonat;
        Day:=vonTag;
      end;
      with VonZeit_MW do begin
        Hour:=vonStunde;
        Min:=vonMinute;
        Sec:=vonSekunde;
        HSec:=0;
      end;
    end;
  end;

  Delay (2000);  // damit keine FFC-Bl�cke mit gleichem Zeitstempel am Daten-
                 // anfang entstehen -> Abbruchkriterium "Datenwiederholung"

  { Datum, Zeit (mitlaufend) initialisieren: }
  Datum:=Datum_Init;
  Zeit:=Zeit_Init;

  // "Datenstrom-Schleife" starten:
  //	-> Daten aus Ger�t auslesen: Me�periodenwerte, Monatsende-Z�hlerst�nde des
  //	   nicht setzbaren Z�hlers und Statuscodes (Meldungen)
  //	-> Daten laufen zeitlich r�ckw�rts ein (j�ngste zuerst)
  Stop:=false;
  FFxSatz:='';
  first:=true;
  cBefehl:='x';
  Befehl:=GetDS100_LeseBefehl (cBefehl);
  R.Antwort:=''; // Neues Daten-Rohfile f�r Kanal anlegen

  while not Stop do begin
    // Daten lesen:
    if not TMRGCustomCommObj (CommObj).SendElsterDS100Command (Befehl, ad_File,
      MRGTimeouts.ElsterDS100Prot, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      DeleteFile (RohFileName);
      exit;
    end;

    if not WriteRohfile (RohfileName, R.Antwort) then begin
      FehlerGruppeCodeUpdate (ST_FILEERROR, FILEERR_COULDNOTWRITE);
      DeleteFile (RohFileName);
      exit;
    end;

    // "Datenstrom-Schleife" stoppen, wenn...:
    //		1. gew�nschter Zeitbereich empfangen wurde (Daten-Zeitstempel in Rohdaten pr�fen !)
    //		2. bereits empfangene Daten nochmal empfangen werden (Ger�t beginnt nach dem �ltesten
    //		   Datensatz wieder von vorne)
    //		3. FF1-Satz empfangen wurde (Neustart)
    S:=ExtractString (R.Antwort, NUL, '%', 0);  // alles ab einschlie�lich % wegschneiden
    S:=Copy (S, 2, length (S));  // erstes Zeichen wegschneiden (Befehlszeichen)
    FFxSatz:=FFxSatz + S;
    PosFF:=DS100_FFxPos(FFxSatz); { Position eines evtl. vorhand. FFx-Steuercodes }
    // FFxSatz solange abarbeiten bis kein FFx-Steuercode mehr enthalten ist:
    while (PosFF <> 0) AND not Stop do begin
      Daten:=Copy (FFxSatz, 1, PosFF-1);
      Steuercode:=Copy (FFxSatz, PosFF, 3);
      c:=Steuercode [3];  // Code FFx
      FFxSatz:=copy (FFxSatz, PosFF+3, length (FFxSatz));  // Daten des n�chsten FFx-Satzes
      case c of
        '1':  // Neustart
          Stop:=true;

        'A':  // Monat-Grenze
          begin
            Get_DS100_FFADatumZeit (Daten, JahrBuf, MonatBuf, TagBuf, StundeBuf, MinuteBuf);
            Datum.year:=JahrBuf;
            Datum.month:=MonatBuf;
            Datum.day:=TagBuf;
          end;

        'B':  // Tag-Grenze
          begin
            Get_DS100_FFBDatumZeit (Daten, TagBuf, StundeBuf, MinuteBuf);
            // Monatswechsel erkennen (Daten laufen zeitlich r�ckw�rts ein):
            if TagBuf > Datum.day then
              P_Vortag (Datum);

            Datum.day:=TagBuf;
          end;

        'C':  // Auslesen-Beginn
          begin
            Get_DS100_FFCDatumZeit (Daten, DatumBuf, ZeitBuf);
            if first then begin
              // Datum bei erstem FFC-Satz setzen:
              Datum:=DatumBuf;
              Zeit:=ZeitBuf;
              Zeit.hour:=0;
              Zeit.min:=0;
              Zeit.sec:=0;
              // Auslese-Beginn merken:
              ABDatum:=DatumBuf;
              ABZeit:=ZeitBuf;
              first:=false;
            end
            else begin
              // Datenlesen beenden, wenn gleiches ABDatumZeit nochmal kommt:
              if (CmpDate (DatumBuf, ABDatum) + CmpTime (ZeitBuf, ABZeit)) = 0 then
                Stop:=true;
            end;
          end;
      end; // case c

      PosFF:=DS100_FFxPos (FFxSatz);  // Position eines evtl. weiteren FFx-Steuercodes
    end;  // while (PosFF <> 0) AND not Stop

    // Stop, wenn von-Auslesezeitpunkt erreicht ist:
    if (CmpDate (Datum, VonDatum_MW) + CmpTime (Zeit, VonZeit_MW)) <= 0 then
      Stop:=true;
  end;  // while not Stop

  Result:=true;
end;


{-------------------- Modbus-Routinen f�r Primus/Prilog 400 -------------------}

{---------------------------------------------------------------------}
function TMRGAbruf.ReadModbusArchiv_PrimusPrilog (AMrgTyp: integer;
  AArchivtyp: TArchivtyp; AbrufRec: TAbrufRec; dtVon, dtBis: TDateTime;
  MBAbrufData: TMBAbrufData; var sFilename_Archiv: string): boolean;
{---------------------------------------------------------------------}
{ Modbus-Archiv eines Primus/Prilog 400 lesen und abgerufene Archivdatens�tze in
  Datei schreiben;
  �bergaben: Ger�tetyp
             Archivtyp (f�r Messwerte, Meldungen)
             Abruf-Record
             Daten-Zeitbereich von-bis
             MBAbrufData-Record mit Modbusregister-Konfiguration des Archivs
  R�ckgabe: Name der Datei mit abgerufenen Archivdatens�tzen
  Ergebnis: true, wenn Modbus-Archivabruf erfolgreich }
type
  TArchiveReadMode =
    (arm_Start,
     arm_ArchivZeigerInit,
     arm_ArchivZeigerInitDone
    );

var
  SlaveData: TSlaveData;
  RRL: TRegisterRequestList;
  RegisterRequestData: TRegisterRequestData;
  iRecOffset: word;
  dtArchivRec: TDateTime;
  bStop: boolean;
  i: integer;
  iMaxArchivRecCount: word;
  iRecCount: integer;
  bDummy: boolean;
  dtArchivZeiger: TDateTime;
  bOK: boolean;
  ArchiveReadMode: TArchiveReadMode;
  sDummy: string;
  MBRegisterDef_ArchivZeiger: TMBRegisterDef;
  ParamMrgData: TParamMrgData;

begin
  Result:=false;
  sFilename_Archiv:='';  // Vorbelegung R�ckgabe

  SetModbusSlaveData_AdresseByteOrder (AMrgTyp, SlaveData);

  // Zuerst den Archiv-Zeiger im Ger�t l�schen, um den Zeitstempel des letzten
  // (j�ngsten) im Ger�t vorhandenen Archivdatensatzes lesen zu k�nnen:
  with RegisterRequestData do begin
    FktCode:=5;  // Force single coil
    StartAdresse:=59;  // fixe Coil-Adresse 59
    AnzahlBytes:=0;  // verwendet
    AnzahlBits:=0;  // nicht verwendet
    RegisterKonvListe:=nil;  // nicht verwendet
    Typ:=C_MBWertTyp_W;
    Wert_Einstellen:=IntToStr ($FF00);  // ON-State f�r Coil setzen
  end;

  // Modbus-Request f�r L�schen des Archiv-Zeigers versenden, Response empfangen:
  if not SendModbusRequest (SlaveData, RegisterRequestData,
                            MRGTimeouts.ModbusProt, bOK, sDummy) then exit;

  { Antwort auf "Archiv-Zeiger l�schen"-Kommando auswerten:
    -> pr�fen, ob L�schen erfolgreich war }
  if not bOK then begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
    exit;
  end;

  { Archivzeiger: aus der allgemeinen Parameternummer die ger�tespezifische ermitteln }
  if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                        CP_RMG_PrimusPrilog_ArchivZeiger,
                                                        ParamMrgData) then begin  // 11.02.2022, WW
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
    exit;
  end;

  with MBRegisterDef_ArchivZeiger do begin  // 11.02.2022, WW
    StartAdresse:=StrToIntDef (ParamMrgData.Parameternummer_im_MRG, 0);
    Typ:=ParamMrgData.ParaDatentyp;
    AnzahlBytes:=ParamMrgData.ParaByteLen;
  end;

  RRL:=TRegisterRequestList.Create;
  try
    // Datei zum Zwischenspeichern der abgerufenen Archivwerte anlegen:
    sFilename_Archiv:=CreateTempRohFile (FWorkPath, prefix_MB_Konv);
    if sFilename_Archiv = '' then begin
      FehlerGruppeCodeUpdate(ST_FILEERROR, FILEERR_COULDNOTWRITE);
      exit;
    end;

    ArchiveReadMode:=arm_Start;  // Modus setzen: Archiv lesen beginnt
    iMaxArchivRecCount:=1;  // Beim Start nur 1 Archivdatensatz lesen (den j�ngsten)
    iRecOffset:=0;  // Zum Auslesen ab dem 1. (j�ngsten) Archivdatensatz
    dtArchivZeiger:=-1;

    bStop:=false;
    repeat
      // Laden der Modbus-Requests zum Lesen von Archivdatens�tzen:
      FillMBRegisterRequestListe_ArchivDataRec (AMrgTyp, AArchivtyp,
                                                iRecOffset, iMaxArchivRecCount,
                                                RRL, MBAbrufData, iRecCount);
      // Die Register des Archivdatensatzes abfragen:
      for i:=0 to RRL.Count - 1 do begin
        RegisterRequestData:=TRegisterRequestDataObj (RRL [i]).Data;
        // Modbus-Request versenden, Response empfangen:
        if not SendModbusRequest (SlaveData, RegisterRequestData,
                                  MRGTimeouts.ModbusProt,
                                  bDummy, sDummy) then begin
          DeleteFile (sFilename_Archiv);
          exit;
        end;
      end;  { for i }

      if ArchiveReadMode = arm_ArchivZeigerInitDone then  // 25.11.2020, WW
        // Zeitstempel des letzten gelesenen Archivdatensatzes ermitteln und auswerten:
        dtArchivRec:=GetModbusArchivRec_DatumZeit (AMrgTyp, AArchivtyp,
                                                   iRecOffset + iRecCount - 1,
                                                   RRL, MBAbrufData)
      else  // Bugfix: Pr�fung auf ersten Archivdatensatz; 25.11.2020, WW
        // Zeitstempel des ersten gelesenen Archivdatensatzes ermitteln und auswerten:
        dtArchivRec:=GetModbusArchivRec_DatumZeit (AMrgTyp, AArchivtyp,
                                                   iRecOffset, RRL, MBAbrufData);

      if not (dtArchivRec > -1) then begin
        // Archivdatensatz-Zeitstempel nicht vorhanden
        DeleteFile (sFilename_Archiv);
        case AArchivtyp of
          at_Periodenarchiv: FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_MESSKONV);
          at_Ereignisarchiv: FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_MELDKONV);
        end;
        exit;
      end;

      case ArchiveReadMode of
        arm_Start:
          begin
            // Beim Start mu� zuerst der Archiv-Zeiger gesetzt werden (sofern �berhaupt
            // ein Archivdatensatz vorhanden ist):
            if (dtArchivRec > 0) then begin
              // Archiv-Zeiger setzen abh�ngig vom Bis-Zeitpunkt:
              if dtBis > dtArchivRec then
                dtArchivZeiger:=dtArchivRec  // Archiv-Zeiger auf letzten (j�ngsten) Archivdatensatz
              else
                dtArchivZeiger:=dtBis;  // Archiv-Zeiger auf Archivdatensatz des Bis-Zeitpunkts.
                                        // Falls dazu kein Archivdatensatz im Ger�t existiert,
                                        // positioniert das Ger�t auf den n�chst-j�ngeren
                                        // Archivdatensatz.

              // Modbus-Parameter "Archiv-Zeiger" einstellen:
              if not WriteModbusArchivZeiger_PrimusPrilog (MBRegisterDef_ArchivZeiger.StartAdresse,
                                                           MBRegisterDef_ArchivZeiger.Typ,
                                                           MBRegisterDef_ArchivZeiger.AnzahlBytes,
                                                           dtArchivZeiger) then begin
                DeleteFile (sFilename_Archiv);
                exit;
              end;

              // Ab jetzt maximal alle im Ger�t vorhandenen Archivdatens�tze lesen:
              iMaxArchivRecCount:=GetMB_MaxArchivRecCount (AMrgTyp, AArchivtyp,
                                                           MBAbrufData);

              ArchiveReadMode:=arm_ArchivZeigerInit;  // Modus: Archiv-Zeiger initialisiert
              Continue;  // Nochmal f�r die erste produktive Datensatz-Abfrage mit
                         // gesetztem Archiv-Zeiger
            end else
              ArchiveReadMode:=arm_ArchivZeigerInitDone;  // Modus: Archiv-Zeiger-Initialisierung beendet
          end;

        arm_ArchivZeigerInit:
          begin
            // Wenn kein Archivdatensatz gelesen werden konnte (obwohl beim Lesen
            // OHNE gesetzten Archivzeiger der letzte Datensatz VORHANDEN ist);
            // 22.07.2019, WW
            if not (dtArchivRec > 0) then begin
              // "Archiv-Zeiger" auf Winterzeit setzen (f�r den Fall, da� das Ger�t
              // auf Sommerzeit l�uft)
              dtArchivZeiger:=IncHour (dtArchivZeiger, -1);

              // Modbus-Parameter "Archiv-Zeiger" einstellen:
              if not WriteModbusArchivZeiger_PrimusPrilog (MBRegisterDef_ArchivZeiger.StartAdresse,
                                                           MBRegisterDef_ArchivZeiger.Typ,
                                                           MBRegisterDef_ArchivZeiger.AnzahlBytes,
                                                           dtArchivZeiger) then begin
                DeleteFile (sFilename_Archiv);
                exit;
              end;

              ArchiveReadMode:=arm_ArchivZeigerInitDone;  // Modus: Archiv-Zeiger-Initialisierung beendet
              Continue;  // Nochmal versuchen f�r die erste produktive Datensatz-
                         // Abfrage mit auf Winterzeit gesetztem Archiv-Zeiger
            end else
              ArchiveReadMode:=arm_ArchivZeigerInitDone;  // Modus: Archiv-Zeiger-Initialisierung beendet
          end;
      end;  // case ArchiveReadMode

      // Archivdatens�tze in Datei zwischenspeichern f�r Konvertierung:
      if not WriteModbusArchivRecFile (AMrgTyp, AArchivtyp, AbrufRec, dtVon, dtBis,
                                       sFilename_Archiv, RRL, MBAbrufData) then begin
        DeleteFile (sFilename_Archiv);
        exit;
      end;

      inc (iRecOffset, iRecCount);  // Zum Auslesen der weiteren Archivdatens�tze (zeitlich absteigend)

      // Kein weiteres Auslesen von Archivdatens�tzen, wenn von-Zeitpunkt oder
      // ein unbelegter Archivdatensatz mit Zeitstempel 0 gelesen wurde:
      if (not (dtArchivRec > 0)) OR   // 22.07.2019, WW
         (not AbrufRec.AlleDaten AND (CmpDateTime (dtArchivRec, dtVon) <= 0)) then
        bStop:=true
      else begin
        // Pr�fen, ob Ende des Archiv-Registerbereichs erreicht ist:
        if (iRecOffset >= iMaxArchivRecCount) then begin
          // Ist erreicht. Jetzt Archiv-Zeiger im Ger�t neu setzen, um weiterlesen zu k�nnen

          // Modbus-Parameter "Archiv-Zeiger" auf Zeitstempel des letzten gelesenen
          // Archivdatensatzes einstellen:
          if not WriteModbusArchivZeiger_PrimusPrilog (MBRegisterDef_ArchivZeiger.StartAdresse,
                                                       MBRegisterDef_ArchivZeiger.Typ,
                                                       MBRegisterDef_ArchivZeiger.AnzahlBytes,
                                                       dtArchivRec) then begin
            DeleteFile (sFilename_Archiv);
            exit;
          end;

          // ... und den Datensatz-Offset auf 1 stellen, um den n�chst-�lteren
          // Archivdatensatz zu lesen:
          iRecOffset:=1;
        end;
      end;
    until bStop;
  finally
    RRL.Free;
  end;

  Result:=true;
end;

{---------------------------------------------------------------------------}
function TMRGAbruf.WriteModbusArchivZeiger_PrimusPrilog (iStartAdresse: word;
  sTyp: string; iAnzahlBytes: integer; dtZeiger: TDateTime): boolean;
{---------------------------------------------------------------------------}
{ Modbus-Parameter "Archiv-Zeiger" im Primus/Prilog 400 einstellen;
  �bergaben: Register-Startadresse des Archivzeiger-Parameters
             Modbus-Datentyp
             Anzahl der Bytes
             Zeitstempel f�r Archiv-Zeiger
  Ergebnis: true, wenn Schreiben des Archiv-Zeiger-Parameters erfolgreich }
var
  bOK: boolean;
  dt: TDateTime;
  dt2000: TDateTime;

begin
  Result:=false;

  // Korrektur, falls erforderlich (auf den Archiv-Zeiger k�nnen nur Zeitstempel
  // mit Jahr >= 2000 geschrieben werden):
  dt2000:=EncodeDate (2000, 1, 1);
  if CmpDateTime (dtZeiger, dt2000) < 0 then
    dt:=dt2000
  else
    dt:=dtZeiger;

  // Modbus-Parameter "Archiv-Zeiger" einstellen:
  if not WriteModbusParameter_PrimusPrilog (iStartAdresse, sTyp, iAnzahlBytes,
                                            DateTimeToStr (dt), bOK) then exit;

  { Pr�fen, ob Archiv-Zeiger im Ger�t tats�chlich ge�ndert wurde }
  if not bOK then begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
    exit;
  end;

  Result:=true;
end;

{------------------------------------------------------------------------}
function TMRGAbruf.WriteModbusParameter_PrimusPrilog (iStartAdresse: word;
  sTyp: string; iAnzahlBytes: integer; sWertNeu: string; var bOK: boolean;
  iErweiterteStatusgruppe: integer = 0): boolean;
{------------------------------------------------------------------------}
{ Modbus-Parameter im Primus/Prilog 400 einstellen;
  �bergaben: Register-Startadresse des Parameters
             Modbus-Datentyp
             Anzahl der Bytes
             Erweiterte Statusgruppe (optional, EST_...-Konstanten)
  R�ckgabe: Flag 'bOK': true, wenn Einstellen erfolgreich
  Ergebnis: true, wenn Schreiben des Modbus-Parameters erfolgreich }
var
  SlaveData: TSlaveData;
  RegisterRequestData: TRegisterRequestData;
  bPreset: boolean;
  iID: integer;
  sWert: string;
  sPW: string;
  bPWLogin: boolean;
  ParamMrgData: TParamMrgData;
  sDummy: string;

begin
  Result:=false;
  bOK:=true;  // Default: Parametrierung erfolgreich

  SetModbusSlaveData_AdresseByteOrder (mrgtyp_Primus, SlaveData);

  // 1. Versuch: Parameter per Modbus setzen ohne Passwort-Login (wenn
  // kein Passwort im Ger�t definiert ist, ist kein Passwort-Login n�tig.)
  bPWLogin:=false;  // Default: Kein Passwort-Login
  with RegisterRequestData do begin
    FktCode:=16;
    StartAdresse:=iStartAdresse;  // Registeradresse Parameter
    AnzahlBytes:=iAnzahlBytes;
    AnzahlBits:=0;  // nicht verwendet
    RegisterKonvListe:=nil;  // nicht verwendet
    Typ:=sTyp;
    Wert_Einstellen:=sWertNeu;
  end;

  // Modbus-Request zum Setzen des Parameters versenden, Response empfangen:
  if SendModbusRequest (SlaveData, RegisterRequestData,
                        MRGTimeouts.ModbusProt, bPreset, sDummy,
                        iErweiterteStatusgruppe) then begin
    { Antwort auf "Parameter setzen"-Kommando auswerten:
      -> pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
    if not bPreset then
      bOK:=false;
  end
  else begin
    if (Fehlergruppe = iErweiterteStatusgruppe + COM_MODBUSERROR) AND
       (Fehlercode = MODBUSERR_EXC_SLAVEDEVICEFAILURE) then
      bPWLogin:=true  // Flag setzen: Weiterer Versuch mit Passwort-Login
    else
      exit;
  end;

  if bPWLogin then begin
    // 2. Versuch: Passwort ins Ger�t �bertragen; 22.07.2019, WW
    //             -> Mu� jedes Mal vor dem Einstellen eines Parameters gemacht
    //                werden (einmal je Abruf reicht nicht !) 
    // Zuerst aus der Passwortnummer des Verbindungsaufbau-Kommandos die
    // Passwort-ID des Ger�ts ermitteln:
    iID:=PasswortNrToDevicePasswortID (FMrgDefData.MrgTyp, VerbAufbau_PasswortNr);

    // Vorab-Pr�fung des Passwort:
    //   -> 1..6 Zeichen lang
    //   -> auf unerlaubte Zeichen (per Modbus nur '0'..'9' erlaubt)
    if (length (VerbAufbau_Passwort) < 1) OR
       (length (VerbAufbau_Passwort) > 6) OR
       (not isIntString (VerbAufbau_Passwort)) then begin
      FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
      exit;
    end;

    // Einstell-Wert f�r Modbus-Kommando "Passwort senden" bilden
    sPW:=F_LeftPad (VerbAufbau_Passwort, '0', 6);  // vorne mit 0 auff�llen
    sWert:=IntToStr (iID - 801) + sPW;  // Einstellwert

    { Modbusregister-Definition des Passwort-Parameters ermitteln und in
      FMBRegisterDef_Para_Passwort f�r den weiteren Abruf merken: 11.02.2022, WW }
    if FMBRegisterDef_Para_Passwort.StartAdresse = 0 then begin  // wenn noch nicht ermittelt
      { aus der allgemeinen Parameternummer die ger�tespezifische ermitteln: }
      if not FParamMrgKonfigList.FindParamMrgData_ByAllgNr (FMrgKonvData.ParameterGruppe,
                                                            CP_RMG_PrimusPrilog_Passwort,
                                                            ParamMrgData) then begin  // 11.02.2022, WW
        FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
        exit;
      end;

      with FMBRegisterDef_Para_Passwort do begin  // 11.02.2022, WW                   
        StartAdresse:=StrToIntDef (ParamMrgData.Parameternummer_im_MRG, 0);
        Typ:=ParamMrgData.ParaDatentyp;
        AnzahlBytes:=ParamMrgData.ParaByteLen;
      end;
    end;

    with RegisterRequestData do begin
      FktCode:=16;
      StartAdresse:=FMBRegisterDef_Para_Passwort.StartAdresse;  // Registeradresse Ger�te-Passwort
      AnzahlBytes:=FMBRegisterDef_Para_Passwort.AnzahlBytes;
      AnzahlBits:=0;  // nicht verwendet
      RegisterKonvListe:=nil;  // nicht verwendet
      Typ:=FMBRegisterDef_Para_Passwort.Typ;
      Wert_Einstellen:=sWert;
    end;

    // Modbus-Request zum Senden des Passworts versenden, Response empfangen:
    if not SendModbusRequest (SlaveData, RegisterRequestData,
                              MRGTimeouts.ModbusProt, bPreset, sDummy,
                              iErweiterteStatusgruppe) then exit;

    { Antwort auf "Passwort senden"-Kommando auswerten:
      -> pr�fen, ob Passwort ins Ger�t korrekt �bertragen wurde }
    if not bPreset then
      bOK:=false;

    // Wenn das Schreiben des Passworts erfolgreich war, dann "unlock section",
    // d.h. es erfolgt der Login-Versuch zu dem zuvor gesendeten Passwort:
    if bPreset then begin
       with RegisterRequestData do begin
        FktCode:=5;  // Force single coil
        StartAdresse:=17;  // fixe Coil-Adresse 17
        AnzahlBytes:=0;  // verwendet
        AnzahlBits:=0;  // nicht verwendet
        RegisterKonvListe:=nil;  // nicht verwendet
        Typ:=C_MBWertTyp_W;
        Wert_Einstellen:=IntToStr ($FF00);  // ON-State f�r Coil setzen
      end;

      // Modbus-Request f�r Passwort-Login versenden, Response empfangen:
      if not SendModbusRequest (SlaveData, RegisterRequestData,
                                MRGTimeouts.ModbusProt, bPreset, sDummy,
                                iErweiterteStatusgruppe) then begin
        if (Fehlergruppe = iErweiterteStatusgruppe + COM_MODBUSERROR) AND
           (Fehlercode = MODBUSERR_EXC_SLAVEDEVICEFAILURE) then
          // Modbus-Fehler durch aussagekr�ftigeren Passwort-Login-Fehler ersetzen
          FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);

        exit;
      end;

      { Antwort auf "unlock section"-Kommando auswerten:
        -> pr�fen, ob Passwort-Login erfolgreich war }
      if not bPreset then
        bOK:=false;

      if bPreset then begin
        // Parameter nochmal versuchen zu setzen:
        with RegisterRequestData do begin
          FktCode:=16;
          StartAdresse:=iStartAdresse;  // Registeradresse Parameter
          AnzahlBytes:=iAnzahlBytes;
          AnzahlBits:=0;  // nicht verwendet
          RegisterKonvListe:=nil;  // nicht verwendet
          Typ:=sTyp;
          Wert_Einstellen:=sWertNeu;
        end;

        // Modbus-Request zum Setzen des Parameters versenden, Response empfangen:
        if not SendModbusRequest (SlaveData, RegisterRequestData,
                                  MRGTimeouts.ModbusProt, bPreset, sDummy,
                                  iErweiterteStatusgruppe) then begin
          // Modbus-Fehler durch aussagekr�ftigeren Fehler "Parameter wurde nicht
          // ver�ndert" ersetzen
          if iErweiterteStatusgruppe = EST_ZEITSYNCERROR then
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_NOSUCCESS)
          else
            FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
          exit;
        end;

        { Antwort auf "Parameter setzen"-Kommando auswerten:
          -> pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
        if not bPreset then
          bOK:=false;
      end;
    end;
  end;  // if PWLogin

  Result:=true;
end;


{-------------------- Modbus-Routinen f�r TME400, RSM200 ----------------------}

{---------------------------------------------------------------------}
function TMRGAbruf.ReadModbusArchiv_TME400 (AMrgTyp: integer;
  AArchivtyp: TArchivtyp; AbrufRec: TAbrufRec; dtVon, dtBis: TDateTime;
  var sFilename_Archiv: string): boolean;
{---------------------------------------------------------------------}
{ Modbus-Archiv eines TME400 lesen und abgerufene Archivdatens�tze in Datei
  schreiben;
  -> Auch f�r RSM200
  �bergaben: Ger�tetyp
             Datenart (Messwerte, Meldungen)
             Abruf-Record
             Daten-Zeitbereich von-bis
  R�ckgabe: Name der Datei mit abgerufenen Archivdatens�tzen
  Ergebnis: true, wenn Modbus-Archivabruf erfolgreich }
var
  SlaveData: TSlaveData;
  RRL: TRegisterRequestList;
  RegisterRequestData: TRegisterRequestData;
  iRecOffset: word;
  dtArchivRec: TDateTime;
  bStop: boolean;
  i: integer;
  iMaxArchivRecCount: word;
  iRecCount: integer;
  bDummy: boolean;
  Archivheader: TTME400Archivheader;
  iMinRegisterAdr: integer;
  sDummy: string;
  Dummy: TMBAbrufData;  // f�r TME400 nicht verwendet

begin
  Result:=false;
  sFilename_Archiv:='';  // Vorbelegung R�ckgabe

  SetModbusSlaveData_AdresseByteOrder (AMrgTyp, SlaveData);

  // Zuerst den Archivheader lesen:
  if not ReadModbusArchivheader_TME400 (AMrgTyp, AArchivtyp, Archivheader) then exit;

  // Archiv auslesen, wenn der Archivheader kein leeres Archiv anzeigt:
  if (Archivheader.Index_aeltest <> $FFFF) AND
     (Archivheader.Index_neuest <> $FFFF) then begin
    RRL:=TRegisterRequestList.Create;
    try
      // Datei zum Zwischenspeichern der abgerufenen Archivwerte anlegen:
      sFilename_Archiv:=CreateTempRohFile (FWorkPath, prefix_MB_Konv);
      if sFilename_Archiv = '' then begin
        FehlerGruppeCodeUpdate(ST_FILEERROR, FILEERR_COULDNOTWRITE);
        exit;
      end;

      // Maximal vorhandene Archivdatens�tze im Ger�t:
      iMaxArchivRecCount:=GetMB_MaxArchivRecCount (AMrgTyp, AArchivtyp, Dummy);

      // Es wird begonnen mit dem Auslesen der j�ngsten Archivdatens�tze beginnend
      // vom neuesten Index bis runter zu Index 0:
      iRecOffset:=Archivheader.Index_neuest;
      iMinRegisterAdr:=0;

      bStop:=false;
      repeat
        // Laden der Modbus-Requests zum Lesen von Archivdatens�tzen:
        FillMBRegisterRequestListe_ArchivDataRec (AMrgTyp, AArchivtyp,
                                                  iRecOffset, iMaxArchivRecCount,
                                                  RRL, Dummy,
                                                  iRecCount, iMinRegisterAdr);
        // Die Register des Archivdatensatzes abfragen:
        for i:=0 to RRL.Count - 1 do begin
          RegisterRequestData:=TRegisterRequestDataObj (RRL [i]).Data;
          // Modbus-Request versenden, Response empfangen:
          if not SendModbusRequest (SlaveData, RegisterRequestData,
                                    MRGTimeouts.ModbusProt,
                                    bDummy, sDummy) then begin
            DeleteFile (sFilename_Archiv);
            exit;
          end;
        end;  { for i }

        // Zeitstempel des ersten (�ltesten) der gelesenen Archivdatens�tze ermitteln
        // und auswerten:
        dtArchivRec:=GetModbusArchivRec_DatumZeit (AMrgTyp, AArchivtyp,
                                                   iRecOffset + 1 - iRecCount,
                                                   RRL, Dummy);

        if not (dtArchivRec > -1) then begin
          // Archivdatensatz-Zeitstempel nicht vorhanden
          DeleteFile (sFilename_Archiv);
          case AArchivtyp of
            at_Periodenarchiv:
              FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_MESSKONV);

            at_Ereignisarchiv,
            at_Parameterarchiv_eichamtlich,
            at_Parameterarchiv_nichteichamtlich:
              FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_MELDKONV);
          end;
          exit;
        end;

        // Archivdatens�tze in Datei zwischenspeichern f�r Konvertierung:
        if not WriteModbusArchivRecFile (AMrgTyp, AArchivtyp, AbrufRec, dtVon, dtBis,
                                         sFilename_Archiv, RRL, Dummy) then begin
          DeleteFile (sFilename_Archiv);
          exit;
        end;

        if iRecOffset >= iRecCount then begin
          dec (iRecOffset, iRecCount);  // Zum Auslesen der weiteren �lteren Archivdatens�tze

          // Kein weiteres Auslesen von Archivdatens�tzen, wenn von-Zeitpunkt oder
          // der Archivdatensatz mit dem �ltesten Index gelesen wurde:
          if (iRecOffset < iMinRegisterAdr) OR
             (not AbrufRec.AlleDaten AND (CmpDateTime (dtArchivRec, dtVon) <= 0)) then begin
            bStop:=true;
          end;
        end
        else begin
          if (iMinRegisterAdr = 0) AND
            (Archivheader.Index_neuest < Archivheader.Index_aeltest) then begin
            // Weiter Auslesen der n�chsten Archivdatens�tze beginnend
            // vom maximalen Index bis runter zum �ltesten Index:
            iRecOffset:=iMaxArchivRecCount - 1;
            iMinRegisterAdr:=Archivheader.Index_aeltest;
          end else
            bStop:=true;
        end;
      until bStop;
    finally
      RRL.Free;
    end;
  end;  // if (Archivheader.Index_aeltest <> $FFFF) ...

  Result:=true;
end;

{------------------------------------------------------------------------}
function TMRGAbruf.ReadModbusArchivheader_TME400 (AMrgTyp: integer;
  AArchivtyp: TArchivtyp; var Archivheader: TTME400Archivheader): boolean;
{------------------------------------------------------------------------}
{ Liest die Modbus-Archivheader eines TME400-Archivs und liefert den aktuellen
  Archivheader zur�ck;
  -> Auch f�r RSM200
  �bergaben: Ger�tetyp
             Archivtyp
  R�ckgabe: Aktueller Archivheader
  Ergebnis: true, wenn Lesen der Modbus-Archivheader und ermitteln des aktuellen
            Archivheaders erfolgreich }
var
  SlaveData: TSlaveData;
  RRL: TRegisterRequestList;
  RegisterRequestData: TRegisterRequestData;
  i: integer;
  bDummy: boolean;
  AHL: TTME400ArchivheaderList;
  sDummy: string;

begin
  Result:=false;

  Archivheader:=Init_TME400Archivheader;  // Vorbelegung R�ckgabe

  SetModbusSlaveData_AdresseByteOrder (AMrgTyp, SlaveData);

  RRL:=TRegisterRequestList.Create;
  try
    // Laden der Modbus-Requests zum Lesen der Archivheader-Datens�tze:
    FillMBRegisterRequestListe_Archivheader_TME400 (AMrgTyp, AArchivtyp, RRL);

    // Die Register der Archivheader-Datens�tze abfragen:
    for i:=0 to RRL.Count - 1 do begin
      RegisterRequestData:=TRegisterRequestDataObj (RRL [i]).Data;
      // Modbus-Request versenden, Response empfangen:
      if not SendModbusRequest (SlaveData, RegisterRequestData,
                                MRGTimeouts.ModbusProt,
                                bDummy, sDummy) then begin
        exit;
      end;
    end;  { for i }

    AHL:=TTME400ArchivheaderList.Create;
    try
      // Gelesene Archivheader-Datens�tze in Archivheaderliste laden:
      if not AHL.LoadFromModbusRegisterRequestList (RRL, SlaveData.ByteOrder) then begin
        case AArchivtyp of
          at_Periodenarchiv:
            FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_MESSKONV);

          at_Ereignisarchiv,
          at_Parameterarchiv_eichamtlich,
          at_Parameterarchiv_nichteichamtlich:
            FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_MELDKONV);
        end;
        exit;
      end;

      // Den aktuellen Archivheader-Datensatz ermitteln:
      if not AHL.GetHeaderAktuell (Archivheader) then begin
        case AArchivtyp of
          at_Periodenarchiv:
            FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_MWINVALID);

          at_Ereignisarchiv,
          at_Parameterarchiv_eichamtlich,
          at_Parameterarchiv_nichteichamtlich:
            FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_MEINVALID);
        end;
        exit;
      end;
    finally
      AHL.Free;
    end;
  finally
    RRL.Free;
  end;

  Result:=true;
end;

{---------------------------------------------------------------------------}
function TMRGAbruf.WriteModbusParameter_TME400 (AMrgTyp: integer;
  iStartAdresse: word; sTyp: string; iAnzahlBytes: integer; sWertNeu: string;
  var bOK: boolean; iErweiterteStatusgruppe: integer = 0): boolean;
{---------------------------------------------------------------------------}
{ Modbus-Parameter im TME400 einstellen;
  -> Auch f�r RSM200
  �bergaben: Ger�tetyp
             Register-Startadresse des Parameters
             Modbus-Datentyp
             Anzahl der Bytes
             Erweiterte Statusgruppe (optional, EST_...-Konstanten)
  R�ckgabe: Flag 'bOK': true, wenn Einstellen erfolgreich
  Ergebnis: true, wenn Schreiben des Modbus-Parameters erfolgreich }
const
  // Bit f�r 'Zugriffslevel Codewort aktiv' im Statusregister
  C_TME400_Status_CodewortOpen = $02;  // Bit 1
  C_RSM200_Status_CodewortOpen = $01;  // Bit 0

var
  SlaveData: TSlaveData;
  RegisterRequestData: TRegisterRequestData;
  bPreset: boolean;
  iPW: integer;
  Code: integer;
  bPWLogin: boolean;
  MBRegisterDef: TMBRegisterDef;
  iStatusregister: cardinal;
  bCodewortAktiv: boolean;
  sDummy: string;
  iCodewortOpen: integer;

begin
  Result:=false;
  bOK:=true;  // Default: Parametrierung erfolgreich

  if AMrgTyp in [mrgtyp_TME400_VCF, mrgtyp_TME400_VMF] then begin
    MBRegisterDef:=TME400_MBRegisterDef_Para_Codewort;
    iCodewortOpen:=C_TME400_Status_CodewortOpen;
  end
  else if AMrgTyp in [mrgtyp_RSM200_VCF, mrgtyp_RSM200_VMF] then begin  // 09.01.2024, WW
    MBRegisterDef:=RSM200_MBRegisterDef_Para_Codewort;
    iCodewortOpen:=C_RSM200_Status_CodewortOpen;
  end else
    iCodewortOpen:=0;

  SetModbusSlaveData_AdresseByteOrder (AMrgTyp, SlaveData);

  // 1. Versuch: Parameter per Modbus setzen ohne Passwort-Login (f�r Parameter
  // mit Zugriffslevel N oder bei bereits erfolgter Codewort-Freigabe ist keine
  // Eingabe des Codewort n�tig.)
  bPWLogin:=false;  // Default: Kein Passwort-Login
  with RegisterRequestData do begin
    FktCode:=16;
    StartAdresse:=iStartAdresse;  // Registeradresse Parameter
    AnzahlBytes:=iAnzahlBytes;
    AnzahlBits:=0;  // nicht verwendet
    RegisterKonvListe:=nil;  // nicht verwendet
    Typ:=sTyp;
    Wert_Einstellen:=sWertNeu;
  end;

  // Modbus-Request zum Setzen des Parameters versenden, Response empfangen:
  if SendModbusRequest (SlaveData, RegisterRequestData,
                        MRGTimeouts.ModbusProt, bPreset, sDummy,
                        iErweiterteStatusgruppe) then begin
    { Antwort auf "Parameter setzen"-Kommando auswerten:
      -> pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
    if not bPreset then
      bOK:=false;
  end
  else begin
    if (Fehlergruppe = iErweiterteStatusgruppe + COM_MODBUSERROR) AND
       (Fehlercode = MODBUSERR_EXC_ILLEGALDATAVALUE) then   
      bPWLogin:=true  // Flag setzen: Weiterer Versuch mit Passwort-Login
    else
      exit;
  end;

  if bPWLogin then begin
    // 2. Versuch: Passwort ins Ger�t �bertragen, wenn Codewort-Freigabe im Ger�t
    // aktuell nicht aktiv ist.

    // Parameter "Statusregister" lesen, um zu pr�fen, ob die Freigabe des Codeworts
    // aktiv ist:
    if not ReadModbusStatusregister_TME400 (AMrgTyp, iStatusregister,
                                            iErweiterteStatusgruppe) then exit;

    // Bit f�r 'Zugriffslevel Codewort aktiv' im Statusregister pr�fen: }
    bCodewortAktiv:=(iStatusregister AND iCodewortOpen) <> 0;

    if not bCodewortAktiv then begin
      // Vorab-Pr�fung des Passwort:
      // -> erlaubter Wertebereich f�r Word (Registergr��e des "Codewort Freigabe"-Parameters)
      Val (VerbAufbau_Passwort, iPW, Code);
      if (Code <> 0) OR (iPW < 0) OR (iPW > MaxWord) then begin
        FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
        exit;
      end;

      with RegisterRequestData do begin
        FktCode:=16;
        StartAdresse:=MBRegisterDef.StartAdresse;  // Registeradresse Ger�te-Codewort Freigabe
        AnzahlBytes:=MBRegisterDef.AnzahlBytes;
        AnzahlBits:=0;  // nicht verwendet
        RegisterKonvListe:=nil;  // nicht verwendet
        Typ:=MBRegisterDef.Typ;
        Wert_Einstellen:=IntToStr (iPW);  // Einstellwert Codewort
      end;

      // Modbus-Request zum Setzen des Passworts versenden, Response empfangen:
      if not SendModbusRequest (SlaveData, RegisterRequestData,
                                MRGTimeouts.ModbusProt, bPreset, sDummy,
                                iErweiterteStatusgruppe) then exit;

      { Antwort auf "Passwort setzen"-Kommando auswerten:
        -> pr�fen, ob Passwort ins Ger�t korrekt �bertragen wurde }
      if not bPreset then
        bOK:=false;

      if bPreset then begin
        // Wenn das Setzen des Passworts erfolgreich war, dann Parameter "Statusregister"
        // lesen, um zu pr�fen, ob die Freigabe des Codeworts erfolgreich war:

        // Erst noch kurz warten, sonst ist das Statusregister im Ger�t ggf. noch
        // nicht aktualisiert (RSM200); 09.01.2024, WW
        Sleep (500);
        if not ReadModbusStatusregister_TME400 (AMrgTyp, iStatusregister,
                                                iErweiterteStatusgruppe) then exit;

        // Bit f�r 'Zugriffslevel Codewort aktiv' im Statusregister pr�fen: }
        bCodewortAktiv:=(iStatusregister AND iCodewortOpen) <> 0;

        if not bCodewortAktiv then begin
          // Wenn Codewort-Freigabe nicht aktiv ist, ist das Passwort falsch:
          FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
          exit;
        end else
          FParametrierModus_gesetzt:=true;  // Flag setzen: Codewort erfolgreich freigegeben
                                            // -> Als Merker, um die Codewort-Freigabe am Ende
                                            //    des Abrufs wieder zu deaktivieren

        // Wenn die Freigabe des Codeworts erfolgreich war, dann Parameter nochmal
        // versuchen einzustellen:

        // Parameter per Modbus setzen:
        with RegisterRequestData do begin
          FktCode:=16;
          StartAdresse:=iStartAdresse;  // Registeradresse Parameter
          AnzahlBytes:=iAnzahlBytes;
          AnzahlBits:=0;  // nicht verwendet
          RegisterKonvListe:=nil;  // nicht verwendet
          Typ:=sTyp;
          Wert_Einstellen:=sWertNeu;
        end;

        // Modbus-Request zum Setzen des Parameters versenden, Response empfangen:
        if not SendModbusRequest (SlaveData, RegisterRequestData,
                                  MRGTimeouts.ModbusProt, bPreset, sDummy,
                                  iErweiterteStatusgruppe) then begin
          // Modbus-Fehler durch aussagekr�ftigeren Fehler "Parameter wurde nicht
          // ver�ndert" ersetzen
          if iErweiterteStatusgruppe = EST_ZEITSYNCERROR then
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_NOSUCCESS)
          else
            FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
          exit;
        end;

        { Antwort auf "Parameter setzen"-Kommando auswerten:
          -> pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
        if not bPreset then
          bOK:=false;
      end;
    end else
      bOK:=false;  // Parameter konnte trotz bereits aktiver Codewort-Freigabe nicht ge�ndert werden
  end;  // if PWLogin

  Result:=true;
end;

{------------------------------------------------------------------------------}
function TMRGAbruf.ReadModbusStatusregister_TME400 (AMrgTyp: integer;
  var iStatusregister: cardinal; iErweiterteStatusgruppe: integer = 0): boolean;
{------------------------------------------------------------------------------}
{ Liest den Modbus-Parameter 'Statusregister' eines TME400 und liefert dessen
  Wert zur�ck;
  -> Auch f�r RSM200
  �bergaben: Ger�tetyp
             Erweiterte Statusgruppe (optional, EST_...-Konstanten)
  R�ckgabe: Statusregister-Wert
  Ergebnis: true, wenn Lesen des Statusregisters erfolgreich }
var
  sParaNrAllg_Statusregister: string;
  PL: TParameterListe;
  sWert: string;

begin
  Result:=false;
  iStatusregister:=0;  // Vorbelegung R�ckgabe

  if AMrgTyp in [mrgtyp_TME400_VCF, mrgtyp_TME400_VMF] then
    sParaNrAllg_Statusregister:=CP_RMG_TME400_Statusregister
  else if AMrgTyp in [mrgtyp_RSM200_VCF, mrgtyp_RSM200_VMF] then  // 09.01.2024, WW
    sParaNrAllg_Statusregister:=CP_RMG_RSM200_Statusregister;

  PL:=TParameterListe.Create (FKonfigPath);
  try
    if not AbrufParameter (sParaNrAllg_Statusregister, nil, PL) then begin
      FehlerGruppeCodeUpdate (iErweiterteStatusgruppe + Fehlergruppe, Fehlercode);
      exit;
    end;
  finally
    PL.Free;
  end;

  // abgerufenes Statusregister aus Parameterliste lesen:
  if ParameterListe_MRGAbruf.GetValue (sParaNrAllg_Statusregister, sWert) then
    iStatusregister:=StrToInt64Def ('$' + sWert, 0);  // Rohwert ist in Hex

  Result:=true;
end;


{---------------------- Modbus-Routinen f�r FLOWSIC500 ------------------------}

{---------------------------------------------------------------}
function TMRGAbruf.ReadModbusArchiv_SICK (AArchivtyp: TArchivtyp;
  AbrufRec: TAbrufRec; dtVon, dtBis: TDateTime;
  var sFilename_Archiv: string; var iRecType: word): boolean;
{---------------------------------------------------------------}
{ Modbus-Archiv eines FLOWSIC500 lesen und abgerufene Archivdatens�tze in
  Datei schreiben;
  �bergaben: Datenart (Messwerte, Meldungen)
             Abruf-Record
             Daten-Zeitbereich von-bis
  R�ckgaben: Name der Datei mit abgerufenen Archivdatens�tzen
             Typnummer der gelesenen Archiv-Datens�tze
  Ergebnis: true, wenn Modbus-Archivabruf erfolgreich }
var
  SlaveData: TSlaveData;
  RegisterRequestData: TRegisterRequestData;
  iRecOffset: word;
  dtArchivRec: TDateTime;
  bStop: boolean;
  iMaxArchivRecCount: word;
  iRecCount: integer;
  bDummy: boolean;
  iMinRecIndex: integer;
  MBRegisterDef: TMBRegisterDef;
  sDownloadBuf: string;
  sRohDaten: string;
  iRecIndex_neuest: word;
  iRecIndex_aeltest: word;
  iRecCountAvail: word;
  iRecSize: word;
  iRecNextPos: word;
  iDownloadAdr: word;
  iRecIdMax_Check: cardinal;

begin
  Result:=false;
  // Vorbelegung R�ckgaben
  sFilename_Archiv:='';
  iRecType:=0;

  SetModbusSlaveData_AdresseByteOrder (mrgtyp_FLOWSIC500, SlaveData);

  // Zuerst die Parameter mit den Archivinformationen lesen:
  if not ReadModbusArchivInfo_SICK (AArchivtyp, iRecCountAvail, iMaxArchivRecCount,
                                    iRecSize, iRecType, iRecNextPos) then exit;

  // Archiv auslesen, wenn der Archivheader kein leeres Archiv anzeigt:
  if iRecCountAvail > 0 then begin
    // Datei zum Zwischenspeichern der abgerufenen Archivwerte anlegen:
    sFilename_Archiv:=CreateTempRohFile (FWorkPath, prefix_MB_Konv);
    if sFilename_Archiv = '' then begin
      FehlerGruppeCodeUpdate(ST_FILEERROR, FILEERR_COULDNOTWRITE);
      exit;
    end;

    // Der Index des j�ngsten Archivdatensatzes wird berechnet aus der Anzahl
    // der im Ger�t vorhandenen Datens�tze und der maximal m�glichen Anzahl:
    if iRecCountAvail < iMaxArchivRecCount then begin
      iRecIndex_neuest:=iRecCountAvail - 1;
      iRecIndex_aeltest:=0;
    end
    else begin
      if iRecNextPos > 0 then
        iRecIndex_neuest:=iRecNextPos - 1
      else
        iRecIndex_neuest:=iMaxArchivRecCount;
      iRecIndex_aeltest:=iRecNextPos;
    end;

    // Es wird begonnen mit dem Auslesen der j�ngsten Archivdatens�tze beginnend
    // vom neuesten Index bis runter zu Index 0:
    iRecOffset:=iRecIndex_neuest;  
    iMinRecIndex:=0;    

    // Default: H�chste zur�ckzuliefernde Record-ID lesen (aus 1. gelesenem 
    // Download-Puffer)
    iRecIdMax_Check:=0;

    bStop:=false;
    repeat
      // Download-Adresse zum Lesen von Archivdatens�tzen schreiben:
      if not WriteModbusArchivDownloadAdresse_SICK (AArchivtyp, iRecOffset,
                                                    iMaxArchivRecCount,
                                                    iRecSize, iRecCount,
                                                    iDownloadAdr,
                                                    iMinRecIndex) then begin
        DeleteFile (sFilename_Archiv);
        exit;
      end;

      // Modbus-Request zum Lesen des Download-Puffers versenden, Response empfangen:
      MBRegisterDef:=FLOWSIC500_MBRegisterDef_Para_DownloadPuffer;

      with RegisterRequestData do begin
        FktCode:=3;
        StartAdresse:=MBRegisterDef.StartAdresse;  // Registeradresse Download-Puffer
        AnzahlBytes:=MBRegisterDef.AnzahlBytes;
        AnzahlBits:=0;  // nicht verwendet
        RegisterKonvListe:=nil;  // nicht verwendet
        Typ:=MBRegisterDef.Typ;
        Wert_Einstellen:='';  // nicht verwendet
      end;

      if not SendModbusRequest (SlaveData, RegisterRequestData,
                                MRGTimeouts.ModbusProt, bDummy,
                                sDownloadBuf) then begin
        DeleteFile (sFilename_Archiv);
        exit;
      end;

      // Gelesenen Download-Puffer plausibilisieren:
      if not Valid_SICK_DownloadPuffer (sDownloadBuf, iDownloadAdr, iRecSize) then begin
        DeleteFile (sFilename_Archiv);
        case AArchivtyp of
          at_Periodenarchiv:
            FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_MWINVALID);

          at_Ereignisarchiv,
          at_Parameterarchiv_eichamtlich,
          at_Parameterarchiv_nichteichamtlich,
          at_Parameterarchiv_Gas:
            FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_MEINVALID);
        end;
        exit;
      end;

      // Zeitstempel des ersten (�ltesten) der gelesenen Archivdatens�tze ermitteln:
      dtArchivRec:=GetFirstArchivRec_DatumZeit_SICK (sDownloadBuf);

      // Download-Puffer trimmen (Archivdatens�tze ausschneiden, Reihenfolge umkehren):
      // -> Da w�hrend des Lesens vom neuesten zur�ck bis zum �ltesten Datensatz
      //    bei vollem Archiv der oder die �ltesten Datens�tze an deren Position
      //    inzwischen durch neuere �berschrieben sein k�nnen (und in diesem Fall
      //    zu verwerfen sind !), erfolgt eine zus�tzliche Pr�fung auf die h�chste
      //    Record-ID.
      sRohDaten:=TrimAndSwap_SICK_DownloadPuffer (sDownloadBuf, iRecSize, iRecCount,
                                                  iRecIdMax_Check);

      // Archivdatens�tze in Datei zwischenspeichern f�r Konvertierung:
      if not WriteRohfile (sFilename_Archiv, sRohDaten) then begin
        DeleteFile (sFilename_Archiv);
        FehlerGruppeCodeUpdate (ST_FILEERROR, FILEERR_COULDNOTWRITE);
        exit;
      end;

      if iRecOffset >= iRecCount then begin
        dec (iRecOffset, iRecCount);  // Zum Auslesen der weiteren �lteren Archivdatens�tze

        // Kein weiteres Auslesen von Archivdatens�tzen, wenn von-Zeitpunkt oder
        // der Archivdatensatz mit dem �ltesten Index gelesen wurde:
        if (iRecOffset < iMinRecIndex) OR
           (not AbrufRec.AlleDaten AND (CmpDateTime (dtArchivRec, dtVon) <= 0)) then begin
          bStop:=true;
        end;
      end
      else begin
        if (iMinRecIndex = 0) AND
           (iRecIndex_neuest < iRecIndex_aeltest) then begin
          // Weiter Auslesen der n�chsten Archivdatens�tze beginnend
          // vom maximalen Index bis runter zum �ltesten Index:
          iRecOffset:=iMaxArchivRecCount - 1;
          iMinRecIndex:=iRecIndex_aeltest;
        end else
          bStop:=true;
      end;
    until bStop;
  end;  // if iRecCountAvail > 0

  Result:=true;
end;

{-------------------------------------------------------------------}
function TMRGAbruf.ReadModbusArchivInfo_SICK (AArchivtyp: TArchivtyp;
  var iRecCountAvail: word; var iRecMax: word; var iRecSize: word;
  var iRecType: word; var iRecNextPos: word): boolean;
{-------------------------------------------------------------------}
{ Liest die Modbus-Archivinfo-Parameter eines SICK-Archivs und liefert diese zur�ck;
  �bergabe: Archivtyp
  R�ckgaben: Anzahl der vorhandenen Archiv-Datens�tze
             Maximal m�gliche Anzahl an Archiv-Datens�tzen
             Gr��e des Archiv-Datensatzes (Bytes)
             Datensatz-Typnummer
             Position (Index) des n�chsten vom Ger�t zu archivierenden Datensatzes
  Ergebnis: true, wenn Lesen der Modbus-Archivinfo-Parameter erfolgreich }
var
  PL: TParameterListe;
  slParaAllg: TStringList;
  sParaNrAllg_RecCountAvail: string;
  sParaNrAllg_RecMax: string;
  sParaNrAllg_RecSize: string;
  sParaNrAllg_RecType: string;
  sParaNrAllg_RecNextPos: string;
  sWert: string;

begin
  Result:=false;
  // Vorbelegung R�ckgaben:
  iRecCountAvail:=0;
  iRecMax:=0;
  iRecSize:=0;
  iRecType:=0;
  iRecNextPos:=0;

  case AArchivtyp of
    at_Periodenarchiv:
      begin
        sParaNrAllg_RecCountAvail:=CP_SICK_FLOWSIC500_MPArchiv_RecCount;
        sParaNrAllg_RecMax:=CP_SICK_FLOWSIC500_MPArchiv_RecMax;
        sParaNrAllg_RecSize:=CP_SICK_FLOWSIC500_MPArchiv_RecSize;
        sParaNrAllg_RecType:=CP_SICK_FLOWSIC500_MPArchiv_RecType;
        sParaNrAllg_RecNextPos:=CP_SICK_FLOWSIC500_MPArchiv_RecNextPos;
      end;

    at_Ereignisarchiv:
      begin
        sParaNrAllg_RecCountAvail:=CP_SICK_FLOWSIC500_EreignisLB_RecCount;
        sParaNrAllg_RecMax:=CP_SICK_FLOWSIC500_EreignisLB_RecMax;
        sParaNrAllg_RecSize:=CP_SICK_FLOWSIC500_EreignisLB_RecSize;
        sParaNrAllg_RecType:=CP_SICK_FLOWSIC500_EreignisLB_RecType;
        sParaNrAllg_RecNextPos:=CP_SICK_FLOWSIC500_EreignisLB_RecNextPos;
      end;

    at_Parameterarchiv_nichteichamtlich:
      begin
        sParaNrAllg_RecCountAvail:=CP_SICK_FLOWSIC500_ParaLB_RecCount;
        sParaNrAllg_RecMax:=CP_SICK_FLOWSIC500_ParaLB_RecMax;
        sParaNrAllg_RecSize:=CP_SICK_FLOWSIC500_ParaLB_RecSize;
        sParaNrAllg_RecType:=CP_SICK_FLOWSIC500_ParaLB_RecType;
        sParaNrAllg_RecNextPos:=CP_SICK_FLOWSIC500_ParaLB_RecNextPos;
      end;

    at_Parameterarchiv_eichamtlich:
      begin
        sParaNrAllg_RecCountAvail:=CP_SICK_FLOWSIC500_MetrologLB_RecCount;
        sParaNrAllg_RecMax:=CP_SICK_FLOWSIC500_MetrologLB_RecMax;
        sParaNrAllg_RecSize:=CP_SICK_FLOWSIC500_MetrologLB_RecSize;
        sParaNrAllg_RecType:=CP_SICK_FLOWSIC500_MetrologLB_RecType;
        sParaNrAllg_RecNextPos:=CP_SICK_FLOWSIC500_MetrologLB_RecNextPos;
      end;

    at_Parameterarchiv_Gas:
      begin
        sParaNrAllg_RecCountAvail:=CP_SICK_FLOWSIC500_GasParaLB_RecCount;
        sParaNrAllg_RecMax:=CP_SICK_FLOWSIC500_GasParaLB_RecMax;
        sParaNrAllg_RecSize:=CP_SICK_FLOWSIC500_GasParaLB_RecSize;
        sParaNrAllg_RecType:=CP_SICK_FLOWSIC500_GasParaLB_RecType;
        sParaNrAllg_RecNextPos:=CP_SICK_FLOWSIC500_GasParaLB_RecNextPos;
      end;
  end;

  PL:=TParameterListe.Create (FKonfigPath);
  try
    slParaAllg:=TStringList.Create;  // Liste f�r abzurufende allg. Parameternummern
    try
      slParaAllg.Add (sParaNrAllg_RecCountAvail);
      slParaAllg.Add (sParaNrAllg_RecMax);
      slParaAllg.Add (sParaNrAllg_RecSize);
      slParaAllg.Add (sParaNrAllg_RecType);
      slParaAllg.Add (sParaNrAllg_RecNextPos);

      if not AbrufParameter ('', slParaAllg, PL) then exit;
    finally
      slParaAllg.Free;
    end;
  finally
    PL.Free;
  end;

  // Archivinfo-Werte aus Parameterliste lesen:
  if ParameterListe_MRGAbruf.GetValue (sParaNrAllg_RecCountAvail, sWert) then
    iRecCountAvail:=StrToIntDef (sWert, 0);
  if ParameterListe_MRGAbruf.GetValue (sParaNrAllg_RecMax, sWert) then
    iRecMax:=StrToIntDef (sWert, 0);
  if ParameterListe_MRGAbruf.GetValue (sParaNrAllg_RecSize, sWert) then
    iRecSize:=StrToIntDef (sWert, 0);
  if ParameterListe_MRGAbruf.GetValue (sParaNrAllg_RecType, sWert) then
    iRecType:=StrToIntDef (sWert, 0);
  if ParameterListe_MRGAbruf.GetValue (sParaNrAllg_RecNextPos, sWert) then
    iRecNextPos:=StrToIntDef (sWert, 0);

  Result:=true;
end;

{-------------------------------------------------------------------------------}
function TMRGAbruf.WriteModbusArchivDownloadAdresse_SICK (AArchivtyp: TArchivtyp;
  iRecOffset, iMaxRecCount, iRecByteSize: word; var iRecCount: integer;
  var iDownloadAdr: word; iMinRecIndex: integer): boolean;
{-------------------------------------------------------------------------------}
{ Download-Adresse zum Lesen von SICK-Archivdatens�tzen im Ger�t einstellen;
  �bergaben: Archivtyp
             Datensatz-Offset (0 = j�ngster Datensatz, 1 = 2.-j�ngster usw.)
             Maximale Anzahl der zu lesenden Datens�tze des Archivs
             Minimaler Datensatz-Index
  R�ckgaben: Anzahl der Datens�tze, die per Download-Puffer gelesen werden
             Download-Adresse
  Ergebnis: true, wenn Einstellen der Download-Adresse erfolgreich }

  {------------------------------------------------------------------}
  function GetRecCount_Request (iArchivRecByteSize: integer): integer;
  {------------------------------------------------------------------}
  { Anzahl der Archivdatens�tze berechnen, die per Download-Puffer maximal
    ausgelesen werden k�nnen;
    �bergabe: Byte-Gr��e des Archivdatensatzes
    Ergebnis: Anzahl der Archivdatens�tze }
  begin
    Result:=FLOWSIC500_MBRegisterDef_Para_DownloadPuffer.AnzahlBytes DIV
            iArchivRecByteSize;

    // Pr�fen, ob das untere Ende des Archiv-Indexbereichs �berschritten w�re:
    if (iRecOffset < (iMinRecIndex + Result - 1)) then
      Result:=iRecOffset + 1 - iMinRecIndex;  // Anzahl Datens�tze nur bis zum
                                              // unteren Ende des Archiv-Indexbereichs
  end;

var
  bOK: boolean;
  iArchivID: integer;

begin
  Result:=false;

  // Anzahl der Archivdatens�tze berechnen, die per Download-Puffer maximal
  // ausgelesen werden k�nnen:
  iRecCount:=GetRecCount_Request (iRecByteSize);

  // Download-Adresse abh�ngig von der Anzahl der Archivdatens�tze je Download...
  // -> z.B. Datensatz-Offset = 200, maximale Anzahl Datens�tze = 10:
  //    Abfrage der Download-Adressen 191..200
  iDownloadAdr:=iRecOffset + 1 - iRecCount;

  // ... und der Archiv-ID (LB_ID 1..7 aus SICK Excel-Registertabelle minus 1):
  case AArchivtyp of
    at_Ereignisarchiv: iArchivID:=0;  // Ereignislogbuch
    at_Parameterarchiv_nichteichamtlich: iArchivID:=1;  // Parameterlogbuch
    at_Parameterarchiv_eichamtlich: iArchivID:=2;  // Eichamtliches Logbuch
    at_Periodenarchiv: iArchivID:=3;  // Messperiodenarchiv
    at_Parameterarchiv_Gas: iArchivID:=6;  // Gasparameterlogbuch
  else
    iArchivID:=6;
  end;

  iDownloadAdr:=(iArchivID * 10000) + iDownloadAdr;

  // Modbus-Parameter "Download-Adresse" einstellen:
  if not WriteModbusParameter_SICK (FLOWSIC500_MBRegisterDef_Para_DownloadAdresse.StartAdresse,
                                    FLOWSIC500_MBRegisterDef_Para_DownloadAdresse.Typ,
                                    FLOWSIC500_MBRegisterDef_Para_DownloadAdresse.AnzahlBytes,
                                    IntToStr (iDownloadAdr),
                                    bOK) then begin
    exit;
  end;

  { Pr�fen, ob Download-Adresse im Ger�t tats�chlich ge�ndert wurde }
  if not bOK then begin
    case AArchivtyp of
      at_Periodenarchiv:
        FehlerGruppeCodeUpdate (EST_MWLESENERROR, MWLESENERR_ZUGRIFFSRECHT);
        
      at_Ereignisarchiv,
      at_Parameterarchiv_nichteichamtlich,
      at_Parameterarchiv_eichamtlich,
      at_Parameterarchiv_Gas:
        FehlerGruppeCodeUpdate (EST_MELESENERROR, MELESENERR_ZUGRIFFSRECHT);
    else
      FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
    end;
    exit;
  end;

  Result:=true;
end;

{------------------------------------------------------------------------}
function TMRGAbruf.WriteModbusParameter_SICK (iStartAdresse: word;
  sTyp: string; iAnzahlBytes: integer; sWertNeu: string; var bOK: boolean;
  iErweiterteStatusgruppe: integer = 0): boolean;
{------------------------------------------------------------------------}
{ Modbus-Parameter im FLOWSIC500 einstellen;
  �bergaben: Register-Startadresse des Parameters
             Modbus-Datentyp
             Anzahl der Bytes
             Erweiterte Statusgruppe (optional, EST_...-Konstanten)
  R�ckgabe: Flag 'bOK': true, wenn Einstellen erfolgreich
  Ergebnis: true, wenn Schreiben des Modbus-Parameters erfolgreich }
var
  SlaveData: TSlaveData;
  RegisterRequestData: TRegisterRequestData;
  bPreset: boolean;
  bKonfigModus: boolean;
  iAccessLevel: cardinal;
  bKonfigModusAktiv: boolean;
  sDummy: string;

begin
  Result:=false;
  bOK:=true;  // Default: Parametrierung erfolgreich

  SetModbusSlaveData_AdresseByteOrder (mrgtyp_FLOWSIC500, SlaveData);

  // 1. Versuch: Parameter per Modbus setzen ohne ggf. erforderlichen Konfigurationsmodus
  // im Ger�t (f�r manche Parameter oder bei bereits gesetztem Konfigurationsmodus
  // ist kein Aktivieren des Konfigurationsmodus n�tig.)
  bKonfigModus:=false;  // Default: Kein Setzen des Konfigurationsmodus
  with RegisterRequestData do begin
    FktCode:=16;
    StartAdresse:=iStartAdresse;  // Registeradresse Parameter
    AnzahlBytes:=iAnzahlBytes;
    AnzahlBits:=0;  // nicht verwendet
    RegisterKonvListe:=nil;  // nicht verwendet
    Typ:=sTyp;
    Wert_Einstellen:=sWertNeu;
  end;

  // Modbus-Request zum Setzen des Parameters versenden, Response empfangen:
  if SendModbusRequest (SlaveData, RegisterRequestData,
                        MRGTimeouts.ModbusProt, bPreset, sDummy,
                        iErweiterteStatusgruppe) then begin
    { Antwort auf "Parameter setzen"-Kommando auswerten:
      -> pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
    if not bPreset then
      bOK:=false;
  end
  else begin
    if (Fehlergruppe = iErweiterteStatusgruppe + COM_MODBUSERROR) AND
       (Fehlercode = MODBUSERR_EXC_SLAVEDEVICEFAILURE) then  // Anm. WW: K�nnte evtl. auch vom AccessLevel-Bit abh�ngig gemacht werden.
                                                             // Bislang allerdings kein Grund bekannt, da� dies erforderlich ist.
      bKonfigModus:=true  // Flag setzen: Weiterer Versuch mit Konfigurationsmodus
    else
      exit;
  end;

  if bKonfigModus then begin
    if VerbAufbau_PasswortNr in [1, 2] then begin
      // 2. Versuch: Konfigurationsmodus im Ger�t einschalten, wenn dieser im Ger�t
      // aktuell nicht aktiv ist. Ist nur m�glich, wenn man Admin oder autorisierter
      // Nutzer ist (PW-Nummer 1 oder 2)

      // Parameter "Access level" lesen, um zu pr�fen, ob der Konfigurationsmodus
      // aktiv ist:
      if not ReadModbusAccessLevel_SICK (iAccessLevel, iErweiterteStatusgruppe) then exit;

      // Bit 7 des Access level: Konfigurationsmodus
      bKonfigModusAktiv:=(iAccessLevel AND $80) <> 0;

      if not bKonfigModusAktiv then begin
        // Konfigurationsmodus im Ger�t einschalten
        if not SetKonfigurationsmodus_SICK (true, iErweiterteStatusgruppe) then exit;

        FParametrierModus_gesetzt:=true;  // Flag setzen: Konfigurationsmodus wurde eingeschaltet
                                          // -> Als Merker, um den Konfigurationsmodus am Ende
                                          //    des Abrufs wieder zu deaktivieren

        // Wenn die Freigabe des Codeworts erfolgreich war, dann Parameter nochmal
        // versuchen einzustellen:
        with RegisterRequestData do begin
          FktCode:=16;
          StartAdresse:=iStartAdresse;  // Registeradresse Parameter
          AnzahlBytes:=iAnzahlBytes;
          AnzahlBits:=0;  // nicht verwendet
          RegisterKonvListe:=nil;  // nicht verwendet
          Typ:=sTyp;
          Wert_Einstellen:=sWertNeu;
        end;

        // Modbus-Request zum Setzen des Parameters versenden, Response empfangen:
        if not SendModbusRequest (SlaveData, RegisterRequestData,
                                  MRGTimeouts.ModbusProt, bPreset, sDummy,
                                  iErweiterteStatusgruppe) then begin
          // Modbus-Fehler durch aussagekr�ftigeren Fehler "Parameter wurde nicht
          // ver�ndert" ersetzen
          if iErweiterteStatusgruppe = EST_ZEITSYNCERROR then
            FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_NOSUCCESS)
          else
            FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
          exit;
        end;

        { Antwort auf "Parameter setzen"-Kommando auswerten:
          -> pr�fen, ob Parameter im Ger�t tats�chlich ge�ndert wurde }
        if not bPreset then
          bOK:=false;
      end else
        bOK:=false;  // Parameter konnte trotz bereits aktivem Konfigurationsmodus nicht ge�ndert werden
    end else
      bOK:=false;  // Parameter kann bei Login als Nutzer oder Gast nicht ge�ndert werden
  end;  // if bKonfigModus AND (VerbAufbau_PasswortNr in [1, 2])

  Result:=true;
end;

{--------------------------------------------------------------}
function TMRGAbruf.SetKonfigurationsmodus_SICK (bOnOff: boolean;
  iErweiterteStatusgruppe: integer = 0): boolean;
{--------------------------------------------------------------}
{ Konfigurationsmodus im FLOWSIC500 ein-/ausschalten (Modbus);
  �bergaben: Flag: Ein (true), Aus (false)
             Erweiterte Statusgruppe (optional, EST_...-Konstanten)
  Ergebnis: true, wenn Setzen des Konfigurationsmodus erfolgreich }
var
  SlaveData: TSlaveData;
  RegisterRequestData: TRegisterRequestData;
  bPreset: boolean;
  MBRegisterDef: TMBRegisterDef;
  sDummy: string;

begin
  Result:=false;

  SetModbusSlaveData_AdresseByteOrder (mrgtyp_FLOWSIC500, SlaveData);

  // Registeradresse f�r das Ein-/Ausschalten des Konfigurationsmodus:
  if bOnOff then
    MBRegisterDef:=FLOWSIC500_MBRegisterDef_Cmd_SetMaintenanceMode
  else
    MBRegisterDef:=FLOWSIC500_MBRegisterDef_Cmd_ResetMaintenanceMode;

  with RegisterRequestData do begin
    FktCode:=16;
    StartAdresse:=MBRegisterDef.StartAdresse;
    AnzahlBytes:=MBRegisterDef.AnzahlBytes;
    AnzahlBits:=0;  // nicht verwendet
    RegisterKonvListe:=nil;  // nicht verwendet
    Typ:=MBRegisterDef.Typ;
    Wert_Einstellen:='1';
  end;

  // Modbus-Request zum Ein-/Ausschalten des Konfigurationsmodus versenden, Response empfangen:
  if not SendModbusRequest (SlaveData, RegisterRequestData,
                            MRGTimeouts.ModbusProt, bPreset, sDummy,
                            iErweiterteStatusgruppe) then exit;

  { Antwort auf "Ein-/Ausschalten des Konfigurationsmodus"-Kommando auswerten:
    -> pr�fen, ob Kommando ins Ger�t korrekt �bertragen wurde }
  if not bPreset then begin
    FehlerGruppeCodeUpdate (iErweiterteStatusgruppe + COM_MODBUSERROR,
                            MODBUSERR_RESP_WRONGREGISTEREADDRESS);
    exit;
  end;

  Result:=true;
end;

{------------------------------------------------------------------------}
function TMRGAbruf.ReadModbusAccessLevel_SICK (var iAccessLevel: cardinal;
  iErweiterteStatusgruppe: integer = 0): boolean;
{------------------------------------------------------------------------}
{ Liest den Modbus-Parameter 'Access level' eines FLOWSIC500 und liefert dessen
  Wert zur�ck;
  �bergabe: Erweiterte Statusgruppe (optional, EST_...-Konstanten)
  R�ckgabe: AccessLevel-Wert
  Ergebnis: true, wenn Lesen des Access level erfolgreich }
var
  sParaNrAllg_AccessLevel: string;
  PL: TParameterListe;
  sWert: string;

begin
  Result:=false;
  iAccessLevel:=0;  // Vorbelegung R�ckgabe

  sParaNrAllg_AccessLevel:=CP_SICK_FLOWSIC500_AccessLevel;

  PL:=TParameterListe.Create (FKonfigPath);
  try
    if not AbrufParameter (sParaNrAllg_AccessLevel, nil, PL) then begin
      FehlerGruppeCodeUpdate (iErweiterteStatusgruppe + Fehlergruppe, Fehlercode);
      exit;
    end;
  finally
    PL.Free;
  end;

  // abgerufenen Access level aus Parameterliste lesen:
  if ParameterListe_MRGAbruf.GetValue (sParaNrAllg_AccessLevel, sWert) then
    iAccessLevel:=StrToInt64Def ('$' + sWert, 0);  // Rohwert ist in Hex

  Result:=true;
end;

{-----------------------------------------------------------------}
function TMRGAbruf.Valid_SICK_DownloadPuffer (sDownloadBuf: string;
  iDownloadAdr, iRecSize: word): boolean;
{-----------------------------------------------------------------}
{ Plausibilisiert gelesenen SICK-Download-Puffer;
  �bergaben: Download-Puffer
             Soll-Download-Adresse des ersten enthaltenen Datensatzes
             Soll-Datensatzgr��e (Bytes)
  Ergebnis: True, wenn Download-Puffer ok }
var
  iRecCount: byte;
  iBytesOneRecord: integer;
  iBytesRecords: integer;
  iMaxBytesRecords: integer;
  iRecAdr: word;
  iRecStartPos: integer;
  i: integer;
  S: string;

begin
  Result:=false;

  // Der Download-Puffer darf nicht leer sein:
  if length (sDownloadBuf) > 0 then begin
    iRecCount:=Bin2Byte (sDownloadBuf [1]);  // 1. Byte: Anzahl der enthaltenen Datens�tze

    // Es mu� mindestens 1 Datensatz enthalten sein:
    if iRecCount > 0 then begin
      // Ist die Anzahl der Datens�tze plausibel ? Pr�fen, ob die Anzahl incl.
      // der jeweils vorangestellten Datensatz-Adresse (2 Bytes) im Download-Puffer
      // Platz hat:
      iBytesOneRecord:=2 + iRecSize;
      iBytesRecords:=iBytesOneRecord * iRecCount;
      iMaxBytesRecords:=
        FLOWSIC500_MBRegisterDef_Para_DownloadPuffer.AnzahlBytes - 1;  // ohne 1. Byte
      if iBytesRecords <= iMaxBytesRecords then begin
        // Sind die korrekten Datensatz-Adressen enthalten ? Pr�fen, ob mit
        // �bergebener Download-Adresse beginnend und fortlaufend:
        for i:=0 to iRecCount - 1 do begin
          // 2 Bytes Datensatz-Adresse, Datensatz-Gr��e, Offset 1. Byte
          iRecStartPos:=(iBytesOneRecord * i) + 1 + 1;
          S:=Copy (sDownloadBuf, iRecStartPos, 2);
          iRecAdr:=Bin2Word (S);

          if iRecAdr <> (iDownloadAdr + i) then exit;  // Fehler unerwartete Datensatz-Adresse
        end;

        Result:=true;  // OK
      end;
    end;
  end;
end;

{-----------------------------------------------------------------------}
function TMRGAbruf.TrimAndSwap_SICK_DownloadPuffer (sDownloadBuf: string;
  iRecSize, iRecCountMax: word; var iRecIdMax_Check: cardinal): string;
{-----------------------------------------------------------------------}
{ Schneidet am Anfang das 1. Byte (Anzahl der Datens�tze) und am Ende die F�llbytes
  des Download-Puffers weg, sodass nur die reinen Datensatz-Rohdaten incl.
  Datensatz-Adressen bleiben; Zus�tzlich erfolgt ein Ermitteln bzw. Pr�fen der
  h�chsten im Download-Puffer enthaltenen Record-ID
  -> Datens�tze werden in umgekehrter Reihenfolge zur�ckgegeben
  �bergaben: Download-Puffer
             Datensatzgr��e (Bytes)
             Maximale Anzahl an Datens�tzen, auf die getrimmt werden soll
  �bergabe/R�ckgabe:
             H�chste im Ergebnis zur�ckzugebende Record-ID
               (0 = H�chste im Download-Puffer enthaltene Record-ID lesen)
  Ergebnis: Getrimmter und geswappter Download-Puffer }
var
  iRecCount: byte;
  iBytesRecords: integer;
  iBytesOneRecord: integer;
  iRecStartPos: integer;
  sRecord: string;
  i: integer;
  S: string;
  iPosRecordID: integer;
  sRecID: string;
  iRecID: cardinal;
  bCheck_RecIdMax: boolean;
  bAdd: boolean;

begin
  // H�chste Record-ID lesen oder pr�fen?
  bCheck_RecIdMax:=iRecIdMax_Check > 0;  // 0 = lesen; > 0 = pr�fen 

  S:=sDownloadBuf;

  // 1. Byte: Anzahl der enthaltenen Datens�tze lesen und wegschneiden
  iRecCount:=Bin2Byte (sDownloadBuf [1]);
  System.Delete (S, 1, 1);

  // Auf maximale Soll-Anzahl an Datens�tzen trimmen
  if iRecCountMax < iRecCount then
    iRecCount:=iRecCountMax;

  // Anzahl der von den Datens�tzen belegten Bytes incl. der jeweils vorangestellten
  // Datensatz-Adresse (2 Bytes):
  iBytesOneRecord:=2 + iRecSize;
  iBytesRecords:=iBytesOneRecord * iRecCount;

  // NULL-F�llbytes am Ende wegschneiden:
  S:=Copy (S, 1, iBytesRecords);

  // Datens�tze in umgekehrter Reihenfolge zur�ckgeben:
  Result:='';
  for i:=iRecCount - 1 downto 0 do begin
    iRecStartPos:=(iBytesOneRecord * i) + 1;
    sRecord:=Copy (S, iRecStartPos, iBytesOneRecord);

    // Record-ID lesen/pr�fen:
    // Die Record-ID steht im Download-Puffer unabh�ngig vom Datensatztyp der
    // einzelnen Archive und Logb�cher immer an einer fixen Position nach der
    // Datensatz-Adresse (2 Bytes):
    iPosRecordID:=3;
    sRecID:=Copy (sRecord, iPosRecordID, 4);  // Record-ID (4 Bytes)
    iRecID:=Bin2Longword (sRecID);

    bAdd:=true;
    if bCheck_RecIdMax then begin  // Record-ID pr�fen
      if iRecID > iRecIdMax_Check then  // Record-ID �berschreitet h�chte erlaubte
        bAdd:=false;  // Datensatz weglassen
    end
    else begin  // h�chste Record-ID lesen
      if iRecID > iRecIdMax_Check then
        iRecIdMax_Check:=iRecID;
    end;

    if bAdd then
      Result:=Result + sRecord;
  end;
end;

{------------------------------------------------------------------------------------}
function TMRGAbruf.GetFirstArchivRec_DatumZeit_SICK (sDownloadBuf: string): TDateTime;
{------------------------------------------------------------------------------------}
{ Liefert Zeitstempel des ersten (�ltesten) im Download-Puffer enthaltenen
  SICK-Archivdatensatzes;
  �bergaben: Download-Puffer
  Ergebnis: Zeitstempel }
var
  iPosTimestamp: integer;
  S: string;
  c: cardinal;

begin
  // Der Zeitstempel des ersten Datensatzes steht im Download-Puffer unabh�ngig
  // vom Datensatztyp der einzelnen Archive und Logb�cher immer an
  // einer fixen Position nach Anzahl Datens�tze (1 Byte), Datensatz-Adresse (2 Bytes),
  // Datensatz-ID (4 Bytes):
  iPosTimestamp:=8;
  S:=Copy (sDownloadBuf, iPosTimestamp, 4);  // UNIX-Zeitstempel (4 Bytes)
  c:=Bin2Longword (S);
  UnixSekundenToDateTime (c, Result);
end;


{--------------- Routinen f�r internes FTL-Protokoll (Tritschler) -------------}

{-----------------------------------------------}
function TMRGAbruf.Init_FTLintern_Kommunikation (
  iErweiterteStatusgruppe: integer = 0): boolean;
{-----------------------------------------------}
{ Einleiten der Ger�te-Kommunikation f�r internes FTL-Protokoll;
  �bergabe: Erweiterte Statusgruppe (optional, EST_...-Konstanten)
  Ergebnis: true, wenn erfolgreich }
const
  CMaxVersuche_Init_FTLintern = 6;
  CTimeout_Init_FTLintern     = 2000;
  // -> Die Konstanten-Werte beruhen auf Testergebnissen (lokale IR-Schnittstelle, IP):
  //    VC2: Antwort beim 2. Versuch (TO 2 s), Antwort kommt nach 0,08 s (TO 5s: keine Antwort !)
  //    VC3: Antwort schon beim 1. Versuch, nach 0,03 s
  //    IP-Kommunikation: H�ufig zuerst Timeout, dann FTL-Fehlerantwort, dann FTL-OK-Antwort

var
  Befehl: string;
  Versuch: byte;
  R: TRueckgabe;

begin
  Result:=false;

  { Weck-Befehl senden, ggf. wiederholen bis Antwort kommt: }
  Befehl:=GetTritschler_FTLinternKommando (001, '', 'L');

  Versuch:=0;
  while true do begin
    inc (Versuch);
    if not TMRGCustomCommObj (CommObj).SendTritschlerFTL_internCommand
      (Befehl, CTimeout_Init_FTLintern, R, NoCarrier) then begin
      if ((R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT)) OR
         (R.Fehlergruppe = COM_FTLINTERN_ERROR) then begin
        if Versuch >= CMaxVersuche_Init_FTLintern then begin
          FehlerGruppeCodeUpdate (iErweiterteStatusgruppe + R.Fehlergruppe, R.Fehlercode);
          exit;  { Timeout-Fehler nach letztem Versuch }
        end;
      end
      else begin
        FehlerGruppeCodeUpdate (iErweiterteStatusgruppe + R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end
    else begin  { OK }
      Result:=true;
      Break;
    end;
  end;  { while }
end;

end.


