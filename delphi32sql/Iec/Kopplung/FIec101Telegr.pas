{******************************************************************************}
{* Unit: Telegramm-Formular für IEC-Kopplung (32-Bit), IEC 870-5-101          *}
{* 26.05.2003  WW                                                             *}
{******************************************************************************}
unit FIec101Telegr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, DateUtils,
  IecConst, FIecDaten, IecLog, WChars, IecIniFile, Iec870Util, FIecCustomTelegr,
  IecImportDirList, LogFile, O_IecRemoteMonitor;

type
  { Struktur für IEC-Telegramm 101 }
  TIec101Telegramm = record
    bE5: boolean;  { true, wenn Einzelsteuerzeichen E5 }
    Funktionscode: byte;  { Funktionscode im Steuerfeld }
    Liniennummer: integer;  { Liniennummer im Adressfeld }
    DatenEinheitIdent: TIecDatenEinheitIdent;  { Identifikationsfeld der Dateneinheit }
    sObjekte: string;  { Informationsobjekt(e) }
  end;

  { Stati für symmetrische Übertragung }
  TIec101SymmStatus = (iss_Wait,
                       iss_SndLinkStatus,
                       iss_RecLinkStatus,
                       iss_SndLinkReset,
                       iss_RecLinkReset,
                       iss_SndLinkReady,
                       iss_RecLinkReady
                      );

  { Telegramm-Formular IEC 870-5-101 }
  TFormTelegrIec101 = class(TFormCustomTelegrIec)
    GroupBox1: TGroupBox;
    lStart: TLabel;
    lLaenge: TLabel;
    lSteuerfeld: TLabel;
    lAdressfeld: TLabel;
    lTypkennung: TLabel;
    lChecksum: TLabel;
    lEnde: TLabel;
    EStart1: TEdit;
    ELaenge1: TEdit;
    ESteuerfeld: TEdit;
    EAdressfeldLow: TEdit;
    ETypkennung: TEdit;
    EChecksum: TEdit;
    EEnde: TEdit;
    GroupBox2: TGroupBox;
    Label8: TLabel;
    lFktCode_dez: TLabel;
    EFormatPruefung: TEdit;
    EFktCode_dez: TEdit;
    EAdressfeldHigh: TEdit;
    lLinienNr_dez: TLabel;
    ELinienNr_dez: TEdit;
    lTypkennung_dez: TLabel;
    ETypkennung_dez: TEdit;
    ELaenge2: TEdit;
    EStart2: TEdit;
    lEinzel: TLabel;
    eEinzel: TEdit;
    GroupBox3: TGroupBox;
    lAnzSekSendTelegr: TLabel;
    eAnzSekSendTelegr: TEdit;
    lAnzPrmSendTelegr: TLabel;
    eAnzPrmSendTelegr: TEdit;
    lTimeout: TLabel;
    eTimeout: TEdit;
    lDFC: TLabel;
    eDFC: TEdit;
  private
    { Private-Deklarationen }
    Telegramm_senden: byte;
    Prm_SendTelegrammList: TSendTelegrammList;  { Liste mit zu versendenden Telegrammen der Primärstation }
    Sek_SendTelegrammList: TSendTelegrammList;  { Liste mit zu versendenden Telegrammen der Sekundärstation }
    EmpfTelegramm: TIec101Telegramm;  { von Leitwarte empfangenes IEC-Telegramm als Record }
    bEmpfDIR_symm: boolean;  { DIR-Bit im von LW empfangenen Telegramm (symmetrische Übertragung ) }
    bEmpfDFC: boolean;  { DFC-Bit im von LW empfangenen Sekundärstation-Telegramm }

    bSendFCB_letzt: boolean;  { Merker für FCB des letzten gesendeten Telegramms }
    bEmpfFCB_letzt: boolean;  { Merker für FCB des letzten empfangenen Telegramms }
    ErstesTelegramm: boolean;  { Flag zum Prüfen des ersten empfangenen Telegramms }
    LiniennummerMerk: integer;  { Merker für Liniennummer }
    LinkReset_LW: boolean;  { Flag zum Prüfen, ob LW Link-Reset durchgeführt hat }
    dtDelayPrmTestLink: TDateTime;

    TG_Counter_Standby: integer;      { Telegrammzähler für Überwachung Normal-/Standbybetrieb }
    Sec_Counter_Standby: integer;     { Sekundenzähler für Überwachung Normal-/Standbybetrieb }

    Einzelsteuerzeichen_E5: boolean;  { true: E5 statt ACK-Telegramm senden }
    Zyklus_Verbindungstest: integer;  { in ms }
    procedure Get_IniTelegr (bCheckIniChanged: boolean);

    Procedure Clear_TelegrAnzeige;
    Procedure Update_TelegrAnzeige;
    Procedure Update_DFCAnzeige;
    Procedure Update_SendeTelegrammListAnzeige;

    Function Check_TelegrammFormat: integer;
    function Set_EmpfTelegrammRecord: boolean;
    Function Get_DIR: boolean;
    Function Get_PRM: boolean;
    Function Get_FCB_ACD: boolean;
    Function Get_FCV_DFC: boolean;
    Function DecodeTelegramm: boolean;

    Procedure Set_SendTelegrammAngekommen (aPRM: boolean);
(*{$IFDEF Redundanz}
    procedure Check_Betriebsart;
{$ENDIF}*)

    procedure Set_Telegramm_FesteLaenge (aPRM: boolean; aFktCode: integer);

    Procedure Uebertragung_Sekundaer;
    Procedure Uebertragung_Primaer (CommStatus: TIecCommStatus);
  protected
    Procedure Set_SendTelegrammBereit (aPRM_101: boolean); override;
  public
    { Public-Deklarationen }
    Uebertragungsprozedur: TUebertragungsprozedur;  { symmetrische/unsymmetrische Übertragung }
    SymmStatus: TIec101SymmStatus;
    constructor Create (AOwner: TComponent; AFunktion: TIEC870_Funktion;
                        ANetProgDir: string;
                        AImportDirList: TImportDirList;
                        ARemoteMonitorObj: TRemoteMonitorObj); override;
    destructor Destroy; override;
    function CheckTelegr: boolean;
    procedure Update_TimeoutAnzeige (S: string);
    function Get_SendTelegramm (var SendTelegr: string; var isPrmTelegr: boolean): boolean;
    Procedure Set_SendTelegrammVersendet (aPRM: boolean);
    Procedure Uebertragung (CommStatus: TIecCommStatus; var isSek: boolean);
  end;

var
  FormTelegrIec101: TFormTelegrIec101;

implementation

{$R *.dfm}

const
  { Stati für das Versenden eines Telegramms }
  ts_none = $00;  // es soll kein Telegramm versendet werden
  ts_Prm  = $01;  // es soll das nächste Primärstation-Telegramm versendet werden
  ts_Sek  = $02;  // es soll das nächste Sekundärstation-Telegramm versendet werden


{ TFormTelegrIec101 }

{--------------------------------------------------------------------------}
constructor TFormTelegrIec101.Create (AOwner: TComponent;
                                      AFunktion: TIEC870_Funktion;
                                      ANetProgDir: string;
                                      AImportDirList: TImportDirList;
                                      ARemoteMonitorObj: TRemoteMonitorObj);
{--------------------------------------------------------------------------}
begin
  inherited Create (AOwner, AFunktion, ANetProgDir, AImportDirList, ARemoteMonitorObj);

  ErstesTelegramm:=true;
  LinkReset_LW:=false;  // LW hat Link-Reset noch nicht durchgeführt
  Get_IniTelegr (false);  { liest Ini-Einstellungen für Telegramme }

  Prm_SendTelegrammList:=TSendTelegrammList.Create;
  Sek_SendTelegrammList:=TSendTelegrammList.Create;
  Telegramm_senden:=ts_none;

  FillChar (EmpfTelegramm, SizeOf (EmpfTelegramm), 0);  // Vorbelegung: Empfangstelegramm-Record
  EmpfTelegramm.Liniennummer:=-1;  // Vorbelegung: Liniennummer ist nicht vorhanden
  LiniennummerMerk:=EmpfTelegramm.Liniennummer;  // Vorbelegung: Merk-Liniennummer

  bEmpfDIR_symm:=true;  // Vorbelegung: DIR=1 (Station A nach Station B }
  bEmpfDFC:=false;  // Vorbelegung: DFC=0 (weitere Nachrichten werden von LW angenommen)

  bSendFCB_letzt:=false;  // Vorbelegung: wie nach Normierung
  bEmpfFCB_letzt:=false;  // Vorbelegung: wie nach Normierung
  SymmStatus:=iss_Wait;
  dtDelayPrmTestLink:=0;

  if Uebertragungsprozedur = tp_symmetrisch then begin  // symmetrische Übertragung
    lTimeout.Enabled:=true;
    eTimeout.Enabled:=true;

    lAnzPrmSendTelegr.Enabled:=true;
    eAnzPrmSendTelegr.Enabled:=true;
  end
  else begin
    lTimeout.Enabled:=false;
    eTimeout.Enabled:=false;
    eTimeout.Color:=clBtnFace;

    lAnzPrmSendTelegr.Enabled:=false;
    eAnzPrmSendTelegr.Enabled:=false;
    eAnzPrmSendTelegr.Color:=clBtnFace;
  end;

  TG_Counter_Standby:=0;
  Sec_Counter_Standby:=0;
end;

{-----------------------------------}
destructor TFormTelegrIec101.Destroy;
{-----------------------------------}
begin
  Sek_SendTelegrammList.Free;
  Prm_SendTelegrammList.Free;
  inherited Destroy;
end;

{------------------------------------------------------------}
procedure TFormTelegrIec101.Update_TimeoutAnzeige (S: string);
{------------------------------------------------------------}
{ Ausgabe des Timeouts für Empfangstelegramm;
  Übergabe: Anzeige-String }
begin
  eTimeout.Text:=S;
  Application.ProcessMessages;
end;

{--------------------------------------------------------------------}
procedure TFormTelegrIec101.Get_IniTelegr (bCheckIniChanged: boolean);
{--------------------------------------------------------------------}
{ INI-Konfiguration für 101-Telegramme lesen;
  Übergabe: Flag 'bCheckIniChanged': wenn true, wird nur bei geänderter INI-
              Konfiguration gelesen }
var
  Iec32Ini: TIec32Ini;
begin
  Iec32Ini:=TIec32Ini.Create (FNetProgDir);
  try
    // INI-Einstellungen, welche nur zum Programmstart gelesen werden (können):
    if (not bCheckIniChanged) then begin
      Uebertragungsprozedur:=Iec32Ini.Uebertragungsprozedur;
      Einzelsteuerzeichen_E5:=Iec32Ini.Einzelsteuerzeichen_E5;
      Adressfeld_Bytes:=Iec32Ini.Adressfeld_Bytes;
      ASDU_Bytes:=Iec32Ini.ASDU_Bytes;
      InfoObj_Bytes:=Iec32Ini.InfoObj_Bytes;
      Herkunftsadresse:=Iec32Ini.Herkunftsadresse;
      Telegramm_MaxBytes:=Iec32Ini.Telegramm_MaxBytes;  // max. Telegrammlänge L (Anzahl der Anwenderdaten-Oktette)
    end;

    // INI-Einstellungen, welche zur Laufzeit neu gelesen werden (können):
    if (not bCheckIniChanged) OR
       (bCheckIniChanged AND
        not (SameDateTime (Iec32Ini.FileDate, FIniDate))) then begin       
      FIniDate:=Iec32Ini.FileDate;  // Zeitstempel der INI-Datei merken; 10.03.2015, WW

      Zyklus_Verbindungstest:=Iec32Ini.Zyklus_Verbindungstest;
      bTK_103_Aktiv:=Iec32Ini.TypkennungAktiv [103];  // Typkennung 103 (Uhrzeit-Synchronisation) aktiv
    end;
  finally
    Iec32Ini.Free;
  end;
end;

{----------------------------------------------}
function TFormTelegrIec101.CheckTelegr: boolean;
{----------------------------------------------}
var
  S: string;

begin
  Result:=true;
  { Funktion 'Zentrale für Norm 101 nicht implementiert: }
  if Funktion = fkt_iec870_Zentrale then begin
    S:='Die Funktion ''Zentrale'' wird für die Norm IEC 870-5-101 nicht unterstützt !';
    WriteProgramLog (S, lt_Error);
    MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
    Result:=false;
    exit;
  end;

  { Prüfung Länge Adressfeld der Verbindungsschicht: }
  if (Uebertragungsprozedur = tp_unsymmetrisch) AND (Adressfeld_Bytes = 0) then begin
    S:='Prüfung der INI-Einstellung für die Anzahl der Oktette im Adressfeld der Verbindungsschicht (IEC 870-5-101):' + #13 +
       '[Verbindungsschicht] Adressfeld_Bytes = ' + IntToStr (Adressfeld_Bytes) + #13#13 +
       'Übertragung ohne Adressfeld ist im unsymmetrischen Betrieb nicht erlaubt !';
    WriteProgramLog (S, lt_Error);
    MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
    Result:=false;
    exit;
  end;

  { Prüfung IEC-Konfiguration: }
  if not FormDatenIec.Check_KonfigDB (Adressfeld_Bytes, ASDU_Bytes, InfoObj_Bytes) then begin
    Result:=false;
    exit;
  end;
end;

(*{$IFDEF Redundanz}
{-----------------------------------------}
procedure TFormTelegrIec.Check_Betriebsart;
{-----------------------------------------}
var
  OldStandbyBetrieb: boolean;
begin
  OldStandbyBetrieb:=StandbyBetrieb;
  StandbyBetrieb:=TG_Counter_Standby < Telegrammzahl_Standby;
  { Prüfen, ob ein Wechsel der Betriebsart stattgefunden hat: }
  if OldStandbyBetrieb <> StandbyBetrieb then begin
    { es hat ein Wechsel stattgefunden }
    if StandbyBetrieb then begin                            { Standby-Betrieb }
      FormMainIec32.pnBottom.Caption:=SMsgStandbyBetrieb;
      FormMainIec32.pnBottom.Color:=C_ColorStandbyBetrieb;
      if IECLogFile <> nil then         { Wechsel in Log-Datei protokollieren }
        IECLogFile.Write (SMsgNormalbetrieb + ' -> ' + SMsgStandbybetrieb, '', '');
    end
    else begin                                                { Normalbetrieb }
      FormMainIec32.pnBottom.Caption:=SMsgNormalbetrieb;
      FormMainIec32.pnBottom.Color:=C_ColorNormalBetrieb;
      if IECLogFile <> nil then         { Wechsel in Log-Datei protokollieren }
        IECLogFile.Write (SMsgStandbybetrieb + ' -> ' + SMsgNormalbetrieb, '', '');
    end;
  end;

  TG_Counter_Standby:=0;                       { Telegrammzähler zurücksetzen }
end;
{$ENDIF}*)

{-------------------------------------------------------------------------------}
function TFormTelegrIec101.Get_SendTelegramm (var SendTelegr: string;
                                              var isPrmTelegr: boolean): boolean;
{-------------------------------------------------------------------------------}
{ gibt nächstes zu versendendes Telegramm zurück;
  Variable 'Telegramm_senden' gibt an, aus welcher Sende-Telegrammliste das
  Telegramm zu holen ist (Liste für Primärstation- oder Liste für Sekundärstation-
  Telegramme)
  Rückgaben: Sende-Telegramm
             Flag 'isPrmTelegr' (wenn true, ist Sende-Telegramm ein Telegramm der
                                 Primärstation)
  Ergebnis: true, wenn Telegramm in entsprechender Telegrammliste vorhanden ist }
var
  sLogHeader: string;
  sLogDatensatzInfo: string;

begin
  Result:=false;
  SendTelegr:='';
  isPrmTelegr:=false;
  sLogHeader:='';

  { zuerst prüfen, ob zu versendendes Sekundärstation-Telegramm vorhanden ist (Anfrage
    beantworten): }
  if (((Telegramm_senden AND ts_Sek) <> 0) AND (Sek_SendTelegrammList.Count > 0)) then begin
    SendTelegr:=TSendTelegrammObj (Sek_SendTelegrammList[0]).Daten.Telegramm;
    sLogHeader:=TSendTelegrammObj (Sek_SendTelegrammList[0]).Daten.LogHeader;
    sLogDatensatzInfo:=TSendTelegrammObj (Sek_SendTelegrammList[0]).Daten.LogDatensatzInfo;
    Result:=true;
  end;

  { wenn kein Sekundärstation-Telegramm vorhanden ist, prüfen, ob zu versendendes
    Primärstation-Telegramm vorhanden ist (eigene Anfrage): }
  if not Result then begin
    if (((Telegramm_senden AND ts_Prm) <> 0) AND (Prm_SendTelegrammList.Count > 0)) then begin
      SendTelegr:=TSendTelegrammObj (Prm_SendTelegrammList[0]).Daten.Telegramm;
      sLogHeader:=TSendTelegrammObj (Prm_SendTelegrammList[0]).Daten.LogHeader;
      sLogDatensatzInfo:=TSendTelegrammObj (Prm_SendTelegrammList[0]).Daten.LogDatensatzInfo;
      isPrmTelegr:=true;  // Primärstation-Telegramm
      Result:=true;
    end;
  end;

  if Result then begin  // zu versendendes Telegramm vorhanden
    if IECLogFile <> nil then { Telegramm in Log-Datei protokollieren }
      if length (sLogHeader) > 0 then  // nur, wenn Log-Header vorhanden ist (Datentelegramme)
        IECLogFile.Write (sLogHeader, sLogDatensatzInfo, SendTelegr);
  end;
end;

{----------------------------------------------------------------------}
Procedure TFormTelegrIec101.Set_SendTelegrammBereit (aPRM_101: boolean);
{----------------------------------------------------------------------}
{ Flag für zur Versendung bereites Telegramm setzen;
  Übergabe: 'aPRM' (wenn true, Bereit-Flag für Primärstation-Telegramm setzen, ansonsten
                    für Sekundärstation-Telegramm) }
begin
  if aPRM_101 then
    Telegramm_senden:=Telegramm_senden OR ts_Prm
  else
    Telegramm_senden:=Telegramm_senden OR ts_Sek;

  Update_SendeTelegrammListAnzeige;  // Anzeige der Sendetelegrammliste akualisieren
end;

{---------------------------------------------------------------------}
Procedure TFormTelegrIec101.Set_SendTelegrammVersendet (aPRM: boolean);
{---------------------------------------------------------------------}
{ Flag für zur Versendung bereites Telegramm rücksetzen;
  Übergabe: 'aPRM' (wenn true, Bereit-Flag für Primärstation-Telegramm rücksetzen, ansonsten
                    für Sekundärstation-Telegramm) }
begin
  if aPRM then
    Telegramm_senden:=Telegramm_senden AND not ts_Prm
  else
    Telegramm_senden:=Telegramm_senden AND not ts_Sek;

  Update_SendeTelegrammListAnzeige;  // Anzeige der Sendetelegrammliste akualisieren
end;

{----------------------------------------------------------------------}
Procedure TFormTelegrIec101.Set_SendTelegrammAngekommen (aPRM: boolean);
{----------------------------------------------------------------------}
{ Telegramm aus Sende-Telegrammliste löschen;
  Übergabe: 'aPRM' (wenn true, wird Telegramm aus Liste für Primärstation-Telegramme
                    gelöscht, ansonsten aus Liste für Sekundärstation-Telegramme) }
begin
  if aPRM then begin
    if Prm_SendTelegrammList.Count > 0 then
      Prm_SendTelegrammList.Delete(0);  // erstes Primärstation-Telegramm rauslöschen
  end
  else begin
    if Sek_SendTelegrammList.Count > 0 then
      Sek_SendTelegrammList.Delete(0);  // erstes Sekundärstation-Telegramm rauslöschen
  end;
  Update_SendeTelegrammListAnzeige;  // Anzeige der Sendetelegrammliste akualisieren
end;

{----------------------------------------------------------------------------------------}
Procedure TFormTelegrIec101.Uebertragung (CommStatus: TIecCommStatus; var isSek: boolean);
{----------------------------------------------------------------------------------------}
{ Aktion nach empfangenem Telegramm ausführen;
  Übergabe: Übertragungsstatus
  Rückgabe: Flag 'isSek' (wenn true, ist das Empfangstelegramm ein Telegramm der
                          Sekundärstation) }
var
  bEmpfDFC_neu: boolean;

begin
  isSek:=false;

  if CommStatus = ics_Receive then begin  // Telegramm empfangen
    { Gültigkeitsprüfung des empfangenen Telegramms: }
    if not DecodeTelegramm then begin
      { Was tun bei ungültigem Empfangstelegramm ?
        -> l.t. Norm:
           Empfangstelegramm ist Primärstation-Telegramm (Anfrage der Gegenstelle):
             Keine Antwort (und letztes Sendetelegramm weiter behalten, wenn
             vorhanden), Gegenstelle wiederholt Anfrage mit unverändertem FCB
             (unsymmetrischer Betrieb: IEC 870-5-2, Kap. 5.3.3 und 5.3.5;
              symmetrischer Betrieb: IEC 870-5-2, Kap. 6.3.4)

           Empfangstelegramm ist Sekundärstation-Telegramm (Antwort der Gegenstelle):
             weiter auf Empfangstelegramm warten, letztes versendetes Telegramm wird
             nach Timeout wiederholt (symmetrischer Betrieb: IEC 870-5-2, Kap. 6.3.4) }
      exit;
    end;

    { PRM-Prüfung des empfangenen Telegramms (Primär-/Sekundärstation) : }
    if (not EmpfTelegramm.bE5) AND Get_PRM then begin  { Telegramm einer (veranlassenden) Primärstation (LW) }
      Uebertragung_Sekundaer;  { Aktion der Kopplung als Sekundärstation }
    end
    else begin  { Telegramm einer (antwortenden) Sekundärstation (LW) }
      isSek:=true;
      { DFC-Prüfung des empfangenen Telegramms: }
      if EmpfTelegramm.bE5 then  { Einzelzeichen-Antwort }
        bEmpfDFC_neu:=false  // DFC=0 (LW nimmt Nachrichten an)
      else
        bEmpfDFC_neu:=Get_FCV_DFC;  // DFC-Bit lesen
      { Wechsel des DFC-Bit in Logfile protokollieren: }
      if (IECLogFile <> nil) AND (bEmpfDFC_neu <> bEmpfDFC) then begin
        if bEmpfDFC_neu then
          IECLogFile.Write ('DFC=1', '', '')
        else
          IECLogFile.Write ('DFC=0', '', '')
      end;
      bEmpfDFC:=bEmpfDFC_neu;
      Update_DFCAnzeige;  // DFC-Anzeige aktualisieren

      Uebertragung_Primaer (CommStatus);  { Aktion der Kopplung als Primärstation }
    end;
  end
  else begin
    if Uebertragungsprozedur = tp_symmetrisch then  // symmetrische Übertragung
      Uebertragung_Primaer (CommStatus);  { Aktion der Primärstation (Kopplung) bei Timeout oder aufgrund Idle-Zustand }
  end;
end;

{-------------------------------------------------}
Procedure TFormTelegrIec101.Uebertragung_Sekundaer;
{-------------------------------------------------}
{ nach empfangenem (Primärstation-) Telegramm entsprechende Aktion der
  Sekundärstation ausführen }
const
  iDummy: integer = 0;

var
  bFCB_neu: boolean;
  bFCV_neu: boolean;
  S: string;
  Abfragekennung_QOI: integer;
  AlleStationen: boolean;
  iCause : byte;
  iCause_KZW : byte;

begin
  { FCB, FCV überprüfen -> ist letztes gesendetes Daten-Telegramm angekommen ? }
  bFCV_neu:=Get_FCV_DFC;  { 'Telegrammfolgebit gültig'-Bit lesen }
  if bFCV_neu then  { ist gültig }
    bFCB_neu:=Get_FCB_ACD  { Telegrammfolgebit lesen }
  else
    bFCB_neu:=false;  { Vorbelegung }

  if (bFCV_neu AND (bFCB_neu = not bEmpfFCB_letzt)) OR
     (not bFCV_neu) OR ErstesTelegramm then begin
    { positive Bestätigung der LW: "Telegramm angekommen" bzw. erste Anforderung }
    ErstesTelegramm:=false;

    { wenn letztes versendetes Telegramm ein Datentelegramm war, übertragenen
      Datenbereich in IEC-Daten-Tabelle updaten: }
    Update_DatenGesendet (Sek_SendTelegrammList, 0);
    Set_SendTelegrammAngekommen (C_PRM_0);  { versendetes Telegramm aus Sende-Telegrammliste löschen }
    if bFCV_neu then  { wenn FCV-Bit gesetzt ist (FCB ist gültig) }
      bEmpfFCB_letzt:=bFCB_neu;  { Empfangs-FCB-Merker aktualisieren }

    { Auswertung Funktionscode des empfangenen Telegramms: }
    case EmpfTelegramm.Funktionscode of
      C_FKT_PRM_0_LINK_RESET:
        begin
          Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_0_ACK);
          { Normierbefehl: im nächsten Empfangstelegramm wird FCB=1 erwartet }
          bEmpfFCB_letzt:=false;
          { von LW durchgeführte Normierung der Verbindungsschicht war erfolgreich.
            Für symmetrische Betriebsart: jetzt Link-Statusabfrage durch Kopplung }
          SymmStatus:=iss_SndLinkStatus;
          LinkReset_LW:=true;  // von LW durchgeführter Link-Reset ist abgeschlossen
          if IECLogFile <> nil then
            IECLogFile.Write ('Link OK, LW', '', '');
        end;

      C_FKT_PRM_2_LINK_TEST:
        begin
          // nur bei symmetrischer Übertragung und wenn Link-Reset durch LW erfolgt
          // ist (das Nicht-Beantworten soll die LW dazu bringen, den Link-Reset
          // durchzuführen):
          if (Uebertragungsprozedur = tp_symmetrisch) AND LinkReset_LW then
            Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_0_ACK);
        end;

      C_FKT_PRM_3_DATEN_CONFIRM,
      C_FKT_PRM_4_DATEN_NOREPLY:
        begin
          // bei symmetrischer Übertragung nur, wenn Link-Reset durch LW erfolgt
          // ist (das Nicht-Beantworten soll die LW dazu bringen, den Link-Reset
          // durchzuführen):
          if (Uebertragungsprozedur = tp_unsymmetrisch) OR
             ((Uebertragungsprozedur = tp_symmetrisch) AND LinkReset_LW) then begin
            case EmpfTelegramm.DatenEinheitIdent.Typkennung of
              C_TK_Generalabfrage:  { Generalabfrage-Telegramm }
                begin
                  { ...mit Abfragekennung QOI 'Stationsabfrage, global' }
                  Abfragekennung_QOI:=integer(EmpfTelegramm.sObjekte [InfoObj_Bytes+1]);
                  if Abfragekennung_QOI = C_AFK_Stationsabfrage_global then begin
                    if IECLogFile <> nil then begin
                      S:='Generalabfrage: ';
                      if Adressfeld_Bytes > 0 then
                        S:=S + 'Linie = ' + IntToStr (EmpfTelegramm.Liniennummer) + ', '
                      else
                        S:=S + 'Linie = nicht vorhanden' + ', ';
                      S:=S + 'Station = ' + IntToStr (EmpfTelegramm.DatenEinheitIdent.Stationsnummer);
                      IECLogFile.Write (S, '', '');
                    end;

                    { es müssen anschließend Daten für Generalabfrage gesendet werden }
                    LiniennummerMerk:=-1;  // damit Daten-Telegramme für GA neu geladen werden

                    { Datentelegramm mit Funktionscode 3 quittieren (Send/Confirm),
                      mit Funktionscode 4 nicht (Send/No Reply) ! }
                    if EmpfTelegramm.Funktionscode = C_FKT_PRM_3_DATEN_CONFIRM then
                      Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_0_ACK);

                    if ASDU_Bytes > 1 then
                      AlleStationen:=EmpfTelegramm.DatenEinheitIdent.Stationsnummer = $FFFF  { 2 Byte Stationsnummer }
                    else
                      AlleStationen:=EmpfTelegramm.DatenEinheitIdent.Stationsnummer = $FF;  { 1 Byte Stationsnummer }

                    { GA-Stationsnummernliste füllen für nachfolgende Telegramme
                      'Generalabfrage, Bestätigung/Ende der Aktivierung': }
                    if AlleStationen then
                      FormDatenIec.FillGA_StationsNrListe_Alle (EmpfTelegramm.Liniennummer,
                                                                FGA_StationsNrListe)
                    else
                      FillGA_StationsNrListe_Station (EmpfTelegramm.DatenEinheitIdent.Stationsnummer);

                    { Bereich bereits gesendeter Daten auf zu sendenden Generalabfrage-Bereich rücksetzen: }
                    FormDatenIec.SetGesendetBis_General (EmpfTelegramm.Liniennummer,
                                                         EmpfTelegramm.DatenEinheitIdent.Stationsnummer,
                                                         Adressfeld_Bytes, ASDU_Bytes);
                    { vorhandenen Datenbereich einlesen (alle Datentypen): }
                    FormDatenIec.F_Update (true, true, true, true);
                    LinienNrVerwList.Clear;  { Liste zurücksetzen }

                    { nächster Funktionsstatus: Generalabfrage, Bestätigung der Aktivierung }
                    FIecFktStatus:=if_GA_AckActivate;
                  end
                  else begin
                    Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_15_NOSERVICE);
                    if IECLogFile <> nil then
                      IECLogFile.Write ('Generalabfrage: Nicht unterstützte Abfragekennung QOI (' +
                                        IntToStr (Abfragekennung_QOI) + ')', '', '');
                  end;
                end;

              C_TK_Uhrzeit_Synchronisation:  { Uhrzeit-Synchronisations-Telegramm }
                begin
                  { INI neu lesen, wenn geändert; 10.03.2015, WW }
                  Get_IniTelegr (true);

                  if bTK_103_Aktiv then begin  // Typkennung 103 per INI aktiviert
                    { ...mit Übertragungsursache 'Aktivierung' }
                    if EmpfTelegramm.DatenEinheitIdent.UebertrUrsache = C_URS_Activate then begin
                      if IECLogFile <> nil then begin
                        S:='Uhrzeit-Synchronisation: Station = ' +
                        IntToStr (EmpfTelegramm.DatenEinheitIdent.Stationsnummer);
                        IECLogFile.Write (S, '', '');
                      end;

                      { Telegramm 'Uhrzeit-Synchronisation, Bestätigung der Aktivierung' senden: }
                      if Uebertragungsprozedur = tp_unsymmetrisch then
                        Set_Telegramm_UhrzeitSynchronisation (Sek_SendTelegrammList, norm_iec870_101,
                           C_PRM_0, not bEmpfDIR_symm, bSendFCB_letzt,
                           C_FKT_SEK_8_DATEN,
                           iDummy, iDummy,  // nur für 104
                           EmpfTelegramm.Liniennummer, C_URS_AckActivate,
                           EmpfTelegramm.DatenEinheitIdent.Stationsnummer,  // Stationsnummer aus Empfangstelegramm
                           EmpfTelegramm.sObjekte)  // Info-Objektadresse und Zeit aus Empfangstelegramm

                        { -> Es erfolgt keine Synchronisation der PC-Zeit ! }
                      else begin
                        { Datentelegramm mit Funktionscode 3 quittieren (Send/Confirm),
                          mit Funktionscode 4 nicht (Send/No Reply) ! }
                        if EmpfTelegramm.Funktionscode = C_FKT_PRM_3_DATEN_CONFIRM then
                          Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_0_ACK);

                        { nächster Funktionsstatus: Uhrzeit-Synchronisation }
                        FIecFktStatus:=if_UhrzeitSync;
                      end;
                    end
                    else begin
                      // keine Aktion
                      if IECLogFile <> nil then
                        IECLogFile.Write ('Uhrzeit-Synchronisation: Nicht unterstützte Übertragungsursache (' +
                                          IntToStr (EmpfTelegramm.DatenEinheitIdent.UebertrUrsache) + ')', '', '');
                    end;
                  end
                  else begin
                    Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_15_NOSERVICE);
                    if IECLogFile <> nil then
                      IECLogFile.Write ('Nicht unterstützte Typkennung (' +
                                        IntToStr (EmpfTelegramm.DatenEinheitIdent.Typkennung) + ')', '', '');
                  end;
                end;
            else
              Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_15_NOSERVICE);
              if IECLogFile <> nil then
                IECLogFile.Write ('Nicht unterstützte Typkennung (' +
                                  IntToStr (EmpfTelegramm.DatenEinheitIdent.Typkennung) + ')', '', '');
            end;  { case }
          end;
        end;

      C_FKT_PRM_9_LINK_STATUS:
        begin
          Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_11_LINK_STATUS);
          { von LW wird mit Abfrage Link-Status/Normierung der Verbindungsschicht begonnen.
            Für symmetrische Betriebsart: Kopplung soll jetzt nicht aktiv Anfragen senden }
          SymmStatus:=iss_Wait;
        end;

      C_FKT_PRM_10_ABFRAGE_DATEN_KLASSE1:
        begin
          // nur bei unsymmetrischer Übertragung:
          if (Uebertragungsprozedur = tp_unsymmetrisch) then
            Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_15_NOSERVICE);
        end;

      C_FKT_PRM_11_ABFRAGE_DATEN_KLASSE2:
        begin
          // nur bei unsymmetrischer Übertragung:
          if (Uebertragungsprozedur = tp_unsymmetrisch) then begin
            if Redundanzbetrieb = C_Redundanz_Passiv then
              Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_9_NACK_NODATA)  { im passiven Redundanz-Betrieb keine Daten schicken }
            else begin
              if (Sek_SendTelegrammList.Count > 0) AND
                 (EmpfTelegramm.Liniennummer = LiniennummerMerk) then
                // es sind noch Daten zur Liniennummer in der Sekundär-Sendetelegrammliste vorhanden:
                Set_SendTelegrammBereit (C_PRM_0)  // nächstes Sekundärstation-Telegramm versenden
              else begin
                LiniennummerMerk:=EmpfTelegramm.Liniennummer;

                case FIecFktStatus of
                  if_GA_AckActivate:  // Generalabfrage: Bestätigung der Aktivierung
                     begin
                       { Telegramm 'Generalabfrage, Bestätigung der Aktivierung' senden: }
                       Set_Telegramm_Generalabfrage (Sek_SendTelegrammList, norm_iec870_101,
                         C_PRM_0, not bEmpfDIR_symm, bSendFCB_letzt,
                         C_FKT_SEK_8_DATEN,
                         iDummy, iDummy,  // nur für 104
                         EmpfTelegramm.Liniennummer, C_URS_AckActivate,
                         FGA_StationsNrListe);  

                       { nächster Funktionsstatus: Generalabfrage, erste Daten senden }
                       FIecFktStatus:=if_GA_Daten_Start;
                     end;

                  if_Inaktiv, if_GA_Daten_Start, if_GA_Daten_Continue:  // Funktion inaktiv oder Generalabfrage-Daten senden
                    begin
                      // Daten neu laden und in Sende-Telegrammliste stellen
                      // -> Übertragungsursache 'spontan' bzw. 'Generalabfrage'
                      if (FIecFktStatus = if_GA_Daten_Start) then begin
                        iCause := C_URS_GeneralAbfr;
                        iCause_KZW := C_URS_GeneralAbfr;

                        { nächster Funktionsstatus: Generalabfrage, weitere Daten senden }
                        FIecFktStatus:=if_GA_Daten_Continue;
                      end
                      else begin
                        // GA inaktiv und GA weitere Daten (Set_Telegramme_Daten_KZW darf für GA-Daten nur
                        // einmal mit C_URS_GeneralAbfr aufgerufen werden)
                        iCause_KZW := C_URS_spontan;

                        if FIecFktStatus = if_GA_Daten_Continue then
                          iCause := C_URS_GeneralAbfr
                        else
                          iCause := C_URS_spontan;
                      end;

                      if not Set_Telegramme_Daten_KZW (Sek_SendTelegrammList, norm_iec870_101,
                                                       C_PRM_0, not bEmpfDIR_symm, bSendFCB_letzt, C_FKT_SEK_8_DATEN,
                                                       iDummy, iDummy,  // nur für 104
                                                       EmpfTelegramm.Liniennummer, iCause_KZW, false) then
                        if not Set_Telegramme_Daten_MRG (Sek_SendTelegrammList, norm_iec870_101,
                                                         C_PRM_0, not bEmpfDIR_symm, bSendFCB_letzt, C_FKT_SEK_8_DATEN,
                                                         iDummy, iDummy,  // nur für 104
                                                         EmpfTelegramm.Liniennummer, iCause, false) then
                          if not Set_Telegramme_Daten_DSfG (Sek_SendTelegrammList, norm_iec870_101,
                                                            C_PRM_0, not bEmpfDIR_symm, bSendFCB_letzt, C_FKT_SEK_8_DATEN,
                                                            iDummy, iDummy,  // nur für 104
                                                            EmpfTelegramm.Liniennummer, iCause, false) then begin
                            { Es sind keine Daten vorhanden }
                            if (FIecFktStatus = if_GA_Daten_Continue) then begin
                              { Telegramm 'Generalabfrage, Ende der Aktivierung' senden: }
                              Set_Telegramm_Generalabfrage (Sek_SendTelegrammList, norm_iec870_101,
                                C_PRM_0, not bEmpfDIR_symm, bSendFCB_letzt,
                                C_FKT_SEK_8_DATEN,
                                iDummy, iDummy,  // nur für 104
                                EmpfTelegramm.Liniennummer, C_URS_EndActivate,
                                FGA_StationsNrListe); 

                              { nächster Funktionsstatus: inaktiv }
                              FIecFktStatus:=if_Inaktiv;
                            end else
                              Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_9_NACK_NODATA);
                          end;
                    end;
                end;  // case FIecGAStatus
              end;
            end;
          end;
        end;
    else
      // bei symmetrischer Übertragung nur, wenn Link-Reset durch LW erfolgt
      // ist (das Nicht-Beantworten soll die LW dazu bringen, den Link-Reset
      // durchzuführen):
      if (Uebertragungsprozedur = tp_unsymmetrisch) OR
         ((Uebertragungsprozedur = tp_symmetrisch) AND LinkReset_LW) then
        Set_Telegramm_FesteLaenge (C_PRM_0, C_FKT_SEK_1_NACK);
    end;
  end else  { gleiches FCB kommt nochmal (lt. Norm auch gleiches Telegramm !) }
    Set_SendTelegrammBereit (C_PRM_0);  // Sekundärstation-Telegramm nochmal versenden
end;

{----------------------------------------------------------------------------}
Procedure TFormTelegrIec101.Uebertragung_Primaer (CommStatus: TIecCommStatus);
{----------------------------------------------------------------------------}
{ Aktion der Primärstation ausführen: nach empfangenem (Sekundärstation-) Telegramm,
  Timeout beim (Sekündär-)Telegrammempfang oder aufgrund Idle-Zustand;
  Übergabe: IEC-Übertragungsstaus }
const
  iDummy: integer = 0;

var
  isTimeout: boolean;
  bSendLinkTest: boolean;
  iCause : byte;
  iCause_KZW : byte;
  
begin
  isTimeout:=CommStatus = ics_Timeout_EmpfSek;

  while true do begin
    case SymmStatus of
      iss_Wait:
        begin
          Break; // nix tun (wir warten darauf, daß die LW als Primärstation ein Telegramm schickt.)
        end;

      iss_SndLinkStatus:  // Linkstatus-Abfrage an LW senden
        begin
          Set_Telegramm_FesteLaenge (C_PRM_1, C_FKT_PRM_9_LINK_STATUS);
          SymmStatus:=iss_RecLinkStatus;
          Break;
        end;

      iss_RecLinkStatus:  // Antwort auf Linkstatus-Telegramm auswerten
        begin
          if (not isTimeout) AND (not EmpfTelegramm.bE5) AND
            (EmpfTelegramm.Funktionscode = C_FKT_SEK_11_LINK_STATUS) then begin
            Set_SendTelegrammAngekommen (C_PRM_1);  { versendetes Telegramm aus Sende-Telegrammliste löschen }
            SymmStatus:=iss_SndLinkReset;  // jetzt Linkreset-Telegramm an LW senden
          end
          else begin
            SymmStatus:=iss_SndLinkStatus;  // Linkstatus-Abfrage wiederholen
          end;
        end;

      iss_SndLinkReset:  // Linkreset-Telegramm an LW senden
        begin
          Set_Telegramm_FesteLaenge (C_PRM_1, C_FKT_PRM_0_LINK_RESET);
          SymmStatus:=iss_RecLinkReset;
          Break;
        end;

      iss_RecLinkReset:  // Antwort auf Linkreset-Telegramm auswerten
        begin
          if (not isTimeout) AND (EmpfTelegramm.bE5 OR
             (not EmpfTelegramm.bE5 AND (EmpfTelegramm.Funktionscode = C_FKT_SEK_0_ACK))) then begin
            Set_SendTelegrammAngekommen (C_PRM_1);  { versendetes Telegramm aus Sende-Telegrammliste löschen }
            bSendFCB_letzt:=false;  { Normierbefehl: nächstes Sendetelegramm mit FCB=1 }
            SymmStatus:=iss_SndLinkReady;  // -> jetzt ist Link normiert, wir können (Daten-)Telegramme an LW senden
            if IECLogFile <> nil then
              IECLogFile.Write ('Link OK, Kopplung', '', '');
          end
          else begin
            Symmstatus:=iss_SndLinkStatus;  // wir probieren es wieder von vorn mit der Linkstatus-Abfrage
          end;
        end;

      iss_SndLinkReady:  // Telegramm nach Normierung an LW senden
        begin
          bSendLinkTest:=false;
          if not bEmpfDFC AND (Redundanzbetrieb <> C_Redundanz_Passiv) then begin
            // DFC=0: Nachrichten werden von LW angenommen
            // im passiven Redundanz-Betrieb keine Daten schicken
            if (Prm_SendTelegrammList.Count > 0) AND
               (EmpfTelegramm.Liniennummer = LiniennummerMerk) then begin
              // es sind noch Telegramme zur Liniennummer in der Primär-Sendetelegrammliste vorhanden:
              Set_SendTelegrammBereit (C_PRM_1);  // nächstes Primärstation-Telegramm versenden
            end
            else begin
              LiniennummerMerk:=EmpfTelegramm.Liniennummer;

              case FIecFktStatus of
                if_GA_AckActivate:  // Generalabfrage: Bestätigung der Aktivierung
                   begin
                     { Telegramm 'Generalabfrage, Bestätigung der Aktivierung' senden: }
                     Set_Telegramm_Generalabfrage (Prm_SendTelegrammList, norm_iec870_101,
                       C_PRM_1, not bEmpfDIR_symm, bSendFCB_letzt,
                       C_FKT_PRM_3_DATEN_CONFIRM,
                       iDummy, iDummy,  // nur für 104
                       EmpfTelegramm.Liniennummer, C_URS_AckActivate,
                       FGA_StationsNrListe); 

                     { nächster Funktionsstatus: Generalabfrage, erste Daten senden }
                     FIecFktStatus:=if_GA_Daten_Start;
                   end;

                if_Inaktiv, if_GA_Daten_Start, if_GA_Daten_Continue:  // Funktion inaktiv oder Generalabfrage-Daten senden
                  begin
                    // Daten neu laden und in Sende-Telegrammliste stellen
                    // -> Übertragungsursache 'spontan' bzw. 'Generalabfrage'
                    if (FIecFktStatus = if_GA_Daten_Start) then begin
                      iCause := C_URS_GeneralAbfr;
                      iCause_KZW := C_URS_GeneralAbfr;

                      { nächster Funktionsstatus: Generalabfrage, weitere Daten senden }
                      FIecFktStatus:=if_GA_Daten_Continue;
                    end
                    else begin
                      // GA inaktiv und GA weitere Daten (Set_Telegramme_Daten_KZW darf für GA-Daten nur
                      // einmal mit C_URS_GeneralAbfr aufgerufen werden)
                      iCause_KZW := C_URS_spontan;

                      if FIecFktStatus = if_GA_Daten_Continue then
                        iCause := C_URS_GeneralAbfr
                      else
                        iCause := C_URS_spontan;
                    end;

                    if not Set_Telegramme_Daten_KZW (Prm_SendTelegrammList, norm_iec870_101,
                                                     C_PRM_1, not bEmpfDIR_symm, bSendFCB_letzt, C_FKT_PRM_3_DATEN_CONFIRM,
                                                     iDummy, iDummy,  // nur für 104
                                                     EmpfTelegramm.Liniennummer, iCause_KZW, false) then
                      if not Set_Telegramme_Daten_MRG (Prm_SendTelegrammList, norm_iec870_101,
                                                       C_PRM_1, not bEmpfDIR_symm, bSendFCB_letzt, C_FKT_PRM_3_DATEN_CONFIRM,
                                                       iDummy, iDummy,  // nur für 104
                                                       EmpfTelegramm.Liniennummer, iCause, false) then
                        if not Set_Telegramme_Daten_DSfG (Prm_SendTelegrammList, norm_iec870_101,
                                                          C_PRM_1, not bEmpfDIR_symm, bSendFCB_letzt, C_FKT_PRM_3_DATEN_CONFIRM,
                                                          iDummy, iDummy,  // nur für 104
                                                          EmpfTelegramm.Liniennummer, iCause, false) then begin
                          { Es sind keine Daten vorhanden }
                          if (FIecFktStatus = if_GA_Daten_Continue) then begin
                            { Telegramm 'Generalabfrage, Ende der Aktivierung' senden: }
                            Set_Telegramm_Generalabfrage (Prm_SendTelegrammList, norm_iec870_101,
                              C_PRM_1, not bEmpfDIR_symm, bSendFCB_letzt,
                              C_FKT_PRM_3_DATEN_CONFIRM,
                              iDummy, iDummy,  // nur für 104
                              EmpfTelegramm.Liniennummer, C_URS_EndActivate,
                              FGA_StationsNrListe); 

                            { nächster Funktionsstatus: inaktiv }
                            FIecFktStatus:=if_Inaktiv;
                          end else
                            bSendLinkTest:=true;  // Flag setzen zum Versenden eines Linktest-Telegramms
                        end;
                  end;

                if_UhrzeitSync:
                  begin
                    Set_Telegramm_UhrzeitSynchronisation (Prm_SendTelegrammList, norm_iec870_101,
                       C_PRM_1, not bEmpfDIR_symm, bSendFCB_letzt,
                       C_FKT_PRM_3_DATEN_CONFIRM,
                       iDummy, iDummy,  // nur für 104
                       EmpfTelegramm.Liniennummer, C_URS_AckActivate,
                       EmpfTelegramm.DatenEinheitIdent.Stationsnummer,  // Stationsnummer aus Empfangstelegramm
                       EmpfTelegramm.sObjekte);  // Info-Objektadresse und Zeit aus Empfangstelegramm

                    { -> Es erfolgt keine Synchronisation der PC-Zeit ! }

                    { nächster Funktionsnstatus: inaktiv }
                    FIecFktStatus:=if_Inaktiv;
                  end;
              end;  // case FIecGAStatus
            end;
          end else // DFC=1: Nachrichten (Fa. PSI: Daten) können einen Datenüberlauf verursachen
            bSendLinkTest:=true;  // solange DFC-Bit gesetzt ist: Flag setzen zum ersatzweisen Versenden eines Linktest-Telegramms

          if bSendLinkTest then begin
            if dtDelayPrmTestLink > 0 then begin  // Verzögerungszeit ist gesetzt
              if Now >= dtDelayPrmTestLink then begin  // Verzögerungszeit ist erreicht
                dtDelayPrmTestLink:=0;
                Set_Telegramm_FesteLaenge (C_PRM_1, C_FKT_PRM_2_LINK_TEST);  // Linktest-Telegramm jetzt senden
                SymmStatus:=iss_RecLinkReady;
              end;
            end
            else begin
              { INI neu lesen, wenn geändert; 10.03.2015, WW }
              Get_IniTelegr (true);

              dtDelayPrmTestLink:=Now + EncodeTime (0, 0,
                Zyklus_Verbindungstest DIV 1000, Zyklus_Verbindungstest MOD 1000);  // Verzögerungszeit setzen
            end;
          end
          else begin
            dtDelayPrmTestLink:=0;
            SymmStatus:=iss_RecLinkReady;
          end;
          Break;
        end;

      iss_RecLinkReady:  // CONFIRM-Antwort auf nach Normierung gesendetes Telegramm auswerten
        begin
          if (not isTimeout) AND (EmpfTelegramm.bE5 OR
             (not EmpfTelegramm.bE5 AND (EmpfTelegramm.Funktionscode = C_FKT_SEK_0_ACK))) then begin  // OK
            if Prm_SendTelegrammList.Count > 0 then begin
              { wenn FCV-Bit im Sendetelegramm gesetzt ist (FCB ist gültig): }
              if TSendTelegrammObj (Prm_SendTelegrammList[0]).Daten.FCV_101 then
                bSendFCB_letzt:=not bSendFCB_letzt;  { Sende-FCB-Merker toggeln }

              { wenn letztes versendetes Telegramm ein Datentelegramm war, übertragenen
                Datenbereich in IEC-Daten-Tabelle updaten: }
              if FIecFktStatus <> if_GA_AckActivate then  // wenn keine Generalabfrage vorliegt
                Update_DatenGesendet (Prm_SendTelegrammList, 0);
              Set_SendTelegrammAngekommen (C_PRM_1);  { versendetes Telegramm aus Sende-Telegrammliste löschen }
            end
            else begin
              if IECLogFile <> nil then
                IECLogFile.Write ('FEHLER s_RecLinkReady: Prm_SendTelegrammList.Count = 0', '', '', lt_Error);
            end;
          end;
          SymmStatus:=iss_SndLinkReady;
        end;
    end;  { case SymmStatus }
  end; { while true }
end;


{---------------------- Telegrammanzeige --------------------------------------}

{----------------------------------------------}
Procedure TFormTelegrIec101.Clear_TelegrAnzeige;
{----------------------------------------------}
{ Telegrammanzeige löschen }
begin
  EEinzel.Text:='';
  EStart1.Text:='';
  EStart2.Text:='';
  ELaenge1.Text:='';
  ELaenge2.Text:='';
  ESteuerfeld.Text:='';
  EAdressfeldLow.Text:='';
  EAdressfeldHigh.Text:='';
  ETypkennung.Text:='';
  EChecksum.Text:='';
  EEnde.Text:='';

  EFormatPruefung.Text:='';
  EFktCode_dez.Text:='';
  ETypkennung_dez.Text:='';
end;

{-----------------------------------------------}
Procedure TFormTelegrIec101.Update_TelegrAnzeige;
{-----------------------------------------------}
{ Telegrammanzeige aktualisieren }
var
  L: integer;
  Pos: integer;

begin
  if length (sEmpfTelegramm) < 1 then exit;

  case integer(sEmpfTelegramm[1]) of
    C_EINZEL_E5: begin
      lEinzel.Enabled:=true;
      EEinzel.Text:='0x' + IntToHex(integer(sEmpfTelegramm[1]),2);
      EEinzel.Enabled:=true;
      EEinzel.Color:=clWindow;

      // gibts nicht bei Einzelzeichen:
      EStart1.Enabled:=false;
      EStart1.Color:=clBtnFace;
      lStart.Enabled:=false;

      ELaenge1.Enabled:=false;
      ELaenge1.Color:=clBtnFace;
      ELaenge2.Enabled:=false;
      ELaenge2.Color:=clBtnFace;
      lLaenge.Enabled:=false;

      EStart2.Enabled:=false;
      EStart2.Color:=clBtnFace;

      ESteuerfeld.Enabled:=false;
      ESteuerfeld.Color:=clBtnFace;
      lSteuerfeld.Enabled:=false;

      EAdressfeldLow.Enabled:=false;
      EAdressfeldLow.Color:=clBtnFace;
      EAdressfeldHigh.Enabled:=false;
      EAdressfeldHigh.Color:=clBtnFace;
      lAdressfeld.Enabled:=false;

      ETypkennung.Enabled:=false;
      ETypkennung.Color:=clBtnFace;
      lTypkennung.Enabled:=false;

      EChecksum.Enabled:=false;
      EChecksum.Color:=clBtnFace;
      lChecksum.Enabled:=false;

      EEnde.Enabled:=false;
      EEnde.Color:=clBtnFace;
      lEnde.Enabled:=false;

      // keine Dezimal-Ausgaben:
      EFktCode_dez.Enabled:=false;
      EFktCode_dez.Color:=clBtnFace;
      lFktCode_dez.Enabled:=false;
      { Liniennummer nicht disabled anzeigen, wenn Adressfeldlänge > 0: }
      if Adressfeld_Bytes <= 0 then begin
        ELinienNr_dez.Enabled:=false;
        ELinienNr_dez.Color:=clBtnFace;
        lLinienNr_dez.Enabled:=false;
      end;
      ETypkennung_dez.Enabled:=false;
      ETypkennung_dez.Color:=clBtnFace;
      lTypkennung_dez.Enabled:=false;
    end;

    C_START_FEST: begin  // Telegramm mit fester Länge
      ELinienNr_dez.Text:='';

      EStart1.Text:='0x' + IntToHex(integer(sEmpfTelegramm[1]),2);
      EStart1.Enabled:=true;
      EStart1.Color:=clWindow;
      lStart.Enabled:=true;

      if length (sEmpfTelegramm) < 2 then exit;
      ESteuerfeld.Text:='0x' + IntToHex(integer(sEmpfTelegramm[2]),2);
      ESteuerfeld.Enabled:=true;
      ESteuerfeld.Color:=clWindow;
      lSteuerfeld.Enabled:=true;

      Pos:=2;
      if Adressfeld_Bytes > 0 then begin
        inc(Pos);
        if length (sEmpfTelegramm) < Pos then exit;
        EAdressfeldLow.Text:='0x' + IntToHex(integer(sEmpfTelegramm[Pos]),2);
        EAdressfeldLow.Enabled:=true;
        EAdressfeldLow.Color:=clWindow;
        lAdressfeld.Enabled:=true;
        if Adressfeld_Bytes > 1 then begin
          inc(Pos);
          if length (sEmpfTelegramm) < Pos then exit;
          EAdressfeldHigh.Text:='0x' + IntToHex(integer(sEmpfTelegramm[Pos]),2);
          EAdressfeldHigh.Enabled:=true;
          EAdressfeldHigh.Color:=clWindow;
        end
        else begin
          EAdressfeldHigh.Enabled:=false;
          EAdressfeldHigh.Color:=clBtnFace;
        end;
      end
      else begin
        EAdressfeldLow.Enabled:=false;
        EAdressfeldLow.Color:=clBtnFace;
        EAdressfeldHigh.Enabled:=false;
        EAdressfeldHigh.Color:=clBtnFace;
        lAdressfeld.Enabled:=false;
      end;

      inc(Pos);
      if length (sEmpfTelegramm) < Pos then exit;
      EChecksum.Text:='0x' + IntToHex(integer(sEmpfTelegramm[Pos]),2);
      EChecksum.Enabled:=true;
      EChecksum.Color:=clWindow;
      lChecksum.Enabled:=true;

      inc(Pos);
      if length (sEmpfTelegramm) < Pos then exit;
      EEnde.Text:='0x' + IntToHex(integer(sEmpfTelegramm[Pos]),2);
      EEnde.Enabled:=true;
      EEnde.Color:=clWindow;
      lEnde.Enabled:=true;

      // gibts nicht bei Telegrammen fester Länge:
      EEinzel.Enabled:=false;
      EEinzel.Color:=clBtnFace;
      lEinzel.Enabled:=false;
      ELaenge1.Enabled:=false;
      ELaenge1.Color:=clBtnFace;
      ELaenge2.Enabled:=false;
      ELaenge2.Color:=clBtnFace;
      lLaenge.Enabled:=false;
      EStart2.Enabled:=false;
      EStart2.Color:=clBtnFace;
      ETypkennung.Enabled:=false;
      ETypkennung.Color:=clBtnFace;
      lTypkennung.Enabled:=false;
      { Dezimal-Ausgabe Typkennung: }
      ETypkennung_dez.Enabled:=false;
      ETypkennung_dez.Color:=clBtnFace;
      lTypkennung_dez.Enabled:=false;
      { Liniennummer nicht disabled anzeigen, wenn Adressfeldlänge > 0 }
      if Adressfeld_Bytes > 0 then begin
        ELinienNr_dez.Enabled:=true;
        ELinienNr_dez.Color:=clWindow;
        lLinienNr_dez.Enabled:=true;
      end
      else begin
        ELinienNr_dez.Enabled:=false;
        ELinienNr_dez.Color:=clBtnFace;
        lLinienNr_dez.Enabled:=false;
      end;
    end;

    C_START_VAR: begin  // Telegramm mit variabler Länge
      ELinienNr_dez.Text:='';

      EStart1.Text:='0x' + IntToHex(integer(sEmpfTelegramm[1]),2);
      EStart1.Enabled:=true;
      EStart1.Color:=clWindow;
      lStart.Enabled:=true;

      if length (sEmpfTelegramm) < 2 then exit;
      ELaenge1.Text:='0x' + IntToHex(integer(sEmpfTelegramm[2]),2);
      ELaenge1.Enabled:=true;
      ELaenge1.Color:=clWindow;

      if length (sEmpfTelegramm) < 3 then exit;
      ELaenge2.Text:='0x' + IntToHex(integer(sEmpfTelegramm[3]),2);
      ELaenge2.Enabled:=true;
      ELaenge2.Color:=clWindow;
      lLaenge.Enabled:=true;

      if length (sEmpfTelegramm) < 4 then exit;
      EStart2.Text:='0x' + IntToHex(integer(sEmpfTelegramm[4]),2);
      EStart2.Enabled:=true;
      EStart2.Color:=clWindow;

      if length (sEmpfTelegramm) < 5 then exit;
      ESteuerfeld.Text:='0x' + IntToHex(integer(sEmpfTelegramm[5]),2);
      ESteuerfeld.Enabled:=true;
      ESteuerfeld.Color:=clWindow;
      lSteuerfeld.Enabled:=true;

      Pos:=5;
      if Adressfeld_Bytes > 0 then begin
        inc(Pos);
        if length (sEmpfTelegramm) < Pos then exit;
        EAdressfeldLow.Text:='0x' + IntToHex(integer(sEmpfTelegramm[Pos]),2);
        EAdressfeldLow.Enabled:=true;
        EAdressfeldLow.Color:=clWindow;
        lAdressfeld.Enabled:=true;
        if Adressfeld_Bytes > 1 then begin
          inc(Pos);
          if length (sEmpfTelegramm) < Pos then exit;
          EAdressfeldHigh.Text:='0x' + IntToHex(integer(sEmpfTelegramm[Pos]),2);
          EAdressfeldHigh.Enabled:=true;
          EAdressfeldHigh.Color:=clWindow;
        end
        else begin
          EAdressfeldHigh.Enabled:=false;
          EAdressfeldHigh.Color:=clBtnFace;
        end;
      end
      else begin
        EAdressfeldLow.Enabled:=false;
        EAdressfeldLow.Color:=clBtnFace;
        EAdressfeldHigh.Enabled:=false;
        EAdressfeldHigh.Color:=clBtnFace;
        lAdressfeld.Enabled:=false;
      end;

      inc(Pos);
      if length (sEmpfTelegramm) < Pos then exit;
      ETypkennung.Text:='0x' + IntToHex(integer(sEmpfTelegramm[Pos]),2);
      ETypkennung.Enabled:=true;
      ETypkennung.Color:=clWindow;
      lTypkennung.Enabled:=true;
      // Dezimal-Ausgabe Typkennung:
      ETypkennung_dez.Text:=IntToStr(integer(sEmpfTelegramm[Pos]));
      ETypkennung_dez.Enabled:=true;
      ETypkennung_dez.Color:=clWindow;
      lTypkennung_dez.Enabled:=true;

      L:=Integer(sEmpfTelegramm[2]);
      Pos:=4+L+1;
      if length (sEmpfTelegramm) < Pos then exit;
      EChecksum.Text:='0x' + IntToHex(integer(sEmpfTelegramm[Pos]),2);
      EChecksum.Enabled:=true;
      EChecksum.Color:=clWindow;
      lChecksum.Enabled:=true;

      Pos:=4+L+2;
      if length (sEmpfTelegramm) < Pos then exit;
      EEnde.Text:='0x' + IntToHex(integer(sEmpfTelegramm[Pos]),2);
      EEnde.Enabled:=true;
      EEnde.Color:=clWindow;
      lEnde.Enabled:=true;

      // gibts nicht bei Telegrammen variabler Länge:
      EEinzel.Enabled:=false;
      EEinzel.Color:=clBtnFace;
      { Liniennummer nicht disabled anzeigen, wenn Adressfeldlänge > 0 }
      if Adressfeld_Bytes > 0 then begin
        ELinienNr_dez.Enabled:=true;
        ELinienNr_dez.Color:=clWindow;
        lLinienNr_dez.Enabled:=true;
      end
      else begin
        ELinienNr_dez.Enabled:=false;
        ELinienNr_dez.Color:=clBtnFace;
        lLinienNr_dez.Enabled:=false;
      end;
    end;
  end;
end;

{--------------------------------------------}
Procedure TFormTelegrIec101.Update_DFCAnzeige;
{--------------------------------------------}
{ DFC-Statusanzeige aktualisieren }
begin
  if bEmpfDFC then begin
    eDFC.Text:='1';
    eDFC.Color:=clYellow;
  end
  else begin
    eDFC.Text:='0';
    eDFC.Color:=clWindow;
  end;
end;

{-----------------------------------------------------------}
procedure TFormTelegrIec101.Update_SendeTelegrammListAnzeige;
{-----------------------------------------------------------}
{ Anzeige der Sendetelegrammlisten akualisieren }
begin
  eAnzPrmSendTelegr.Text:=IntToStr (Prm_SendTelegrammList.Count);  // Anzahl der Primärstation-Telegramme in Liste
  eAnzSekSendTelegr.Text:=IntToStr (Sek_SendTelegrammList.Count);  // Anzahl der Sekundärstation-Telegramme in Liste

  if (Telegramm_senden AND ts_Prm) <> 0 then
    eAnzPrmSendTelegr.Color:=clYellow  // zur Versendung bereit: gelb
  else
    eAnzPrmSendTelegr.Color:=clWindow;

  if (Telegramm_senden AND ts_Sek) <> 0 then
    eAnzSekSendTelegr.Color:=clYellow  // zur Versendung bereit: gelb
  else
    eAnzSekSendTelegr.Color:=clWindow;
end;


{---------------------- Telegrammauswertung -----------------------------------}

{--------------------------------------------------------}
Function TFormTelegrIec101.Check_TelegrammFormat: integer;
{--------------------------------------------------------}
{ prüft Format des von der LW empfangenen Telegramms und setzt den Empfangstelegramm-Record }
var
  res: integer;
  sCS: string;
  CS: integer;
  L: integer;

begin
  if length (sEmpfTelegramm) < 1 then begin
    Result:=C_TGFORMAT_Err_TelegrLen;  { Fehler Telegrammlänge }
    exit;
  end;

  case Integer(sEmpfTelegramm[1]) of
    C_EINZEL_E5: begin  // Einzelzeichen E5
      res:=C_TGFORMAT_OK { OK }
    end;

    C_START_FEST: begin  // Telegramm mit fester Länge
      if length (sEmpfTelegramm) < (4+Adressfeld_Bytes) then begin
        Result:=C_TGFORMAT_Err_TelegrLen;  { Fehler Telegrammlänge }
        exit;
      end;

      if integer(sEmpfTelegramm[4+Adressfeld_Bytes]) = C_STOP then begin
        sCS:=Copy (sEmpfTelegramm, 2, 1+Adressfeld_Bytes);  // String, über den Checksumme gebildet wird
        CS:=Get_IEC870_101_Checksumme_Modulo256(sCS);
        if integer(sEmpfTelegramm[3+Adressfeld_Bytes]) = CS then
          res:=C_TGFORMAT_OK { OK }
        else
          res:=C_TGFORMAT_Err_Checksum; { Fehler Checksumme }
      end else
        res:=C_TGFORMAT_Err_StopChar; { Fehler Stopzeichen }
    end;

    C_START_VAR: begin  // Telegramm mit variabler Länge
      if length (sEmpfTelegramm) < 4 then begin
        Result:=C_TGFORMAT_Err_TelegrLen;  { Fehler Telegrammlänge }
        exit;
      end;

      if integer(sEmpfTelegramm[4]) = C_START_VAR then begin  // 4. Zeichen nochmal Startzeichen
        if sEmpfTelegramm[2] = sEmpfTelegramm[3] then begin  // 2. und 3. Zeichen: Länge Anwenderdaten
          L:=integer(sEmpfTelegramm[2]);
          if length (sEmpfTelegramm) < (4+L+2) then begin
            Result:=C_TGFORMAT_Err_TelegrLen;  { Fehler Telegrammlänge }
            exit;
          end;

          if integer(sEmpfTelegramm[4+L+2]) = C_STOP then begin
            sCS:=Copy(sEmpfTelegramm, 5, L);  // String, über den Checksumme gebildet wird
            CS:=Get_IEC870_101_Checksumme_Modulo256(sCS);
            if integer(sEmpfTelegramm[4+L+1]) = CS then
              res:=C_TGFORMAT_OK { OK }
            else
              res:=C_TGFORMAT_Err_Checksum; { Fehler Checksumme }
          end else
            res:=C_TGFORMAT_Err_StopChar; { Fehler Stopzeichen }
        end else
          res:=C_TGFORMAT_Err_LenChar; { Fehler Längenzeichen }
      end else
        res:=C_TGFORMAT_Err_StartChar; { Fehler Startzeichen }
    end;
  else
    res:=C_TGFORMAT_Err_StartChar; { Fehler Startzeichen }
  end;  { case }

  Result:=res;
end;

{----------------------------------------------------------}
function TFormTelegrIec101.Set_EmpfTelegrammRecord: boolean;
{----------------------------------------------------------}
{ belegt Empfangstelegramm-Record mit Rohtelegramm-Inhalt }
var
  L: integer;
  S: string;
  Herkunftsadresse_Bytes: byte;

begin
  Result:=true;
  with EmpfTelegramm do begin
    case integer(sEmpfTelegramm[1]) of
      C_EINZEL_E5: begin  // Einzelzeichen E5
        { Einzelzeichen-Flag setzen }
        bE5:=true;
        { nicht enthalten: Funktionscode, Liniennummer, Typkennung, SQ-Bit und
                           Anzahl der Objekte (variable Strukturkennung),
                           Übertragungsursache, Stationsnummer, Informationsobjekte }
        Funktionscode:=0;
        { Liniennummer nicht überschreiben, wird weiter benötigt ! }
        with DatenEinheitIdent do begin
          Typkennung:=0;
          SQ_Bit:=false;
          AnzahlObjekte:=0;
          UebertrUrsache:=0;
          Stationsnummer:=0;
        end;
        sObjekte:='';
      end;

      C_START_FEST: begin  // Telegramm mit fester Länge
        { Richtungsbit im Empfangstelegramm }
        if Uebertragungsprozedur = tp_symmetrisch then  // symmetrische Übertragung
          bEmpfDIR_symm:=Get_DIR;

        { Einzelzeichen-Flag löschen }
        bE5:=false;
        { Funktionscode }
        Funktionscode:=integer(sEmpfTelegramm[2]) AND $0F;
        { Liniennummer }
        if Adressfeld_Bytes > 0 then begin  // mit Adressfeld
          Liniennummer:=integer(sEmpfTelegramm[3]);
          if Adressfeld_Bytes > 1 then  // 2 Oktette für Adressfeld
            Liniennummer:=Liniennummer + (integer(sEmpfTelegramm[4]) * 256);
        end;

        { nicht enthalten: Typkennung, SQ-Bit und Anzahl der Objekte (variable
                           Strukturkennung), Übertragungsursache, Stationsnummer,
                           Informationsobjekte }
        with DatenEinheitIdent do begin
          Typkennung:=0;
          SQ_Bit:=false;
          AnzahlObjekte:=0;
          UebertrUrsache:=0;
          Stationsnummer:=0;
        end;
        sObjekte:='';

        { Dezimal-Anzeige Funktionscode und Liniennummer }
        EFktCode_dez.Text:=IntToStr(Funktionscode);
        EFktCode_dez.Enabled:=true;
        EFktCode_dez.Color:=clWindow;
        lFktCode_dez.Enabled:=true;
        if Adressfeld_Bytes > 0 then  // mit Adressfeld
          ELinienNr_dez.Text:=IntToStr(Liniennummer);
      end;

      C_START_VAR: begin  // Telegramm mit variabler Länge
        { Richtungsbit im Empfangstelegramm }
        if Uebertragungsprozedur = tp_symmetrisch then  // symmetrische Übertragung
          bEmpfDIR_symm:=Get_DIR;

        { Einzelzeichen-Flag löschen }
        bE5:=false;
        { Funktionscode }
        Funktionscode:=integer(sEmpfTelegramm[5]) AND $0F;
        { Liniennummer }
        if Adressfeld_Bytes > 0 then begin  // mit Adressfeld
          Liniennummer:=integer(sEmpfTelegramm[6]);
          if Adressfeld_Bytes > 1 then  // 2 Oktette für Adressfeld
            Liniennummer:=Liniennummer + (integer(sEmpfTelegramm[7]) * 256);
        end;
        with DatenEinheitIdent do begin
          { Typkennung }
          Typkennung:=integer(sEmpfTelegramm[6+Adressfeld_Bytes]);
          { Variable Strukturkennung: SQ-Bit }
          SQ_Bit:=(integer(sEmpfTelegramm[7+Adressfeld_Bytes]) AND $80) <> 0;
          { Variable Strukturkennung: Anzahl der Objekte }
          AnzahlObjekte:=integer(sEmpfTelegramm[7+Adressfeld_Bytes]) AND $7F;
          { Übertragungsursache (wenn Herkunftsadresse vorhanden, wird sie nicht ausgewertet) }
          UebertrUrsache:=integer(sEmpfTelegramm[8+Adressfeld_Bytes]);
          if Herkunftsadresse < 0 then  // 25.06.2007, WW
            Herkunftsadresse_Bytes:=0
          else
            Herkunftsadresse_Bytes:=1;
          { Stationsnummer }
          Stationsnummer:=integer(sEmpfTelegramm[9+Adressfeld_Bytes+Herkunftsadresse_Bytes]);
          if ASDU_Bytes > 1 then  // 2 Oktette für ASDU
            Stationsnummer:=Stationsnummer + (integer(sEmpfTelegramm[10+Adressfeld_Bytes+Herkunftsadresse_Bytes]) * 256);
        end;
        { Informationsobjekte }
        L:=integer(sEmpfTelegramm[2]);  // Telegrammlänge
        S:=Copy (sEmpfTelegramm, 1, 4+L+2);  // gesamtes Telegramm bis einschließlich Stopzeichen
        S:=Copy (S, 9+Adressfeld_Bytes+Herkunftsadresse_Bytes+ASDU_Bytes, length (S));  // ab Beginn Objekte
        sObjekte:=Copy (S, 1, length (S)-2);  // Checksumme, Stopzeichen wegschneiden

        { Dezimal-Anzeige Funktionscode und Liniennummer }
        EFktCode_dez.Text:=IntToStr(Funktionscode);
        EFktCode_dez.Enabled:=true;
        EFktCode_dez.Color:=clWindow;
        lFktCode_dez.Enabled:=true;
        if Adressfeld_Bytes > 0 then  // mit Adressfeld
          ELinienNr_dez.Text:=IntToStr(Liniennummer);
      end;
    else
      Result:=false;
    end;
  end;  { with EmpfTelegramm }
end;

{--------------------------------------------------}
Function TFormTelegrIec101.DecodeTelegramm: boolean;
{--------------------------------------------------}
{ Telegramm auf korrektes Format prüfen und Prüfergebnis anzeigen.
  Wenn Format OK: Empfangstelegramm-Record setzen
  Ergebnis: true, wenn Telegrammformat OK }
var
  FormatErg: integer;

begin
  Clear_TelegrAnzeige;  { Telegrammanzeige löschen }
  Update_TelegrAnzeige;  { Telegramm anzeigen }

  FormatErg:=Check_TelegrammFormat;  { Telegramm prüfen }
  EFormatPruefung.Text:=Get_TelegrammFormatCheck_Text (FormatErg);  { Telegramm-Prüfergebnis anzeigen }
  if FormatErg = C_TGFORMAT_OK then begin
    EFormatPruefung.Color:=clWindow;
    Result:=Set_EmpfTelegrammRecord;  { Empfangstelegramm-Record setzen }
  end
  else begin
    if IECLogFile <> nil then
      IECLogFile.Write ('FEHLER Telegrammformat: ' + EFormatPruefung.Text, '', sEmpfTelegramm, lt_Error);
    EFormatPruefung.Color:=clRed;
    Result:=false;
  end;
end;

{------------------------------------------}
Function TFormTelegrIec101.Get_DIR: boolean;
{------------------------------------------}
{ liest DIR (physikalische Übertragungsrichtung) aus Telegramm;
  Ergebnis: true, wenn DIR = 1
            false, wenn DIR = 0 }
var
  StFeld: char;

begin
  case integer(sEmpfTelegramm[1]) of
    C_START_FEST: StFeld:=sEmpfTelegramm[2];
    C_START_VAR:  StFeld:=sEmpfTelegramm[5];
  else
    StFeld:=NUL;
  end;
  Result:=(integer(StFeld) AND $80) <> 0;  { DIR: 0/1 }
end;

{------------------------------------------}
Function TFormTelegrIec101.Get_PRM: boolean;
{------------------------------------------}
{ liest PRM (Primärnachricht-Bit) aus Telegramm;
  Ergebnis: true, wenn PRM = 1
            false, wenn PRM = 0 }
var
  StFeld: char;

begin
  case integer(sEmpfTelegramm[1]) of
    C_START_FEST: StFeld:=sEmpfTelegramm[2];
    C_START_VAR:  StFeld:=sEmpfTelegramm[5];
  else
    StFeld:=NUL;
  end;
  Result:=(integer(StFeld) AND $40) <> 0;  { PRM: 0/1 }
end;

{----------------------------------------------}
Function TFormTelegrIec101.Get_FCB_ACD: boolean;
{----------------------------------------------}
{ liest FCB (Telegrammfolgebit) aus Telegramm einer Primärstation bzw.
  ACD (Zugriffsanforderung) aus Telegramm einer Sekundärstation;
  Ergebnis: true, wenn FCB/ACD = 1
            false, wenn FCB/ACD = 0 }
var
  StFeld: char;

begin
  case integer(sEmpfTelegramm[1]) of
    C_START_FEST: StFeld:=sEmpfTelegramm[2];
    C_START_VAR:  StFeld:=sEmpfTelegramm[5];
  else
    StFeld:=NUL;
  end;
  Result:=(integer(StFeld) AND $20) <> 0;  { FCB/ACD: 0/1 }
end;

{----------------------------------------------}
Function TFormTelegrIec101.Get_FCV_DFC: boolean;
{----------------------------------------------}
{ liest FCV (Telegrammfolgebit gültig) aus Telegramm einer Primärstation bzw.
  DFC (Datenflußsteuerung) aus Telegramm einer Sekundärstation;
  Ergebnis: true, wenn FCV/DFC = 1 (gültig)
            false, wenn FCV/DFC = 0 (ungültig) }
var
  StFeld: char;

begin
  case integer(sEmpfTelegramm[1]) of
    C_START_FEST: StFeld:=sEmpfTelegramm[2];
    C_START_VAR:  StFeld:=sEmpfTelegramm[5];
   else
     StFeld:=NUL;
  end;
  Result:=(integer(StFeld) AND $10) <> 0;  { FCV/DFC: 0/1 }
end;


{------------------------------------------------------------------------------}

{---------------------------------------------------------------------------------------}
Procedure TFormTelegrIec101.Set_Telegramm_FesteLaenge (aPRM: boolean; aFktCode: integer);
{---------------------------------------------------------------------------------------}
{ Telegramm fester Länge bilden und in Sende-Telegrammliste ablegen;
  Übergabe: Funktionscode für Steuerfeld }
var
  S: string;
  GesendetDaten: TGesendetDaten;
  SendTelegrammRec: TSendTelegrammRec;
  bFCV_Buf: boolean;
  STL: TSendTelegrammList;

begin
  if Uebertragungsprozedur = tp_unsymmetrisch then begin  // unsymmetrische Übertragung
    bFCV_Buf:=false;  // FCV in Telegramm der Sekundärstation nicht enthalten
    S:=Get_IEC870_101_TelegrammFesteLaenge_Unsymmetrisch_Sekundaer (C_ACD_0, C_DFC_0,
         aFktCode, EmpfTelegramm.Liniennummer, Adressfeld_Bytes);  // ACD=0, DFC=0
  end
  else begin // symmetrische Übertragung
    if aPRM then begin  // Telegramm der Primärstation
      bFCV_Buf:=Get_IEC870_101_FCV (aFktCode);  // Funktionscode-abhängiges FCV ermitteln
      S:=Get_IEC870_101_TelegrammFesteLaenge_Symmetrisch_Primaer (not bEmpfDIR_symm,
           not bSendFCB_letzt, bFCV_Buf, aFktCode, EmpfTelegramm.Liniennummer,
           Adressfeld_Bytes);  // invertiertes DIR, getoggeltes FCB
    end
    else begin // Telegramm der Sekundärstation
      bFCV_Buf:=false;  // FCV in Telegramm der Sekundärstation nicht enthalten
      if Einzelsteuerzeichen_E5 AND (aFktCode = C_FKT_SEK_0_ACK) then
        S:=char (C_EINZEL_E5)  // Einzelsteuerzeichen E5 statt ACK-Telegramm
      else
        S:=Get_IEC870_101_TelegrammFesteLaenge_Symmetrisch_Sekundaer (not bEmpfDIR_symm,
             C_DFC_0, aFktCode, EmpfTelegramm.Liniennummer, Adressfeld_Bytes);  // invertiertes DIR, DFC=0
    end;
  end;

  FillChar (GesendetDaten, SizeOf (GesendetDaten), 0);  // unbenutzt; GesendetDaten-Info nur bei Datentelegrammen

  { Sendetelegramm-Record zusammenstellen: }
  SendTelegrammRec.Telegramm:=S;
  SendTelegrammRec.FCV_101:=bFCV_Buf;
  SendTelegrammRec.SendefolgeNr_104:=-1;  // nicht benutzt, nur für 104
  SendTelegrammRec.TelegrammTyp:=tt_NoData;
  SendTelegrammRec.GesendetDaten:=GesendetDaten;
  SendTelegrammRec.LogHeader:='';  // ohne Log-Informationen
  SendTelegrammRec.LogDatensatzInfo:='';  // ohne Log-Informationen

  if aPRM then
    STL:=Prm_SendTelegrammList
  else
    STL:=Sek_SendTelegrammList;
  STL.Delete_AlleTelegramme;  // evtl. noch vorhandene (Daten-)Telegramme löschen
  STL.Insert_Telegramm (true, SendTelegrammRec);  // am Listenende anhängen

  Set_SendTelegrammBereit (aPRM);
end;

end.

