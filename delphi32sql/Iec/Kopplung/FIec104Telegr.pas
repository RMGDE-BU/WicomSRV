{******************************************************************************}
{* Unit: Telegramm-Formular für IEC-Kopplung (32-Bit), IEC 870-5-104          *}
{* 31.05.2007  WW                                                             *}
{******************************************************************************}
unit FIec104Telegr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, DateUtils, IecConst, FIecDaten, IecLog, WChars, IecIniFile,
  Iec870Util, FIecCustomTelegr, T_Zeit, IecImportThr, IecImportTelegrList,
  IecImportDirList, LogFile, O_IecRemoteMonitor;

type
  TIec104SteuerfeldFormat = (sfmt_I, sfmt_S, sfmt_U);

  { Struktur für IEC-Telegramm 104 }
  TIec104Telegramm = record
    SteuerfeldFormat: TIec104SteuerfeldFormat;
    SendefolgeNr: integer;
    EmpfangsfolgeNr: integer;
    Funktion_U: byte;
    DatenEinheitIdent: TIecDatenEinheitIdent;  { Identifikationsfeld der Dateneinheit }
    sObjekte: string;  { Informationsobjekt(e) }
  end;

  { Telegramm-Formular IEC 870-5-104 }
  TFormTelegrIec104 = class(TFormCustomTelegrIec)
    GroupBox1: TGroupBox;
    lStart: TLabel;
    lLaenge: TLabel;
    lSteuerfeldOkt1: TLabel;
    lTypkennung: TLabel;
    EStart: TEdit;
    ELaenge: TEdit;
    ESteuerfeld1: TEdit;
    ETypkennung: TEdit;
    GroupBox2: TGroupBox;
    Label8: TLabel;
    lFunktion_txt: TLabel;
    EFormatPruefung: TEdit;
    EFunktion_txt: TEdit;
    lTypkennung_dez: TLabel;
    ETypkennung_dez: TEdit;
    GroupBox3: TGroupBox;
    lAnzSendTelegr: TLabel;
    eAnzSendTelegr: TEdit;
    lSteuerfeldOkt2: TLabel;
    ESteuerfeld2: TEdit;
    lSteuerfeldOkt3: TLabel;
    ESteuerfeld3: TEdit;
    lSteuerfeldOkt4: TLabel;
    ESteuerfeld4: TEdit;
    eSendFolgeNr_dez: TEdit;
    lSendFolgeNr_dez: TLabel;
    eEmpfFolgeNr_dez: TEdit;
    lEmpfFolgeNr_dez: TLabel;
    GroupBox4: TGroupBox;
    lTimeout_t1: TLabel;
    eTimeout_t1: TEdit;
    lTimeout_t2: TLabel;
    eTimeout_t2: TEdit;
    lTimeout_t3: TLabel;
    eTimeout_t3: TEdit;
    lTelegrZaehler_k: TLabel;
    eTelegrZaehler_k: TEdit;
    lTelegrZaehler_w: TLabel;
    eTelegrZaehler_w: TEdit;
    lDatenuebertragung: TLabel;
    eDatenuebertragung: TEdit;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    eSendFolgeZS: TEdit;
    eEmpfFolgeZS: TEdit;
    lAckZS: TLabel;
    eAckZS: TEdit;
    lAnzUnquittTelegr: TLabel;
    eAnzUnquittTelegr: TEdit;
  private
    { Private-Deklarationen }
    Telegramm_senden: byte;  { Anzahl der zur Versendung freigegebenen Telegramme }
    SendTelegrammList: TSendTelegrammList;  { Liste mit zu versendenden Telegrammen }
    I_UnquittiertTelegrammList: TSendTelegrammList;  { Liste mit unquittierten Telegrammen im I-Format }
    EmpfTelegramm: TIec104Telegramm;  { von Leitwarte empfangenes IEC-Telegramm als Record }
    Alles_quittiert: boolean;

    Anz_k: integer;  { max. Anzahl an zu versendenden I-Format-Telegramme, dann warten auf Quittung der Gegenstelle }
    Anz_w: integer;  { max. Anzahl an empfangenen I-Format-Telegrammen, dann Quittung an Gegenstelle senden }

    isSTARTDT: boolean;
    SendefolgeZS_VS: integer;   // Sendefolgezählerstand
    EmpfangsfolgeZS_VR: integer;  // Empfangsfolgezählerstand
    AckZS: integer;  // Zählerstand für bestätigte Sendefolgenummern
    kZS: integer;  // Zählerstand für gesendete, unquittierte I-Format-Telegramme
    wZS: integer;  // Zählerstand für empfangene, unquittierte I-Format-Telegramme

    FIsClosing: boolean;
    procedure Get_IniTelegr (bCheckIniChanged: boolean);

    procedure Reset_Telegrammzaehler;
    procedure Inc_EmpfangsfolgeZS_VR;
    procedure Set_AckZS (aAckZS: integer);
    procedure Set_kZS (akZS: integer);
    procedure Inc_wZS;
    procedure Reset_wZS;
    procedure Set_StartDT (OnOff: boolean);

    Procedure Clear_TelegrAnzeige;
    Procedure Update_TelegrAnzeige;
    Procedure Update_SendeTelegrammListAnzeige;
    Procedure Update_StartDTAnzeige;
    Procedure Update_kZSAnzeige;
    Procedure Update_wZSAnzeige;
    Procedure Update_SendefolgeZS_VSAnzeige;
    Procedure Update_EmpfangsfolgeZS_VRAnzeige;
    Procedure Update_AckZSAnzeige;

    Function Check_TelegrammFormat: integer;
    function Get_Funktion_U_Text (Funktion_U: byte): string;
    function Set_EmpfTelegrammRecord: boolean;
    Function DecodeTelegramm: boolean;

    Procedure Set_SendTelegrammAngekommenBySendefolgeNr (SendefolgeNr_von1: integer;
      SendefolgeNr_bis1: integer; SendefolgeNr_von2: integer; SendefolgeNr_bis2: integer);
    Procedure Update_DatenGesendetBySendefolgeNr (SendefolgeNr_von1: integer;
      SendefolgeNr_bis1: integer; SendefolgeNr_von2: integer; SendefolgeNr_bis2: integer);
    Procedure Set_Telegramm_S;

    function Quittiere_Telegramme: boolean;
    Procedure Uebertragung_Empfang;
  protected
    Procedure Set_SendTelegrammBereit (aPRM_101: boolean); override;
  public
    { Public-Deklarationen }
    constructor Create (AOwner: TComponent; AFunktion: TIEC870_Funktion;
                        ANetProgDir: string;
                        AImportDirList: TImportDirList;
                        ARemoteMonitorObj: TRemoteMonitorObj); override;
    destructor Destroy; override;
    function CheckTelegr: boolean;
    procedure Init_Uebertragung;
    procedure Close_Uebertragung;
    procedure Update_Timeout_t1Anzeige (S: string);
    procedure Update_Timeout_t2Anzeige (S: string);
    procedure Update_Timeout_t3Anzeige (S: string);
    procedure Inc_SendefolgeZS_VS;
    procedure Inc_kZS;
    function Get_SendTelegramm (var SendTelegr: string;
      var TelegrammTyp: TTelegrammTyp): boolean;
    Procedure Set_SendTelegrammVersendet;
    Procedure Set_SendTelegrammUnquittiert_I;
    Procedure Set_SendTelegrammAngekommen;
    Procedure Set_Telegramm_U (aFunktion_U: byte);
    Procedure Uebertragung (CommStatus: TIecCommStatus);
    property IsClosing: boolean read FIsClosing;
  end;

var
  FormTelegrIec104: TFormTelegrIec104;

implementation

uses
  FIec104Port;

{$R *.dfm}

{ TFormTelegrIec104 }

{--------------------------------------------------------------------------}
constructor TFormTelegrIec104.Create (AOwner: TComponent;
                                      AFunktion: TIEC870_Funktion;
                                      ANetProgDir: string;
                                      AImportDirList: TImportDirList;
                                      ARemoteMonitorObj: TRemoteMonitorObj);
{--------------------------------------------------------------------------}
begin
  inherited Create (AOwner, AFunktion, ANetProgDir, AImportDirList, ARemoteMonitorObj);
  FIsClosing:=false;

  Get_IniTelegr (false);  { liest Ini-Einstellungen für Telegramme }

  SendTelegrammList:=TSendTelegrammList.Create;
  I_UnquittiertTelegrammList:=TSendTelegrammList.Create;

  FillChar (EmpfTelegramm, SizeOf (EmpfTelegramm), 0);  // Vorbelegung: Empfangstelegramm-Record
  Init_Uebertragung;   // Telegramm-Übertragung initialiseren
end;

{-----------------------------------}
destructor TFormTelegrIec104.Destroy;
{-----------------------------------}
begin
  I_UnquittiertTelegrammList.Free;
  SendTelegrammList.Free;

  inherited Destroy;
end;

{--------------------------------------------------------------------}
procedure TFormTelegrIec104.Get_IniTelegr (bCheckIniChanged: boolean);
{--------------------------------------------------------------------}
{ INI-Konfiguration für 104-Telegramme lesen;
  Übergabe: Flag 'bCheckIniChanged': wenn true, wird nur bei geänderter INI-
              Konfiguration gelesen }
var
  Iec32Ini: TIec32Ini;
begin
  Iec32Ini:=TIec32Ini.Create (FNetProgDir);
  try
    // INI-Einstellungen, welche nur zum Programmstart gelesen werden (können):
    if (not bCheckIniChanged) then begin
      Adressfeld_Bytes:=0;  // fest ohne Adressfeld
      ASDU_Bytes:=2;  // fest 2 Oktette
      InfoObj_Bytes:=3;  // fest 3 Oktette
      Herkunftsadresse:=Iec32Ini.Herkunftsadresse;
      if Herkunftsadresse < 0 then
        Herkunftsadresse:=0;  // immer mit Herkunftsadresse; Vorbelegung 0, wenn nicht vorhanden
      Telegramm_MaxBytes:=Iec32Ini.APDU_MaxBytes;  // max. Länge der APDU (Anzahl der Steuerfeld- und ASDU-Oktette)

      Anz_k:=Iec32Ini.Anz_k;
      Anz_w:=Iec32Ini.Anz_w;

      RedundanzStatusmeldung:=Iec32Ini.RedundanzStatusmeldung;
      RedundanzStatusmeldung_ASDU:=Iec32Ini.RedundanzStatusmeldung_ASDU;
      RedundanzStatusmeldung_InfoObj:=Iec32Ini.RedundanzStatusmeldung_InfoObj;
    end;

    // INI-Einstellungen, welche zur Laufzeit neu gelesen werden (können):
    if (not bCheckIniChanged) OR
       (bCheckIniChanged AND
        not (SameDateTime (Iec32Ini.FileDate, FIniDate))) then begin     
      FIniDate:=Iec32Ini.FileDate;  // Zeitstempel der INI-Datei merken; 10.03.2015, WW

      bDebugRohdaten:=Iec32Ini.DebugRohdaten;
      bTK_103_Aktiv:=Iec32Ini.TypkennungAktiv [103];  // Typkennung 103 (Uhrzeit-Synchronisation) aktiv

      // für Funktion 'Zentrale'
      bDatenImport:=Iec32Ini.Import;
      iDatenImport_VerzoegerungSek:=Iec32Ini.ImportVerzoegerungSekunden; 
    end;
  finally
    Iec32Ini.Free;
  end;
end;

{----------------------------------------------}
function TFormTelegrIec104.CheckTelegr: boolean;
{----------------------------------------------}
var
  S: string;

begin
  Result:=true;
  { Prüfung max. Telegrammzähler k, w: }
  if (Anz_w/Anz_k) > (2/3) then begin
    S:='Prüfung der INI-Einstellungen für max. Telegrammzähler k und w (IEC 870-5-104):' + #13 +
       '[104] k = ' + IntToStr (Anz_k) + ', w = ' + IntToStr (Anz_w) + #13#13 +
       'w sollte Zweidrittel von k nicht überschreiten !' + #13 +
       'Standardwerte: k = 12, w = 8';
    WriteProgramLog (S, lt_Warning);
    MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONWARNING + MB_OK);
    // Programmstart fortsetzen, keine Rückfrage mehr wg. Programmvariante 'Dienst'; 10.03.2015, WW
  end;

  { Prüfung IEC-Konfiguration: }
  if not FormDatenIec.Check_KonfigDB (Adressfeld_Bytes, ASDU_Bytes, InfoObj_Bytes) then begin
    Result:=false;
    exit;
  end;
end;

{--------------------------------------------}
procedure TFormTelegrIec104.Init_Uebertragung;
{--------------------------------------------}
{ Initialisieren der Telegramm-Übertragung }
begin
  Reset_Telegrammzaehler;  // Telegrammzählerstände zurücksetzen
  Set_StartDT (false) ;  // Station: Daten dürfen nicht an Zentrale gesendet werden
                         // Zentrale: Datenübertragung wurde in Gegenstelle noch nicht aktiviert
  // Sendetelegrammliste leeren:
  Telegramm_senden:=0;
  SendTelegrammList.Delete_AlleTelegramme;
  I_UnquittiertTelegrammList.Delete_AlleTelegramme;
  Update_SendeTelegrammListAnzeige;  // Anzeige der Sendetelegrammliste akualisieren

  Alles_quittiert:=false;
  if Funktion = fkt_iec870_Station then
    FormDatenIec.F_Unlock;  // Sperren in IEC-Datentabellen aufheben
  LinienNrVerwList.Clear;  // Linienverwaltungsliste zurücksetzen

  Set_SendRedundanzTelegrammNow (true);  // Redundanz-Betriebszustands-Telegramm soll gesendet werden
end;

{---------------------------------------------}
procedure TFormTelegrIec104.Close_Uebertragung;
{---------------------------------------------}
{ Beenden der Telegramm-Übertragung }
begin
  FIsClosing:=true;
  if Funktion = fkt_iec870_Zentrale then begin  // wir sind Zentrale
    // Datenübertragung in Unterstation deaktivieren: STOPDT: act schicken
    Set_Telegramm_U (C_FKT_STOPDT_ACT);
    // Warten bis STOPDT: con empfangen wurde oder Verbindung zur Gegenstelle
    // nicht mehr besteht:
    while isSTARTDT AND FormPortIec104.Connected do
      Delay (1);
  end;
end;

{-------------------------------------------------}
procedure TFormTelegrIec104.Reset_Telegrammzaehler;
{-------------------------------------------------}
{ Telegrammzählerstände zurücksetzen und anzeigen }
begin
  SendefolgeZS_VS:=0;
  EmpfangsfolgeZS_VR:=0;
  AckZS:=0;
  kZS:=0;
  wZS:=0;

  // Anzeige Telegrammzählerstände:
  Update_SendefolgeZS_VSAnzeige;
  Update_EmpfangsfolgeZS_VRAnzeige;
  Update_AckZSAnzeige;
  Update_kZSAnzeige;
  Update_wZSAnzeige;
end;

{----------------------------------------------}
procedure TFormTelegrIec104.Inc_SendefolgeZS_VS;
{----------------------------------------------}
{ Sendefolgezählerstand inkrementieren und anzeigen }
begin
  Inc_IEC870_104_Telegrammfolgenummer (SendefolgeZS_VS);
  Update_SendefolgeZS_VSAnzeige;
end;

{-------------------------------------------------}
procedure TFormTelegrIec104.Inc_EmpfangsfolgeZS_VR;
{-------------------------------------------------}
{ Empfangsfolgezählerstand inkrementieren und anzeigen }
begin
  Inc_IEC870_104_Telegrammfolgenummer (EmpfangsfolgeZS_VR);
  Update_EmpfangsfolgeZS_VRAnzeige;
end;

{------------------------------------------------------}
procedure TFormTelegrIec104.Set_AckZS (aAckZS: integer);
{------------------------------------------------------}
{ Ack-Zählerstand setzen und anzeigen }
begin
  AckZS:=aAckZS;
  Update_AckZSAnzeige;
end;

{----------------------------------}
procedure TFormTelegrIec104.Inc_kZS;
{----------------------------------}
{ k-Zählerstand inkrementieren und anzeigen }
begin
  inc (kZS);
  Update_kZSAnzeige;
end;

{--------------------------------------------------}
procedure TFormTelegrIec104.Set_kZS (akZS: integer);
{--------------------------------------------------}
{ k-Zählerstand setzen und anzeigen }
begin
  kZS:=akZS;
  Update_kZSAnzeige;
end;

{----------------------------------}
procedure TFormTelegrIec104.Inc_wZS;
{----------------------------------}
{ w-Zählerstand inkrementieren und anzeigen }
begin
  inc (wZS);
  Update_wZSAnzeige;
end;

{------------------------------------}
procedure TFormTelegrIec104.Reset_wZS;
{------------------------------------}
{ w-Zählerstand zurücksetzen und anzeigen }
begin
  wZS:=0;
  Update_wZSAnzeige;
end;

{-------------------------------------------------------}
procedure TFormTelegrIec104.Set_StartDT (OnOff: boolean);
{-------------------------------------------------------}
{ STARTDT-Flag setzen/rücksetzen und anzeigen }
begin
  isSTARTDT:=OnOff;
  Update_StartDTAnzeige;
end;

{--------------------------------------------------------------------------------------}
function TFormTelegrIec104.Get_SendTelegramm (var SendTelegr: string;
                                              var TelegrammTyp: TTelegrammTyp): boolean;
{--------------------------------------------------------------------------------------}
{ gibt nächstes zu versendendes Telegramm zurück;
  Rückgaben: Sende-Telegramm
             Telegrammtyp
  Ergebnis: true, wenn Telegramm in Telegrammliste vorhanden ist }
var
  sLogHeader: string;
  sLogDatensatzInfo: string;

begin
  Result:=false;
  SendTelegr:='';
  TelegrammTyp:=tt_NoData;
  sLogHeader:='';

  { prüfen, ob zu versendendes Telegramm vorhanden ist: }
  if (Telegramm_senden > 0) AND (SendTelegrammList.Count > 0) then begin
    SendTelegr:=TSendTelegrammObj (SendTelegrammList[0]).Daten.Telegramm;
    TelegrammTyp:=TSendTelegrammObj (SendTelegrammList[0]).Daten.TelegrammTyp;
    sLogHeader:=TSendTelegrammObj (SendTelegrammList[0]).Daten.LogHeader;
    sLogDatensatzInfo:=TSendTelegrammObj (SendTelegrammList[0]).Daten.LogDatensatzInfo;
    Result:=true;
  end;

  if Result then begin  // zu versendendes Telegramm vorhanden
    if IECLogFile <> nil then { Telegramm in Log-Datei protokollieren }
      if length (sLogHeader) > 0 then  // nur, wenn Log-Header vorhanden ist (Datentelegramme)
        IECLogFile.Write (sLogHeader, sLogDatensatzInfo, SendTelegr);
  end else
    Telegramm_senden:=0;
end;

{----------------------------------------------------------------------}
Procedure TFormTelegrIec104.Set_SendTelegrammBereit (aPRM_101: boolean);
{----------------------------------------------------------------------}
{ Flag für zur Versendung bereites Telegramm setzen; }
begin
  Telegramm_senden:=1;  // ein Sendetelegramm zur Versendung freigeben
  Update_SendeTelegrammListAnzeige;  // Anzeige der Sendetelegrammliste akualisieren
end;

{-----------------------------------------------------}
Procedure TFormTelegrIec104.Set_SendTelegrammVersendet;
{-----------------------------------------------------}
{ Flags für zur Versendung bereites Telegramm rücksetzen }
begin
  if Telegramm_senden > 0 then
    dec (Telegramm_senden);  // Telegrammfreigabe-Zähler runterzählen

  Update_SendeTelegrammListAnzeige;  // Anzeige der Sendetelegrammliste akualisieren
end;

{---------------------------------------------------------}
Procedure TFormTelegrIec104.Set_SendTelegrammUnquittiert_I;
{---------------------------------------------------------}
{ Flags für zur Versendung bereites Telegramm rücksetzen }
begin
  // versendetes Telegramm in Liste für unquittierte I-Format-Telegramme eintragen:
  if SendTelegrammList.Count > 0 then begin
    I_UnquittiertTelegrammList.Insert_Telegramm (true, TSendTelegrammObj (SendTelegrammList[0]).Daten);
    SendTelegrammList.Delete(0);  // versendetes Telegramm rauslöschen
    Alles_quittiert:=false;
  end;

  Update_SendeTelegrammListAnzeige;  // Anzeige der Sendetelegrammliste akualisieren
end;

{------------------------------------------------------}
Procedure TFormTelegrIec104.Set_SendTelegrammAngekommen;
{------------------------------------------------------}
{ Telegramm mit Index 0 aus Sende-Telegrammliste löschen }
begin
  if SendTelegrammList.Count > 0 then
    SendTelegrammList.Delete(0);  // erstes Telegramm rauslöschen

  Update_SendeTelegrammListAnzeige;  // Anzeige der Sendetelegrammliste akualisieren
end;

{------------------------------------------------------------------------------------------------}
Procedure TFormTelegrIec104.Set_SendTelegrammAngekommenBySendefolgeNr (SendefolgeNr_von1: integer;
      SendefolgeNr_bis1: integer; SendefolgeNr_von2: integer; SendefolgeNr_bis2: integer);
{------------------------------------------------------------------------------------------------}
{ Telegramme mit Sendefolgenummern, welche in den übergebenen von-bis-Wertebereichen
  liegen, aus Sende-Telegrammliste löschen;
  Übergabe: von/bis-Sendefolgenummernbereich 1
            von/bis-Sendefolgenummernbereich 2 }
var
  SendefolgeNr: integer;
  Count_Merk: integer;

begin
  Count_Merk:=I_UnquittiertTelegrammList.Count;
  while I_UnquittiertTelegrammList.Count > 0 do begin
    SendefolgeNr:=TSendTelegrammObj (I_UnquittiertTelegrammList [0]).Daten.SendefolgeNr_104;
    if SendefolgeNr >= 0 then begin  // Telegramm mit Sendefolgenummer
      if ((SendefolgeNr >= SendefolgeNr_von1) AND (SendefolgeNr <= SendefolgeNr_bis1)) OR
         ((SendefolgeNr >= SendefolgeNr_von2) AND (SendefolgeNr <= SendefolgeNr_bis2)) then
        I_UnquittiertTelegrammList.Delete(0)
      else
        Break;
    end else
      Break;
  end;

  Update_SendeTelegrammListAnzeige;  // Anzeige der Sendetelegrammliste akualisieren

  // wenn alle Telegramme quittert wurden: Flag setzen
  if (Count_Merk > 0) AND (I_UnquittiertTelegrammList.Count = 0) then
    Alles_quittiert:=true;
end;


{------------------------- Gesendete Daten ------------------------------------}

{-----------------------------------------------------------------------------------------}
procedure TFormTelegrIec104.Update_DatenGesendetBySendefolgeNr (SendefolgeNr_von1: integer;
      SendefolgeNr_bis1: integer; SendefolgeNr_von2: integer; SendefolgeNr_bis2: integer);
{-----------------------------------------------------------------------------------------}
{ Daten zu Telegrammen mit Sendefolgenummern, welche in den übergebenen von-bis-Wertebereichen
  liegen, in Iec-Datentabellen vermerken bzw. Kurzzeitwert-Datei löschen;
  Übergabe: von/bis-Sendefolgenummernbereich 1
            von/bis-Sendefolgenummernbereich 2 }
var
  i: integer;
  SendefolgeNr: integer;

begin
  for i:=0 to (I_UnquittiertTelegrammList.Count-1) do begin
    SendefolgeNr:=TSendTelegrammObj (I_UnquittiertTelegrammList [i]).Daten.SendefolgeNr_104;
    if SendefolgeNr >= 0 then begin  // Telegramm mit Sendefolgenummer
      if ((SendefolgeNr >= SendefolgeNr_von1) AND (SendefolgeNr <= SendefolgeNr_bis1)) OR
         ((SendefolgeNr >= SendefolgeNr_von2) AND (SendefolgeNr <= SendefolgeNr_bis2)) then
        Update_DatenGesendet (I_UnquittiertTelegrammList, i)
      else
        Break;
    end else
      Break;
  end;
end;

{------------------------ Telegrammquittierung --------------------------------}

{-------------------------------------------------------}
function TFormTelegrIec104.Quittiere_Telegramme: boolean;
{-------------------------------------------------------}
{ Aktionen nach Empfang eines Quittungstelegramms der Gegenstelle;
  Ergebnis: true, wenn alle versendeten I-Telegramme quittiert wurden }
var
  SendefolgeNr_von1: integer;
  SendefolgeNr_bis1: integer;
  SendefolgeNr_von2: integer;
  SendefolgeNr_bis2: integer;

begin
  { Prüfung Empfangsfolgenummer: }
  if EmpfTelegramm.EmpfangsfolgeNr <> AckZS then begin  // Ack muß aktualisiert werden
    { von/bis-Bereich für zu löschende Sendefolgenummern ermitteln: }
    SendefolgeNr_von1:=AckZS;  // bisheriger ACK-Zählerstand
    if EmpfTelegramm.EmpfangsfolgeNr = 0 then
      SendefolgeNr_bis1:=C_IEC870_104_MaxTelegrFolgenummer
    else
      SendefolgeNr_bis1:=EmpfTelegramm.EmpfangsfolgeNr - 1;

    if SendefolgeNr_von1 > SendefolgeNr_bis1 then begin  // Überlauf
      SendefolgeNr_von2:=0;
      SendefolgeNr_bis2:=SendefolgeNr_bis1;
      SendefolgeNr_bis1:=C_IEC870_104_MaxTelegrFolgenummer;
    end
    else begin
      SendefolgeNr_von2:=-1;  // unbenutzt
      SendefolgeNr_bis2:=-1;  // unbenutzt
    end;

    if Funktion = fkt_iec870_Station then begin  // wir sind Unterstation
      { an Zentrale übertragene Datenbereiche in IEC-Daten-Tabelle updaten und
        Telegramme aus Sende-Telegrammliste löschen: }
      Update_DatenGesendetBySendefolgeNr (SendefolgeNr_von1,
        SendefolgeNr_bis1, SendefolgeNr_von2, SendefolgeNr_bis2);
    end;
    Set_SendTelegrammAngekommenBySendefolgeNr (SendefolgeNr_von1,
      SendefolgeNr_bis1, SendefolgeNr_von2, SendefolgeNr_bis2);

    Set_AckZS (EmpfTelegramm.EmpfangsfolgeNr);  // ACK-Zählerstand aktualisieren
    Set_kZS (SendefolgeZS_VS - AckZS);  // k-Zählerstand aktualisieren
  end;

  Result:=SendefolgeZS_VS = AckZS;
end;

{--------------------------------------------------------------------}
Procedure TFormTelegrIec104.Uebertragung (CommStatus: TIecCommStatus);
{--------------------------------------------------------------------}
{ Aktion nach empfangenem Telegramm ausführen;
  Übergabe: Übertragungsstatus }
const
  bDummy: boolean = false;
  iDummy: integer = 0;
var
  iCause : byte;
  iCause_KZW : byte;

begin
  if CommStatus = ics_Receive then begin  // Telegramm empfangen
    { Gültigkeitsprüfung des empfangenen Telegramms: }
    if not DecodeTelegramm then begin
      { Was tun bei ungültigem Empfangstelegramm ?
        -> l.t. Norm: keine Antwort, t1-Timeout-Überwachung greift (IEC 870-5-104,
           Kap. 5.1, 5.2, 5.3) }
      exit;
    end;

    Uebertragung_Empfang;  { Aktion der Kopplung auf empfangenes Telegramm der Gegenstelle }
  end
  else if CommStatus = ics_Timeout_t2 then begin
    // nach Timeout t2: S-Format-Telegramm zur Quittierung schicken
    Reset_wZS;  // w-Zählerstand zurücksetzen
    Set_Telegramm_S;
  end
  else if CommStatus = ics_Timeout_t3 then begin
    Set_Telegramm_U (C_FKT_TESTFR_ACT);  // nach Timeout t3: TESTFR: act versenden
  end
  else if CommStatus = ics_Idle then begin
    if Funktion = fkt_iec870_Station then begin  // wir sind Unterstation
      { Daten an Zentrale senden: }
      if isSTARTDT then begin  // Daten dürfen an die Zentrale gesendet werden
        if kZS < Anz_k then begin  // max. k-Zählerstand ist noch nicht erreicht
          if (Redundanzbetrieb <> C_Redundanz_Aus) AND bSend_RedundanzTelegramm_Now then begin
            // Telegramm mit Redundanz-Betriebszustand (aktiv/passiv) soll versendet werden
            Set_SendRedundanzTelegrammNow (false);  // Flag zurücksetzen
            Set_Telegramm_Redundanz_AktivPassiv (SendTelegrammList,
              Redundanzbetrieb <> C_Redundanz_Passiv, norm_iec870_104,
              bDummy, bDummy, bDummy, iDummy,  // nur für 101
              SendefolgeZS_VS, EmpfangsfolgeZS_VR,
              -1, C_URS_spontan);
          end
          else if Redundanzbetrieb <> C_Redundanz_Passiv then begin  // im passiven Redundanz-Betrieb keine Daten schicken
            if SendTelegrammList.Count > 0 then  begin
              // es ist noch mind. 1 nicht versendetes Daten-Telegramm in der
              // Sendetelegrammliste vorhanden:
              if TSendTelegrammObj (SendTelegrammList[0]).Daten.TelegrammTyp in
                [tt_Data_MRG, tt_Data_DSfG, tt_Data_DSfG_KZW, tt_Data_intern] then begin
                Set_SendTelegrammBereit (bDummy); // nächstes Telegramm versenden
              end;
            end
            else begin
              case FIecFktStatus of
                if_GA_AckActivate:  // Generalabfrage: Bestätigung der Aktivierung
                  begin
                    { Telegramm 'Generalabfrage, Bestätigung der Aktivierung' senden: }
                    Set_Telegramm_Generalabfrage (SendTelegrammList, norm_iec870_104,
                      bDummy, bDummy, bDummy, iDummy,  // nur für 101
                      SendefolgeZS_VS, EmpfangsfolgeZS_VR,
                      -1, C_URS_AckActivate,
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

                    // Daten neu laden und in Sende-Telegrammliste stellen
                    // -> Datensatz in IEC-Datentabelle sperren bzw. aus Kurzzeitwertliste
                    //    gleich wieder löschen, um Mehrfach-Übertragung noch nicht
                    //    quittierter Telegramme zu Verhindern.
                    if not Set_Telegramme_Daten_KZW (SendTelegrammList, norm_iec870_104,
                                                     bDummy, bDummy, bDummy, iDummy,  // nur für 101
                                                     SendefolgeZS_VS, EmpfangsfolgeZS_VR,
                                                     -1, iCause_KZW, true) then
                      if not Set_Telegramme_Daten_MRG (SendTelegrammList, norm_iec870_104,
                                                       bDummy, bDummy, bDummy, iDummy,  // nur für 101
                                                       SendefolgeZS_VS, EmpfangsfolgeZS_VR,
                                                       -1, iCause, true) then
                        if not Set_Telegramme_Daten_DSfG (SendTelegrammList, norm_iec870_104,
                                                          bDummy, bDummy, bDummy, iDummy,  // nur für 101
                                                          SendefolgeZS_VS, EmpfangsfolgeZS_VR,
                                                          -1, iCause, true) then begin
                          { Es sind keine Daten vorhanden }
                          if (FIecFktStatus = if_GA_Daten_Continue) then begin
                            { Telegramm 'Generalabfrage, Ende der Aktivierung' senden: }
                            Set_Telegramm_Generalabfrage (SendTelegrammList, norm_iec870_104,
                              bDummy, bDummy, bDummy, iDummy,  // nur für 101
                              SendefolgeZS_VS, EmpfangsfolgeZS_VR,
                              -1, C_URS_EndActivate,
                              FGA_StationsNrListe);

                            { nächster Funktionsstatus: inaktiv }
                            FIecFktStatus:=if_Inaktiv;
                          end;

                          if Alles_quittiert then begin
                            Alles_quittiert:=false;
                            FormDatenIec.F_Unlock;  // Sperren in IEC-Datentabellen aufheben
                            LinienNrVerwList.Clear;  // Linienverwaltungsliste zurücksetzen
                          end;
                        end;
                  end;
              end;  // case FIecGAStatus
            end;
          end;
        end;
      end;
    end;
  end;
end;

{-----------------------------------------------}
Procedure TFormTelegrIec104.Uebertragung_Empfang;
{-----------------------------------------------}
{ Aktion der Kopplung auf empfangenes Telegramm der Gegenstelle }
const
  bDummy: boolean = false;
  iDummy: integer = 0;
var
  S: string;
  Abfragekennung_QOI: integer;
  AlleStationen: boolean;
  IecImportTelegrData: TIecImportTelegrData;

begin
  { Auswertung Funktionscode des empfangenen Telegramms: }
  case EmpfTelegramm.SteuerfeldFormat of
    sfmt_I:  { I-Format }
      begin
        FormPortIec104.StartTimer_t2;  // t2-Timeout-Überwachung starten
        Inc_wZS; // w-Zählerstand für empfangene I-Format-Telegramme hochzählen

        { Prüfung Sendefolgenummer: }
        if EmpfTelegramm.SendefolgeNr = EmpfangsfolgeZS_VR then begin  // Sendefolgenummer OK
          Inc_EmpfangsfolgeZS_VR;  // Empfangsfolgezähler V(R) hochzählen

          if Quittiere_Telegramme then begin  // übertragene Datenbereiche updaten, Sende-Telegramme löschen
            FormPortIec104.StopTimer_t1;  // t1-Timeout-Überwachung stoppen
          end;

          // nach max. w Telegrammen: S-Format-Telegramm zur Quittierung schicken
          if wZS >= Anz_w then begin
            Reset_wZS;  // w-Zählerstand zurücksetzen
            FormPortIec104.StopTimer_t2;  // t2-Timeout-Überwachung stoppen
            Set_Telegramm_S;
          end;

          if Funktion = fkt_iec870_Station then begin  // wir sind Unterstation
            // Datentelegramm der Zentrale auswerten und Aktion ausführen:
            case EmpfTelegramm.DatenEinheitIdent.Typkennung of
              C_TK_Generalabfrage:  { Generalabfrage-Telegramm }
                begin
                  { ...mit Abfragekennung QOI 'Stationsabfrage, global' }
                  Abfragekennung_QOI:=integer(EmpfTelegramm.sObjekte [InfoObj_Bytes+1]);
                  if Abfragekennung_QOI = C_AFK_Stationsabfrage_global then begin
                    if IECLogFile <> nil then begin
                      S:='Generalabfrage: Station = ' +
                      IntToStr (EmpfTelegramm.DatenEinheitIdent.Stationsnummer);
                      IECLogFile.Write (S, '', '');
                    end;

                    if ASDU_Bytes > 1 then
                      AlleStationen:=EmpfTelegramm.DatenEinheitIdent.Stationsnummer = $FFFF  { 2 Byte Stationsnummer }
                    else
                      AlleStationen:=EmpfTelegramm.DatenEinheitIdent.Stationsnummer = $FF;  { 1 Byte Stationsnummer }

                    { GA-Stationsnummernliste füllen für nachfolgende Telegramme
                      'Generalabfrage, Bestätigung/Ende der Aktivierung': }
                    if AlleStationen then
                      FormDatenIec.FillGA_StationsNrListe_Alle (-1, FGA_StationsNrListe)
                    else
                      FillGA_StationsNrListe_Station (EmpfTelegramm.DatenEinheitIdent.Stationsnummer);

                    { Bereich bereits gesendeter Daten auf zu sendenden Generalabfrage-Bereich rücksetzen: }
                    FormDatenIec.SetGesendetBis_General (-1, EmpfTelegramm.DatenEinheitIdent.Stationsnummer,
                                                         Adressfeld_Bytes, ASDU_Bytes);
                    { vorhandenen Datenbereich einlesen (alle Datentypen): }
                    FormDatenIec.F_Update (true, true, true, true);
                    LinienNrVerwList.Clear;  { Liste zurücksetzen }

                    { nächster Funktionsstatus: Generalabfrage, Bestätigung der Aktivierung }
                    FIecFktStatus:=if_GA_AckActivate;

                    { wenn GA auf ASDU für Redundanz-Betriebszustandsmeldung oder
                      auf alle Stationen erfolgt: Flag setzen, um Redundanz-
                      Betriebszustandsmeldung zu senden }
                    if AlleStationen OR
                      (EmpfTelegramm.DatenEinheitIdent.Stationsnummer = RedundanzStatusmeldung_ASDU) then
                      Set_SendRedundanzTelegrammNow (true);    {!! Test steht noch aus }
                  end
                  else begin
                    // keine Aktion
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
                      Set_Telegramm_UhrzeitSynchronisation (SendTelegrammList, norm_iec870_104,
                        bDummy, bDummy, bDummy, iDummy,  // nur für 101
                        SendefolgeZS_VS, EmpfangsfolgeZS_VR,
                        -1, C_URS_AckActivate,
                        EmpfTelegramm.DatenEinheitIdent.Stationsnummer,  // Stationsnummer aus Empfangstelegramm
                        EmpfTelegramm.sObjekte);  // Info-Objektadresse und Zeit aus Empfangstelegramm

                      { -> Es erfolgt keine Synchronisation der PC-Zeit ! }
                    end
                    else begin
                      // keine Aktion
                      if IECLogFile <> nil then
                        IECLogFile.Write ('Uhrzeit-Synchronisation: Nicht unterstützte Übertragungsursache (' +
                                          IntToStr (EmpfTelegramm.DatenEinheitIdent.UebertrUrsache) + ')', '', '');
                    end;
                  end
                  else begin
                    // keine Aktion
                    if IECLogFile <> nil then
                      IECLogFile.Write ('Nicht unterstützte Typkennung (' +
                                        IntToStr (EmpfTelegramm.DatenEinheitIdent.Typkennung) + ')', '', '');
                  end;
                end;
            else
              // keine Aktion
              if IECLogFile <> nil then
                IECLogFile.Write ('Nicht unterstützte Typkennung (' +
                                  IntToStr (EmpfTelegramm.DatenEinheitIdent.Typkennung) + ')', '', '');
            end;  { case }
          end
          else begin  // wir sind Zentrale
            { INI neu lesen, wenn geändert; 10.03.2015, WW }
            Get_IniTelegr (true);

            { Import-Thread starten, wenn noch nicht erfolgt: }
            if not Assigned (IecImportThread) and (isBde) then
              IecImportThread:=TIecImportThread.CreateIt (IecImportTelegrListe,
                InfoObj_Bytes, IECLogFile, WieserIniFileName, FImportDirList);
            { Datenimport-Termin, DebugRohdaten-Flag und Datenimport-Flag aktualisieren: }
            IecImportThread.ImportTermin:=IncSecond (Now, iDatenImport_VerzoegerungSek);
            IecImportThread.ZwFileLoeschen:=not bDebugRohdaten;
            IecImportThread.DatenImport:=bDatenImport;

            { Datentelegramm-Record belegen: }
            IecImportTelegrData.LinienNr:=-1;
            IecImportTelegrData.Telegramm:=sEmpfTelegramm;
            IecImportTelegrData.DatenEinheitIdent:=EmpfTelegramm.DatenEinheitIdent;
            IecImportTelegrData.DatenObjekte:=EmpfTelegramm.sObjekte;
            { ...und in Telegrammliste eintragen: }
            IecImportTelegrListe.SetTelegramm (IecImportTelegrData);
          end;
        end
        else begin  // falsche Empfangsfolgenummer im Telegramm der Zentrale
          if IECLogFile <> nil then
            IECLogFile.Write ('FEHLER I-Format: Empfangsfolgenummer größer Sendefolgezähler ' +
                              '(N(R) = ' + IntToStr (EmpfTelegramm.EmpfangsfolgeNr) + ', ' +
                              'V(S) = ' + IntToStr (SendefolgeZS_VS) + ')', '', '', lt_Error);
          // keine Aktion, Timeout t1 spricht an
          exit;
        end;
      end;

    sfmt_S:  { S-Format }
      begin
        if Quittiere_Telegramme then begin  // übertragene Datenbereiche updaten, Sende-Telegramme löschen
          FormPortIec104.StopTimer_t1;  // t1-Timeout-Überwachung stoppen
        end;
      end;

    sfmt_U:  { U-Format }
      begin
        case EmpfTelegramm.Funktion_U of
          C_FKT_STARTDT_ACT:
            begin
              if Funktion = fkt_iec870_Station then begin  // wir sind Unterstation
                if IECLogFile <> nil then
                  IECLogFile.Write ('STARTDT: act', '', '');
                Set_Telegramm_U (C_FKT_STARTDT_CON);  // Bestätigung STARTDT: con schicken
                Set_StartDT (true);  // Daten dürfen ab jetzt an die Zentrale gesendet werden
              end
              else begin  // wir sind Zentrale
                // Wenn STARTDT_act empfangen wird, ist die Gegenstelle offenbar
                // ebenfalls eine Zentrale. Eine Kommunikation 'Zentrale - Zentrale'
                // unterstützen wir nicht. Also: nix tun !
              end;
            end;

          C_FKT_STARTDT_CON:
            begin
              if Funktion = fkt_iec870_Station then begin  // wir sind Unterstation
                // Wir schicken kein STARTDT_act, da wir Server sind. Daher darf auch
                // kein STARTDT_con kommen. Wenn doch, wäre das ein Fehler der
                // Gegenstelle. Also: nix tun !
              end
              else begin  // wir sind Zentrale
                if IECLogFile <> nil then
                  IECLogFile.Write ('STARTDT: con', '', '');
                Set_StartDT (true);  // ab jetzt ist die Datenübertragung in der Gegenstelle aktiviert

                // wenn es Antwort auf gesendetes STARTDT: act ist:
                if SendTelegrammList.Count > 0 then begin
                  if TSendTelegrammObj (SendTelegrammList[0]).Daten.TelegrammTyp = tt_104_StartDt_act then begin
                    Set_SendTelegrammAngekommen;  { versendetes Telegramm aus Sende-Telegrammliste löschen }
                    FormPortIec104.StopTimer_t1; { t1-Timeoutüberwachung beenden }
                  end
                  else begin
                    if IECLogFile <> nil then
                      IECLogFile.Write ('FEHLER C_FKT_STARTDT_CON: TelegrammTyp <> STARTDT: act', '', '', lt_Error);
                  end;
                end
                else begin
                  if IECLogFile <> nil then
                    IECLogFile.Write ('FEHLER C_FKT_STARTDT_CON: SendTelegrammList.Count = 0', '', '', lt_Error);
                end;
              end;
            end;

          C_FKT_STOPDT_ACT:
            begin
              if Funktion = fkt_iec870_Station then begin  // wir sind Unterstation
                if IECLogFile <> nil then
                  IECLogFile.Write ('STOPDT: act', '', '');

                Set_StartDT (false);  // Daten dürfen ab jetzt nicht mehr an Zentrale gesendet werden
                Set_Telegramm_U (C_FKT_STOPDT_CON);  // Bestätigung STOPDT: con schicken

                // zuerst noch ausstehende Quittierung senden:
                if wZS > 0 then begin
                  Reset_wZS;  // w-Zählerstand zurücksetzen
                  FormPortIec104.StopTimer_t2;  // t2-Timeout-Überwachung stoppen
                  Set_Telegramm_S; // wird VOR 'STOPDT con'-Telegramm in Sendeliste gestellt

                  inc (Telegramm_senden);  // 2 Telegramme stehen zur Versendung bereit
                end;
              end
              else begin  // wir sind Zentrale
                // Wenn STOPDT_act empfangen wird, ist die Gegenstelle offenbar
                // ebenfalls eine Zentrale. Eine Kommunikation 'Zentrale - Zentrale'
                // unterstützen wir nicht. Also: nix tun !
              end;
            end;

          C_FKT_STOPDT_CON:
            begin
              if Funktion = fkt_iec870_Station then begin  // wir sind Unterstation
                // Wir schicken kein STOPDT_act, da wir Server sind. Daher darf auch
                // kein STOPDT_con kommen. Wenn doch, wäre das ein Fehler der
                // Gegenstelle. Also: nix tun !
              end
              else begin  // wir sind Zentrale
                if IECLogFile <> nil then
                  IECLogFile.Write ('STOPDT: con', '', '');
                Set_StartDT (false);  // ab jetzt ist die Datenübertragung in der Gegenstelle deaktiviert

                // wenn es Antwort auf gesendetes STOPDT: act ist:
                if SendTelegrammList.Count > 0 then begin
                  if TSendTelegrammObj (SendTelegrammList[0]).Daten.TelegrammTyp = tt_104_StopDt_act then begin
                    Set_SendTelegrammAngekommen;  { versendetes Telegramm aus Sende-Telegrammliste löschen }
                    FormPortIec104.StopTimer_t1; { t1-Timeoutüberwachung beenden }
                  end
                  else begin
                    if IECLogFile <> nil then
                      IECLogFile.Write ('FEHLER C_FKT_STOPDT_CON: TelegrammTyp <> STOPDT: act', '', '', lt_Error);
                  end;
                end
                else begin
                  if IECLogFile <> nil then
                    IECLogFile.Write ('FEHLER C_FKT_STOPDT_CON: SendTelegrammList.Count = 0', '', '', lt_Error);
                end;
              end;
            end;

          C_FKT_TESTFR_ACT:
            begin
              Set_Telegramm_U (C_FKT_TESTFR_CON);  // Bestätigung TESTFR: con schicken
            end;

          C_FKT_TESTFR_CON:
            begin
              // wenn es Antwort auf gesendetes TESTFR: act ist:
              if SendTelegrammList.Count > 0 then begin
                if TSendTelegrammObj (SendTelegrammList[0]).Daten.TelegrammTyp = tt_104_TestFr_act then begin
                  Set_SendTelegrammAngekommen;  { versendetes Telegramm aus Sende-Telegrammliste löschen }
                  FormPortIec104.StopTimer_t1; { t1-Timeoutüberwachung beenden }
                end
                else begin
                  if IECLogFile <> nil then
                    IECLogFile.Write ('FEHLER C_FKT_TESTFR_CON: TelegrammTyp <> TESTFR: act', '', '', lt_Error);
                end;
              end
              else begin
                if IECLogFile <> nil then
                  IECLogFile.Write ('FEHLER C_FKT_TESTFR_CON: SendTelegrammList.Count = 0', '', '', lt_Error);
              end;
            end;
        end;  { EmpfTelegramm.Funktion_U }
      end;
  end;  { case EmpfTelegramm.SteuerfeldFormat }
end;


{---------------------- Telegrammanzeige --------------------------------------}

{----------------------------------------------}
Procedure TFormTelegrIec104.Clear_TelegrAnzeige;
{----------------------------------------------}
{ Telegrammanzeige löschen }
begin
  EStart.Text:='';
  ELaenge.Text:='';
  ESteuerfeld1.Text:='';
  ESteuerfeld2.Text:='';
  ESteuerfeld3.Text:='';
  ESteuerfeld4.Text:='';
  ETypkennung.Text:='';

  EFormatPruefung.Text:='';
  eSendFolgeNr_dez.Text:='';
  eEmpfFolgeNr_dez.Text:='';
  EFunktion_txt.Text:='';
  ETypkennung_dez.Text:='';
end;

{-----------------------------------------------}
Procedure TFormTelegrIec104.Update_TelegrAnzeige;
{-----------------------------------------------}
{ Telegrammanzeige aktualisieren }
begin
  { APCI: }
  if length (sEmpfTelegramm) < 1 then exit;
  EStart.Text:='0x' + IntToHex(integer(sEmpfTelegramm[1]),2);

  if length (sEmpfTelegramm) < 2 then exit;
  ELaenge.Text:='0x' + IntToHex(integer(sEmpfTelegramm[2]),2);

  if length (sEmpfTelegramm) < 3 then exit;
  ESteuerfeld1.Text:='0x' + IntToHex(integer(sEmpfTelegramm[3]),2);

  if length (sEmpfTelegramm) < 4 then exit;
  ESteuerfeld2.Text:='0x' + IntToHex(integer(sEmpfTelegramm[4]),2);

  if length (sEmpfTelegramm) < 5 then exit;
  ESteuerfeld3.Text:='0x' + IntToHex(integer(sEmpfTelegramm[5]),2);

  if length (sEmpfTelegramm) < 6 then exit;
  ESteuerfeld4.Text:='0x' + IntToHex(integer(sEmpfTelegramm[6]),2);

  { ASDU: }
  if length (sEmpfTelegramm) > 6 then begin
    ETypkennung.Text:='0x' + IntToHex(integer(sEmpfTelegramm[7]),2);
    ETypkennung.Enabled:=true;
    ETypkennung.Color:=clWindow;
    lTypkennung.Enabled:=true;
    // Dezimal-Ausgabe Typkennung:
    ETypkennung_dez.Text:=IntToStr(integer(sEmpfTelegramm[7]));
    ETypkennung_dez.Enabled:=true;
    ETypkennung_dez.Color:=clWindow;
    lTypkennung_dez.Enabled:=true;
  end
  else begin
    ETypkennung.Enabled:=false;
    ETypkennung.Color:=clBtnFace;
    lTypkennung.Enabled:=false;
    ETypkennung_dez.Enabled:=false;
    ETypkennung_dez.Color:=clBtnFace;
    lTypkennung_dez.Enabled:=false;
  end;
end;

{-----------------------------------------------------------}
procedure TFormTelegrIec104.Update_SendeTelegrammListAnzeige;
{-----------------------------------------------------------}
{ Anzeige der Sendetelegrammlisten akualisieren }
begin
  eAnzSendTelegr.Text:=IntToStr (SendTelegrammList.Count);  // Anzahl der Sende-Telegramme in Liste
  eAnzUnquittTelegr.Text:=IntToStr (I_UnquittiertTelegrammList.Count);  // Anzahl der unquittierten I-Telegramme in Liste

  if Telegramm_senden > 0 then
    eAnzSendTelegr.Color:=clYellow  // zur Versendung bereit: gelb
  else
    eAnzSendTelegr.Color:=clWindow;
end;

{------------------------------------------------}
Procedure TFormTelegrIec104.Update_StartDTAnzeige;
{------------------------------------------------}
{ Anzeige für Datenübertragungsstatus STARTDT/STOPDT aktualisieren }
begin
  if isSTARTDT then begin
    eDatenuebertragung.Text:='STARTDT';
    eDatenuebertragung.Color:=clWindow;
  end
  else begin
    eDatenuebertragung.Text:='STOPDT';
    eDatenuebertragung.Color:=clYellow;
  end;
end;

{---------------------------------------------------------------}
procedure TFormTelegrIec104.Update_Timeout_t1Anzeige (S: string);
{---------------------------------------------------------------}
{ Ausgabe des t1-Timeouts;
  Übergabe: Anzeige-String }
begin
  eTimeout_t1.Text:=S;
  Application.ProcessMessages;
end;

{---------------------------------------------------------------}
procedure TFormTelegrIec104.Update_Timeout_t2Anzeige (S: string);
{---------------------------------------------------------------}
{ Ausgabe des t2-Timeouts;
  Übergabe: Anzeige-String }
begin
  eTimeout_t2.Text:=S;
  Application.ProcessMessages;
end;

{---------------------------------------------------------------}
procedure TFormTelegrIec104.Update_Timeout_t3Anzeige (S: string);
{---------------------------------------------------------------}
{ Ausgabe des t3-Timeouts;
  Übergabe: Anzeige-String }
begin
  eTimeout_t3.Text:=S;
  Application.ProcessMessages;
end;

{--------------------------------------------}
Procedure TFormTelegrIec104.Update_kZSAnzeige;
{--------------------------------------------}
{ Anzeige für k-Zählerstand (Resttelegramme) aktualisieren }
begin
  eTelegrZaehler_k.Text:=IntToStr (Anz_k - kZS);
end;

{--------------------------------------------}
Procedure TFormTelegrIec104.Update_wZSAnzeige;
{--------------------------------------------}
{ Anzeige für w-Zählerstand (Resttelegramme) aktualisieren }
begin
  eTelegrZaehler_w.Text:=IntToStr (Anz_w - wZS);
end;

{--------------------------------------------------------}
Procedure TFormTelegrIec104.Update_SendefolgeZS_VSAnzeige;
{--------------------------------------------------------}
{ Anzeige für Sendefolgezählerstand aktualisieren }
begin
  eSendFolgeZS.Text:=IntToStr (SendefolgeZS_VS);
end;

{-----------------------------------------------------------}
Procedure TFormTelegrIec104.Update_EmpfangsfolgeZS_VRAnzeige;
{-----------------------------------------------------------}
{ Anzeige für Empfangsfolgezählerstand aktualisieren }
begin
  eEmpfFolgeZS.Text:=IntToStr (EmpfangsfolgeZS_VR);
end;

{----------------------------------------------}
Procedure TFormTelegrIec104.Update_AckZSAnzeige;
{----------------------------------------------}
{ Anzeige für Ack-Zählerstand aktualisieren }
begin
  eAckZS.Text:=IntToStr (AckZS);
end;


{---------------------- Telegrammauswertung -----------------------------------}

{--------------------------------------------------------}
Function TFormTelegrIec104.Check_TelegrammFormat: integer;
{--------------------------------------------------------}
{ prüft Format des von der LW empfangenen Telegramms und setzt den Empfangstelegramm-Record }
var
  res: integer;
  L: integer;

begin
  if length (sEmpfTelegramm) < 6 then begin
    Result:=C_TGFORMAT_Err_TelegrLen;  { Fehler Telegrammlänge }
    exit;
  end;

  if integer(sEmpfTelegramm[1]) = C_START_VAR then begin  // 1. Zeichen Startzeichen
    L:=integer(sEmpfTelegramm[2]);  // 2. Zeichen Länge
    if length (sEmpfTelegramm) < (L+2) then begin
      Result:=C_TGFORMAT_Err_TelegrLen;  { Fehler Telegrammlänge }
      exit;
    end;
    res:=C_TGFORMAT_OK; { OK }
  end else
    res:=C_TGFORMAT_Err_StartChar; { Fehler Startzeichen }

  Result:=res;
end;

{------------------------------------------------------------------------}
function TFormTelegrIec104.Get_Funktion_U_Text (Funktion_U: byte): string;
{------------------------------------------------------------------------}
{ liefert Ergebnistext zu Funktion des U-Format-Steuerfelds }
var
  S: string;

begin
  case Funktion_U of
    C_FKT_STARTDT_ACT: S:='STARTDT: act';
    C_FKT_STARTDT_CON: S:='STARTDT: con';
    C_FKT_STOPDT_ACT:  S:='STOPDT: act';
    C_FKT_STOPDT_CON:  S:='STOPDT: con';
    C_FKT_TESTFR_ACT:  S:='TESTFR: act';
    C_FKT_TESTFR_CON:  S:='TESTFR: con';
  else
    S:='undefiniert';
  end;
  Result:=S
end;

{----------------------------------------------------------}
function TFormTelegrIec104.Set_EmpfTelegrammRecord: boolean;
{----------------------------------------------------------}
{ belegt Empfangstelegramm-Record mit Rohtelegramm-Inhalt }
var
  L: integer;
  S: string;

begin
  Result:=true;
  FillChar (EmpfTelegramm, SizeOf (EmpfTelegramm), 0);  // Vorbelegung: Empfangstelegramm-Record
  with EmpfTelegramm do begin
    if integer(sEmpfTelegramm[1]) = C_START_VAR then begin
      { Steuerfeldformat: I, S oder U }
      if (integer(sEmpfTelegramm[3]) AND $01) = 0 then begin  { I-Format }
        SteuerfeldFormat:=sfmt_I;
        { Sendefolgenummer: }
        SendefolgeNr:=((integer(sEmpfTelegramm[3]) AND $FE) +
                       (integer(sEmpfTelegramm[4]) * 256)) SHR 1;
        { Empfangsfolgenummer: }
        EmpfangsfolgeNr:=((integer(sEmpfTelegramm[5]) AND $FE) +
                          (integer(sEmpfTelegramm[6]) * 256)) SHR 1;
        with DatenEinheitIdent do begin
          { Typkennung }
          Typkennung:=integer(sEmpfTelegramm[7]);
          { Variable Strukturkennung: SQ-Bit }
          SQ_Bit:=(integer(sEmpfTelegramm[8]) AND $80) <> 0;
          { Variable Strukturkennung: Anzahl der Objekte }
          AnzahlObjekte:=integer(sEmpfTelegramm[8]) AND $7F;
          { Übertragungsursache (1 Oktett fest für Herkunftsadresse, wird nicht ausgewertet) }
          UebertrUrsache:=integer(sEmpfTelegramm[9]);
          { Stationsnummer: 2 Oktette für ASDU fest }
          Stationsnummer:=integer(sEmpfTelegramm[11]) +
                          (integer(sEmpfTelegramm[12]) * 256);
        end;
        { Informationsobjekte }
        L:=integer(sEmpfTelegramm[2]);  // Telegrammlänge
        S:=Copy (sEmpfTelegramm, 1, 2+L);  // gesamtes Telegramm
        sObjekte:=Copy (S, 13, length (S));  // ab Beginn Objekte

        { Dezimal-Anzeige Sende- und Empfangsfolgenummer }
        eSendFolgeNr_dez.Text:=IntToStr (SendefolgeNr);
        eSendFolgeNr_dez.Enabled:=true;
        eSendFolgeNr_dez.Color:=clWindow;
        lSendFolgeNr_dez.Enabled:=true;
        eEmpfFolgeNr_dez.Text:=IntToStr (EmpfangsfolgeNr);
        eEmpfFolgeNr_dez.Enabled:=true;
        eEmpfFolgeNr_dez.Color:=clWindow;
        lEmpfFolgeNr_dez.Enabled:=true;

        EFunktion_txt.Enabled:=false;
        EFunktion_txt.Color:=clBtnFace;
        lFunktion_txt.Enabled:=false;
      end
      else begin
        if (integer(sEmpfTelegramm[3]) AND $02) = 0 then begin  { S-Format }
          SteuerfeldFormat:=sfmt_S;
          { Empfangsfolgenummer: }
          EmpfangsfolgeNr:=((integer(sEmpfTelegramm[5]) AND $FE) +
                            (integer(sEmpfTelegramm[6]) * 256)) SHR 1;

          { Dezimal-Anzeige Empfangsfolgenummer }
          eEmpfFolgeNr_dez.Text:=IntToStr (EmpfangsfolgeNr);
          eEmpfFolgeNr_dez.Enabled:=true;
          eEmpfFolgeNr_dez.Color:=clWindow;
          lEmpfFolgeNr_dez.Enabled:=true;

          eSendFolgeNr_dez.Enabled:=false;
          eSendFolgeNr_dez.Color:=clBtnFace;
          lSendFolgeNr_dez.Enabled:=false;
          EFunktion_txt.Enabled:=false;
          EFunktion_txt.Color:=clBtnFace;
          lFunktion_txt.Enabled:=false;
        end
        else begin  { U-Format }
          SteuerfeldFormat:=sfmt_U;
          { Funktion: }
          case (integer(sEmpfTelegramm[3]) AND $FC) of
            C_FKT_STARTDT_ACT: Funktion_U:=C_FKT_STARTDT_ACT;
            C_FKT_STARTDT_CON: Funktion_U:=C_FKT_STARTDT_CON;
            C_FKT_STOPDT_ACT:  Funktion_U:=C_FKT_STOPDT_ACT;
            C_FKT_STOPDT_CON:  Funktion_U:=C_FKT_STOPDT_CON;
            C_FKT_TESTFR_ACT:  Funktion_U:=C_FKT_TESTFR_ACT;
            C_FKT_TESTFR_CON:  Funktion_U:=C_FKT_TESTFR_CON;
          end;

          { Text-Anzeige Funktion }
          EFunktion_txt.Text:=Get_Funktion_U_Text (Funktion_U);
          EFunktion_txt.Enabled:=true;
          EFunktion_txt.Color:=clWindow;
          lFunktion_txt.Enabled:=true;

          eSendFolgeNr_dez.Enabled:=false;
          eSendFolgeNr_dez.Color:=clBtnFace;
          lSendFolgeNr_dez.Enabled:=false;
          eEmpfFolgeNr_dez.Enabled:=false;
          eEmpfFolgeNr_dez.Color:=clBtnFace;
          lEmpfFolgeNr_dez.Enabled:=false;
        end;
      end;
    end else
      Result:=false;
  end;  { with EmpfTelegramm }
end;

{--------------------------------------------------}
Function TFormTelegrIec104.DecodeTelegramm: boolean;
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


{------------------------------------------------------------------------------}

{------------------------------------------}
Procedure TFormTelegrIec104.Set_Telegramm_S;
{------------------------------------------}
{ Telegramm mit Steuerfeld im S-Format bilden und in Sende-Telegrammliste ablegen }
const
  bDummy: boolean = false;

var
  S: string;
  GesendetDaten: TGesendetDaten;
  SendTelegrammRec: TSendTelegrammRec;

begin
  S:=Get_IEC870_104_Telegramm_Format_S (EmpfangsfolgeZS_VR);

  FillChar (GesendetDaten, SizeOf (GesendetDaten), 0);  // unbenutzt; GesendetDaten-Info nur bei Datentelegrammen

  { Sendetelegramm-Record zusammenstellen: }
  SendTelegrammRec.Telegramm:=S;
  SendTelegrammRec.FCV_101:=false;  // Dummy-Wert, nicht benutzt bei 104
  SendTelegrammRec.SendefolgeNr_104:=-1;  // nicht benutzt im S-Format
  SendTelegrammRec.TelegrammTyp:=tt_104_Quitt_Con;  // S-Format-Telegramm ist eine Quittung
  SendTelegrammRec.GesendetDaten:=GesendetDaten;
  SendTelegrammRec.LogHeader:='';  // ohne Log-Informationen
  SendTelegrammRec.LogDatensatzInfo:='';  // ohne Log-Informationen

  // Vorhandene, noch nicht versendete Daten-Telegramme löschen.
  // -> Der Empfangsfolgezählerstand hat sich geändert. Die noch nicht versendeten
  //    Daten-Telegramme der Liste tragen jedoch noch den alten Empfangsfolgezählerstand !
  // -> Telegramm mit Meldung für Redundanz-Betriebszustand nicht löschen ! Es
  //    steht alleine in der Sendetelegrammliste.
  SendTelegrammList.Delete_Telegramme_Daten;                
  SendTelegrammList.Insert_Telegramm (false, SendTelegrammRec);  // am Listenanfang eintragen

  Set_SendTelegrammBereit (bDummy);
end;

{--------------------------------------------------------------}
Procedure TFormTelegrIec104.Set_Telegramm_U (aFunktion_U: byte);
{--------------------------------------------------------------}
{ Telegramm mit Steuerfeld im U-Format bilden und in Sende-Telegrammliste ablegen;
  Übergabe: Funktionscode für Steuerfeld }
const
  bDummy: boolean = false;

var
  S: string;
  GesendetDaten: TGesendetDaten;
  SendTelegrammRec: TSendTelegrammRec;

begin
  S:=Get_IEC870_104_Telegramm_Format_U (aFunktion_U);

  FillChar (GesendetDaten, SizeOf (GesendetDaten), 0);  // unbenutzt; GesendetDaten-Info nur bei Datentelegrammen

  { Sendetelegramm-Record zusammenstellen: }
  SendTelegrammRec.Telegramm:=S;
  SendTelegrammRec.FCV_101:=false;  // Dummy-Wert, nicht benutzt bei 104
  SendTelegrammRec.SendefolgeNr_104:=-1;  // nicht benutzt im U-Format
  if Is_IEC870_104_con_Funktion_U (aFunktion_U) then
    SendTelegrammRec.TelegrammTyp:=tt_104_Quitt_Con
  else if aFunktion_U = C_FKT_STARTDT_ACT then
    SendTelegrammRec.TelegrammTyp:=tt_104_StartDt_act
  else if aFunktion_U = C_FKT_STOPDT_ACT then
    SendTelegrammRec.TelegrammTyp:=tt_104_StopDt_act
  else if aFunktion_U = C_FKT_TESTFR_ACT then
    SendTelegrammRec.TelegrammTyp:=tt_104_TestFr_act
  else
    SendTelegrammRec.TelegrammTyp:=tt_NoData;
  SendTelegrammRec.GesendetDaten:=GesendetDaten;
  SendTelegrammRec.LogHeader:='';  // ohne Log-Informationen
  SendTelegrammRec.LogDatensatzInfo:='';  // ohne Log-Informationen

  // vorhandene (Daten-)Telegramme nicht löschen
  SendTelegrammList.Insert_Telegramm (false, SendTelegrammRec);  // am Listenanfang eintragen

  Set_SendTelegrammBereit (bDummy);
end;

end.
