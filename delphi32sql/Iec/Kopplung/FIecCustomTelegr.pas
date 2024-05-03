{******************************************************************************}
{* Unit: Basis-Telegramm-Formular für IEC-Kopplung (32-Bit), IEC 870-5        *}
{* 18.06.2007  WW                                                             *}
{******************************************************************************}
unit FIecCustomTelegr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, Contnrs, WStream, WStrUtils, IecConst, FIecDaten, IecLog,
  WChars, IecIniFile, Iec870Util, IecImportThr, IecImportTelegrList,
  IecImportDirList, Novell, LogFile, O_IecRemoteMonitor;

type
  { Objekt für TLinienNrVerwList }
  TLinienNrVerwObj = class(TObject)
    LinienNr: integer;
    NeueDatenda: TNeueDatenda;
  public
    procedure SetData (ALinienNr: integer; ANeueDatenda: TNeueDatenda);
  end;

  { Liste zur Verwaltung der zu jeder angeforderten IEC-Liniennummer vorhandenen Daten }
  TLinienNrVerwList = class(TObjectList)
  private
    function FindEintrag (ALinienNr: integer): integer;
  public
    procedure GetFlag (ALinienNr: integer; ADatentyp: TDatentyp; var Value: boolean);
    function UpdateFlag (ALinienNr: integer; ADatentyp: TDatentyp;
      AValue: boolean): boolean;
    procedure UpdateFlags (ADatentyp: TDatentyp; AValue: boolean);
  end;

  { Record für TSendTelegrammObj }
  TSendTelegrammRec = record
    Telegramm: string;
    FCV_101: boolean;  // nur für 101, symm. Übertragung: true = Primärstation-Telegramm mit FCV=1
    SendefolgeNr_104: integer;  // nur für 104
    TelegrammTyp: TTelegrammTyp;
    GesendetDaten: TGesendetDaten;
    LogHeader: string;
    LogDatensatzInfo: string;
  end;

  { Objekt für TSendTelegrammList }
  TSendTelegrammObj = class(TObject)
    Daten: TSendTelegrammRec;
  public
    procedure SetData (ADaten: TSendTelegrammRec);
  end;

  { Liste der zu versendenden Telegramme }
  TSendTelegrammList = class(TObjectList)
  public
    procedure Insert_Telegramm (amEnde: boolean; Rec: TSendTelegrammRec);
    procedure Delete_AlleTelegramme;
    procedure Delete_Telegramme_Daten;
  end;

  { Kommunikationsstati }
  TIecCommStatus = (ics_Receive,          // Telegramm empfangen
                    ics_Timeout_EmpfSek,  // Timeout bei Sekundär-Telegramm-Empfang (101)
                    ics_Timeout_t2,       // Timeout t2 (104)
                    ics_Timeout_t3,       // Timeout t3 (104)
                    ics_Idle              // Ruhezustand: nichts senden, nichts empfangen
                   );

  { Funktionsstati }
  TIecFktStatus = (if_Inaktiv,           // Funktion inaktiv (es läuft keine Generalabfrage, Uhrzeit-Synchronisation etc.)
                   if_GA_AckActivate,    // Generalabfrage: Bestätigung der Aktivierung
                   if_GA_Daten_Start,    // Generalabfrage: Start GA-Daten senden
                   if_GA_Daten_Continue, // Generalabfrage: Weitere GA-Daten senden
                   if_UhrzeitSync        // Uhrzeit-Synchronisation
                 );

  { Telegramm-Formular }
  TFormCustomTelegrIec = class(TForm)
  private
    { Private-Deklarationen }
    FHeartbeatFile_Datatransfer : TFileName;  // Lebenszeichendatei für Datentransfer zur IEC-Gegenstelle
    FRemoteMonitorObj: TRemoteMonitorObj;
    FRemoteMonitorControlList: TList;
    function Get_MaxAnzahlObjekte (aIEC870_Norm: TIEC870_Norm; aTypKennung: byte): integer;
    procedure Set_TelegrammeDaten_Mess (aSTL: TSendTelegrammList; aIEC870_Norm: TIEC870_Norm;
      aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
      aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
      aLinienNr: integer; aUrsache: byte; aTypKennung: byte; aKonfigIEC: TKonfigIEC;
      aAufzMax_MW_normiert: double; aLogHeader: string; aMessDatenListe: TDatenListe;
      aAufzMin_MW_normiert: double = 0; bDeleteAtStart: boolean = True);
    procedure Set_TelegrammeDaten_Meld (aSTL: TSendTelegrammList; aIEC870_Norm: TIEC870_Norm;
      aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
      aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
      aLinienNr: integer; aUrsache: byte; aTypKennung: byte; aKonfigIEC: TKonfigIEC;
      aLogHeader: string; aMeldDatenListe: TDatenListe; bDeleteAtStart: boolean = True);
    procedure Fill_RemoteMonitorControlList;
  protected
    FNetProgDir: string;
    FImportDirList: TImportDirList;
    FIniDate: TDateTime;

    Funktion: TIEC870_Funktion;
    sEmpfTelegramm: string;  { von Leitwarte empfangenes IEC-Telegramm als Rohstring }
    IECLogFile: TIecLogFile;
    IecImportTelegrListe: TIecImportTelegrList;
    IecImportThread: TIecImportThread;
    bDatenImport: boolean;
    iDatenImport_VerzoegerungSek: integer;
    bDebugRohdaten: boolean;
    bTK_103_Aktiv: boolean;
    bSend_RedundanzTelegramm_Now: boolean;  // Flag zur Erkennung, daß ein Redundanz-
                                            // Betriebszustands-Telegramm jetzt gesendet werden soll
    Adressfeld_Bytes: byte;  { Anzahl Bytes im Adressfeld der Verbindungsschicht }
    ASDU_Bytes: byte;  { Anzahl Bytes in der gemeinsamen Adresse der ASDU }
    InfoObj_Bytes: byte; { Anzahl Bytes in der Adresse des Informationsobjekts }
    Herkunftsadresse: integer; { 2. Oktett in der Uebertragungsursache }
    Telegramm_MaxBytes: byte;  { max. Telegrammlänge }
    RedundanzStatusmeldung: boolean;  { true: mit Meldungstelegramm für Redundanz-Betriebszustand }
    RedundanzStatusmeldung_ASDU: integer;  { ASDU-Adresse für Redundanz-Betriebszustands-Meldung }
    RedundanzStatusmeldung_InfoObj: integer;  { Infoobjekt-Adresse für Redundanz-Betriebszustands-Meldung }

    FIecFktStatus: TIecFktStatus;  // Aktueller Funktionsstatus (Abwicklung einer Generalabfrage, Uhrzeit-Synchronisation etc.)
    FGA_StationsNrListe: TStringList;  // Liste mit Generalabfrage-Stationsnummern
    Procedure Set_SendTelegrammBereit (aPRM_101: boolean); virtual; abstract;

    function Get_TelegrammFormatCheck_Text (TelegrFmtErrCode: integer): string;
    procedure Update_DatenGesendet (aSendTelegrammList: TSendTelegrammList; aListIndex: integer);

    { nur für MRG-Daten: }
    function Set_Telegramme_Daten_MRG (aSTL: TSendTelegrammList; aIEC870_Norm: TIEC870_Norm;
      aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
      aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
      aLinienNr: integer; aUrsache: byte; bSperren: boolean): boolean;
    { nur für DSfG-Daten: }
    function Set_Telegramme_Daten_DSfG (aSTL: TSendTelegrammList; aIEC870_Norm: TIEC870_Norm;
      aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
      aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
      aLinienNr: integer; aUrsache: byte; bSperren: boolean): boolean;
    { nur für Kurzzeitwerte: }
    function Set_Telegramme_Daten_KZW (aSTL: TSendTelegrammList; aIEC870_Norm: TIEC870_Norm;
      aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
      aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
      aLinienNr: integer; aUrsache: byte; bLoeschen: boolean): boolean;
    { nur für Redundanzbetrieb: }
    function Set_Telegramm_Redundanz_AktivPassiv (aSTL: TSendTelegrammList;
      bRedundanzAktiv: boolean; aIEC870_Norm: TIEC870_Norm;
      aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
      aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
      aLinienNr: integer; aUrsache: byte): boolean;

    procedure Set_Telegramm_Generalabfrage (aSTL: TSendTelegrammList;
      aIEC870_Norm: TIEC870_Norm;
      aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
      aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
      aLinienNr: integer; aUrsache: byte; aStationsNrListe: TStrings);

    procedure Set_Telegramm_UhrzeitSynchronisation (aSTL: TSendTelegrammList;
      aIEC870_Norm: TIEC870_Norm;
      aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
      aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
      aLinienNr: integer; aUrsache: byte; aStationsNr: integer; aInfoObj: string);

    procedure FillGA_StationsNrListe_Station (aStationsNr: integer);
  public
    { Public-Deklarationen }
    LinienNrVerwList: TLinienNrVerwList;
    constructor Create (AOwner: TComponent; AFunktion: TIEC870_Funktion;
                        ANetProgDir: string;
                        AImportDirList: TImportDirList;
                        ARemoteMonitorObj: TRemoteMonitorObj); reintroduce; virtual;
    destructor Destroy; override;
    procedure CloseImportThread;
    procedure Set_IecLogfile (AIecLogfile: TIecLogFile);
    procedure Set_EmpfTelegramm (aEmpfTelegr: string);
    procedure Set_SendRedundanzTelegrammNow (OnOff: boolean);
    procedure SendToRemoteClient (sNorm: string);
    property HeartbeatFile_Datatransfer: TFileName read FHeartbeatFile_Datatransfer
      write FHeartbeatFile_Datatransfer;
  end;

var
  FormCustomTelegrIec: TFormCustomTelegrIec;

implementation

{$R *.DFM}


{ TLinienNrVerwObj }

{----------------------------------------------------------------------------------}
procedure TLinienNrVerwObj.SetData (ALinienNr: integer; ANeueDatenda: TNeueDatenda);
{----------------------------------------------------------------------------------}
begin
  LinienNr:=ALinienNr;
  NeueDatenda:=ANeueDatenda;
end;


{ TLinienNrVerwList }

{-------------------------------------------------------------------}
function TLinienNrVerwList.FindEintrag (ALinienNr: integer): integer;
{-------------------------------------------------------------------}
{ Eintrag für Liniennummer in Liste suchen;
  Übergabe: Liniennummer
  Ergebnis: Listenindex, wenn Eintrag gefunden (-1, wenn nicht gefunden) }
var
  i: integer;
begin
  Result:=-1;
  for i:=0 to Count - 1 do begin
    if TLinienNrVerwObj (Items [i]).LinienNr = ALinienNr then begin { Eintrag gefunden }
      Result:=i;
      Break;
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure TLinienNrVerwList.GetFlag (ALinienNr: integer; ADatentyp: TDatentyp;
  var Value: boolean);
{----------------------------------------------------------------------------}
{ liefert "NeueDatenda"-Flag für übergebene Liniennummer/Datentyp;
  Übergabe: Liniennummer
            Datentyp
  Rückgabe: "NeueDatenda"-Flag }
var
  i: integer;
  NDD: TNeueDatenda;
  NeueDatendaObj: TLinienNrVerwObj;
begin
  i:=FindEintrag (ALinienNr);
  if i < 0 then begin  { Eintrag nicht gefunden }
    { Daten-Telegrammversendung enablen/disablen: }
    with NDD do begin
      MRG_MW:=MRG_freigeschaltet;
      MRG_ME:=MRG_freigeschaltet;
      DSfG_AK:=DSfG_freigeschaltet;
      DSfG_LB:=DSfG_freigeschaltet;
      DSfG_KZW:=KZW_freigeschaltet;
    end;
    { neuer Listen-Eintrag mit Default-Werten: }
    NeueDatendaObj:=TLinienNrVerwObj.Create;
    NeueDatendaObj.SetData (ALinienNr, NDD);
    i:=Add (NeueDatendaObj);
  end;

  { Rückgabewert: }
  with TLinienNrVerwObj (Items [i]).NeueDatenda do begin
    case ADatentyp of
      dt_MRG_MW: Value:=MRG_MW;
      dt_MRG_ME: Value:=MRG_ME;
      dt_DSfG_AK: Value:=DSfG_AK;
      dt_DSfG_LB: Value:=DSfG_LB;
      dt_DSfG_KZW: Value:=DSfG_KZW;
    else  { unbekannter Datentyp }
      Value:=false;
    end;
  end;
end;

{------------------------------------------------------------------------------}
function TLinienNrVerwList.UpdateFlag (ALinienNr: integer; ADatentyp: TDatentyp;
  AValue: boolean): boolean;
{------------------------------------------------------------------------------}
{ setzt "NeueDatenda"-Flag für übergebene Liniennummer/Datentyp;
  Übergabe: Liniennummer
            Datentyp
            "NeueDatenda"-Flag }
var
  i: integer;
begin
  Result:=false;
  i:=FindEintrag (ALinienNr);
  if i >= 0 then begin  { Eintrag gefunden }
    { Wert für Datentyp im Listen-Eintrag updaten: }
    with TLinienNrVerwObj (Items [i]).NeueDatenda do begin
      case ADatentyp of
        dt_MRG_MW: MRG_MW:=AValue;
        dt_MRG_ME: MRG_ME:=AValue;
        dt_DSfG_AK: DSfG_AK:=AValue;
        dt_DSfG_LB: DSfG_LB:=AValue;
        dt_DSfG_KZW: DSfG_KZW:=AValue;
      else  { unbekannter Datentyp }
        exit;
      end;
    end;
    Result:=true;
  end;
end;

{------------------------------------------------------------------------------}
procedure TLinienNrVerwList.UpdateFlags (ADatentyp: TDatentyp; AValue: boolean);
{------------------------------------------------------------------------------}
{ setzt "NeueDatenda"-Flag für übergebenen Datentyp für alle Liniennummern;
  Übergabe: Datentyp
            "NeueDatenda"-Flag }
var
  i: integer;
begin
  for i:=0 to Count - 1 do begin
    { Wert für Datentyp im Listen-Eintrag updaten: }
    with TLinienNrVerwObj (Items [i]).NeueDatenda do begin
      case ADatentyp of
        dt_MRG_MW: MRG_MW:=AValue;
        dt_MRG_ME: MRG_ME:=AValue;
        dt_DSfG_AK: DSfG_AK:=AValue;
        dt_DSfG_LB: DSfG_LB:=AValue;
        dt_DSfG_KZW: DSfG_KZW:=AValue;
      else  { unbekannter Datentyp }
        exit;
      end;
    end;
  end;
end;


{ TSendTelegrammObj }

{--------------------------------------------------------------}
procedure TSendTelegrammObj.SetData (ADaten: TSendTelegrammRec);
{--------------------------------------------------------------}
begin
  Daten:=ADaten;
end;


{ TSendTelegrammList }

{--------------------------------------------------------------------------------------}
procedure TSendTelegrammList.Insert_Telegramm (amEnde: boolean; Rec: TSendTelegrammRec);
{--------------------------------------------------------------------------------------}
{ Telegramm in Sende-Telegrammliste eintragen;
  Übergaben: Flag 'amEnde': true = Telegramm wir am Listenende angehängt
                            false = Telegramm wir am Listenanfang eingetragen
             Sende-Telegramm-Record }
var
  SendTelegrammObj: TSendTelegrammObj;
begin
  SendTelegrammObj:=TSendTelegrammObj.Create;
  SendTelegrammObj.SetData (Rec);
  if amEnde then
    Add (SendTelegrammObj)  // am Ende der Sende-Telegrammliste anhängen
  else
    Insert (0, SendTelegrammObj);  // am Listenanfang eintragen
end;

{-------------------------------------------------}
procedure TSendTelegrammList.Delete_AlleTelegramme;
{-------------------------------------------------}
{ alle Einträge aus Sende-Telegrammliste löschen }
begin
  Clear;
end;

{---------------------------------------------------}
procedure TSendTelegrammList.Delete_Telegramme_Daten;
{---------------------------------------------------}
{ alle Daten-Telegramme aus Sende-Telegrammliste löschen }
var
  i: integer;
begin
  for i:=(Count - 1) downto 0 do
    if TSendTelegrammObj (Items [i]).Daten.TelegrammTyp in
       [tt_Data_MRG, tt_Data_DSfG, tt_Data_DSfG_KZW] then
      Delete (i);
end;


{ TFormCustomTelegrIec }

{-----------------------------------------------------------------------------}
constructor TFormCustomTelegrIec.Create (AOwner: TComponent;
                                         AFunktion: TIEC870_Funktion;
                                         ANetProgDir: string;
                                         AImportDirList: TImportDirList;
                                         ARemoteMonitorObj: TRemoteMonitorObj);
{-----------------------------------------------------------------------------}
begin
  inherited Create (AOwner);
  Funktion:=AFunktion;
  FNetProgDir:=ANetProgDir;
  FImportDirList:=AImportDirList;
  FRemoteMonitorObj:=ARemoteMonitorObj;

  { Defaults: }
  FIniDate:=-1;  // Default: Zeitstempel der INI-Datei unbekannt
  IECLogFile:=nil;
  FHeartbeatFile_Datatransfer := '';  // Kein Heartbeatfile

  bDatenImport:=false;
  iDatenImport_VerzoegerungSek:=0;
  bDebugRohdaten:=false;
  bTK_103_Aktiv:=false;

  IecImportThread:=nil;
  IecImportTelegrListe:=TIecImportTelegrList.Create;
  IecImportTelegrListe.Duplicates:=dupAccept;  // mehrfache, gleiche Einträge erlaubt

  FRemoteMonitorControlList:=TList.Create;
  Fill_RemoteMonitorControlList;  // 10.03.2015, WW

  Set_SendRedundanzTelegrammNow (false);

  Adressfeld_Bytes:=0;
  ASDU_Bytes:=0;
  InfoObj_Bytes:=0;
  Herkunftsadresse:=-1;
  Telegramm_MaxBytes:=0;
  RedundanzStatusmeldung:=false;
  RedundanzStatusmeldung_ASDU:=0;
  RedundanzStatusmeldung_InfoObj:=0;

  LinienNrVerwList:=TLinienNrVerwList.Create;
  LinienNrVerwList.Clear;  { Liste zurücksetzen }

  FIecFktStatus:=if_Inaktiv;  // Funktion inaktiv (keine Generalabfrage, Uhrzeit-Synchronisation etc.)
  FGA_StationsNrListe:=TStringList.Create;
  FGA_StationsNrListe.Sorted:=true;  // Voraussetzung für dupIgnore
  FGA_StationsNrListe.Duplicates:=dupIgnore;  // keine Duplikate

  { vorhandenen Datenbereich einlesen (alle Datentypen) nur bei Funktion 'Station': }
  if Funktion = fkt_iec870_Station then begin
    FormDatenIec.F_Unlock;  // Sperren in IEC-Datentabellen aufheben
    FormDatenIec.F_Update (true, true, true, true);  // MRG/DSfG-Archivdaten
    if KZW_freigeschaltet AND ((FormDatenIec.DSfGArchivAuswahl = daa_Archiv_KZW) OR
                               (FormDatenIec.DSfGArchivAuswahl = daa_nur_KZW)) then
      FormDatenIec.KZWListe.LadeKZWFiles;  // DSfG-Kurzzeitwerte
  end;
end;

{--------------------------------------}
destructor TFormCustomTelegrIec.Destroy;
{--------------------------------------}
begin
  FGA_StationsNrListe.Free;
  LinienNrVerwList.Free;
  FRemoteMonitorControlList.Free;
  IecImportTelegrListe.Free;
  inherited Destroy;
end;

{-----------------------------------------------}
procedure TFormCustomTelegrIec.CloseImportThread;
{-----------------------------------------------}
{ IEC-Importthread beenden }
begin
  if Assigned (IecImportThread) then begin
    try
      IecImportThread.Resume;    // Thread fortsetzen, falls er unterbrochen ist (sonst reagiert er nicht)
      IecImportThread.Terminate; // Thread-Beenden einleiten
      IecImportThread.WaitFor;   // warten bis Thread beendet ist
    except
      // Ungültiges Handle unterdrücken
    end;
  end;
end;

{-----------------------------------------------------------------------}
procedure TFormCustomTelegrIec.Set_IecLogfile (AIecLogfile: TIecLogFile);
{-----------------------------------------------------------------------}
begin
  IECLogFile:=AIecLogfile;
end;

{---------------------------------------------------------------------}
Procedure TFormCustomTelegrIec.Set_EmpfTelegramm (aEmpfTelegr: string);
{---------------------------------------------------------------------}
{ Empfangstelegramm-Variable mit Übergabe (Roh-String) belegen }
begin
  sEmpfTelegramm:=aEmpfTelegr;
(*{$IFDEF Redundanz}
  { im StandBy-Betrieb wird mit dem ersten Telegramm im Zeitfenster die Zeitüberwachung neu gestartet: }
  if StandbyBetrieb AND (TG_Counter_Standby = 0) then
    Sec_Counter_Standby:=0;
  inc (TG_Counter_Standby);    { Telegramme mitzählen für Normal-/Standbybetrieb-Überwachung }
{$ENDIF}*)
end;

{----------------------------------------------------------------------------}
procedure TFormCustomTelegrIec.Set_SendRedundanzTelegrammNow (OnOff: boolean);
{----------------------------------------------------------------------------}
{ Flag zur Erkennung, daß ein Redundanz-Betriebszustands-Telegramm gesendet
  werden soll, setzen/rücksetzen }
begin
  // nur wenn Programm im Redundanzmodus läuft und Redundanz-Statusmeldung
  // konfiguriert ist:
  if (Redundanzbetrieb <> C_Redundanz_Aus) AND RedundanzStatusmeldung then
    bSend_RedundanzTelegramm_Now:=OnOff
  else
    bSend_RedundanzTelegramm_Now:=false;  // nie senden
end;


{---------------------- Telegrammauswertung -----------------------------------}

{----------------------------------------------------------------------------------------------}
function TFormCustomTelegrIec.Get_TelegrammFormatCheck_Text (TelegrFmtErrCode: integer): string;
{----------------------------------------------------------------------------------------------}
{ liefert Ergebnistext zu Telegrammformat-Fehlercode }
var
  S: string;

begin
  case TelegrFmtErrCode of
    C_TGFORMAT_OK:            S:='OK';
    C_TGFORMAT_Err_StartChar: S:='Startzeichen';
    C_TGFORMAT_Err_StopChar:  S:='Stopzeichen';
    C_TGFORMAT_Err_Checksum:  S:='Prüfsumme';
    C_TGFORMAT_Err_LenChar:   S:='Längenzeichen';
    C_TGFORMAT_Err_TelegrLen: S:='Telegr.länge';
  else
    S:='undef. Fehler';
  end;

  if TelegrFmtErrCode = C_TGFORMAT_OK then
    Result:=S
  else
    Result:=S + ' (' + IntToStr (TelegrFmtErrCode) + ')';  // mit Fehlercode
end;


{------------------------- Gesendete Daten ------------------------------------}

{------------------------------------------------------------------------------------------}
procedure TFormCustomTelegrIec.Update_DatenGesendet (aSendTelegrammList: TSendTelegrammList;
                                                     aListIndex: integer);
{------------------------------------------------------------------------------------------}
{ erfolgreich übertragene Daten in der MRG-/DSfG-Iec-Datentabelle vermerken bzw.
  Kurzzeitwert-Datei löschen (Sende-Telegramm mit übergebenem Listenindex) }
var
  TelegrammTyp: TTelegrammTyp;
  GesendetDaten: TGesendetDaten;
  bOK : boolean;
begin
  if aSendTelegrammList.Count > aListIndex then begin
    TelegrammTyp:=TSendTelegrammObj (aSendTelegrammList[aListIndex]).Daten.TelegrammTyp;
    GesendetDaten:=TSendTelegrammObj (aSendTelegrammList[aListIndex]).Daten.GesendetDaten;
    with GesendetDaten do begin
      { nach Datentelegramm-Versendung 'UpdateGesendetbis' durchführen: }
      if TelegrammTyp = tt_Data_DSfG_KZW then
        FormDatenIec.KZWListe.DeleteKZWFile (DSfG_KZW_Index)  // übertragenen Kurzzeitwert löschen (Liste und Dateien)
      else if TelegrammTyp = tt_Data_DSfG then
        FormDatenIec.UpdateGesendetBis_DSfG (DSfG.StationId, DSfG.InstanzId,
          DSfG.Arch_LogbNr, DSfG.Kanal_MeldNr, BisRefNr, BisDZ, BisOrdNr,
          Datentyp)
      else if TelegrammTyp = tt_Data_MRG then
        FormDatenIec.UpdateGesendetBis_MRG (MRG.Kennung, MRG.Kanal_MeldNr,
          BisOrdNr, BisDZ, Datentyp);

      // Ggf. Heartbeatfile für aktiven Datentransfer schreiben:
      if (FHeartbeatFile_Datatransfer <> '') AND
         (TelegrammTyp in [tt_Data_MRG, tt_Data_DSfG, tt_Data_DSfG_KZW]) then  // 05.03.2015, WW
        with TFileStreamExt.Create(FHeartbeatFile_Datatransfer, fmCreate, bOK) do Free;
    end;
  end;
end;

{--------------- Datentelegramme mit Messdaten und Meldungen ------------------}

{---------------------------------------------------------------------------------}
procedure TFormCustomTelegrIec.Set_TelegrammeDaten_Mess (aSTL: TSendTelegrammList;
  aIEC870_Norm: TIEC870_Norm;
  aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
  aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
  aLinienNr: integer; aUrsache: byte; aTypKennung: byte;
  aKonfigIEC: TKonfigIEC; aAufzMax_MW_normiert: double; aLogHeader: string;
  aMessDatenListe: TDatenListe; aAufzMin_MW_normiert: double = 0; bDeleteAtStart: boolean = True);
{---------------------------------------------------------------------------------}
{ IEC-Datentelegramme mit Messdaten zusammenstellen }
var
  DF: string;
  i: integer;
  DatenfeldZaehler: byte;
  sTelegramm: string;
  sLogDatensatzInfo: string;
  dWert: double;
  Wert_gueltig: boolean;
  DatumZeit: TDateTime;
  Ordnungsnummer: integer;
  Referenznummer: integer;
  LogInfo: string;
  LetztOrdNr: integer;
  LetztDZ: TDateTime;
  LetztRefNr: integer;
  GesendetDaten: TGesendetDaten;
  SendTelegrammRec: TSendTelegrammRec;
  MaxDatenfelder: integer;
  bFCB_Buf: boolean;
  bFCV_Buf: boolean;
  iCause : integer;
begin
  // evtl. noch vorhandene (Daten-)Telegramme löschen
  if (bDeleteAtStart) then aSTL.Delete_AlleTelegramme;

  { ermitteln, wieviele Datenfelder (Objekte) max. in ein Telegramm passen: }
  MaxDatenfelder:=Get_MaxAnzahlObjekte (aIEC870_Norm, aTypKennung);  // 22.02.2007, WW
  if MaxDatenfelder <= 0 then begin
    if IECLogFile <> nil then
      IECLogFile.Write ('FEHLER: Telegrammlänge zu klein !', '', '', lt_Error);
  end
  else begin
    { Vorbelegungen für Datenfeld: }
    DF:='';
    DatenfeldZaehler:=0;
    { Vorbelegung für Datensatz-Log: }
    sLogDatensatzInfo:='';
    { Vorbelegung für FCB (bei 101-Primärstation-Telegramm): }
    bFCB_Buf:=aSendFCB_letzt_101;

    { Datenfelder aus Messdaten-Liste zusammensetzen: }
    for i:=0 to aMessDatenListe.Count - 1 do begin
      Application.ProcessMessages;
      if (not (TObject(aMessDatenListe[i]) is TMessDatenObj)) then Continue;

      dWert:=TMessDatenObj (aMessDatenListe[i]).Daten.Wert;
      Wert_gueltig:=TMessDatenObj (aMessDatenListe[i]).Daten.Wert_gueltig;
      DatumZeit:=TMessDatenObj (aMessDatenListe[i]).Daten.DatumZeit;
      Ordnungsnummer:=TMessDatenObj (aMessDatenListe[i]).Daten.Ordnungsnummer;
      Referenznummer:=TMessDatenObj (aMessDatenListe[i]).Daten.Referenznummer;
      LogInfo:=TMessDatenObj (aMessDatenListe[i]).Daten.LogInfo;
      iCause := TMessDatenObj (aMessDatenListe[i]).TransferCause;
      if (iCause = 0) then iCause := aUrsache;

      inc (DatenfeldZaehler);
      { Informationsobjekt zu Messwert bilden: }
      DF:=DF + Get_IEC870_Informationsobjekt_Mess (aTypKennung,
        aKonfigIEC.InfoObj_low, aKonfigIEC.InfoObj_medium, aKonfigIEC.InfoObj_high,
        InfoObj_Bytes, dWert, Wert_gueltig, DatumZeit, aAufzMax_MW_normiert,
        aAufzMin_MW_normiert);

      sLogDatensatzInfo:=sLogDatensatzInfo + LogInfo + CR + LF;

      LetztDZ:=DatumZeit;
      LetztOrdNr:=Ordnungsnummer;
      LetztRefNr:=Referenznummer;

      { Anzahl der Datenfelder prüfen, damit maximale Telegrammlänge nicht
        überschritten wird: }
      if (DatenfeldZaehler >= MaxDatenfelder) OR
         (i = (aMessDatenListe.Count - 1)) then begin
        case aIEC870_Norm of
          norm_iec870_101:  { 101-Daten-Telegramm mit Messdaten zusammensetzen }
            begin
              if aPRM_101 then begin // Telegramm der Primärstation
                bFCB_Buf:=not bFCB_Buf;  // FCB toggeln
                bFCV_Buf:=Get_IEC870_101_FCV (aFktCode_101);  // Funktionscode-abhängiges FCV ermitteln

                sTelegramm:=Get_IEC870_101_TelegrammDaten_Symmetrisch_Primaer (aDIR_symm_101,
                  bFCB_Buf, bFCV_Buf, aFktCode_101, aLinienNr, iCause, Herkunftsadresse,
                  aTypKennung, aKonfigIEC.StationsNr, DF, DatenfeldZaehler,
                  Adressfeld_Bytes, ASDU_Bytes);  // invertiertes DIR
              end
              else begin // Telegramm der Sekundärstation
                bFCV_Buf:=false;  // FCV in Telegramm der Sekundärstation nicht enthalten

                sTelegramm:=Get_IEC870_101_TelegrammDaten_Unsymmetrisch_Sekundaer (C_ACD_0, C_DFC_0,
                  aFktCode_101, aLinienNr, iCause, Herkunftsadresse,
                  aTypKennung, aKonfigIEC.StationsNr, DF, DatenfeldZaehler,
                  Adressfeld_Bytes, ASDU_Bytes);  // ACD=0, DFC=0
              end;
            end;

          norm_iec870_104:  { 104-Daten-Telegramm mit Messdaten zusammensetzen }
            begin
              bFCV_Buf:=false;  // nicht benutzt bei 104

              sTelegramm:=Get_IEC870_104_TelegrammDaten_I (aSendeFolgeNr_104, aEmpfFolgeNr_104,
                iCause, Herkunftsadresse, aTypKennung, aKonfigIEC.StationsNr,
                DF, DatenfeldZaehler, Adressfeld_Bytes, ASDU_Bytes);
            end;
        else
          bFCV_Buf:=false;  // nicht benutzt
          sTelegramm:='';
        end;  // case

        { in Sende-Telegrammliste eintragen: }
        FillChar (GesendetDaten, SizeOf (GesendetDaten), 0);  // alle nicht benutzten Felder mit Null belegen
        with GesendetDaten do begin
          BisDZ:=LetztDZ;
          BisOrdNr:=LetztOrdNr;
          BisRefNr:=LetztRefNr;
          Datentyp:='S'; { Kennzeichen, daß Stundenwert-Zeitpunkt geupdatet werden soll }
          { -> MRG/DSfG-spezifische Informationen für Messwertdaten nach dem Aufruf
               der Prozedur in den Sendetelegramm-Objekten setzen !}
        end;

        { Sendetelegramm-Record zusammenstellen: }
        SendTelegrammRec.Telegramm:=sTelegramm;
        SendTelegrammRec.FCV_101:=bFCV_Buf;
        SendTelegrammRec.SendefolgeNr_104:=aSendeFolgeNr_104;
        SendTelegrammRec.GesendetDaten:=GesendetDaten;
        SendTelegrammRec.LogHeader:=aLogHeader;
        SendTelegrammRec.LogDatensatzInfo:=sLogDatensatzInfo;
        aSTL.Insert_Telegramm (true, SendTelegrammRec);  // am Listenende anhängen

        { Vorbelegungen für Datenfeld des nächsten Telegramms: }
        DF:='';
        DatenfeldZaehler:=0;
        { Vorbelegung für Datensatz-Log des nächsten Telegramms: }
        sLogDatensatzInfo:='';

        { für nächstes 104-Telegramm: }
        if aIEC870_Norm = norm_iec870_104 then
          Inc_IEC870_104_Telegrammfolgenummer (aSendeFolgeNr_104);  // lokal hochzählen für Telegramme in Sendeliste
      end;
    end;  { for i }
  end;
end;

{--------------------------------------------------------------------------------}
procedure TFormCustomTelegrIec.Set_TelegrammeDaten_Meld (aSTL: TSendTelegrammList;
  aIEC870_Norm: TIEC870_Norm;
  aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
  aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
  aLinienNr: integer; aUrsache: byte; aTypKennung: byte;
  aKonfigIEC: TKonfigIEC; aLogHeader: string; aMeldDatenListe: TDatenListe;
  bDeleteAtStart: boolean = True);
{--------------------------------------------------------------------------------}
{ IEC-Datentelegramme mit Meldungen zusammenstellen }
var
  DF: string;
  i: integer;
  DatenfeldZaehler: byte;
  sTelegramm: string;
  sLogDatensatzInfo: string;
  Meldung_ein: boolean;
  DatumZeit: TDateTime;
  LogInfo: string;
  LetztDZ: TDateTime;
  GesendetDaten: TGesendetDaten;
  SendTelegrammRec: TSendTelegrammRec;
  MaxDatenfelder: integer;
  bFCB_Buf: boolean;
  bFCV_Buf: boolean;
  iCause : integer;
begin
  // evtl. noch vorhandene (Daten-)Telegramme löschen
  if (bDeleteAtStart) then aSTL.Delete_AlleTelegramme;

  { ermitteln, wieviele Datenfelder (Objekte) max. in ein Telegramm passen: }
  MaxDatenfelder:=Get_MaxAnzahlObjekte (aIEC870_Norm, aTypKennung);  // 22.02.2007, WW
  if MaxDatenfelder <= 0 then begin
    if IECLogFile <> nil then
      IECLogFile.Write ('FEHLER: Telegrammlänge zu klein !', '', '', lt_Error);
  end
  else begin
    { Vorbelegungen für Datenfeld: }
    DF:='';
    DatenfeldZaehler:=0;
    { Vorbelegung für Datensatz-Log: }
    sLogDatensatzInfo:='';
    { Vorbelegung für FCB (bei 101-Primärstation-Telegramm): }
    bFCB_Buf:=aSendFCB_letzt_101;

    { Datenfelder aus Messdaten-Liste zusammensetzen: }
    for i:=0 to aMeldDatenListe.Count - 1 do begin
      Application.ProcessMessages;
      if (not (TObject(aMeldDatenListe[i]) is TMeldDatenObj)) then Continue;

      Meldung_ein:=TMeldDatenObj (aMeldDatenListe[i]).Daten.Ein;
      DatumZeit:=TMeldDatenObj (aMeldDatenListe[i]).Daten.DatumZeit;
      LogInfo:=TMeldDatenObj (aMeldDatenListe[i]).Daten.LogInfo;
      iCause := TMeldDatenObj (aMeldDatenListe[i]).TransferCause;
      if (iCause = 0) then iCause := aUrsache;

      inc (DatenfeldZaehler);
      { Informationsobjekt zu Meldung bilden: }
      DF:=DF + Get_IEC870_Informationsobjekt_Meld (aTypKennung,
        aKonfigIEC.InfoObj_low, aKonfigIEC.InfoObj_medium, aKonfigIEC.InfoObj_high, InfoObj_Bytes,
        Meldung_ein, DatumZeit);

      sLogDatensatzInfo:=sLogDatensatzInfo + LogInfo + CR + LF;

      LetztDZ:=DatumZeit;

      { Anzahl der Datenfelder prüfen, damit maximale Telegrammlänge nicht
        überschritten wird: }
      if (DatenfeldZaehler >= MaxDatenfelder) OR
         (i = (aMeldDatenListe.Count - 1)) then begin
        case aIEC870_Norm of
          norm_iec870_101:  { 101-Daten-Telegramm mit Meldungen zusammensetzen }
            begin
              if aPRM_101 then begin // Telegramm der Primärstation
                bFCB_Buf:=not bFCB_Buf;  // FCB toggeln
                bFCV_Buf:=Get_IEC870_101_FCV (aFktCode_101);  // Funktionscode-abhängiges FCV ermitteln

                sTelegramm:=Get_IEC870_101_TelegrammDaten_Symmetrisch_Primaer (aDIR_symm_101,
                  bFCB_Buf, bFCV_Buf, aFktCode_101, aLinienNr, iCause, Herkunftsadresse,
                  aTypKennung, aKonfigIEC.StationsNr, DF, DatenfeldZaehler,
                  Adressfeld_Bytes, ASDU_Bytes)  // invertiertes DIR
              end
              else begin // Telegramm der Sekundärstation
                bFCV_Buf:=false;  // FCV in Telegramm der Sekundärstation nicht enthalten

                sTelegramm:=Get_IEC870_101_TelegrammDaten_Unsymmetrisch_Sekundaer (C_ACD_0, C_DFC_0,
                  aFktCode_101, aLinienNr, iCause, Herkunftsadresse,
                  aTypKennung, aKonfigIEC.StationsNr, DF, DatenfeldZaehler,
                  Adressfeld_Bytes, ASDU_Bytes);  // ACD=0, DFC=0
              end;
            end;

          norm_iec870_104:  { 104-Daten-Telegramm mit Meldungen zusammensetzen }
            begin
              bFCV_Buf:=false;  // nicht benutzt bei 104

              sTelegramm:=Get_IEC870_104_TelegrammDaten_I (aSendeFolgeNr_104, aEmpfFolgeNr_104,
                iCause, Herkunftsadresse, aTypKennung, aKonfigIEC.StationsNr,
                DF, DatenfeldZaehler, Adressfeld_Bytes, ASDU_Bytes);
            end;
        else
          bFCV_Buf:=false;  // nicht benutzt
          sTelegramm:='';
        end;  // case

        { in Sende-Telegrammliste eintragen: }
        FillChar (GesendetDaten, SizeOf (GesendetDaten), 0);  // alle nicht benutzten Felder mit Null belegen
        with GesendetDaten do begin
          BisDZ:=LetztDZ;
          Datentyp:='M';  { Kennzeichen, daß Meldungs-Zeitpunkt geupdatet werden soll }
          { -> MRG/DSfG-spezifische Informationen für Meldungsdaten nach dem Aufruf
               der Prozedur in den Sendetelegramm-Objekten setzen !}
        end;

        { Sendetelegramm-Record zusammenstellen: }
        SendTelegrammRec.Telegramm:=sTelegramm;
        SendTelegrammRec.FCV_101:=bFCV_Buf;
        SendTelegrammRec.SendefolgeNr_104:=aSendeFolgeNr_104;
        SendTelegrammRec.GesendetDaten:=GesendetDaten;
        SendTelegrammRec.LogHeader:=aLogHeader;
        SendTelegrammRec.LogDatensatzInfo:=sLogDatensatzInfo;
        aSTL.Insert_Telegramm (true, SendTelegrammRec);  // am Listenende anhängen

        { Vorbelegungen für Datenfeld des nächsten Telegramms: }
        DF:='';
        DatenfeldZaehler:=0;
        { Vorbelegung für Datensatz-Log des nächsten Telegramms: }
        sLogDatensatzInfo:='';

        { für nächstes 104-Telegramm: }
        if aIEC870_Norm = norm_iec870_104 then
          Inc_IEC870_104_Telegrammfolgenummer (aSendeFolgeNr_104);  // lokal hochzählen für Telegramme in Sendeliste
      end;
    end;  { for i }
  end;
end;

{-------------------------------------------------------------------------------}
function TFormCustomTelegrIec.Set_Telegramme_Daten_MRG (aSTL: TSendTelegrammList;
  aIEC870_Norm: TIEC870_Norm;
  aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
  aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
  aLinienNr: integer; aUrsache: byte; bSperren: boolean): boolean;
{-------------------------------------------------------------------------------}
{ IEC-Telegramme mit MRG-Daten (Messwerte, Meldungen) zusammenstellen und in
  Sende-Telegrammliste stellen }
var
  TypKennung: byte;
  KonfigMRG_Mess: TKonfigMRG_Mess;
  KonfigMRG_Meld: TKonfigMRG_Meld;
  AufzMax: integer;
  LogHeader: string;
  DatenListe: TDatenListe;
  bFlag: boolean;
  i: integer;

begin
  Result:=false;
  if not MRG_freigeschaltet then exit;

  DatenListe:=TDatenListe.Create;
  try
    {--- Messwerte ---}
    while true do begin
      LinienNrVerwList.GetFlag (aLinienNr, dt_MRG_MW, bFlag);
      if bFlag then begin
        { Liste mit neuen, noch nicht übertragenen MRG-Messwerten füllen: }
        if FormDatenIec.GetNeueDatenMRG_Mess (aLinienNr, InfoObj_Bytes,
          TypKennung, KonfigMRG_Mess, AufzMax, LogHeader, DatenListe, bSperren) then begin

          { Daten-Telegramme mit MRG-Messwerten bilden und in Sende-Telegrammliste stellen: }
          Set_TelegrammeDaten_Mess (aSTL, aIEC870_Norm,
            aPRM_101, aDIR_symm_101, aSendFCB_letzt_101, aFktCode_101,
            aSendeFolgeNr_104, aEmpfFolgeNr_104,
            aLinienNr, aUrsache, TypKennung, KonfigMRG_Mess.IEC, AufzMax, LogHeader, DatenListe);

          { MRG-spezifische Messwert-Informationen in Sende-Telegrammliste setzen: }
          for i:=0 to aSTL.Count - 1 do begin
            with TSendTelegrammObj(aSTL[i]).Daten do begin
              TelegrammTyp:=tt_Data_MRG;
              GesendetDaten.MRG.Kennung:=KonfigMRG_Mess.Kennung;
              GesendetDaten.MRG.Kanal_MeldNr:=KonfigMRG_Mess.KanalNr;
            end;
          end;

          { Daten-Telegramm ist sendebereit: }
          Set_SendTelegrammBereit (aPRM_101);
          Result:=true;
          exit;
        end
        else begin  // keine neuen MRG-Messwerte vorhanden
          bFlag:=false;
          LinienNrVerwList.UpdateFlag (aLinienNr, dt_MRG_MW, bFlag);
        end;
      end;

      if not bFlag AND isMsg_NAD_MRG_MW then begin
        isMsg_NAD_MRG_MW:=false;  // Wieser-Message-Flag zurücksetzen
        if FormDatenIec.F_Update_MRG_MW then  // wenn neue MRG-Messwerte vorhanden sind
          LinienNrVerwList.UpdateFlags (dt_MRG_MW, true)
        else
          Break;
      end else
        Break;
    end;  // while true

    {--- Meldungen ---}
    while true do begin
      LinienNrVerwList.GetFlag (aLinienNr, dt_MRG_ME, bFlag);
      if bFlag then begin
        { Liste mit neuen, noch nicht übertragenen MRG-Meldungen füllen: }
        if FormDatenIec.GetNeueDatenMRG_Meld (aLinienNr, InfoObj_Bytes,
          TypKennung, KonfigMRG_Meld, LogHeader, DatenListe, bSperren) then begin

          { Daten-Telegramm mit MRG-Meldungen bilden: }
          Set_TelegrammeDaten_Meld (aSTL, aIEC870_Norm,
            aPRM_101, aDIR_symm_101, aSendFCB_letzt_101, aFktCode_101,
            aSendeFolgeNr_104, aEmpfFolgeNr_104,
            aLinienNr, aUrsache, TypKennung, KonfigMRG_Meld.IEC, LogHeader, DatenListe);

          { MRG-spezifische Meldungs-Informationen in Sende-Telegrammliste setzen: }
          for i:=0 to aSTL.Count - 1 do begin
            with TSendTelegrammObj(aSTL[i]).Daten do begin
              TelegrammTyp:=tt_Data_MRG;
              GesendetDaten.MRG.Kennung:=KonfigMRG_Meld.Kennung;
              GesendetDaten.MRG.Kanal_MeldNr:=KonfigMRG_Meld.MeldNr;
            end;
          end;

          { Daten-Telegramm ist sendebereit: }
          Set_SendTelegrammBereit (aPRM_101);
          Result:=true;
          exit;
        end
        else begin  // keine neuen MRG-Meldungen vorhanden
          bFlag:=false;
          LinienNrVerwList.UpdateFlag (aLinienNr, dt_MRG_ME, bFlag);
        end;
      end;

      if not bFlag AND isMsg_NAD_MRG_ME then begin
        isMsg_NAD_MRG_ME:=false;  // Wieser-Message-Flag zurücksetzen
        if FormDatenIec.F_Update_MRG_ME then  // wenn neue MRG-Meldungen vorhanden sind
          LinienNrVerwList.UpdateFlags (dt_MRG_ME, true)
        else
          Break;
      end else
        Break;
    end;  // while true
  finally
    DatenListe.Free;
  end;
end;

{--------------------------------------------------------------------------------}
function TFormCustomTelegrIec.Set_Telegramme_Daten_DSfG (aSTL: TSendTelegrammList;
  aIEC870_Norm: TIEC870_Norm;
  aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
  aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
  aLinienNr: integer; aUrsache: byte; bSperren: boolean): boolean;
{--------------------------------------------------------------------------------}
{ IEC-Telegramme mit DSfG-Daten (Archivdaten, Logbuchdaten) zusammenstellen und in
  Sende-Telegrammliste stellen }
var
  TypKennung: byte;
  KonfigDSfG_Arch: TKonfigDSfG_Arch;
  KonfigDSfG_Logb: TKonfigDSfG_Logb;
  LogHeader: string;
  DatenListe: TDatenListe;
  bFlag: boolean;
  i: integer;

begin
  Result:=false;
  if not DSfG_freigeschaltet then exit;

  DatenListe:=TDatenListe.Create;
  try
    {--- Archivdaten ---}
    if (FormDatenIec.DSfGArchivAuswahl = daa_Archiv_KZW) OR
       (FormDatenIec.DSfGArchivAuswahl = daa_nur_Archiv) then begin
      while true do begin
        LinienNrVerwList.GetFlag (aLinienNr, dt_DSfG_AK, bFlag);
        if bFlag then begin
          { Liste mit neuen, noch nicht übertragenen DSfG-Archivwerten füllen: }
          if FormDatenIec.GetNeueDatenDSfG_Arch (aLinienNr, InfoObj_Bytes,
            TypKennung, KonfigDSfG_Arch, LogHeader, DatenListe, bSperren) then begin

            { Daten-Telegramm mit DSfG-Archivwerten bilden: }
            Set_TelegrammeDaten_Mess (aSTL, aIEC870_Norm,
              aPRM_101, aDIR_symm_101, aSendFCB_letzt_101, aFktCode_101,
              aSendeFolgeNr_104, aEmpfFolgeNr_104,
              aLinienNr, aUrsache, TypKennung, KonfigDSfG_Arch.IEC, 0, LogHeader, DatenListe);

            { DSfG-spezifische Messwert-Informationen in Sende-Telegrammliste setzen: }
            for i:=0 to aSTL.Count - 1 do begin
              with TSendTelegrammObj(aSTL[i]).Daten do begin
                TelegrammTyp:=tt_Data_DSfG;
                GesendetDaten.DSfG.StationId:=KonfigDSfG_Arch.StationId;
                GesendetDaten.DSfG.InstanzId:=KonfigDSfG_Arch.InstanzId;
                GesendetDaten.DSfG.Arch_LogbNr:=KonfigDSfG_Arch.ArchNr;
                GesendetDaten.DSfG.Kanal_MeldNr:=KonfigDSfG_Arch.KanalNr;
              end;
            end;

            { Daten-Telegramm ist sendebereit: }
            Set_SendTelegrammBereit (aPRM_101);
            Result:=true;
            exit;
          end
          else begin  // keine neuen DSfG-Archivdaten vorhanden
            bFlag:=false;
            LinienNrVerwList.UpdateFlag (aLinienNr, dt_DSfG_AK, bFlag);
          end;
        end;

        if not bFlag AND isMsg_NAD_DSfG_AK then begin
          isMsg_NAD_DSfG_AK:=false;  // Wieser-Message-Flag zurücksetzen
          if FormDatenIec.F_Update_DSfG_AK then  // wenn neue DSfG-Archivdaten vorhanden sind
            LinienNrVerwList.UpdateFlags (dt_DSfG_AK, true)
          else
            Break;
        end else
          Break;
      end;  // while true
    end;  { if DSfGArchivauswahl }

    {--- Logbuchdaten ---}
    while true do begin
      LinienNrVerwList.GetFlag (aLinienNr, dt_DSfG_LB, bFlag);
      if bFlag then begin
        { Liste mit neuen, noch nicht übertragenen DSfG-Logbuchdaten füllen: }
        if FormDatenIec.GetNeueDatenDSfG_Logb (aLinienNr, InfoObj_Bytes,
          TypKennung, KonfigDSfG_Logb, LogHeader, DatenListe, bSperren) then begin

          { Daten-Telegramm mit DSfG-Logbuchdaten bilden: }
          Set_TelegrammeDaten_Meld (aSTL, aIEC870_Norm,
            aPRM_101, aDIR_symm_101, aSendFCB_letzt_101, aFktCode_101,
            aSendeFolgeNr_104, aEmpfFolgeNr_104,
            aLinienNr, aUrsache, TypKennung, KonfigDSfG_Logb.IEC, LogHeader, DatenListe);

          { DSfG-spezifische Meldungs-Informationen in Sende-Telegrammliste setzen: }
          for i:=0 to aSTL.Count - 1 do begin
            with TSendTelegrammObj(aSTL[i]).Daten do begin
              TelegrammTyp:=tt_Data_DSfG;
              GesendetDaten.DSfG.StationId:=-1;   { Vorbelegung, wird für DSfG-Meldungen nicht herangezogen }
              GesendetDaten.DSfG.InstanzId:=KonfigDSfG_Logb.InstanzId;
              GesendetDaten.DSfG.Arch_LogbNr:=KonfigDSfG_Logb.LogbNr;
              GesendetDaten.DSfG.Kanal_MeldNr:=KonfigDSfG_Logb.MeldNr;
            end;
          end;

          { Daten-Telegramm ist sendebereit: }
          Set_SendTelegrammBereit (aPRM_101);
          Result:=true;
          exit;
        end
        else begin  // keine neuen DSfG-Logbuchdaten vorhanden
          bFlag:=false;
          LinienNrVerwList.UpdateFlag (aLinienNr, dt_DSfG_LB, bFlag);
        end;
      end;

      if not bFlag AND isMsg_NAD_DSfG_LB then begin
        isMsg_NAD_DSfG_LB:=false;  // Wieser-Message-Flag zurücksetzen
        if FormDatenIec.F_Update_DSfG_LB then  // wenn neue DSfG-Logbuchdaten vorhanden sind
          LinienNrVerwList.UpdateFlags (dt_DSfG_LB, true)
        else
          Break;
      end else
        Break;
    end;  // while true
  finally
    DatenListe.Free;
  end;
end;

{-------------------------------------------------------------------------------}
function TFormCustomTelegrIec.Set_Telegramme_Daten_KZW (aSTL: TSendTelegrammList;
  aIEC870_Norm: TIEC870_Norm;
  aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
  aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
  aLinienNr: integer; aUrsache: byte; bLoeschen: boolean): boolean;
{-------------------------------------------------------------------------------}
{ IEC-Telegramme mit Kurzzeitwerten zusammenstellen und in Sende-Telegrammliste stellen }
var
  TypKennung: byte;
  pKonfig : TKonfigDSfG_Arch;
  LogHeader: string;
  DatenListe: TDatenListe;
  bFlag: boolean;
  i: integer;
  fMin, fMax : double;
begin
  Result:=false;
  if not KZW_freigeschaltet then exit;

  if (FormDatenIec.DSfGArchivAuswahl = daa_Archiv_KZW) OR
     (FormDatenIec.DSfGArchivAuswahl = daa_nur_KZW) then begin
    DatenListe:=TDatenListe.Create;
    try
      while true do begin
        LinienNrVerwList.GetFlag (aLinienNr, dt_DSfG_KZW, bFlag);
        if bFlag then begin
          { Liste mit neuen DSfG-Kurzzeitwerten füllen: }
          if FormDatenIec.GetNeueDatenDSfG_KZW (aLinienNr, InfoObj_Bytes,
            TypKennung, pKonfig, LogHeader, DatenListe, i, fMin, fMax, aUrsache)
          then begin
            { Daten-Telegramm mit Messwert-Kurzzeitwerten bilden: }
            Set_TelegrammeDaten_Mess (aSTL, aIEC870_Norm,
              aPRM_101, aDIR_symm_101, aSendFCB_letzt_101, aFktCode_101,
              aSendeFolgeNr_104, aEmpfFolgeNr_104,
             aLinienNr, aUrsache, TypKennung, pKonfig.IEC, fMax, LogHeader,
             DatenListe, fMin);

            { Daten-Telegramm mit Meldungs-Kurzzeitwerten bilden: }
            Set_TelegrammeDaten_Meld (aSTL, aIEC870_Norm,
              aPRM_101, aDIR_symm_101, aSendFCB_letzt_101, aFktCode_101,
              aSendeFolgeNr_104, aEmpfFolgeNr_104,
              aLinienNr, aUrsache, TypKennung, pKonfig.IEC, LogHeader, DatenListe, False);

            { Kurzzeitwert-spezifische Information in Sende-Telegrammliste setzen (es
              wird nur 1 Kurzzeitwert in Telegramm-Sendeliste geladen): }
            if aSTL.Count > 0 then begin
              with TSendTelegrammObj(aSTL[0]).Daten do begin
                TelegrammTyp:=tt_Data_DSfG_KZW;

                // wenn Löschen-Flag gesetzt: Kurzzeitwert aus Kurzzeitwert-Liste
                // und Kurzzeitwert-Dateien löschen; 02.07.2007, WW }
                if bLoeschen then begin
                  FormDatenIec.KZWListe.DeleteKZWFile (i);
                  GesendetDaten.DSfG_KZW_Index:=-1;  // ist schon gelöscht, braucht später nicht gelöscht werden
                end else
                  GesendetDaten.DSfG_KZW_Index:=i;
              end;
            end;

            { Daten-Telegramm ist sendebereit: }
            Set_SendTelegrammBereit (aPRM_101);
            Result:=true;
            exit;
          end
          else begin  // keine neuen Kurzzeitwerte vorhanden
            bFlag:=false;
            LinienNrVerwList.UpdateFlag (aLinienNr, dt_DSfG_KZW, bFlag);
          end;
        end;

        if not bFlag AND isMsg_NKD then begin
          isMsg_NKD:=false;  // Wieser-Message-Flag zurücksetzen
          if FormDatenIec.KZWListe.LadeKZWFiles then  // wenn neue Kurzzeitwerte geladen wurden
            LinienNrVerwList.UpdateFlags (dt_DSfG_KZW, true)
          else
            Break;
        end else
          Break;
      end;  // while true
    finally
      DatenListe.Free;
    end;
  end;  { if DSfGArchivAuswahl }
end;

{----------------------------------------------------------------------------}
function TFormCustomTelegrIec.Set_Telegramm_Redundanz_AktivPassiv (
  aSTL: TSendTelegrammList;
  bRedundanzAktiv: boolean; aIEC870_Norm: TIEC870_Norm;
  aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
  aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
  aLinienNr: integer; aUrsache: byte): boolean;
{----------------------------------------------------------------------------}
{ IEC-Telegramm mit Meldung für Redundanz-Betriebszustand (aktiv/passiv)
  zusammenstellen und in Sende-Telegrammliste stellen }
var
  TypKennung: byte;
  KonfigIEC: TKonfigIEC;
  LogHeader: string;
  DatenListe: TDatenListe;
  i: integer;

begin
  Result:=false;
  DatenListe:=TDatenListe.Create;
  try
    { Liste mit Meldung für Redundanz-Betriebszustand füllen: }
    if FormDatenIec.GetMeldung_Redundanz_AktivPassiv (bRedundanzAktiv,
      aLinienNr, RedundanzStatusmeldung_ASDU, RedundanzStatusmeldung_InfoObj,
      InfoObj_Bytes, TypKennung, KonfigIEC, LogHeader, DatenListe) then begin

      { Daten-Telegramm mit MRG-Meldungen bilden: }
      Set_TelegrammeDaten_Meld (aSTL, aIEC870_Norm,
        aPRM_101, aDIR_symm_101, aSendFCB_letzt_101, aFktCode_101,
        aSendeFolgeNr_104, aEmpfFolgeNr_104,
        aLinienNr, aUrsache, TypKennung, KonfigIEC, LogHeader, DatenListe);

      { MRG-spezifische Meldungs-Informationen in Sende-Telegrammliste setzen: }
      for i:=0 to aSTL.Count - 1 do
        with TSendTelegrammObj(aSTL[i]).Daten do
          TelegrammTyp:=tt_Data_intern;

      { Daten-Telegramm ist sendebereit: }
      Set_SendTelegrammBereit (aPRM_101);
      Result:=true;
    end;
  finally
    DatenListe.Free;
  end;
end;

{----------------------------------------------------------------------------}
procedure TFormCustomTelegrIec.Set_Telegramm_Generalabfrage (
  aSTL: TSendTelegrammList;
  aIEC870_Norm: TIEC870_Norm;
  aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
  aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
  aLinienNr: integer; aUrsache: byte; aStationsNrListe: TStrings);
{----------------------------------------------------------------------------}
{ IEC-Generalabfrage-Telegramm zusammenstellen }
var
  DF: string;
  sTelegramm: string;
  GesendetDaten: TGesendetDaten;
  SendTelegrammRec: TSendTelegrammRec;
  bFCB_Buf: boolean;
  bFCV_Buf: boolean;
  TypKennung: byte;
  DatenfeldZaehler: byte;
  StationsNr: integer;
  i: integer;

begin
  // evtl. noch vorhandene (Daten-)Telegramme löschen
  aSTL.Delete_AlleTelegramme;

  { Vorbelegung für FCB (bei 101-Primärstation-Telegramm): }
  bFCB_Buf:=aSendFCB_letzt_101;

  TypKennung:=C_TK_Generalabfrage;

  { Informationsobjekt bilden: }
  DF:=Get_IEC870_Informationsobjekt_Generalabfrage (InfoObj_Bytes, C_AFK_Stationsabfrage_global);
  DatenfeldZaehler:=1;  // 1 Informationsobjekt

  // Für alle in der GA-Stationsnummernliste enthaltenen Stationsnummern ein Telegramm
  // in die Sendeliste stellen:
  for i:=0 to aStationsNrListe.Count - 1 do begin
    StationsNr:=StrToInt (aStationsNrListe [i]);
    case aIEC870_Norm of
      norm_iec870_101:  { 101-Generalabfrage-Telegramm zusammensetzen }
        begin
          if aPRM_101 then begin // Telegramm der Primärstation
            bFCB_Buf:=not bFCB_Buf;  // FCB toggeln
            bFCV_Buf:=Get_IEC870_101_FCV (aFktCode_101);  // Funktionscode-abhängiges FCV ermitteln

            sTelegramm:=Get_IEC870_101_TelegrammDaten_Symmetrisch_Primaer (aDIR_symm_101,
              bFCB_Buf, bFCV_Buf, aFktCode_101, aLinienNr, aUrsache, Herkunftsadresse,
              TypKennung, StationsNr, DF, DatenfeldZaehler,
              Adressfeld_Bytes, ASDU_Bytes);  // invertiertes DIR
          end
          else begin // Telegramm der Sekundärstation
            bFCV_Buf:=false;  // FCV in Telegramm der Sekundärstation nicht enthalten

            sTelegramm:=Get_IEC870_101_TelegrammDaten_Unsymmetrisch_Sekundaer (C_ACD_0, C_DFC_0,
              aFktCode_101, aLinienNr, aUrsache, Herkunftsadresse,
              TypKennung, StationsNr, DF, DatenfeldZaehler,
              Adressfeld_Bytes, ASDU_Bytes);  // ACD=0, DFC=0
          end;
        end;

      norm_iec870_104:  { 104-Daten-Telegramm mit Messdaten zusammensetzen }
        begin
          bFCV_Buf:=false;  // nicht benutzt bei 104

          sTelegramm:=Get_IEC870_104_TelegrammDaten_I (aSendeFolgeNr_104, aEmpfFolgeNr_104,
            aUrsache, Herkunftsadresse, TypKennung, StationsNr,
            DF, DatenfeldZaehler, Adressfeld_Bytes, ASDU_Bytes);
        end;
    else
      bFCV_Buf:=false;  // nicht benutzt
      sTelegramm:='';
    end;  // case

    { in Sende-Telegrammliste eintragen: }
    FillChar (GesendetDaten, SizeOf (GesendetDaten), 0);  // unbenutzt; GesendetDaten-Info nur bei Datentelegrammen

    { Sendetelegramm-Record zusammenstellen: }
    SendTelegrammRec.Telegramm:=sTelegramm;
    SendTelegrammRec.FCV_101:=bFCV_Buf;
    SendTelegrammRec.SendefolgeNr_104:=aSendeFolgeNr_104;
    SendTelegrammRec.TelegrammTyp:=tt_Data_intern;
    SendTelegrammRec.GesendetDaten:=GesendetDaten;
    SendTelegrammRec.LogHeader:='';  // ohne Log-Informationen
    SendTelegrammRec.LogDatensatzInfo:='';  // ohne Log-Informationen
    aSTL.Insert_Telegramm (true, SendTelegrammRec);  // am Listenende anhängen

    { für nächstes 104-Telegramm: }
    if aIEC870_Norm = norm_iec870_104 then
      Inc_IEC870_104_Telegrammfolgenummer (aSendeFolgeNr_104);  // lokal hochzählen für Telegramme in Sendeliste

    { Telegramm ist sendebereit: }
    Set_SendTelegrammBereit (aPRM_101);
  end;  { for i }
end;

{----------------------------------------------------------------------------}
procedure TFormCustomTelegrIec.Set_Telegramm_UhrzeitSynchronisation (
  aSTL: TSendTelegrammList;
  aIEC870_Norm: TIEC870_Norm;
  aPRM_101, aDIR_symm_101, aSendFCB_letzt_101: boolean; aFktCode_101: integer;
  aSendeFolgeNr_104, aEmpfFolgeNr_104: integer;
  aLinienNr: integer; aUrsache: byte; aStationsNr: integer; aInfoObj: string);
{----------------------------------------------------------------------------}
{ IEC-Uhrzeit-Synchronisations-Telegramm zusammenstellen }
var
  DF: string;
  sTelegramm: string;
  GesendetDaten: TGesendetDaten;
  SendTelegrammRec: TSendTelegrammRec;
  bFCB_Buf: boolean;
  bFCV_Buf: boolean;
  TypKennung: byte;
  DatenfeldZaehler: byte;

begin
  { Vorbelegung für FCB (bei 101-Primärstation-Telegramm): }
  bFCB_Buf:=aSendFCB_letzt_101;

  TypKennung:=C_TK_Uhrzeit_Synchronisation;

  { Informationsobjekt bilden: }
  DF:=aInfoObj;
  DatenfeldZaehler:=1;  // 1 Informationsobjekt

  case aIEC870_Norm of
    norm_iec870_101:  { 101-Generalabfrage-Telegramm zusammensetzen }
      begin
        if aPRM_101 then begin // Telegramm der Primärstation
          bFCB_Buf:=not bFCB_Buf;  // FCB toggeln
          bFCV_Buf:=Get_IEC870_101_FCV (aFktCode_101);  // Funktionscode-abhängiges FCV ermitteln

          sTelegramm:=Get_IEC870_101_TelegrammDaten_Symmetrisch_Primaer (aDIR_symm_101,
            bFCB_Buf, bFCV_Buf, aFktCode_101, aLinienNr, aUrsache, Herkunftsadresse,
            TypKennung, aStationsNr, DF, DatenfeldZaehler,
            Adressfeld_Bytes, ASDU_Bytes);  // invertiertes DIR
        end
        else begin // Telegramm der Sekundärstation
          bFCV_Buf:=false;  // FCV in Telegramm der Sekundärstation nicht enthalten

          sTelegramm:=Get_IEC870_101_TelegrammDaten_Unsymmetrisch_Sekundaer (C_ACD_0, C_DFC_0,
            aFktCode_101, aLinienNr, aUrsache, Herkunftsadresse,
            TypKennung, aStationsNr, DF, DatenfeldZaehler,
            Adressfeld_Bytes, ASDU_Bytes);  // ACD=0, DFC=0
        end;
      end;

    norm_iec870_104:  { 104-Daten-Telegramm mit Messdaten zusammensetzen }
      begin
        bFCV_Buf:=false;  // nicht benutzt bei 104

        sTelegramm:=Get_IEC870_104_TelegrammDaten_I (aSendeFolgeNr_104, aEmpfFolgeNr_104,
          aUrsache, Herkunftsadresse, TypKennung, aStationsNr,
          DF, DatenfeldZaehler, Adressfeld_Bytes, ASDU_Bytes);
      end;
  else
    bFCV_Buf:=false;  // nicht benutzt
    sTelegramm:='';
  end;  // case

  { in Sende-Telegrammliste eintragen: }
  FillChar (GesendetDaten, SizeOf (GesendetDaten), 0);  // unbenutzt; GesendetDaten-Info nur bei Datentelegrammen

  { Sendetelegramm-Record zusammenstellen: }
  SendTelegrammRec.Telegramm:=sTelegramm;
  SendTelegrammRec.FCV_101:=bFCV_Buf;
  SendTelegrammRec.SendefolgeNr_104:=aSendeFolgeNr_104;
  SendTelegrammRec.TelegrammTyp:=tt_Data_intern;
  SendTelegrammRec.GesendetDaten:=GesendetDaten;
  SendTelegrammRec.LogHeader:='';  // ohne Log-Informationen
  SendTelegrammRec.LogDatensatzInfo:='';  // ohne Log-Informationen
  aSTL.Insert_Telegramm (true, SendTelegrammRec);  // am Listenende anhängen

  { Telegramm ist sendebereit: }
  Set_SendTelegrammBereit (aPRM_101);
end;


{------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------------}
procedure TFormCustomTelegrIec.FillGA_StationsNrListe_Station (aStationsNr: integer);
{-----------------------------------------------------------------------------------}
{ Trägt einzelne Stationsnummer in Generalabfrage-Stationsnummernliste ein;
  Übergabe: Stationsnummer }
begin
  FGA_StationsNrListe.Clear;
  FGA_StationsNrListe.Add (IntToStr (aStationsNr));
end;

{------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}
function TFormCustomTelegrIec.Get_MaxAnzahlObjekte (aIEC870_Norm: TIEC870_Norm;
  aTypKennung: byte): integer;
{-----------------------------------------------------------------------------}
{ berechnet die maximal mögliche Anzahl an Objekten (Datenfelder) in einem Telegramm;
  Ergebnis: max. Anzahl Objekte (0 = Fehler, keine Berechnung möglich wegen
                                     unbekannter Typkennung }
const
  CLenSteuerfeld_101      = 1;
  CLenSteuerfeld_104      = 4;

  CLenTypKennung          = 1;
  CLenAnzObjekte          = 1;

var
  LenDatenfeld: integer;
  LenUebertragungsursache: integer;
  LenSteuerfeld: integer;

begin
  { Typkennungs-abhängige Länge des Datenfelds ermitteln: }
  LenDatenfeld:=Get_IEC870_LaengeDatenfeld (aTypKennung);
  if LenDatenfeld = 0 then begin
    Result:=0;  // keine Objekte
    exit;
  end;

  if Herkunftsadresse < 0 then
    LenUebertragungsursache:=1  // 1 Byte Übertragungsursache
  else
    LenUebertragungsursache:=2;  // 2 Byte Übertragungsursache mit Herkunftsadresse

  case aIEC870_Norm of
    norm_iec870_101: LenSteuerfeld:=CLenSteuerfeld_101;
    norm_iec870_104: LenSteuerfeld:=CLenSteuerfeld_104;
  else  // unbekannte Norm
    Result:=0;  // keine Objekte
    exit;
  end;

  Result:=(Telegramm_MaxBytes - LenSteuerfeld - Adressfeld_Bytes - CLenTypKennung -
           CLenAnzObjekte - LenUebertragungsursache - ASDU_Bytes) DIV
           (LenDatenfeld + InfoObj_Bytes);  // jedes Datenfeld mit Info-Objektadresse
end;

{-------------------------- Remote-Monitor ------------------------------------}

{-----------------------------------------------------------}
procedure TFormCustomTelegrIec.Fill_RemoteMonitorControlList;
{-----------------------------------------------------------}
{ Füllt Liste mit allen Kontrollelementen, deren Daten an Remote-Clients gesendet
  werden sollen }
var
  i: integer;

begin
  for i:=0 to Self.ComponentCount - 1 do begin
    if Self.Components [i] is TEdit then begin  // nur Edit-Felder
      if Self.Components [i].Tag <> 0 then  // nur für Remote-Monitoring gekennzeichnete Edits
        FRemoteMonitorControlList.Add (Self.Components [i]);
    end;
  end;
end;

{----------------------------------------------------------------}
procedure TFormCustomTelegrIec.SendToRemoteClient (sNorm: string);
{----------------------------------------------------------------}
{ Sendet Telegramm-Daten der Remote-Monitor-Kontrollelemente an alle verbundenen
  Remote-Monitor-Clients }
begin
  if Assigned (FRemoteMonitorObj) then
    FRemoteMonitorObj.SendTelegrammData (sNorm, FRemoteMonitorControlList);
end;

end.



