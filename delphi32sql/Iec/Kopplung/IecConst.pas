{******************************************************************************}
{* Unit: Konstanten für IEC-Kopplung (32-Bit)                                 *}
{* 26.05.2003  WW                                                             *}
{******************************************************************************}
unit IecConst;

interface

uses
  Windows, Messages, SysUtils, Graphics, Contnrs;

Const
  { aktuelle Version IEC-Kopplung }
  C_Version_IEC = '4.6.0';

  { IEC 870-5-101: }
  { Konstanten Telegrammformat FT 1.2: }
  C_START_FEST = $10;  { Startzeichen für Telegramme fester Länge (Kurzsatz) }
  C_START_VAR  = $68;  { Startzeichen für Telegramme variabler Länge; auch 104 }
  C_STOP       = $16;  { Endezeichen für Telegramme }
  C_EINZEL_E5  = $E5;  { Einzelsteuerzeichen (statt ACK-Telegramm) }

  { Konstanten Linkschicht: }
  { Steuerfeld }
  { Funktionscodes in Telegrammen der Primärstation (unsymmetrische Übertragung:
    in Aufrufrichtung) }
  C_FKT_PRM_0_LINK_RESET              =  0;
  C_FKT_PRM_2_LINK_TEST               =  2;  // nur für symmetrische Übertragung
  C_FKT_PRM_3_DATEN_CONFIRM           =  3;  // Anwenderdaten, Send/Confirm
  C_FKT_PRM_4_DATEN_NOREPLY           =  4;  // Anwenderdaten, Send/No Reply
  C_FKT_PRM_9_LINK_STATUS             =  9;
  C_FKT_PRM_10_ABFRAGE_DATEN_KLASSE1  = 10;  // nur für unsymmetrische Übertragung
  C_FKT_PRM_11_ABFRAGE_DATEN_KLASSE2  = 11;  // nur für unsymmetrische Übertragung

  { Funktionscodes in Telegrammen der Sekundärstation (unsymmetrische Übertragung:
    in Antwortrichtung) }
  C_FKT_SEK_0_ACK          =  0;
  C_FKT_SEK_1_NACK         =  1;
  C_FKT_SEK_8_DATEN        =  8;  // nur für unsymmetrische Übertragung
  C_FKT_SEK_9_NACK_NODATA  =  9;  // nur für unsymmetrische Übertragung
  C_FKT_SEK_11_LINK_STATUS = 11;
  C_FKT_SEK_15_NOSERVICE   = 15;

  { weitere Informationen im Steuerfeld }
  C_PRM_0 = false;  { Primärnachricht PRM=0, d.h. Nachricht aus einer (antwortenden) Sekundärstation }
  C_PRM_1 = true;   { Primärnachricht PRM=1, d.h. Nachricht aus einer (veranlassenden) Primärstation }

  C_ACD_0 = false;  { Zugriffsanforderung ACD=0, d.h. keine Anforderung auf Übertragung
                      von Daten der Klasse 1 }
  C_DFC_0 = false;  { Datenflußsteuerung DFC=0, d.h. weitere Nachrichten werden angenommen.
                      -> In Antworttelegrammen des Kopplungsprogramms würde das dynamische
                      Setzen auf 1 nur Sinn machen, wenn es im Kopplungsprogramm einen
                      Eingangspuffer gäbe, der droht überzulaufen. }

  { IEC 870-5-104: }
  { Funktionscodes im Steuerfeld (U-Format) }
  C_FKT_STARTDT_ACT = $04;
  C_FKT_STARTDT_CON = $08;
  C_FKT_STOPDT_ACT  = $10;
  C_FKT_STOPDT_CON  = $20;
  C_FKT_TESTFR_ACT  = $40;
  C_FKT_TESTFR_CON  = $80;

  { IEC 807-5 allgemein: }
  { Typkennungen }
  C_TK_Einzelmeldung                  =   1;  // ohne Zeitmarke
  C_TK_Einzelmeldung_CP24Time2a       =   2;

  C_TK_Messwert_normiert              =   9;  // ohne Zeitmarke
  C_TK_Messwert_normiert_CP24Time2a   =  10;

  C_TK_Messwert_Gleitkomma            =  13;  // ohne Zeitmarke
  C_TK_Messwert_Gleitkomma_CP24Time2a =  14;
  C_TK_Zaehlwert                      =  15;  // ohne Zeitmarke
  C_TK_Zaehlwert_CP24Time2a           =  16;

  C_TK_Einzelmeldung_CP56Time2a       =  30;

  C_TK_Messwert_normiert_CP56Time2a   =  34;
  C_TK_Messwert_Gleitkomma_CP56Time2a =  36;
  C_TK_Zaehlwert_CP56Time2a           =  37;

  C_TK_Initialisierungsende           =  70;

  C_TK_Generalabfrage                 = 100;
  C_TK_Uhrzeit_Synchronisation        = 103;

  { Abfragekennung QOI zu Generalabfrage }
  C_AFK_Stationsabfrage_global = 20;

  { Übertragungsursachen }
  C_URS_spontan     =  3;
  C_URS_Activate    =  6;
  C_URS_AckActivate =  7;
  C_URS_EndActivate = 10;
  C_URS_GeneralAbfr = 20;

  { Fehler Telegrammaufbau }
  C_TGFORMAT_OK            =  0;
  C_TGFORMAT_Err_StartChar = -1;  // Fehler Startzeichen
  C_TGFORMAT_Err_StopChar  = -2;  // Fehler Stopzeichen
  C_TGFORMAT_Err_Checksum  = -3;  // Fehler Prüfsumme
  C_TGFORMAT_Err_LenChar   = -4;  // Fehler Längenzeichen
  C_TGFORMAT_Err_TelegrLen = -5;  // Fehler Telegrammlänge

  { MRG-Langzeitdaten-Format }
  C_AufzMaxLGZnorm = 9999;  { Aufzeichnungsmaximum normierter LGZ-Daten }

  { Redundanzbetrieb }
  C_Redundanz_Aus    = -1;
  C_Redundanz_Aktiv  =  0;
  C_Redundanz_Passiv =  1;

  C_ColorRedundanzAktiv  = clLime;
  C_ColorRedundanzPassiv = clYellow;

  { Windows-Botschaften }
  WM_TASKBAREVENT         = WM_USER +  1;

resourcestring
  { Texte }
  SMsgRedundanzAktiv         = 'Redundanzbetrieb: Aktiv';
  SMsgRedundanzPassiv        = 'Redundanzbetrieb: Passiv';
  SMsgRedundanzAktiv_Passiv  = 'Redundanzbetrieb: Aktiv -> Passiv';
  SMsgRedundanzPassiv_Aktiv  = 'Redundanzbetrieb: Passiv -> Aktiv';

  SMsgResetZeitbereiche = 'Zeitbereiche der bereits übertragenen Daten wirklich zurücksetzen ?';

  { Fehlertexte }
  SMsgErrAnmeldenWMsg = 'Fehler beim Anmelden im Benachrichtigungssystem !';
  SMsgErrAbmeldenWMsg = 'Fehler beim Abmelden vom Benachrichtigungssystem !';

type
  { IEC 870-5-101/104: Anwendungsschicht }

  { Struktur für Identifikationsfeld der Dateneinheit }
  TIecDatenEinheitIdent = record
    Typkennung: byte;         { Typkennung }
    SQ_Bit: boolean;          { SQ-Bit in der variablen Strukturkennung }
    AnzahlObjekte: byte;      { Anzahl der Objekte in der variablen Strukturkennung }
    UebertrUrsache: byte;     { Übertragungsursache }
    Stationsnummer: integer;  { Stationsnummer in der gemeinsamen Adresse der ASDU }
  end;

  { Prozessinfo-Typen }
  TIECProzessInfo = (iecpi_Einzelmeldung, iecpi_Messwert_normiert,
                     iecpi_Messwert_Gleitkomma, iecpi_Zaehlwert);

  { Datentelegramm-Typen }
  TTelegrammTyp = (tt_NoData,           // es wurde kein Datentelegramm versendet
                   tt_Data_MRG,         // es wurde ein Telegramm mit MRG-LGZ-Daten versendet
                   tt_Data_DSfG,        // es wurde ein Telegramm mit DSfG-Archivdaten versendet
                   tt_Data_DSfG_KZW,    // es wurde ein Telegramm mit DSfG-Kurzzeitwerten versendet
                   tt_Data_intern,      // es wurde ein Telegramm mit internen Daten versendet (z.B. Redundanz-Betriebszustand)
                   tt_104_StartDt_act,  // nur 104: es wurde ein STARTDT: act versendet
                   tt_104_StopDt_act,   // nur 104: es wurde ein STOPDT: act versendet
                   tt_104_TestFr_act,   // nur 104: es wurde ein TESTFR: act versendet
                   tt_104_Quitt_Con);   // nur 104: es wurde ein Quittierungs- oder Bestätigungstelegamm versendet

  { Daten-Typen }
  TDatentyp = (dt_MRG_MW, dt_MRG_ME, dt_DSfG_AK, dt_DSfG_LB, dt_DSfG_KZW);

  TNeueDatenda = record
    MRG_MW: boolean;
    MRG_ME: boolean;
    DSfG_AK: boolean;
    DSfG_LB: boolean;
    DSfG_KZW: boolean;
  end;

  { Struktur für IEC-Konfigurationsdaten }

  TKonfigIEC = record
    LinienNr: integer;
    StationsNr: integer;
    InfoObj_high: byte;
    InfoObj_medium: byte;  // 28.02.2007
    InfoObj_low: byte;
  end;

  TKonfigMRG_Mess = record
    Kennung: string;
    KanalNr: integer;
    IEC: TKonfigIEC;
  end;

  TKonfigMRG_Meld = record
    Kennung: string;
    MeldNr: integer;
    IEC: TKonfigIEC;
  end;

  TKonfigDSfG_Arch = record
    StationId: integer;
    InstanzId: integer;
    ArchNr: integer;
    KanalNr: integer;
    IEC: TKonfigIEC;
  end;

  TKonfigDSfG_Logb = record
    InstanzId: integer;
    LogbNr: integer;
    MeldNr: integer;
    IEC: TKonfigIEC;
  end;

  TMessDaten = record
    Wert: double;
    Wert_gueltig: boolean;
    DatumZeit: TDateTime;
    Ordnungsnummer: integer;
    Referenznummer: integer;
    LogInfo: string;
  end;

  TMessDatenObj = class(TObject)
  public
    Daten: TMessDaten;
    TransferCause: integer;
    constructor Create (ADaten: TMessDaten);
  end;

  TMeldDaten = record
    Ein: boolean;  { true: Meldung ein; false: Meldung aus }
    DatumZeit: TDateTime;
    LogInfo: string;
  end;

  TMeldDatenObj = class(TObject)
  public
    Daten: TMeldDaten;
    TransferCause: integer;
    constructor Create (ADaten: TMeldDaten);
  end;

  TDatenListe = TObjectList;

var
  WieserIniFilename: string = '';
  IECLogDir: string = '';  // 10.03.2015, WW

  { Flags für Wieser-Benachrichtigung: }
  isMsg_NAD_MRG_MW: boolean;
  isMsg_NAD_MRG_ME: boolean;
  isMsg_NAD_DSfG_AK: boolean;
  isMsg_NAD_DSfG_LB: boolean;
  isMsg_NKD: boolean;

  MRG_freigeschaltet: boolean = false;
  DSfG_freigeschaltet: boolean = false;
  KZW_freigeschaltet: boolean = false;

  { Redundanz: }
//  Telegrammzahl_Standby: integer;
//  Zeitfenster_Standby: integer;  { Sekunden }
  Redundanzbetrieb: integer = C_Redundanz_Aus;  { Standard: kein Redundanzbetrieb }
  IsBde : boolean = True;
  
  IsService: boolean = false;

function HexString (S: string): string;
function MessageBoxWhenExe (hWnd: HWND; const Text, Caption: PChar;
  Flags: Longint): Integer;

implementation

{-------------------------------------}
function HexString (S: string): string;
{-------------------------------------}
{ Zeichen aus S in Hex-Format konvertieren }
var
  ErgStr: string;
  i: integer;
begin
  ErgStr:='';
  for i:=1 to length (S) do
    ErgStr:=ErgStr + IntToHex (Byte (S [i]), 2) + ' ';
  Result:=ErgStr;
end;

{-----------------------------------------------------------------}
function MessageBoxWhenExe (hWnd: HWND; const Text, Caption: PChar;
  Flags: Longint): Integer;
{-----------------------------------------------------------------}
{ Zeigt MessageBox, wenn Kopplung nicht als Dienst läuft }
begin
  if not IsService then
    Result:=MessageBox (hWnd, Text, Caption, Flags)
  else
    Result:=0;
end;

{ TMessDatenObj }

{----------------------------------------------------}
constructor TMessDatenObj.Create (ADaten: TMessDaten);
{----------------------------------------------------}
begin
  inherited Create;
  Daten:=ADaten;
  TransferCause := 0; // Optionale Übertragungsursache
end;

{ TMeldDatenObj }

{----------------------------------------------------}
constructor TMeldDatenObj.Create (ADaten: TMeldDaten);
{----------------------------------------------------}
begin
  inherited Create;
  Daten:=ADaten;
  TransferCause := 0; // Optionale Übertragungsursache
end;

end.
