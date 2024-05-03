{******************************************************************************}
{* Unit: Typen und Konstanten f�r MRG-Abrufmodul                              *}
{* 03.12.98 WW                                                                *}
{******************************************************************************}
Unit MSysCon;

INTERFACE

Uses
  Graphics, LGZType, AbrufTimeoutConst, SerMrgModem;

{$H-}

const
  V_MaxKanalZahl: word = C_MaxKanalZahl;   { im Programm verwendete �ber WIESER.INI
                                             einstellbare Kanalzahl }
  RohdatenLoeschen: boolean = true;   { �ber Kommandozeilenparameter "Debug" deaktivierbar }
  CheckMomHalten: boolean = false;    { Flag zum Pr�fen auf "Momentanwerte halten" }

  { Programmstati und zugeordnete Farben }

  ps_NichtAktiv         = 0;
  ps_FupModemInit       = 1;
  ps_FupModemInitFehler = 2;
  ps_KeineVerbindung    = 3;
  ps_VerbindungSteht    = 4;

  cl_Programmstatus: array [ps_NichtAktiv..ps_VerbindungSteht] of TColor =
    (clRed, clYellow, clRed, clBtnFace, clGreen);

Const
  { MRG-Datentypen }
  maxDataTypes = 4;         { Meldungen, Messwerte, Tagess�tze, Pr�fungss�tze }

  dt_Meldungen   = 1;
  dt_Messwerte   = 2;
  dt_Tagessaetze = 3;
  dt_Pruefsaetze = 4;

  { Timeouts }
  CTimeout_ModemInit = 3000;  { Timeout bei Modem-Initialisierung }

  CTimeout_Verbindungsaufbau = 120000;
  CTimeout_Verbindungsabbau  =  10000;   { f�r FUP-1200 erh�ht, bisher 5000 }
  CTimeout_RufAnnahmeModem   =  60000;

  CTimeout_Kennung           =  60000;   { relativ gro� wegen:
                                           1. evtl. verz�gertem Verbindungsaufbau zu Station,
                                              wenn zwei Modems an einer Leitung h�ngen
                                           2. bei Rufentgegennahme kommt auf den Kennungsbefehl evtl.
                                              eine FUP-Fehlermeldung (nach ca. 35 s) }
  CTimeout_Login             =  20000;
  CTimeout_Parameter         =  60000;
  CTimeout_Meldungen         =  60000;
  CTimeout_Messwerte         =  60000;
  CTimeout_Tagessaetze       =  60000;
  CTimeout_Pruefsaetze       =  60000;
  CTimeout_Binaerdatei       =  30000;
  CTimeout_RundpufferReset   =  20000;
  CTimeout_Parametrieren     =  20000;
  CTimeout_Rufausloesung     =  20000;
  CTimeout_DSfGUmschaltung   =  20000;
  CTimeout_DSfGRufliste      =  20000;
  CTimeout_DSfGRufQuittung   =  20000;

  Timeout_Verbindungsaufbau: integer = CTimeout_Verbindungsaufbau;
  Timeout_Verbindungsabbau : integer = CTimeout_Verbindungsabbau;
  Timeout_RufAnnahmeModem  : integer = CTimeout_RufAnnahmeModem;
  Timeout_Kennung          : integer = CTimeout_Kennung;
  Timeout_Login            : integer = CTimeout_Login;
  Timeout_Parameter        : integer = CTimeout_Parameter;
  Timeout_Meldungen        : integer = CTimeout_Meldungen;
  Timeout_Messwerte        : integer = CTimeout_Messwerte;
  Timeout_Tagessaetze      : integer = CTimeout_Tagessaetze;
  Timeout_Pruefsaetze      : integer = CTimeout_Pruefsaetze;
  Timeout_Binaerdatei      : integer = CTimeout_Binaerdatei;
  Timeout_RundpufferReset  : integer = CTimeout_RundpufferReset;
  Timeout_Parametrieren    : integer = CTimeout_Parametrieren;
  Timeout_Rufausloesung    : integer = CTimeout_Rufausloesung;
  Timeout_DSfGUmschaltung  : integer = CTimeout_DSfGUmschaltung;
  Timeout_DSfGRufliste     : integer = CTimeout_DSfGRufliste;
  Timeout_DSfGRufQuittung  : integer = CTimeout_DSfGRufQuittung;

  Timeout_ModemInit    : integer = CTimeout_ModemInit;
  Timeout_FupAntwort   : integer = CMRG_Timeout_FupAntwort;
  Timeout_FupReset     : integer = CMRG_Timeout_FupReset;
  Timeout_IEC1107Telegr: integer = CMRG_Timeout_IEC1107Telegr;
  Timeouts_MRGModem: TMRGModemTimeouts = (ModemAntwort: CTimeout_ModemAntwort;
                                          CRCCheck: CMRG_Timeout_CRCCheck;
                                          ACK01ProtMeldung: CMRG_Timeout_ACK01ProtMeldung);
  Versuche_MRGModem: TMRGModemVersuche = (CRC: CMRG_CRCVersuche;
                                          BCC: CMRG_BCCVersuche;
                                          ACK01Prot: CMRG_ACK01ProtVersuche);


  { FUP-D�-Geschwindigkeiten }

  fup_1200_HDX = 1;
  fup_2400_DX  = 2;

  { Z�hler f�r FUP-Antwort F2 }

  FUP_F2_Count: byte = 0;

IMPLEMENTATION

end.
