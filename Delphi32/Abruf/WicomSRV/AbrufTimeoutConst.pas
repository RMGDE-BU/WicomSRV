{******************************************************************************}
{* Unit: Standard-Timeout-Konstanten f�r WK22-Abrufsystem                     *}
{* Version: 30.12.2003  WW                                                    *}
{******************************************************************************}
unit AbrufTimeoutConst;

interface

const
  { Timeouts f�r Modem-Kommunikation }
  CTimeout_ModemAntwort =   500;  { Zeit in ms, nach der nicht mehr mit neuen
                                    Zeichen von einem Modem gerechnet wird }
  CTimeout_ModemInit    =  3000;  { Timeout f�r Modem-Initialisierung }

  CTimeout_GSMModem     = 30000;  { Timeout GSM-Modembefehle (Antworten k�nnen lange
                                    auf sich warten lassen, z.B. bei PIN-Eingabe,
                                    SMS-Liste lesen, SMS l�schen !) }

  { Timeouts f�r FUP-Kommunikation }
  CMRG_Timeout_FupAntwort =  5000;
  CMRG_Timeout_FupReset   = 30000;

  { Timeouts f�r MRG-Modem-Kommunikation }
  CMRG_Timeout_CRCCheck          =  5000;  { bei Check der CRC-Konfiguration im MRG 800 PTB, EC694 }
  CMRG_Timeout_ACK01ProtMeldung  = 20000;  { beim Warten auf Protokoll-Best�tigung (ACK, NAK etc.) }
  CMRG_Timeout_IEC1107Telegr     = 20000;  { lt. IEC 1107 antwortet das Ger�t zwar nach max. 1,5 s,
                                             ansonsten liegt ein Fehler vor. Trotzdem lieber etwas mehr...
                                             30.04.2019, WW: Bei Elster EK per GSM-Modemverbindung
                                             reichen 10s auch nicht immer, daher erh�ht auf 20 s }
  CMRG_Timeout_TritschlerIECProt = 20000;  { erh�ht von 5 auf 20 s f�r VC2 Vs. 3.x; 05.10.2005 WW }
  CMRG_Timeout_TritschlerFTLProt =  3000;
  CMRG_Timeout_CorusSAMProt      = 10000;
  CMRG_Timeout_ElsterDS100Prot   =  2000;  { f�r serielle DS-100-Kommunikation }
  CMRG_Timeout_ModbusProt        = 10000;  { f�r Modbus-Kommunikation ASCII, RTU, TCP }

  { Timeouts f�r MRG-Abruf }
  CMRG_Timeout_Verbindungsaufbau = 120000;
  CMRG_Timeout_Verbindungsabbau  =  10000;   { f�r FUP-1200 erh�ht, bisher 5000 }
  CMRG_Timeout_RufAnnahmeModem   =  60000;

  CMRG_Timeout_Kennung           =  60000;   { relativ gro� wegen:
                                               1. evtl. verz�gertem Verbindungsaufbau zu Station,
                                                  wenn zwei Modems an einer Leitung h�ngen
                                               2. bei Rufentgegennahme kommt auf den Kennungsbefehl evtl.
                                                  eine FUP-Fehlermeldung (nach ca. 35 s) }
  CMRG_Timeout_Login             =  20000;
  CMRG_Timeout_Parameter         =  60000;
  CMRG_Timeout_Meldungen         =  60000;
  CMRG_Timeout_Messwerte         =  60000;
  CMRG_Timeout_Tagessaetze       =  60000;
  CMRG_Timeout_Pruefsaetze       =  60000;
  CMRG_Timeout_Binaerdatei       =  30000;
  CMRG_Timeout_RundpufferReset   =  20000;
  CMRG_Timeout_Parametrieren     =  20000;
  CMRG_Timeout_Rufausloesung     =  20000;
  CMRG_Timeout_DSfGUmschaltung   =  20000;
  CMRG_Timeout_DSfGRufliste      =  20000;
  CMRG_Timeout_DSfGRufQuittung   =  20000;

  { Timeouts f�r DSfG-Abruf (k�nnen per INI-File angepasst werden) }
  CDSfG_Timeout_Verbindungsaufbau = 120000;
  CDSfG_Timeout_Verbindungsabbau  =   5000;
  CDSfG_Timeout_RufAnnahme        =  60000;
  CDSfG_Timeout_Login             =  10000;
  CDSfG_Timeout_DFUETransparent   =   5000;
  CDSfG_Timeout_Archive           = 180000;
  CDSfG_Timeout_Logbuecher        = 180000;
  CDSfG_Timeout_Datenelemente     =  90000;
  // ab 15.01.2004: Timeout f�r Konfiguration-Lesen entf�llt, stattdessen wird
  //                von 180 auf 90 s verk�rzter Timeout f�r Datenelemente verwendet
  CDSfG_Timeout_Einstellen        =  10000;
  CDSfG_Timeout_DFUEParameter     =  20000;
  CDSfG_Timeout_Binaerdatei       =  30000;

  { Fehler-Versuche MRG }
  CMRG_CRCVersuche       = 3;         { bei fehlerhaftem CRC und CRC-Check (wegen MRG800 PTB) }
  CMRG_BCCVersuche       = 3;         { bei fehlerhaftem BCC }
  CMRG_ACK01ProtVersuche = 3;         { bei fehlerhafter Protokoll-Antwort }
  CMRG_FTLProtVersuche   = 5;         { bei neg. Quittierung/fehlerhafter Blocksumme (Trischler FTL-Protokoll) }

  { Fehler-Versuche DSfG }
  CDSfG_BCCVersuche = 3;        { DSfG: max. Anzahl von Versuchen bei fehlerhaftem BCC }

  { Fehler-Versuche Modbus }
  CModbus_LRC_CRCVersuche = 3;  { Modbus: max. Anzahl von Versuchen bei fehlerhaftem LRC (ASCII) bzw. CRC (RTU) }

implementation

end.
