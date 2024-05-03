{******************************************************************************}
{* Unit: Timeout-Konstanten für Abrufsystem                                   *}
{* Version: 30.12.2003  WW                                                    *}
{******************************************************************************}
unit AbrufTimeoutConst;

interface

const
  { Timeouts für Modem-Kommunikation }
  CTimeout_ModemAntwort =  500;  { Zeit in ms, nach der nicht mehr mit neuen
                                  Zeichen von einem Modem gerechnet wird }
  CTimeout_ModemInit    = 3000;  { Timeout für Modem-Initialisierung }

  { Timeouts für FUP-Kommunikation }
  CMRG_Timeout_FupAntwort =  5000;
  CMRG_Timeout_FupReset   = 30000;

  { Timeouts für MRG-Modem-Kommunikation }
  CMRG_Timeout_CRCCheck         =  5000;  { bei Check der CRC-Konfiguration im MRG 800 PTB, EC694 }
  CMRG_Timeout_ACK01ProtMeldung = 20000;  { beim Warten auf Protokoll-Bestätigung (ACK, NAK etc.) }
  CMRG_Timeout_IEC1107Telegr    = 10000;  { lt. IEC 1107 antwortet das Gerät zwar nach max. 1,5 s,
                                            ansonsten liegt ein Fehler vor. Trotzdem lieber etwas mehr... }

  { Fehler-Versuche MRG }
  CMRG_CRCVersuche       = 3;         { bei fehlerhaftem CRC und CRC-Check (wegen MRG800 PTB) }
  CMRG_BCCVersuche       = 3;         { bei fehlerhaftem BCC }
  CMRG_ACK01ProtVersuche = 3;         { bei fehlerhafter Protokoll-Antwort }

  { Fehler-Versuche DSfG }
  CDSfG_BCCVersuche = 3;        { DSfG: max. Anzahl von Versuchen bei fehlerhaftem BCC }

  { Fehler-Versuche MDE }
  CMDE_CRCVersuche = 3;         { MDE: max. Anzahl von Versuchen bei fehlerhaftem CRC }

implementation

end.
