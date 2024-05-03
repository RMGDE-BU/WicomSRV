{******************************************************************************}
{* Unit: GasX-Statuscodes für ERRTXT32.DLL                                    *}
{* 16.11.2002 WW                                                              *}
{******************************************************************************}
Unit ErrConstGasX;

interface

Const
  // kommandounabhängige Fehlerstati
  GASX_ERR_KOMMANDONICHTPLAUSIBEL     = -99;
  GASX_ERR_KOMMANDONICHTIMPLEMENTIERT = -98;
  GASX_ERR_MRGTYPNICHTIMPLEMENTIERT   = -97;
// frei                                 -96
  GASX_ERR_KOMMANDOSOLLPROZESSID      = -95;    // Prozess-ID ungleich Soll-Prozess-ID aus v-Kommando
  GASX_ERR_SIGNATURLIZENZ             = -94;    // Signaturprüfung nicht lizenziert; 14.04.2014, WW
  GASX_ERR_LIZENZ_ZEIT_ABGELAUFEN     = -93;
// frei                                 -92     // reserviert von Gas-X für allg. Lizenz-Fehler

  // Fehlerstati für  Verbindungsaufbau/-abbau
  GASX_ERR_V_KENNERR_VERBINDUNG       = -1;
  //  Timeout -2 (siehe allgemeine Fehlerstati)
  GASX_ERR_V_COM_NICHT_FREI           = -3;
  GASX_ERR_V_KENNERR_KEINE_VERBINDUNG = -4;
  GASX_ERR_V_PWERR_KEINE_VERBINDUNG   = -5;
  //  Sonstiger Fehler -6 (siehe allgemeine Fehlerstati)
  //  Übertragungsfehler -7 (siehe allgemeine Fehlerstati)
  GASX_ERR_V_LEITUNG_BESETZT          = -8;
  GASX_ERR_V_LEITUNG_MODEM_GESTOERT   = -9;
  GASX_ERR_V_MODEM_FALSCH             = -11;    // verwendetes DÜ-Gerät für Stationsabruf nicht verwendbar

  // Allgemeine Fehlerstati für übrige Befehle:
  //  B-Befehl (MRG: Parameter, DSfG: Datenelemente),
  //  E-Befehl (MRG: Messwerte/Tagessätze, DSfG: Archive),
  //  M-Befehl (MRG: Meldungen, DSfG: Logbücher),
  //  X-Befehl (nur MRG: Prüfungssätze),
  //  }-Befehl (nur MRG: DSfG-Umleitung),
  //  I-Befehl (nur DSfG: Busanalyse)
  //  Z-Befehl (nur MRG: Zeitsynchronisation)
  GASX_ERR_DATENZUGRIFF_INTERN         = -1;  // nicht bei }-Befehl
  GASX_ERR_DSfGUMSCHALTUNG_AUF_ADRESSE = -1;  // nur bei }-Befehl
  GASX_ERR_TIMEOUT                     = -2;
  GASX_ERR_VERB_UNTERBROCHEN           = -3;
  GASX_ERR_PW_PWNR_FALSCH              = -5;  // nicht bei B-, C-Befehl
  GASX_ERR_PARAMETER_UNBEK             = -5;  // nur bei B-, C-Befehl
  GASX_ERR_SONSTIG                     = -6;
  GASX_ERR_UEBERTRAGUNGSFEHLER         = -7;
  GASX_ERR_SIGNATURFEHLER              = -8;  // nur bei B-, E-, M-Befehl; 14.04.2014, WW

  GASX_ERR_PARAMETRIERUNG_ABGEWIESEN   =  8;  // für Zeitsynchronisation, Rundpufferreset, Parameteränderung

  // speziell für Z-Befehl:
  GASX_ERR_ZSYNC_DURCHGEFUEHRT         = 10;
  GASX_ERR_ZSYNC_ABWEICHUNG_ZU_GROSS   = 11;

implementation

end.

