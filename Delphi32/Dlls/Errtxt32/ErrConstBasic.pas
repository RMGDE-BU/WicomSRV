{******************************************************************************}
{* Unit: Vereinfachte Basis-Fehlercodes für ERRTXT32.DLL                      *}
{* 16.08.2011 WW  für MDE-Wegevorgabeprogramm                                 *}
{******************************************************************************}
Unit ErrConstBasic;

interface

Const
  { Fehlercodes für MDE-Wegevorgabeprogramm }
  BASIC_ERR_OK                           =  0;	// OK
  BASIC_ERR_Uebertragungsfehler          =  1;	// Übertragungsfehler
  BASIC_ERR_Loginfehler                  =  2;	// Einloggen nicht möglich (Passwort falsch)
  BASIC_ERR_Lizenz                       =  3;  // Lizenzfehler (Rechner, Programm, Gerätetyp)
  BASIC_ERR_ServerKommando               =  4;	// Fehler Server-Kommando (unplausibel, ungültig etc.)
  BASIC_ERR_Fehler_Datenschreiben        =  5;	// Datenfiles konnten nicht erzeugt/beschrieben werden
  BASIC_ERR_Geraetedaten_nicht_plausibel =  6;	// Gerätedaten unplausibel
  BASIC_ERR_Kennung_falsch               =  7;	// Kennung falsch
  BASIC_ERR_Fehler_ZeitSynch             =  9;	// Fehler bei Zeitsynchronisation
  BASIC_ERR_BCC_Fehler                   = 10;	// Blockprüfzeichen-Fehler (CRC bzw. BCC je nach Gerät)
  BASIC_ERR_Baud_nicht_unterstuetzt      = 11;	// Gerät mit nicht unterstützter Baudrate
  BASIC_ERR_Fehler_COM_PortOeffnen       = 12;	// Fehler COM-Port öffnen

  BASIC_ERR_Sonstig                      = 98;	// Sonstiger (undefinierter) Basis-Fehlercode

implementation

end.

