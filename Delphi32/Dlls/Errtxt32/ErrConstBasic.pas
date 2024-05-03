{******************************************************************************}
{* Unit: Vereinfachte Basis-Fehlercodes f�r ERRTXT32.DLL                      *}
{* 16.08.2011 WW  f�r MDE-Wegevorgabeprogramm                                 *}
{******************************************************************************}
Unit ErrConstBasic;

interface

Const
  { Fehlercodes f�r MDE-Wegevorgabeprogramm }
  BASIC_ERR_OK                           =  0;	// OK
  BASIC_ERR_Uebertragungsfehler          =  1;	// �bertragungsfehler
  BASIC_ERR_Loginfehler                  =  2;	// Einloggen nicht m�glich (Passwort falsch)
  BASIC_ERR_Lizenz                       =  3;  // Lizenzfehler (Rechner, Programm, Ger�tetyp)
  BASIC_ERR_ServerKommando               =  4;	// Fehler Server-Kommando (unplausibel, ung�ltig etc.)
  BASIC_ERR_Fehler_Datenschreiben        =  5;	// Datenfiles konnten nicht erzeugt/beschrieben werden
  BASIC_ERR_Geraetedaten_nicht_plausibel =  6;	// Ger�tedaten unplausibel
  BASIC_ERR_Kennung_falsch               =  7;	// Kennung falsch
  BASIC_ERR_Fehler_ZeitSynch             =  9;	// Fehler bei Zeitsynchronisation
  BASIC_ERR_BCC_Fehler                   = 10;	// Blockpr�fzeichen-Fehler (CRC bzw. BCC je nach Ger�t)
  BASIC_ERR_Baud_nicht_unterstuetzt      = 11;	// Ger�t mit nicht unterst�tzter Baudrate
  BASIC_ERR_Fehler_COM_PortOeffnen       = 12;	// Fehler COM-Port �ffnen

  BASIC_ERR_Sonstig                      = 98;	// Sonstiger (undefinierter) Basis-Fehlercode

implementation

end.

