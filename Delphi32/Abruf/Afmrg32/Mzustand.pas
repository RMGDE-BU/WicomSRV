{******************************************************************************}
{* Unit: Zustandscodes f�r MRG-Abrufmodul                                     *}
{* 08.01.1999 WW                                                              *}
{******************************************************************************}
Unit MZustand;

INTERFACE

Const

  { Zustandscodes }

  z_FupResetFehler          =-12;                          { Programmzust�nde }
  z_FupInitFehler           =-11;
  z_ModemInitFehler         =-10;
  z_LogComNichtZugeordnet   = -9;
  z_ComNichtVorhanden       = -8;
  z_FehlerComOeffnen        = -7;
  z_NichtAktiv              = -6;
  z_RufAbfragen             = -5;
  z_FupReset                = -4;
  z_FupInit                 = -3;
  z_FupVersion              = -2;
  z_ModemInit               = -1;

  z_Bereit                  =  0;                               { Ruhezustand }

  z_VerbindungAufbauen      =  1;                             { Abrufzust�nde }
  z_VerbindungAbbauen       =  2;
  z_VerbindungSteht         =  3;
  z_KennungAbfragen         =  4;
  z_LoginErfolgt            =  5;
  z_ParameterAbrufen        =  6;
  z_MeldungenAbrufen        =  7;
  z_MesswerteAbrufen        =  8;
  z_TagessaetzeAbrufen      =  9;
  z_PruefungssaetzeAbrufen  = 10;
  z_ParameterKonvertieren   = 11;
  z_MeldungenKonvertieren   = 12;
  z_MesswerteKonvertieren   = 13;
  z_PruefsaetzeKonvertieren = 14;
  z_DSFGUmschaltung         = 15;
  z_ResetRundpufferMeld     = 16;
  z_ResetRundpufferMess     = 17;
  z_ResetRundpufferPruef    = 18;
  z_ZeitSynchronisieren     = 19;
  z_RufAnnehmen             = 20;
  z_RuflisteAbfragen        = 21;
  z_RufQuittieren           = 22;
  z_RufDeaktivieren         = 23;
  z_RufReaktivieren         = 24;
  z_ParameterUebertragen    = 25;
  z_RueckrufAusloesen       = 26;
  z_BinaerdateiAbrufen      = 27;
  z_JournalReorganisieren   = 28;
  z_PasswortUebertragen     = 29;
  z_GeraeteKonfiguration    = 30;
  z_GeraetezeitAbfragen     = 31;
  z_InitSequenz1107         = 32;

  ZustandStr: Array [z_FupResetFehler..z_InitSequenz1107] of string =
    ('FUP kann nicht zur�ckgesetzt werden',
     'FUP kann nicht initialisiert werden',
     'Modem kann nicht initialisiert werden',
     'Logische COM nicht zugeordnet',
     'COM nicht vorhanden',
     'COM nicht zu �ffnen',
     'Nicht aktiv',
     'Ruf abfragen',
     'FUP zur�cksetzen',
     'FUP initialisieren',
     'FUP-Version abfragen',
     'Modem initialisieren',

     'Bereit',

     'W�hle',
     'Verbindung abbauen',
     'Verbindung steht',
     'Kennung �berpr�fen',
     'Login erfolgt',
     'Parameter abrufen',
     'Meldungen abrufen',
     'Me�werte abrufen',
     'Tagess�tze abrufen',
     'Pr�fungss�tze abrufen',
     'Parameter konvertieren',
     'Meldungen konvertieren',
     'Me�werte konvertieren',
     'Pr�fungss�tze konvertieren',
     'Umschalten auf DSfG-Teilnehmer',
     'Rundpuffer f�r Meldungen zur�cksetzen',
     'Rundpuffer f�r Me�werte zur�cksetzen',
     'Rundpuffer f�r Pr�fungss�tze zur�cksetzen',
     'Zeit synchronisieren',
     'Ruf annehmen',
     'Rufanregungsliste abfragen',
     'Ruf quittieren',
     'Ruffunktion im Ger�t deaktivieren',
     'Ruffunktion im Ger�t reaktivieren',
     'Parameter �bertragen',
     'R�ckruf ausl�sen',
     'Bin�rdateibefehl senden',
     'Journal reorganisieren',
     'Pa�wort �bertragen',
     'Ger�tekonfiguration ermitteln',
     'Ger�tezeit �berpr�fen',
     'Initialisierungssequenz senden'
     );

IMPLEMENTATION

end.
