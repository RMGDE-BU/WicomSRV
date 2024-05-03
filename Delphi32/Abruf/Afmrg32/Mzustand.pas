{******************************************************************************}
{* Unit: Zustandscodes für MRG-Abrufmodul                                     *}
{* 08.01.1999 WW                                                              *}
{******************************************************************************}
Unit MZustand;

INTERFACE

Const

  { Zustandscodes }

  z_FupResetFehler          =-12;                          { Programmzustände }
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

  z_VerbindungAufbauen      =  1;                             { Abrufzustände }
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
    ('FUP kann nicht zurückgesetzt werden',
     'FUP kann nicht initialisiert werden',
     'Modem kann nicht initialisiert werden',
     'Logische COM nicht zugeordnet',
     'COM nicht vorhanden',
     'COM nicht zu öffnen',
     'Nicht aktiv',
     'Ruf abfragen',
     'FUP zurücksetzen',
     'FUP initialisieren',
     'FUP-Version abfragen',
     'Modem initialisieren',

     'Bereit',

     'Wähle',
     'Verbindung abbauen',
     'Verbindung steht',
     'Kennung überprüfen',
     'Login erfolgt',
     'Parameter abrufen',
     'Meldungen abrufen',
     'Meßwerte abrufen',
     'Tagessätze abrufen',
     'Prüfungssätze abrufen',
     'Parameter konvertieren',
     'Meldungen konvertieren',
     'Meßwerte konvertieren',
     'Prüfungssätze konvertieren',
     'Umschalten auf DSfG-Teilnehmer',
     'Rundpuffer für Meldungen zurücksetzen',
     'Rundpuffer für Meßwerte zurücksetzen',
     'Rundpuffer für Prüfungssätze zurücksetzen',
     'Zeit synchronisieren',
     'Ruf annehmen',
     'Rufanregungsliste abfragen',
     'Ruf quittieren',
     'Ruffunktion im Gerät deaktivieren',
     'Ruffunktion im Gerät reaktivieren',
     'Parameter übertragen',
     'Rückruf auslösen',
     'Binärdateibefehl senden',
     'Journal reorganisieren',
     'Paßwort übertragen',
     'Gerätekonfiguration ermitteln',
     'Gerätezeit überprüfen',
     'Initialisierungssequenz senden'
     );

IMPLEMENTATION

end.
