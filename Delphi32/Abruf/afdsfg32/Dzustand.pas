{******************************************************************************}
{* Unit: Zustandscodes für DSfG-Abrufmodul                                    *}
{* 17.11.1999 WW                                                              *}
{******************************************************************************}
Unit DZustand;

INTERFACE

Const

  { Zustandscodes }

  z_RufAbfragen               = -8;
  z_ModemInitFehler           = -7;                        { Programmzustände }
  z_LogComNichtZugeordnet     = -6;
  z_ComNichtVorhanden         = -5;
  z_FehlerComOeffnen          = -4;
  z_FehlerModemInit           = -3;
  z_ModemInit                 = -2;
  z_NichtAktiv                = -1;

  z_Bereit                    =  0;                             { Ruhezustand }

  z_VerbindungAufbauen        =  1;                           { Abrufzustände }
  z_VerbindungAbbauen         =  2;
  z_VerbindungSteht           =  3;
  z_KennungAbfragen           =  4;
  z_PasswortUebertragen       =  5;
  z_LoginErfolgt              =  6;
  z_AktiveBusTeilnehmer       =  7;
  z_DFUETransparent           =  8;
  z_TelegrammeLesen           =  9;
  z_ArchiveAbrufen            = 10;
  z_LogbuecherAbrufen         = 11;
  z_DatenelementeAbrufen      = 12;
  z_ZeitangabenAbrufen        = 13;
  z_ArchiveKonvertieren       = 14;
  z_LogbuecherKonvertieren    = 15;
  z_DatenelementeKonvertieren = 16;
  z_AllgInstKonfigAbrufen     = 17;
  z_ArchivLogbKonfigAbrufen   = 18;
  z_KonfigurationKonvertieren = 19;
  z_DFUEVersionsdaten         = 20;
  z_DatenelementeUebertragen  = 21;
  z_JournalReorganisieren     = 22;
  z_DFUEParameterAbrufen      = 23;
  z_DFUEParameterUebertragen  = 24;
  z_VerbindungHalten          = 25;
  z_BinaerdateiAbrufen        = 26;
  z_RufAnnehmen               = 27;
  z_RufDeaktivieren           = 28;
  z_RufReaktivieren           = 29;
  z_VerbindungAufbauenTCPIP   = 30;
  z_KavArchiveKonvertieren    = 31;

  ZustandStr: Array [z_RufAbfragen..z_KavArchiveKonvertieren] of string =
    ('Ruf abfragen',
     'Modem kann nicht initialisiert werden',
     'Logische COM nicht zugeordnet',
     'COM nicht vorhanden',
     'COM nicht zu öffnen',
     'Modem nicht initialisiert',
     'Modem initialisieren',
     'Nicht aktiv',

     'Bereit',

     'Wähle',
     'Verbindung abbauen',
     'Verbindung steht',
     'Kennung überprüfen',
     'Paßwort übertragen',
     'Login erfolgt',
     'Aktive Busteilnehmer ermitteln',
     'DFÜ-Instanz transparent schalten',
     'Telegramme entgegennehmen',
     'Archiv abrufen',
     'Logbuch abrufen',
     'Datenelemente abrufen',
     'Zeitangaben abrufen',
     'Archive konvertieren',
     'Logbücher konvertieren',
     'Datenelemente konvertieren',
     'Allg. Instanzkonfiguration abrufen',
     'Archiv-/Logbuchkonfiguration abrufen',
     'Konfigurationsdaten konvertieren',
     'DFÜ-Konfiguration abfragen',
     'Datenelemente übertragen',
     'Journal reorganisieren',
     'DSfG-DFÜ-Parameter abrufen',
     'DSfG-DFÜ-Parameter übertragen',
     'Verbindung halten',
     'Binärdateibefehl abrufen',
     'Ruf annehmen',
     'Ruffunktion in der DSfG-DFÜ deaktivieren',
     'Ruffunktion in der DSfG-DFÜ reaktivieren',
     'Verbinde mit',
     'Kavernenbezogene Archive konvertieren'
     );

IMPLEMENTATION

end.
