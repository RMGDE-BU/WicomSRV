{******************************************************************************}
{* Unit: Typen und Konstanten für DSfG-Abrufmodul                             *}
{* 17.11.99 WW                                                                *}
{******************************************************************************}
Unit DSysCon;

INTERFACE

uses
  Graphics, AbrufTimeoutConst, WSysCon;

const
  RohdatenLoeschen: boolean =
{$IFDEF GAMESS}
  false;                 { für GAMESS immer Rohdaten erhalten ! }
{$ELSE}
  true;                  { über Kommandozeilenparameter "Debug" deaktivierbar }
{$ENDIF}

  CheckMomHalten: boolean = false;            { Flag zum Prüfen auf "Momentanwerte halten" }

  { Baudrate }

  CMaxBaudrate = 57600;  { max. Baudrate Rechner - Modem;
                           telefonseitige Baudrate paßt sich an rechnerseitige an }
  MaxBaudrate: integer = CMaxBaudrate;

  { Timeouts }

  CTimeout_ModemInit         =   3000;

  CTimeout_Verbindungsaufbau = 120000;
  CTimeout_Verbindungsabbau  =   5000;
  CTimeout_RufAnnahme        =  60000;
  CTimeout_Login             =  10000;
  CTimeout_DFUETransparent   =   5000;
  CTimeout_Archive           = 180000;
  CTimeout_Logbuecher        = 180000;
  CTimeout_Datenelemente     = 180000;
  CTimeout_KonfLesen         =  90000;
  CTimeout_Einstellen        =  10000;
  CTimeout_DFUEParameter     =  20000;
  CTimeout_Binaerdatei       =  30000;

  Timeout_ModemInit         : integer = CTimeout_ModemInit;
  Timeout_Verbindungsaufbau : integer = CTimeout_Verbindungsaufbau;
  Timeout_Verbindungsabbau  : integer = CTimeout_Verbindungsabbau;
  Timeout_RufAnnahme        : integer = CTimeout_RufAnnahme;
  Timeout_Login             : integer = CTimeout_Login;
  Timeout_DFUETransparent   : integer = CTimeout_DFUETransparent;
  Timeout_Archive           : integer = CTimeout_Archive;
  Timeout_Logbuecher        : integer = CTimeout_Logbuecher;
  Timeout_Datenelemente     : integer = CTimeout_Datenelemente;
  Timeout_KonfLesen         : integer = CTimeout_KonfLesen;
  Timeout_Einstellen        : integer = CTimeout_Einstellen;
  Timeout_DFUEParameter     : integer = CTimeout_DFUEParameter;
  Timeout_Binaerdatei       : integer = CTimeout_Binaerdatei;

  Timeout_ModemAntwort      : integer = CTimeout_ModemAntwort;
  BCCVersuche               : integer = CDSfG_BCCVersuche;
                            
  { Automatik-Abruf (können im Bedarfsfall verwendet werden, wenn der Automatikabruf
    über Ordnungsnummer Probleme machern sollte. Einstellbar über INI-File !) }
  CAutomatikZeitabruf      = false;   { Standard-Automatikabruf geht über Ordnungsnummer ! }
  CAutomatikErsteDatenTage = 1;       { Tage zurück ab aktueller PC-Zeit }

  AutomatikZeitabruf     : boolean = CAutomatikZeitabruf;
  AutomatikErsteDatenTage: integer = CAutomatikErsteDatenTage;

  { für kavernenbezogene DSfG-Archivdatenkonvertierung: }
  FW_Zeitfenster: integer = C_FW_Zeitfenster;

  { Programmstati und zugeordnete Farben }

  ps_NichtAktiv      = 0;
  ps_ModemInit       = 1;
  ps_ModemInitFehler = 2;
  ps_KeineVerbindung = 3;
  ps_VerbindungSteht = 4;

  cl_Programmstatus: array [ps_NichtAktiv..ps_VerbindungSteht] of TColor =
    (clRed, clYellow, clRed, clBtnFace, clGreen);

  { Stati des DSfG-Abrufmoduls}

  C_Status_Waiting       = 0;
  C_Status_Abruf         = 1;
  C_Status_Konfiguration = 2;
  C_Status_Momentanwerte = 3;
  C_Status_MomentanDfue  = 4;
  C_Status_RufAbfragen   = 5;

  C_AktDSfGAbrufStatus : byte = C_Status_Waiting;

IMPLEMENTATION

end.
