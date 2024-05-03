{******************************************************************************}
{* Unit: Typen und Konstanten f�r Modbus-Master                               *}
{* 15.02.2010  WW                                                             *}
{* 25.11.2014  WN                                                             *}
{* 20.03.2024  WW Fehlertexte f�r IP                                          *}
{******************************************************************************}
Unit ModbusMasterConst;

INTERFACE

uses
  Messages;

const
  { aktuelle Version Modbus-Master }
  CVersion_ModbusMaster = '1.2.5';  { aktuelle Version Modbus-Master }

  { Windows-Botschaften }
  WM_TASKBAREVENT = WM_USER + 1;

  { Logfile-Fehlertexte (nur deutsch) }
  CMsgSrvStarted          = 'Dienst ist gestartet';
  CMsgSrvStopped          = 'Dienst ist beendet';

  CMsgModbusMasterActive   = 'Modbus-Master ist aktiv';   
  CMsgModbusMasterInactive = 'Modbus-Master ist nicht aktiv:';

  CMsgInvalidCOMIPConfig = 'INI-Konfiguration enth�lt ung�ltige COM-Nummer (g�ltig: 1..%d) ' +
    'oder IP-Nummer (g�ltig: %d..%d)';  // 20.03.2024, WW
  CMsgNoCOMIPConfig = 'INI-Konfiguration enth�lt keine COM- oder IP-Einstellungen';  // 20.03.2024, WW

resourcestring
  { Fehlertexte f�r Fensterausgabe }
  SMsgModbusMasterActive = CMsgModbusMasterActive;
  SMsgModbusMasterInactive = CMsgModbusMasterInactive;

  SMsgInvalidCOMIPConfig = CMsgInvalidCOMIPConfig;
  SMsgNoCOMIPConfig = CMsgNoCOMIPConfig;

type
  TMonitorDataType = (mdt_None,  // kein spezifischer Monitordatentyp
                      mdt_Tx,    // Sendedaten
                      mdt_Rx);   // Empfangsdaten

IMPLEMENTATION

end.
