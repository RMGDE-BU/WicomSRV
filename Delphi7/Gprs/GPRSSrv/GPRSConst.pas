{******************************************************************************}
{* Unit: Typen und Konstanten für GPRS-Server                                 *}
{* 02.11.2006 WW                                                              *}
{******************************************************************************}
unit GPRSConst;

interface

const
  { aktuelle Version GPRS-Server }
  CVersion_GPRSSrv = '2.2';

resourcestring
  SMsgSrvInactive           = ' Server ist nicht aktiv: ';
  SMsgErrtxt32DLLNotFound   = 'Datei ERRTXT32.DLL wurde nicht gefunden.';
  SMsgPathServerCreateError = 'PathServer konnte nicht initialisiert werden';
  SMsgPathsDBNotFound       = 'Pfade/Datenbanken nicht vorhanden oder ohne Zugriff';

implementation

end.
