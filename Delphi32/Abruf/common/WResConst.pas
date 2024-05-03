{******************************************************************************}
{* Unit: Konstanten für Zugriff auf ASCII-Resourcendateien                    *}
{* 04.03.2003 WW                                                              *}
{******************************************************************************}
unit WResConst;

interface

const
  CResTrenner   = ';';  { Feld-Trennzeichen in ASCII-Resourcendateien }
  CResDZTrenner = ' ';  { Trennzeichen zwischen Datum und Zeit in einem Datum/Zeit-Feldwert }
  CResKanBitTrenner = ',';  { Trennzeichen zwischen Kanälen in Kanalbitmaske-Feldwert }
  CResTrennerPipe = '|';

  CResTrue = 'T';  { Feldwert "True" }

  CResAbrufParaJaAlle  = 'T';  { Feldwert "Ja, alle Parameter" }
  CResAbrufParaJaMenge = 'M';  { Feldwert "Ja, Menge von Parametern" }

implementation

end.
