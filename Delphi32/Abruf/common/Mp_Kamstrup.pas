{******************************************************************************}
{* Unit: Parameternummern für Hersteller-Gruppe "Kamstrup"                    *}
{* 27.03.2013 WW                                                              *}
{******************************************************************************}
Unit MP_Kamstrup;

interface

Const
  { UNIGAS 300: }
  CP_KAM_UNIGAS300_Gastag = '200112002';
  CP_KAM_UNIGAS300_Zeit   = '200112003';
  CP_KAM_UNIGAS300_Datum  = '200112004';

  { Teilfaktoren der Impulseingänge und Funktionalität Eingang 1: }
  CP_KAM_UNIGAS300_TeilfaktorEing: array[1..3] of string [9] = ('200104001',
                                                                '200104002',
                                                                '200104003');

  CP_KAM_UNIGAS300_TeilfaktorEing1_HF   = '200104004';
  CP_KAM_UNIGAS300_FunktionalitaetEing1 = '200104005';

  { Messbereichsgrenzen für Druck und Temperatur }
  CP_KAM_UNIGAS300_pMin = '200105005';
  CP_KAM_UNIGAS300_pMax = '200105006';
  CP_KAM_UNIGAS300_tMin = '200105007';
  CP_KAM_UNIGAS300_tMax = '200105008';   

implementation

end.
