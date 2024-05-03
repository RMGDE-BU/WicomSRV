{******************************************************************************}
{* Unit: Allgemeine Parameternummern für Gruppe "Elster"                      *}
{* 22.10.2002 WW                                                              *}
{******************************************************************************}
Unit MP_Elster;

interface

Const
  { DL210, DL220, DL240 und EK260, EK 280: }

  { Gerätestatus: }
  CP_ELS_AktGesamtStatus  = '150101001';
  CP_ELS_AktSystemStatus  = '150101002';
  CP_ELS_RegSystemStatus  = '150101004';
  CP_ELS_AktSystemStatus2 = '150101005';
  CP_ELS_RegSystemStatus2 = '150101006';

  CP_ELS_AktStatusK1  = '150101011';
  CP_ELS_AktStatusK2  = '150101012';
  CP_ELS_AktStatusK3  = '150101013';
  CP_ELS_AktStatusK4  = '150101014';
  CP_ELS_AktStatusK5  = '150101015';
  CP_ELS_AktStatusK6  = '150101016';
  CP_ELS_AktStatusK7  = '150101017';
  CP_ELS_AktStatusK8  = '150101018';
  CP_ELS_AktStatusK9  = '150101019';
  CP_ELS_AktStatusK10 = '150101020';

  CP_ELS_RegStatusK1  = '150101021';
  CP_ELS_RegStatusK2  = '150101022';
  CP_ELS_RegStatusK3  = '150101023';
  CP_ELS_RegStatusK4  = '150101024';
  CP_ELS_RegStatusK5  = '150101025';
  CP_ELS_RegStatusK6  = '150101026';
  CP_ELS_RegStatusK7  = '150101027';
  CP_ELS_RegStatusK8  = '150101028';
  CP_ELS_RegStatusK9  = '150101029';
  CP_ELS_RegStatusK10 = '150101030';

  { Gerätedaten: }
  CP_ELS_Tagesgrenze2       = '150102002';
  CP_ELS_Lieferantenschloss = '150102003';
  CP_ELS_Kundenschloss      = '150102004';
  CP_ELS_Stationsnummer     = '150102006';
  CP_ELS_DatumZeit          = '150102011';

  { Tagesgrenzen für Impulskanäle (bislang nur für 1-Kanal-Gerät DL210 relevant): }
  CP_ELS_Tagesgrenze: array[1..1] of string [9] = ('150103101');

  { cp-Werte für Impulskanäle: }
  CP_ELS_cpWert: array[1..4] of string [9] = ('150103081',
                                              '150103082',
                                              '150103083',
                                              '150103084');

  { Meßbereiche Temperatur: }
  CP_ELS_MBoben_T  = '150111001';
  CP_ELS_MBunten_T = '150111003';

  { Meßbereiche Druck: }
  CP_ELS_MBoben_p  = '150111002';
  CP_ELS_MBunten_p = '150111004';


  { DS-100: }

  { Gerätenummern: }
  CP_ELS_DS100_GeraeteNr: array[1..4] of string [9] = ('150210061',
                                                       '150210062',
                                                       '150210063',
                                                       '150210064');

  { cp-Werte für Impulskanäle: }
  CP_ELS_DS100_cpWert: array[1..4] of string [9] = ('150210071',
                                                    '150210072',
                                                    '150210073',
                                                    '150210074');
  { aktuelle Gesamtzähler: }
  CP_ELS_DS100_Gesamtzaehler: array[1..4] of string [9] = ('150210101',
                                                           '150210102',
                                                           '150210103',
                                                           '150210104');
  { aktuelle setzbare Zähler: }
  CP_ELS_DS100_SetzbZaehler: array[1..4] of string [9] = ('150210111',
                                                          '150210112',
                                                          '150210113',
                                                          '150210114');
  { Datum/Zeit: }
  CP_ELS_DS100_DatumZeit: array[1..4] of string [9] = ('150210131',
                                                       '150210132',
                                                       '150210133',
                                                       '150210134');
  { setzbare Zähler, Monatsendwert: }
  CP_ELS_DS100_SetzbZaehlerMonat: array[1..4] of string [9] = ('150210141',
                                                               '150210142',
                                                               '150210143',
                                                               '150210144');


  { Gerätezustände (Virtuelle Parameter): }

  { EK 260: }

  CP_ELS_EK260_GerZustand_aktuell     = '150001001';
  CP_ELS_EK260_GerZustand_gespeichert = '150001002';

  { EK 280: }

  CP_ELS_EK280_GerZustand_aktuell     = '150002001';
  CP_ELS_EK280_GerZustand_gespeichert = '150002002';

  { DL 240: }

  CP_ELS_DL240_GerZustand_aktuell     = '150003001';
  CP_ELS_DL240_GerZustand_gespeichert = '150003002';

  { DL 220: }

  CP_ELS_DL220_GerZustand_aktuell     = '150004001';
  CP_ELS_DL220_GerZustand_gespeichert = '150004002';

  { DL 210: }

  CP_ELS_DL210_GerZustand_aktuell     = '150005001';
  CP_ELS_DL210_GerZustand_gespeichert = '150005002';

  { DL 230: }

  CP_ELS_DL230_GerZustand_aktuell     = '150006001';
  CP_ELS_DL230_GerZustand_gespeichert = '150006002';

implementation

end.
