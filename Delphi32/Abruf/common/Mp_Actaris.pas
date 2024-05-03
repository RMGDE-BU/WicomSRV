{******************************************************************************}
{* Unit: Parameternummern für Hersteller-Gruppe "Actaris"                     *}
{* 26.08.2008 WW                                                              *}
{******************************************************************************}
Unit MP_Actaris;

interface

Const
  { Corus: }

  CP_ACT_CORUS_DateTime                 = '180102001';
  CP_ACT_CORUS_NextSZChange             = '180102002';
  CP_ACT_CORUS_NextWZChange             = '180102003';
  CP_ACT_CORUS_SZWZChangeMode           = '180102004';
  CP_ACT_CORUS_Firmwareversion_HauptCPU = '180101008';
  CP_ACT_CORUS_Tageswechsel             = '180112003';

  { Zähler: }
  CP_ACT_CORUS_Vb       = '180108001';
  CP_ACT_CORUS_Vn       = '180108002';
  CP_ACT_CORUS_Vb_stoer = '180108003';
  CP_ACT_CORUS_Vn_ges   = '180108004';


  { Sparklog: }

  CP_ACT_SPARKLOG_Firmwareversion = '180201005';

  { Impulseingangskonstanten und Dezimalstellen der Impulseingangskonstanten
    für Impulskanäle: }
  CP_ACT_SPARKLOG_ImpEingKonst: array[1..4] of string [9] = ('180202007',
                                                             '180203007',
                                                             '180204007',
                                                             '180205007');
                                                             
  CP_ACT_SPARKLOG_DezImpEingKonst: array[1..4] of string [9] = ('180202008',
                                                                '180203008',
                                                                '180204008',
                                                                '180205008');

implementation

end.
