{******************************************************************************}
{* Unit: gerätespezifische Parameternummern für MRG 910, MRG 800PTB           *}
{* 23.05.2002 WW                                                              *}
{******************************************************************************}
Unit Mp_910;

interface

Const
  { gemeinsame Parameternummern für MRG 800 PTB und MRG 910: }

  CP_910_800PTB_Kanalzahl = '032';         { Anzahl Impulskanäle }

  { Impulskanäle: }
  CP_910_800PTB_ImpEinheit: array [1..4] of string = ('027', '028', '029', '101');


  { Parameternummern für MRG 910: }

  { Analogkanäle: }
  CP_910_AnalogKanalzahl  = '144';
  CP_910_AnalogKanalAktiv = '163';

  CP_910_MBoben    : array [1..2] of string = ('113', '114');
  CP_910_MBunten   : array [1..2] of string = ('115', '116');
  CP_910_AnaEinheit: array [1..2] of string = ('117', '118');
  CP_910_KommaSt   : array [1..2] of string = ('119', '120');
  CP_910_StromBer  : array [1..2] of string = ('121', '122');

  { Parameternummern für MRG 800 (alt): }
  
  CP_800_Faktor: array [1..3] of string = ('005', '007', '009');
  CP_800_Teiler: array [1..3] of string = ('006', '008', '010');

implementation

end.
