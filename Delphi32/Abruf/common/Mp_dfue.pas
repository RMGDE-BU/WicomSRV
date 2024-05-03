{******************************************************************************}
{* Unit: DFÜ Parameternummern für Gruppe "Fernübertragung"                    *}
{* 01.10.1999 WW                                                              *}
{* 26.10.1999 SM                                                              *}
{******************************************************************************}
Unit MP_DFUE;

interface

Const
  CP_DFUE_Anrvz1 = '040200001'; { Anrufverzögerungen }
  CP_DFUE_Anrvz2 = '040200002';
  CP_DFUE_Anrvz3 = '040200003';
  CP_DFUE_Anrvz4 = '040200004';
  CP_DFUE_Anrvz5 = '040200005';
  CP_DFUE_Anrvz6 = '040200006';
  CP_DFUE_MaxWh  = '040200007';

  CP_DFUE_Blocklaenge = '040100001';  {Datenblocklänge Übertragung}
  CP_DFUE_ZUebA       = '040100004'; { Telegrammzeitüberwachungszähler}
  CP_DFUE_ZUebB       = '040100005';
  CP_DFUE_ZUebC       = '040100006';

  CP_DFUE_PW1 = '040300001'; { Passwörter 1..4}
  CP_DFUE_PW2 = '040300002';
  CP_DFUE_PW3 = '040300003';
  CP_DFUE_PW4 = '040300004';

  CP_DFUE_RufMRG    = '040400001';   { Rufnummern MRG/Zentrale }
  CP_DFUE_Zentrale1 = '040400002';
  CP_DFUE_Zentrale2 = '040400003';

implementation

end.
