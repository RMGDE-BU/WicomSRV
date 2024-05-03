{******************************************************************************}
{* Unit: Parameternummern f�r Hersteller-Gruppe "SICK"                        *}
{* 29.09.2021 WW                                                              *}
{******************************************************************************}
Unit MP_SICK;

interface

Const
  { FLOWSIC500: }
  CP_SICK_FLOWSIC500_VersionKommTreiber = '210103100';
  CP_SICK_FLOWSIC500_VersionFW          = '210103101';
  CP_SICK_FLOWSIC500_VersionDSP         = '210103106';

  CP_SICK_FLOWSIC500_Kennung     = '210103128';  // Device tag, Ger�tebezeichnung
  CP_SICK_FLOWSIC500_Status      = '210103200';  // Ger�testatus
  CP_SICK_FLOWSIC500_EichStatus  = '210103210';  // Eichrelevanter Status
  CP_SICK_FLOWSIC500_AccessLevel = '210103256';  // Access level (Parametrierschutz-Status)

(* !! CP_SICK_FLOWSIC500_FaktorVb = '210104100';  // Aufl�sung Vb-Z�hlwerke
  CP_SICK_FLOWSIC500_FaktorVn = '210104101';  // Aufl�sung Vn-Z�hlwerke *)

  CP_SICK_FLOWSIC500_Datum = '210104300';  // Datum (ddmmyyyy)
  CP_SICK_FLOWSIC500_Zeit  = '210104302';  // Zeit (hhmmss)
  CP_SICK_FLOWSIC500_Zeitstempel_UTC  = '210104305';  // Zeitstempel UTC
  CP_SICK_FLOWSIC500_Zeitzone_Minuten = '210104347';  // Zeitzone in Minuten

  CP_SICK_FLOWSIC500_Tagesende = '210105998';  // Gasstunde (hhmm)

  // Ereignislogbuch
  CP_SICK_FLOWSIC500_EreignisLB_RecCount   = '210106202';
  CP_SICK_FLOWSIC500_EreignisLB_RecMax     = '210106203';
  CP_SICK_FLOWSIC500_EreignisLB_RecSize    = '210106204';
  CP_SICK_FLOWSIC500_EreignisLB_RecType    = '210106205';
  CP_SICK_FLOWSIC500_EreignisLB_RecNextPos = '210106206';

  // Parameterlogbuch
  CP_SICK_FLOWSIC500_ParaLB_RecCount   = '210106212';
  CP_SICK_FLOWSIC500_ParaLB_RecMax     = '210106213';
  CP_SICK_FLOWSIC500_ParaLB_RecSize    = '210106214';
  CP_SICK_FLOWSIC500_ParaLB_RecType    = '210106215';
  CP_SICK_FLOWSIC500_ParaLB_RecNextPos = '210106216';

  // Eichamtliches Logbuch
  CP_SICK_FLOWSIC500_MetrologLB_RecCount   = '210106222';
  CP_SICK_FLOWSIC500_MetrologLB_RecMax     = '210106223';
  CP_SICK_FLOWSIC500_MetrologLB_RecSize    = '210106224';
  CP_SICK_FLOWSIC500_MetrologLB_RecType    = '210106225';
  CP_SICK_FLOWSIC500_MetrologLB_RecNextPos = '210106226';

  // Messperiodenarchiv
  CP_SICK_FLOWSIC500_MPArchiv_RecCount   = '210106232';
  CP_SICK_FLOWSIC500_MPArchiv_RecMax     = '210106233';
  CP_SICK_FLOWSIC500_MPArchiv_RecSize    = '210106234';
  CP_SICK_FLOWSIC500_MPArchiv_RecType    = '210106235';
  CP_SICK_FLOWSIC500_MPArchiv_RecNextPos = '210106236';

  // Gasparameterlogbuch
  CP_SICK_FLOWSIC500_GasParaLB_RecCount   = '210106262';
  CP_SICK_FLOWSIC500_GasParaLB_RecMax     = '210106263';
  CP_SICK_FLOWSIC500_GasParaLB_RecSize    = '210106264';
  CP_SICK_FLOWSIC500_GasParaLB_RecType    = '210106265';
  CP_SICK_FLOWSIC500_GasParaLB_RecNextPos = '210106266';  

  // Z�hler Vb, Vn
(* !! CP_SICK_FLOWSIC500_CustomVm    = '210101017';
  CP_SICK_FLOWSIC500_CustomVmErr = '210101019';
  CP_SICK_FLOWSIC500_CustomVb    = '210101021';
  CP_SICK_FLOWSIC500_CustomVbErr = '210101023';
  CP_SICK_FLOWSIC500_CustomVbTot = '210101025';  *)

  CP_SICK_FLOWSIC500_Vm    = '210104102';
  CP_SICK_FLOWSIC500_VmErr = '210104104';
  CP_SICK_FLOWSIC500_Vb    = '210104106';
  CP_SICK_FLOWSIC500_VbErr = '210104108';
  CP_SICK_FLOWSIC500_VbTot = '210104110';

(* !! CP_SICK_FLOWSIC500_MAXLOAD_VNMPA_DELTA = '210106502';
  CP_SICK_FLOWSIC500_MAXLOAD_VNTGA_DELTA = '210106505';
  CP_SICK_FLOWSIC500_MAXLOAD_VNMOA_DELTA = '210106508';
  CP_SICK_FLOWSIC500_MAXLOAD_VNMPA_MAX   = '210106511';
  CP_SICK_FLOWSIC500_MAXLOAD_VNTGA_MAX   = '210106517';
  CP_SICK_FLOWSIC500_MAXLOAD_VBMPA_DELTA = '210106523';
  CP_SICK_FLOWSIC500_MAXLOAD_VBTGA_DELTA = '210106525';
  CP_SICK_FLOWSIC500_MAXLOAD_VBMOA_DELTA = '210106527';
  CP_SICK_FLOWSIC500_MAXLOAD_VBMPA_MAX   = '210106529';
  CP_SICK_FLOWSIC500_MAXLOAD_VBTGA_MAX   = '210106535';
  CP_SICK_FLOWSIC500_MAXLOAD_VNMP_DELTA  = '210106600';
  CP_SICK_FLOWSIC500_MAXLOAD_VNTG_DELTA  = '210106604';
  CP_SICK_FLOWSIC500_MAXLOAD_VNMO_DELTA  = '210106608';
  CP_SICK_FLOWSIC500_MAXLOAD_VNMP_MAX    = '210106612';
  CP_SICK_FLOWSIC500_MAXLOAD_VNTG_MAX    = '210106618';
  CP_SICK_FLOWSIC500_MAXLOAD_VBMP_DELTA  = '210106624';
  CP_SICK_FLOWSIC500_MAXLOAD_VBTG_DELTA  = '210106626';
  CP_SICK_FLOWSIC500_MAXLOAD_VBMO_DELTA  = '210106628';
  CP_SICK_FLOWSIC500_MAXLOAD_VBMP_MAX    = '210106630';
  CP_SICK_FLOWSIC500_MAXLOAD_VBTG_MAX    = '210106636';
*)
implementation

end.
