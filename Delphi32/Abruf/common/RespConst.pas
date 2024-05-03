{******************************************************************************}
{* Unit: Konstanten für Abrufserver-Rückgaben an Client                       *}
{* 17.12.2002  WW                                                             *}
{******************************************************************************}
unit RespConst;

interface

const
  { Trennzeichen in Rückgabe }
  C_RespSeparator = ';';

  C_AnzRespSep_Wieser                = 8; // Anzahl der Trennzeichen in Standard-
                                          // Server-Antworten (Wieser-Version)
  C_AnzRespSepDSfGUmschaltung_Wieser = 9; // Anzahl der Trennzeichen in DSfG-Umschaltung-
                                          // Server-Antwort (Wieser-Version)
  C_AnzRespSepRuf_Wieser             = 9; // Anzahl der Trennzeichen in Ruf-
                                          // Server-Antwort (Wieser-Version)
  // 16.08.2011, WW: Anzahl Trennzeichen mit zusätzlichem Basis-Fehlercode

  { XML-Result }
  C_XMLResult_OK     = 'OK';

  { Datencodes }
  dc_KeineDaten     =    0;
  dc_Parameter      =    1;    // DSfG: Datenelemente, Busanalysedaten
  dc_Meldungen      =    2;    // DSfG: Logbuchdaten
  dc_Messwerte      =    4;    // DSfG: Archivdaten (dc_Messwerte + dc_Tagessaetze)
  dc_Tagessaetze    =    8;
  dc_Pruefsaetze    =   16;
  dc_DSfGDfueDaten  =   32;
  dc_ZeitSyncDaten  =   64;
  dc_DSfGAufmTelegr =  128;
  dc_ParaEinstell   =  256;
  dc_Transparent    =  512;
  dc_MRG_Rufliste   = 1024;
  dc_RufDeaktDaten  = 2048;
  dc_FwUpdateDaten  = 4096;
  dc_VerbAutoDetectDaten = 8192;

  { Ruf-Codes: }
  rc_Ruf_steht_nicht_an          = 0;
  rc_Ruf_angenommen_SMS_GPRS_MRG = 1;
  rc_Ruf_steht_an                = 2;
  rc_Ruf_abgebrochen             = 3;
  rc_Ruf_SMS_GPRS_DSfG           = 4;
  rc_SMS_GPRS_unbekannt          = 5;

  { Rohdaten-Kodierung: }
  rdenc_Base64 = 1;  // Code für Base64-Kodierung

implementation

end.
