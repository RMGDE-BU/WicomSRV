{------------------------------------------------------------------------------}
{ 30.11.1998 GD; Konstanten-Unit für die WinDfu32                              }
{                                                                              }
{ 22.04.1999 GD; Um einige Konstanten aus My_Utils erweitert                   }
{ 11.05.1999 GD; Um Konstanten für STA.DB und WZ_SZ.DB erweitert               }
{ 01.06.1999 GD; Um Konstanten für DSfG erweitert                              }
{ 28.06.1999 GD; Um Konstanten für LAKS erweitert                              }
{ 09.10.2000 GD; Um Konstanten für 'ITyp' erweitert                            }
{ 17.10.2000 GD; Um Konstanten für 'DelList' erweitert                         }
{ 23.11.2000 GD; Um Konstanten für 'WIDISP32' erweitert                        }
{ 12.01.2001 GD; Um Konstanten für DSfGMom/-Param erweitert                    }
{ 18.04.2001 GD; Um Konstanten für DSfGMom/-Param erweitert                    }
{ 10.05.2001 WW; Um Konstanten für DSfG-DFÜ-Parametrierung erweitert           }
{ 12.11.2001 GD; Um Geätebilder- und DSfG-Status- Tabellen erweitert           }
{ 24.01.2002 GD; Um Werteart-Tabelle (DSfG) erweitert                          }
{ 06.02.2002 GD; Um AKA-Parameter-Tabelle (DSfG) erweitert                     }
{ 04.07.2002 WW; Um Felder der Statustabellen (DSfG) erweitert                 }
{ 09.07.2003 HP; Um Felder der GRP.DB erweitert (MRG Gruppenoffset)            }
{ 10.01.2006 GD; Spalten von werteart und delkanaltyp erweitert                }
{ 23.09.2009 WW/GD; Resourcestrings: Datum/Zeit-Formatierungsmasken nur mit 1  }
{                Leerzeichen (wegen Fremdsprachenversion)                      }
{ 05.11.2010 WN; manuelles Nachholen von zur Wiederholung anstehenden SMS      }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 1998, 2010                                    }
{------------------------------------------------------------------------------}
unit WSysCon;

interface

uses
  graphics;

resourcestring
  C_FormatDateTime      = 'dd/mm/yyyy hh:nn:ss';
  SFormatDate           = 'dd/mm/yyyy';
  SFormatDateHourMin    = 'dd/mm/yyyy hh:nn';
  SFormatDateTimeWithMs = 'dd/mm/yyyy hh:nn:ss,zzz';
  S_FormatDateTimeShort = 'dd/mm/ hh:nn';
  S_FormatDateTimeMonthShort = 'dd. mmm. yyyy hh:nn:ss';
  S_FormatTime          = 'hh:nn:ss';
  S_FormatMinSec        = 'nn:ss';

const
  C_FormatDateTimeWithMs = 'dd.mm.yyyy hh:nn:ss,zzz';

  { Flag, ob sich die Applikation in einem Debug-Modus befindet }
  IsDebugFlag: boolean = False;

  { Pseudo-COM-Nummer für TCP/IP-Kommunikation }
  CComTCP_IP = 900;   { wird im Abrufsystem als logische und physikalische
                        Schnittstellen-Nummer verwendet }

  { Flags, welche Gerätetypen im Datenbanksystem vorgesehen sind }
  isDSfG: boolean = False;
  isMRG: boolean = False;
  isLAKS: boolean = False;

  { Datentypen verschlüsselt über Status }
  C_IsMesswerte        = $0001;
  C_IsMeldungen        = $0002;
  C_IsPruefsaetze      = $0004;
  C_IsParameter        = $0008;
  C_IsEBandaenderungen = $0010;
  C_IsArchive          = $0020;
  C_IsLogbuecher       = $0040;
  C_IsDatenelemente    = $0080;
  C_IsBinaerdatei      = $0100;
  C_IsKurzzeitwerte    = $0200;  // per GPRS

  { Datentypen für LAKS-Parametrierung (zusammen mit Abrufart = C_AbrArtParaLesen) }
  C_IsLAKSEBandParameter = $01;

  { Abrufarten }
  C_AbrArtAuto       = 'Auto';
  C_AbrArtManu       = 'Manu';
  C_AbrArtRuf        = 'Ruf';
  C_AbrArtMomStart   = 'MomS';
  C_AbrArtMomStop    = 'MomE';
  C_AbrArtMomHalten  = 'MomH';
  C_AbrArtMomArchiv  = 'MomArchiv';
  C_AbrArtParaStart  = 'ParaStart';
  C_AbrArtParaLesen  = 'ParaLesen';
  C_AbrArtParaSenden = 'ParaSenden';
  C_AbrArtParaEnde   = 'ParaEnde';
  C_AbrArtRufReakt   = 'RufReakt';
  C_AbrArtRueckRuf   = 'RueckRuf';
  C_AbrArtKonfLesen  = 'KonfLesen';
  C_AbrArtMomDfueStart  = 'MomDfueS';
  C_AbrArtMomDfueStop   = 'MomDfueE';
  C_AbrArtMomDfueHalten = 'MomDfueH';
  C_AbrArtBinaerSenden = 'Binaer';

  { Für Anzeigen verwendete Farben - 'clWindow' ist 'keine Farbe' }
  C_ColorMrg         = clAqua;
  C_ColorGruppe      = clYellow;
  C_ColorLAKS        = $0081D75B;
  C_ColorDSfG        = $000080FF; { ab 02.10.2001: orange statt wie bisher
                                    hellrot ($005A5AFF) zur besseren Unterscheidung
                                    von den Journal-Fehlerstatus-Farben rot/grün }

  C_ColorAuto        = $0000F2FF; { Goldgelb }
  C_ColorManu        = $0000D2FF; { Gelbbraun }
  C_ColorMom         = $0000B2FF;
  C_ColorRuf         = $000082FF;
  C_ColorRufReakt    = $0080FF00; { hellgrün }
  C_ColorRueckRuf    = $00AAAA00; { blaugrau }
  C_ColorKonfLesen   = $00FF9090; { hellblau }
  C_ColorMomDfue     = $0072B8B8; { sandbraun }
  C_ColorWieser      = $000080FF; { orange }

  C_ColorBelegtJa    = clRed;
  C_ColorBelegtNein  = clGreen;
  C_ColorVersuch0    = clGreen;
  C_ColorVersuch1    = clRed;
  C_ColorDatentypen  = clLime;

  { Klartext-Anzeigen }
  C_AnzeigeTCP_IP    = 'TCP/IP';

  C_AnzeigeMrg       = 'MRG';
  C_AnzeigeLaks      = 'LAKS';
  C_AnzeigeDSfG      = 'DSfG';
  C_AnzeigeGruppe    = 'Gruppe';

  C_AnzeigeAuto      = 'Automatik';
  C_AnzeigeManu      = 'Manuell';
  C_AnzeigeRuf       = 'Ruf';
  C_AnzeigeMom       = 'Momentan';
  C_AnzeigeRufReakt  = 'Rufreaktivierung';
  C_AnzeigeRueckRuf  = 'Rückruf';
  C_AnzeigeKonfLesen = 'Konfiguration';
  C_AnzeigeMomDfue   = 'DFÜ-Momentan';

  C_AnzeigeBelegtJa  = 'Ja';
  C_AnzeigeBelegtNein = 'Nein';

  { Gerätearten - Einträge in Tabellen }
  C_GerArtMrg        = 'M';
  C_GerArtDSfG       = 'D';
  C_GerArtLAKS       = 'L';
  C_GerArtGruppe     = 'G';
  C_GerArtWICAL      = 'W';
  C_GerArtUnbekannt  = 'U';

  { Benutzer }
  C_BenutzerAuto = 'Auto';
  C_BenutzerManu = 'Manu';

{------------------------------------------------------------------------------}

  { MRG-Typ-Konstanten }

  mrgtyp_MRG3113      =   0;
  mrgtyp_MRG4009      =   1;
  mrgtyp_MRG4013V     =   2;
  mrgtyp_MRG4013Q     =   4;
  mrgtyp_MRG4231      =   5;
  mrgtyp_MRG4229      =   8;
  mrgtyp_MRG3100A     =   9;
  mrgtyp_MRG1029      =  10;
  mrgtyp_MRG3109      =  11;
  mrgtyp_MRG1009      =  12;
  mrgtyp_MRG3129      =  15;
  mrgtyp_MRG3100AF    =  16;
  mrgtyp_MRG4203      =  17;
  mrgtyp_MRG4210      =  18;
  mrgtyp_MRG800       =  20;
  mrgtyp_MRG2001      =  21;
  mrgtyp_MRG2100      =  30;
  mrgtyp_MRG2113      =  31;
  mrgtyp_MRG2115      =  38;
  mrgtyp_MRG2100V1    =  47;
  mrgtyp_MRG800PTB    =  49;
  mrgtyp_MRG2109      = 100;
  mrgtyp_MRG800E      = 101;
  mrgtyp_EC694        = 102;
  mrgtyp_MRG910       = 103;
  mrgtyp_MemoDat65    = 104;
  mrgtyp_EK260        = 105;
  mrgtyp_DL240        = 106;
  mrgtyp_VC2          = 107;
  mrgtyp_KE_Ruhrgas   = 108;
  mrgtyp_FWU          = 109;
  mrgtyp_KE_PPN       = 110;
  mrgtyp_DS100        = 111;
  mrgtyp_TTG_IEC      = 112;
  mrgtyp_Veribox_Mini = 113;
  mrgtyp_DL220        = 114;
  mrgtyp_MRG905       = 115;
  mrgtyp_TTG_FTL      = 116;
  mrgtyp_TDS          = 117;
  mrgtyp_SR           = 118;
  mrgtyp_Corus        = 119;
  mrgtyp_Sparklog     = 120;
  mrgtyp_DL210        = 121;
  mrgtyp_MCO          = 122;
  mrgtyp_EC900        = 123;
  mrgtyp_VC3_VC2komp  = 124;
  mrgtyp_VC3          = 125;
  mrgtyp_Unigas300    = 126;
  mrgtyp_EK280        = 127;
  mrgtyp_DL230        = 128;
  mrgtyp_VCC          = 129;
  mrgtyp_MC2          = 130;
  mrgtyp_Primus       = 131;
  mrgtyp_Prilog       = 132;
  mrgtyp_TME400_VCF   = 133;
  mrgtyp_TME400_VMF   = 134;
  mrgtyp_FLOWSIC500   = 135;
  mrgtyp_RSM200_VCF   = 136;
  mrgtyp_RSM200_VMF   = 137;
  { Allgemeiner Geräteytyp: }
  mrgtyp_Allgemein    = 255;

{------------------------------------------------------------------------------}

  { MRG-Datentypen }
  
  C_MaxMRGDataTypes = 4;    { Meldungen, Messwerte, Tagessätze, Prüfungssätze }

  dt_Meldungen   = 1;
  dt_Messwerte   = 2;
  dt_Tagessaetze = 3;
  dt_Pruefsaetze = 4;

{------------------------------------------------------------------------------}

  { Tabelle für Winter-/Sommerzeit-Umstellung }
  C_TbWZSZ           = 'WZ_SZ.DB';

  C_TfWZSZ_Termin    = 'UmstellungsTermin';  { TDateTime }
  C_TfWZSZ_WZSZ      = 'WZSZ';               { SmallInt: 0 - WZ; 1 - SZ }

  { Tabelle der verfügbare Gerätearten }
  C_TbGeraeteArt     = 'GER_ART.DB';

  C_TfGerArtTyp      = 'GeraeteArtTyp';
  C_TfGerArtName     = 'GeraeteArtName';

  { Tabelle Gruppen-Stammdaten }
  C_TbGrpStamm       = 'GRP.DB';
  C_TbMrgGrp         = 'MRGGRP.DB';

  C_TfGrpId          = 'GruppenIndex';
  C_TfGrpName        = 'GruppenName';
  C_TfGrpPrioritaet  = 'GruppenPrioritaet';
  C_TfGrpOffset      = 'GruppenOffset';

  C_TfMrgId          = 'MrgId';

  { Längen von Stringfeldern in Tabellen }

  szLen_Benutzer   = 20;  { Länge Benutzername }
  szLen_GeraeteArt = 10;  { Länge Geräteart-Kennzeichen }
  szLen_MNrGeraet  = 5;   { Länge gerätespezifischer Meldungsnummern (MRG u. DSfG) }
  szLen_MNrAllg    = 5;   { Länge allgemeiner Meldungsnummern (MRG u. DSfG) }
  szLen_MText      = 80;  { Länge Meldungstext }
  szLen_MTyp       = 1;   { Länge Meldungstyp-Kennzeichen }
  szLen_MArt       = 1;   { Länge Meldungsart-Kennzeichen }
  szLen_Zeitzone   = 1;   { Länge Zeitzonen-Kennzeichen }
  szLen_Bemerkung  = 100; { Länge für Bemerkung }

  
  { FUP-Modemtyp-Konstanten für Abrufserver-Kommando
    Achtung: - Abweichende Werte in StaDfu.db !
             - neue Konstanten für Modemabruf nur in Absprache mit Mummert/Gas-X ! }

  srv_modem_auto     = '0';  { Modem-Datenformat automatisch ermitteln (Kommandorichtung) }
  srv_modem_standard = '0';  { Standard Modem-Datenformat (Antwortrichtung) }

  srv_fup_1200_HDX = 2;  { S2 }
  srv_fup_2400_DX  = 6;  { S6 }

  srv_modem_8N1    = 'A';  { Modemabruf mit Datenformat 8N1 }
  srv_modem_7E1    = 'B';  { Modemabruf mit Datenformat 7E1 }
//srv_modem_???    = 'C';  { reserviert }
  srv_GPRS         = 'G';  { Abruf über bestehende GPRS-Verbindung }
  srv_modbus_TCP_Off = 'M';  { Modbus-Netzwerkabruf nicht mit Modbus TCP; 04.10.2022 WW }
  srv_BT_PuE_K01   = 'P';  { Serieller Abruf über Bluetooth-Adapter P+E K01 }
  srv_modem_SSU    = 'T';  { Abruf Tritschler-Geräte mit Multiplexer SSU }


  { MRG- und DSfG-Meldungen }
  
  { Kürzel für Meldungsarten }

  mart_Hinweis       = 'H';
  mart_Warnung       = 'W';
  mart_Stoerung      = 'S';
  mart_Rechnerfehler = 'R';

  { Kürzel für Meldungstypen }

  mtyp_einwertig  = 'E';
  mtyp_kommt      = 'K';
  mtyp_geht       = 'G';
  mtyp_unbestimmt = 'U';


  { Tabellen für MRG- und DSfG-Meldungen }

  { Haupttabelle für Meldungen }

  C_Tb_WMeldungen = 'WMeldungen.db';

  C_Tf_WMeldungen_MeldungId        = 'MeldungId';
  C_Tf_WMeldungen_Benutzer         = 'Benutzer';
  C_Tf_WMeldungen_GeraeteArt       = 'GeraeteArt';
  C_Tf_WMeldungen_GeraeteId        = 'GeraeteId';        { bei DSfG-Meldungen: InstanzId der Quell-Instanz }
  C_Tf_WMeldungen_InstanzId_Archiv = 'InstanzId_Archiv';
  C_Tf_WMeldungen_LogbuchNr_Archiv = 'LogbuchNr_Archiv';
  C_Tf_WMeldungen_OrdnungsNr       = 'OrdnungsNr';
  C_Tf_WMeldungen_DatumZeit        = 'DatumZeit';
  C_Tf_WMeldungen_Zeitzone         = 'Zeitzone';
  C_Tf_WMeldungen_MNrAllg          = 'MNrAllg';
  C_Tf_WMeldungen_MNrGeraet        = 'MNrGeraet';
  C_Tf_WMeldungen_MText            = 'MText';
  C_Tf_WMeldungen_MTyp             = 'MTyp';
  C_Tf_WMeldungen_MArt             = 'MArt';
  C_Tf_WMeldungen_Quittiert        = 'Quittiert';
  C_Tf_WMeldungen_Bemerkung        = 'Bemerkung';

  C_TI_WMeldungen_ixMeldId = 'ixMeldId';        // Primärindex
  C_TI_WMeldungen_ixDatOrd = 'ixDatOrd';        // Sekundärindex

  { Detail-Tabelle für DSfG-Meldungen }

  C_Tb_WMeldungenDSfG = 'WMeldungenDSfG.db';

  C_Tf_WMeldungenDSfG_MeldungId = 'MeldungId';
  C_Tf_WMeldungenDSfG_Status    = 'Status';
  C_Tf_WMeldungenDSfG_CRC       = 'CRC';

  C_TI_WMeldungenDSfG_ixMeldId = 'ixMeldId';    // Primärindex

  { Detail-Tabelle für Parameteränderungen }

  C_Tb_WMeldungenPara = 'WMeldungenPara.db';

  C_Tf_WMeldungenPara_MeldungId  = 'MeldungId';
  C_Tf_WMeldungenPara_ParaNrAllg = 'ParaNrAllg';
  C_Tf_WMeldungenPara_WertAlt    = 'WertAlt';
  C_Tf_WMeldungenPara_WertNeu    = 'WertNeu';

  C_TI_WMeldungenPara_ixMeldId = 'ixMeldId';    // Primärindex

  { Detail-Tabelle für Alarmierung }

  C_Tb_WMeldungenAlarm = 'WMeldungenAlarm.db';

  C_Tf_WMeldungenAlarm_MeldungId   = 'MeldungId';
  C_Tf_WMeldungenAlarm_PCAlarm     = 'PCAlarm';
  C_Tf_WMeldungenAlarm_RelaisAlarm = 'RelaisAlarm';
  C_Tf_WMeldungenAlarm_VoiceAlarm  = 'VoiceAlarm';
  C_Tf_WMeldungenAlarm_Gedruckt    = 'Gedruckt';

  C_TI_WMeldungenAlarm_ixMeldId = 'ixMeldId';    // Primärindex


  { Konfigurations- und Definitionstabellen für MRG- und DSfG-Meldungen }

  CDBMeldNr          = 'MeldNr.db';
  CDBMeldText        = 'MeldText.db';
  CDBMeldNrStation   = 'MeldNrStation.db';
  CDBMeldTextStation = 'MeldTextStation.db';
  CDBMeldAlarm       = 'MeldAlarm.db';
  CDBMeldGrpDef      = 'MeldGrpDef.db';

  { Tabelle 'MeldNr.db' }

  C_MeldNr_GeraeteArt = 'GeraeteArt';
  C_MeldNr_MeldGrpNr  = 'MeldGrpNr';
  C_MeldNr_MNrGeraet  = 'MNrGeraet';
  C_MeldNr_MNrAllg    = 'MNrAllg';

  { Tabelle 'MeldText.db' }

  C_MeldText_MNrAllg = 'MNrAllg';
  C_MeldText_MText   = 'MText';
  C_MeldText_MArt    = 'MArt';
  C_MeldText_MTyp    = 'MTyp';

  { Tabelle 'MeldNrStation.db' }

  C_MeldNrStation_GeraeteArt = 'GeraeteArt';
  C_MeldNrStation_GeraeteId  = 'GeraeteId';
  C_MeldNrStation_MNrAllg    = 'MNrAllg';
  C_MeldNrStation_MNrStation = 'MNrStation';

  { Tabelle 'MeldTextStation.db' }

  C_MeldTextStation_MNrStation = 'MNrStation';
  C_MeldTextStation_MText      = 'MText';
  C_MeldTextStation_MArt       = 'MArt';

  { Tabelle 'MeldAlarm.db' }

  C_MeldAlarm_GeraeteArt = 'GeraeteArt';
  C_MeldAlarm_GeraeteId  = 'GeraeteId';
  C_MeldAlarm_MNrAllg    = 'MNrAllg';
  C_MeldAlarm_KontaktNr  = 'KontaktNr';

  { Tabelle 'MeldGrpDef.db' }

  C_MeldGrpDef_GeraeteArt = 'GeraeteArt';
  C_MeldGrpDef_MNrAllgMin = 'MNrAllgMin';
  C_MeldGrpDef_MNrAllgMax = 'MNrAllgMax';
  C_MeldGrpDef_Name       = 'Name';

{--------------------------- Konstanten für MRG -------------------------------}

  { MRG-Kanal-Tabelle }

  CDBMrgKanal = 'MrgKanal.db';

  C_MrgKanal_MrgTyp       = 'MrgTyp';
  C_MrgKanal_MrgKanal     = 'MrgKanal';
  C_MrgKanal_KanalId      = 'KanalId';
  C_MrgKanal_Kanalname    = 'Kanalname';
  C_MrgKanal_Kanaltyp     = 'Kanaltyp';
  C_MrgKanal_Einstellbar  = 'Einstellbar';
  C_MrgKanal_Kontroll     = 'Kontroll';
  C_MrgKanal_Eingang      = 'Eingang';
  C_MrgKanal_Einheit      = 'Einheit';
  C_MrgKanal_Kommastellen = 'Kommastellen';


  { Tabelle mit allgemeinen MRG-Parameter-Nummern }

  CDBParamMRG = 'ParamMrg.db';
  CSPara3To5  = 'Para3To5';     { Sekundärindex für ParamMRG }

  C_ParamMRG_Parametergruppe        = 'Parametergruppe';
  C_ParamMRG_Parameternummer        = 'Parameternummer';
  C_ParamMRG_Parameternummer_im_MRG = 'Parameternummer_im_MRG';

  { Tabelle mit MRG-Parameter-Texten }

  C_TbParaText = 'ParaText.db';

  C_TfParameterNummer = 'PARAMETERNUMMER';  { str9 }
  C_TfParameterText   = 'PARAMETERTEXT';    { str 100 }


  { MRG-Parametertabelle (ausgelesene Geräte-Parameter) }

  C_TbMParam         = 'MParam.db';

  C_TfMParaMrgId     = 'MrgId';  { int }
  C_TfMParaNrMrg     = 'Parameternummer_im_MRG';  { str }
  C_TfMParaNr        = 'Parameternummer';   { str }
  C_TfMParaWert      = 'Wert';   { str }

  { MRG-Momentanwerte-Tabellen 'MMomnnnn' }

  CDBMMom = 'MMom';

  C_Mom_Parameternummer_im_MRG = 'Parameternummer_im_MRG';  { str }
  C_Mom_Parameternummer        = 'Parameternummer';  { str }
  C_Mom_Wert                   = 'Wert';  { str }
  C_Mom_Stellen                = 'Stellen';  { smallint; wird für Parametrierung benötigt }


  { MRG-Stammdatentabellen }

  CDBSta      = 'Sta.db';
  CDBStaDfu   = 'StaDFU.db';
  CDBStaAuto  = 'StaAuto.db';
  CDBStaDSFG  = 'StaDSFG.db';
  CDBStaKanal = 'StaKanal.db';
  CDBStaImp   = 'StaImp.db';
  CDBStaAna   = 'StaAna.db';
  CDBStaMDE   = 'StaMDE.db';

  { Längen von Stringfeldern in MRG-Stammdatentabellen }

  szlen_Kennung      = 14;
  szlen_StationsName = 40;
  szlen_Rufnummer    = 20;
  szlen_Passwort     = 10;
  szlen_DSFGAdresse  = 1;
  szlen_Kanalname    = 20;
  szlen_KanalTyp     = 1;
  szlen_Einheit      = 10;

  { Längen von weiteren MRG-Stringfeldern in Tabellen und Resourcendateien }

  szLen_ParaNrAllg = 9;
  szLen_ParaNrMrg  = 20;     { ab 22.10.2002 erweitert von 3 auf 10 Stellen
                               für Elster DL240, EK260 Daten-Adressen
                               02.10.2008: erweitert auf 20 Stellen für EDIS-Kennziffern }
  szLen_ParaWert   = 40;
  szLen_ParaText   = 100;


  { Tabelle 'Sta.db' }

  CSMrgId    = 'SMrgId';        { Sekundärindex }

  C_Sta_Kennung      = 'Kennung';
  C_Sta_Aktiv        = 'Aktiv';
  C_Sta_MrgId        = 'MrgId';
  C_Sta_MrgTyp       = 'MrgTyp';
  C_Sta_StationsName = 'StationsName';
  C_Sta_TagesEnde    = 'TagesEnde';
  C_Sta_Prioritaet   = 'Prioritaet';
  C_Sta_IsWZSZ       = 'WZ_SZ_Umstellung';

  { Tabelle 'StaDfu.db' }

  C_StaDfu_MrgId      = 'MrgId';
  C_StaDfu_Rufnummer  = 'Rufnummer';
  C_StaDfu_Passwort1  = 'Passwort1';
  C_StaDfu_Passwort2  = 'Passwort2';
  C_StaDfu_Passwort3  = 'Passwort3';
  C_StaDfu_Passwort4  = 'Passwort4';
  C_StaDfu_PasswortNr = 'PasswortNr';
  C_StaDfu_ModemTyp   = 'ModemTyp';
  C_StaDfu_LogPort    = 'LogPort';

  { Tabelle 'StaAuto.db' }

  C_StaAuto_MrgId               = 'MrgId';
  C_StaAuto_Automatik           = 'Automatik';
  C_StaAuto_Intervall           = 'Intervall';
  C_StaAuto_Zeitsynchronisation = 'Zeitsynchronisation';
  C_StaAuto_Rundpufferreset     = 'Rundpufferreset';

  { Tabelle 'StaDSFG.db' }

  C_StaDSFG_MrgId    = 'MrgId';
  C_StaDSFG_Adresse  = 'Adresse';
  C_StaDSFG_MasterId = 'MasterId';

  { Tabelle 'StaKanal.db' }

  C_StaKanal_MrgId        = 'MrgId';
  C_StaKanal_MrgKanal     = 'MrgKanal';
  C_StaKanal_KanalId      = 'KanalId';
  C_StaKanal_Aktiv        = 'Aktiv';
  C_StaKanal_Kanalname    = 'Kanalname';
  C_StaKanal_KanalTyp     = 'KanalTyp';
  C_StaKanal_Einstellbar  = 'Einstellbar';
  C_StaKanal_Kontroll     = 'Kontroll';
  C_StaKanal_Eingang      = 'Eingang';
  C_StaKanal_Einheit      = 'Einheit';
  C_StaKanal_Kommastellen = 'Kommastellen';

  { Tabelle 'StaImp.db' }

  C_StaImp_KanalId   = 'KanalId';
  C_StaImp_Faktor    = 'Faktor';
  C_StaImp_Teiler    = 'Teiler';
  C_StaImp_OrgFaktor = 'OrgFaktor';

  { Tabelle 'StaAna.db' }

  C_StaAna_KanalId        = 'KanalId';
  C_StaAna_MessbereichMin = 'MessbereichMin';
  C_StaAna_MessbereichMax = 'MessbereichMax';
  C_StaAna_Strombereich   = 'Strombereich';
  C_StaAna_Offset         = 'Offset';
  C_StaAna_AufzMin        = 'AufzMin';
  C_StaAna_AufzMax        = 'AufzMax';

  { Tabelle 'StaMDE.db' }

  C_StaMDE_MrgId = 'MrgId';

{-------------------------- Konstanten für LAKS -------------------------------}

  { Tabelle für LAKS-Stammdaten }

  C_LTB_LSta                = 'LSta.db';

  C_LTF_LSta_StationsId     = 'StationsId';
  C_LTF_LSta_Stationsname   = 'Stationsname';
  C_LTF_LSta_Kennung        = 'Kennung';
  C_LTF_LSta_LaksTyp        = 'LaksTyp';
  C_LTF_LSta_Aktiv          = 'Aktiv';
  C_LTF_LSta_Summierung     = 'Summierung';
  C_LTF_LSta_SStelleV24     = 'SStelle_V24';
  C_LTF_LSta_SStelleDFU     = 'SStelle_DFU';
  C_LTF_LSta_Rufnummer      = 'Rufnummer';
  C_LTF_LSta_Passwort       = 'Passwort1';

  { Tabelle für LAKS-Typen }

  C_LTB_LTyp                = 'LAKSDef.db';

  C_LTF_LTyp_LaksTyp        = 'LaksTyp';
  C_LTF_LTyp_LaksName       = 'LaksName';

{-------------------------- Konstanten für DSfG -------------------------------}

// Gerätetyp-Nummern ('DelList.db')  // 17.10.2000

  C_GTypNr_Normal   =  0;
  C_GTypNr_MRG2100D =  6;
  C_GTypNr_MRG2200  =  7;
  C_GTypNr_MRG910   =  9;
  C_GTypNr_MRG905   = 16;

// DEA-Typ-Nummern ('DelList.db')  // 17.10.2000

  C_DEA_Typ_Zeichenkette    = '1'; // Zeichenkette
  C_DEA_Typ_GanzZahl        = '2'; // ganze Zahl
  C_DEA_Typ_RationalZahl    = '3'; // rationale Zahl
  C_DEA_Typ_ExponentialZahl = '4'; // Exponentialzahl
  C_DEA_Typ_Bool            = '5'; // boolscher Wert
  C_DEA_Typ_HexZahl         = '6'; // hexadez. Wert
  C_DEA_Typ_UnixZeit        = '7'; // Datum/Uhrzeit als Unix-Zeit

  { Längen von Stringfeldern in DSfG-Stammdatentabellen }

  szlen_DSfGKennung          = 12;
  szlen_DSfGStationsName     = 40;
  szlen_DSfGZweitName        = 40;
  szlen_DSfGInstanzName      = 40;
  szlen_DSfGRufnummer        = 20;
  szlen_DSfGPasswort         = 16;

  szlen_DSfGInstanztyp       =  1;
  szlen_DSfGBusadresse       =  1;
  szlen_DSfGHersteller       = 40;
  szlen_DSfGGeraetetyp       = 40;
  szlen_DSfGFabrikNr         = 20;
  szlen_DSfGSoftwareVersion  = 20;

  szLen_DSfGArchivName       = 40;

  szLen_DSfGKanalName        = 40;
  szLen_DSfGKanaltyp         =  2;
  szLen_DSfGWerteart         =  2;
  szlen_DSfGDEL              = 10;

  szLen_DSfGLogbuchName      = 40;

  { Längen von weiteren DSfG-Stringfeldern in Tabellen }

  szLen_DSfGStatus = 10;
  szLen_DSfGCRC    = 10;

  szlen_DSfGGerTypName = 40;     


  { Instanztypen und -namen }

  C_D_Instanztyp_DFU        = 'D';
  C_D_Instanztyp_DFU2       = 'E';  // 07.05.2018, WW
  C_D_Instanztyp_Gas        = 'G';
  C_D_Instanztyp_Rev        = 'M';
  C_D_Instanztyp_Odor       = 'O';
  C_D_Instanztyp_KGM        = 'Q';
  C_D_Instanztyp_Prot       = 'P';
  C_D_Instanztyp_Reg        = 'R';
  C_D_Instanztyp_Strg       = 'S';
  C_D_Instanztyp_Umw        = 'U';
  C_D_Instanztyp_Wieser     = 'W';
  C_D_Instanztyp_unbest     = 'X';

  C_D_Instanzname_DFU       = 'DFÜ-Instanz';
  C_D_Instanzname_Gas       = 'Gasbeschaffenheitinstanz';
  C_D_Instanzname_Rev       = 'Revisionsinstanz';
  C_D_Instanzname_Odor      = 'Odorierungsinstanz';
  C_D_Instanzname_KGM       = 'Korrelative Gasbeschaffenheitsinstanz';
  C_D_Instanzname_Prot      = 'Protokolldruckerinstanz';
  C_D_Instanzname_Reg       = 'Registrierinstanz';
  C_D_Instanzname_Strg      = 'Steuer-/Überwachungsinstanz';
  C_D_Instanzname_Umw       = 'Umwerterinstanz';
  C_D_Instanzname_Wieser    = 'Wieser-Instanz';
  C_D_Instanzname_unbest    = 'unbestimmte Instanz';

  { Definitions-Tabelle für Datenelemente }   // 17.10.2000
  // ab 08.03.2002 entfallen die Felder 'Beschreibung', 'Standard'

  C_Tb_DelList              = 'DelList';  // Tabelle der Datenelement-Definition

  C_Tf_DelList_DELGrp       = 'DELGrp';           // smallint
  C_Tf_DelList_DEL_Adresse  = 'DEL-Adresse';      // str5
  C_Tf_DelList_Name         = 'Name';             // str100
  C_Tf_DelList_Typ          = 'Typ';              // str1
  C_Tf_DelList_Zugriff      = 'Zugriff';          // str1
  C_Tf_DelList_ETypNr       = 'ETypNr';           // smallint
  C_Tf_DelList_Kanaltyp     = 'Kanaltyp';         // str2        ab 08.03.2002
  C_TI_DelList_ixGrp_DEA    = '';                 // Primärindex


  { Zuordnungs-Tabelle: Datenelementadresse <-> Kanaltyp }   // 11.03.2002

  C_Tb_DelKanaltyp          = 'DelKanaltyp';

  C_Tf_DelKanaltyp_DEA      = 'DEA';
  C_Tf_DelKanaltyp_Kanaltyp = 'Kanaltyp';
  C_Tf_DelKanaltyp_Werteart = 'werteart';


  { Tabelle der Stationen }

  C_DTB_Station             = 'Station.db';

  C_DTF_Station_StationId   = 'StationId';
  C_DTF_Station_Stationsname= 'Stationsname';
  C_DTF_Station_Automatik   = 'Automatik';
  C_DTF_Station_ErsterAbruf = 'Erster Abruf';
  C_DTF_Station_LIInstanz   = 'Login-Instanzname';
  C_DTF_Station_Zweitname   = 'Zweitname';
  C_DTF_Station_Prioritaet  = 'Prioritaet';

  { Tabelle der Instanzen }

  C_DTB_Instanz             = 'Instanz.db';

  C_DTF_Instanz_InstanzId   = 'InstanzId';
  C_DTF_Instanz_StationId   = 'StationId';
  C_DTF_Instanz_Instanzname = 'Instanzname';
  C_DTF_Instanz_Instanztyp  = 'Instanztyp';
  C_DTF_Instanz_Busadresse  = 'Busadresse';
  C_DTF_Instanz_Hersteller  = 'Hersteller';
  C_DTF_Instanz_Geraetetyp  = 'Geraetetyp';
  C_DTF_Instanz_Stand       = 'Stand';
  C_DTF_Instanz_FabrikNr    = 'FabrikNr';
  C_DTF_Instanz_SoftwareVs  = 'SoftwareVersion';
  C_DTF_Instanz_Baujahr     = 'Baujahr';
  C_DTF_Instanz_Inbetriebnahme = 'Inbetriebnahme';
  C_DTF_Instanz_GerTypNr    = 'GerTypNr';

  C_DTI_Instanz_StationId   = 'StationId';    { Sekundärindex }
  CStatAdr = 'SStatAdr';    { Sekundärindex }

  { Tabelle der DFÜ-Instanzen }

  C_DTB_InstDfu             = 'Inst_Dfu.db';

  C_DTF_InstDfu_InstanzId   = 'InstanzId';
  C_DTF_InstDfu_Kennung     = 'Kennung';
  C_DTF_InstDfu_Rufnummer   = 'Rufnummer';
  C_DTF_InstDfu_Adresse1    = 'Adresse 1';
  C_DTF_InstDfu_Adresse2    = 'Adresse 2';
  C_DTF_InstDfu_Adresse3    = 'Adresse 3';
  C_DTF_InstDfu_Adresse4    = 'Adresse 4';
  C_DTF_InstDfu_Passwort1   = 'Passwort 1';
  C_DTF_InstDfu_Passwort2   = 'Passwort 2';
  C_DTF_InstDfu_Passwort3   = 'Passwort 3';
  C_DTF_InstDfu_Passwort4   = 'Passwort 4';
  C_DTF_InstDfu_LogPort     = 'LogPort';

  { Tabelle der DSfG-Wertearten }  // 24.01.2002

  C_Tb_Werteart             = 'Werteart';
  
  C_Tf_Werteart_Kurzname    = 'Kurzname';
  C_Tf_Werteart_Name        = 'Name';
  C_Tf_Werteart_Kommastellen = 'Kommastellen';
  C_Tf_Werteart_AnzeigeMin  = 'anzeigemin';
  C_Tf_Werteart_AnzeigeMax  = 'anzeigemax';

  { Tabelle der Datenelemente für Automatikabruf }

  C_DTB_DDelAuto            = 'DDelAuto.db';

  C_DTF_DDelAuto_InstanzId  = 'InstanzId';
  C_DTF_DDelAuto_DelVon     = 'DelVon';
  C_DTF_DDelAuto_DelBis     = 'DelBis';

  { Tabelle der instanzspezifischen Datenelemente }

  C_DTB_DDelInst                = 'DDelInst.db';

  C_DTF_DDelInst_InstanzId      = 'InstanzId';
  C_DTF_DDelInst_DelVon         = 'DelVon';
  C_DTF_DDelInst_DelBis         = 'DelBis';
  C_DTF_DDelInst_Vorher_Nachher = 'Vorher_Nachher';

  { Tabelle der instanztypspezifischen Datenelemente }

  C_DTB_DDelITyp                = 'DDelITyp.db';

  C_DTF_DDelITyp_Instanztyp     = 'Instanztyp';
  C_DTF_DDelITyp_DelVon         = 'DelVon';
  C_DTF_DDelITyp_DelBis         = 'DelBis';
  C_DTF_DDelITyp_Vorher_Nachher = 'Vorher_Nachher';

  { Tabelle der Instanztyp-Bezeichnungen }  // 09.10.2000

  C_DTB_ITyp                = 'ITyp.db';

  C_DTF_ITyp_InstanzTyp     = 'InstanzTyp';   { Str1 }
  C_DTF_ITyp_Bezeichnung    = 'Bezeichnung';  { Str30 }

  { Tabelle der DSfG-Gerätetypen (Wieser-Instanzen) }

  C_DTB_DSfGDef = 'DSfGDef.db';

  C_DTF_DSfGDef_GerTypNr   = 'GerTypNr';
  C_DTF_DSfGDef_GerTypName = 'GerTypName';
  C_DTF_DSfGDef_DELGrp     = 'DELGrp';
  C_DTF_DSfGDef_MeldGrp    = 'MeldGrp';
  C_DTF_DSfGDef_StatusGrp  = 'StatusGrp';


  { Tabelle der Archive }

  C_DTB_Archive             = 'Archive.db';

  C_DTF_Archive_InstanzID   = 'InstanzID';
  C_DTF_Archive_ArchivNr    = 'ArchivNr';
  C_DTF_Archive_Name        = 'Name';
  C_DTF_Archive_Automatik   = 'Automatik';

  { Tabelle der Archivkanäle }

  C_DTB_AKanaele            = 'AKanaele.db';

  C_DTF_AKanaele_KanalNr    = 'KanalNr';
  C_DTF_AKanaele_InstanzID  = 'InstanzID';
  C_DTF_AKanaele_ArchivNr   = 'ArchivNr';
  C_DTF_AKanaele_Name       = 'Name';
  C_DTF_AKanaele_Kanaltyp   = 'Kanaltyp';
  C_DTF_AKanaele_Werteart   = 'Werteart';
  C_DTF_AKanaele_EADR       = 'EADR';
  C_DTF_AKanaele_Automatik  = 'Automatik';
  C_DTF_AKanaele_QuellDEL   = 'QuellDEL';

  CSArchivK = 'Archiv-K';    { Sekundärindex }

  { Tabelle der Logbücher }

  C_DTB_Logbuch             = 'Logbuch.db';

  C_DTF_Logbuch_InstanzID   = 'InstanzID';
  C_DTF_Logbuch_LogbuchNr   = 'LogbuchNr';
  C_DTF_Logbuch_Name        = 'Name';
  C_DTF_Logbuch_EADR        = 'EADR';
  C_DTF_Logbuch_Automatik   = 'Automatik';

  { Tabelle der Indizes für die Werte-Speicherung }

  C_DTB_Index                 = 'Index.db';

  C_DTF_Index_Index           = 'Index';
  C_DTF_Index_Kennung         = 'Kennung';
  C_DTF_Index_StatNr          = 'StatNr';
  C_DTF_Index_ArchivGruppenNr = 'ArchivGruppenNr';
  C_DTF_Index_Instanz         = 'Instanz';
  C_DTF_Index_Geraetetyp      = 'Geraetetyp';

  { Kanaltypen }
  { DSfG und MRG-Originalwerte: }
  kt_MW = 'MW';         { Meßwert (Analogwert) }
  kt_ZS = 'ZS';         { Zählerstand }
  kt_ZW = 'ZW';         { Zählwert (Menge) }
  { nur DSfG: }
  kt_ST = 'ST';         { Status }
  kt_FW = 'FW';         { Fahrweg }
  kt_TX = 'TX';         { Text }             // 04.07.2002
  kt_SZ = 'SZ';  // Störzähler  // 06.02.2013  WN
  kt_SU = 'SU';  // Summenkanal // 06.02.2013  WN
  kt_ZM = 'ZM';  // Zählwert (Menge) aus Zählerstand // 06.02.2013  WN
  kt_SM = 'SM';  // Störmenge   // 15.07.2013  WN
  kt_XZ = 'XZ';  // Maximum aus Zählerstandsdifferenz (Menge), nur Listreport // 02.07.2014
  kt_XM = 'XM';  // Maximum aus Messwert oder Zählwert, nur Listreport // 02.07.2014
  kt_XD = 'XD';  // Zeitstempel des Maximum (ZS), nur Listreport // 02.07.2014
  kt_XT = 'XT';  // Zeitstempel des Maximum (MW, ZW), nur Listreport // 02.07.2014


  { Tabelle für Momentanwerte der DSfG-DFÜ-Instanz }

  C_Tb_DMomDfue         = 'DMDF';

  C_Tf_DMomDfue_Befehl  = 'Befehl';    { str3 }
  C_Tf_DMomDfue_ParaAdr = 'ParaAdr';   { str3 }
  C_Tf_DMomDfue_Wert    = 'Wert';      { str40 }
  C_Tf_DMomDfue_Stellen = 'Stellen';       { smallint }

  { Tabelle für das Einstellen von DSfG-DFÜ-Parametern und NTY-Masken }

  C_Tb_DDfuePE         = 'DDFE';

  C_Tf_DDfuePE_Befehl  = 'Befehl';    { str3 }
  C_Tf_DDfuePE_ParaAdr = 'ParaAdr';   { str3 }
  C_Tf_DDfuePE_WertAlt = 'WertAlt';   { str40 }
  C_Tf_DDfuePE_WertNeu = 'WertNeu';   { str40 }
  C_Tf_DDfuePE_Stellen = 'Stellen';   { smallint }
  C_Tf_DDfuePE_StaAktu = 'StaAktu';   { log }
  C_Tf_DDfuePE_Aendern = 'Aendern';   { log }
  C_Tf_DDfuePE_Fertig  = 'Fertig';    { log }



  { Definitionstabelle für Wieser-DSfG-DFÜ-Parameter }

  C_Tb_DDfuPara            = 'DDfuPara.db';

  C_Tf_DDfuPara_ParaNummer = 'ParaNummer';  { smallint }
  C_Tf_DDfuPara_ParaName   = 'ParaName';    { str100 }
  C_Tf_DDfuPara_aenderbar  = 'Aenderbar';   { log }
  C_Tf_DDfuPara_Typ        = 'Typ';         { str1 }
  C_Tf_DDfuPara_DefText    = 'DefText';     { str100 }
  C_Tf_DDfuPara_DefWert    = 'DefWert';     { str100 }


  { Definitionstabelle für Nachrichtentypen der DSfG-DFÜ }

  C_Tb_DNTY                = 'DNTY.db';

  C_Tf_DNTY_Nachrichtentyp = 'Nachrichtentyp';  { str1 }
  C_Tf_DNTY_Bezeichnung    = 'Bezeichnung';     { str100 }
  C_Tf_DNTY_Aufmerksamkeit = 'Aufmerksamkeit';  { log }


  { Definitionstabelle für anstehende Fehler der Wieser DSfG-DFÜ }

  C_Tb_DErrAnst            = 'DErrAnst.db';

  C_Tf_DErrAnst_Fehler     = 'Fehler';      { str1 }
  C_Tf_DErrAnst_Fehlertext = 'Fehlertext';  { str255 }


  { Definitionstabelle für DSfG-Fehler der Wieser DSfG-DFÜ }

  C_Tb_DErrDSfG            = 'DErrDSfG.db';

  C_Tf_DErrDSfG_Fehler     = 'Fehler';      { smallint }
  C_Tf_DErrDSfG_Fehlertext = 'Fehlertext';  { str255 }


  { DSfG-Binärdatei mit zu sendendem Befehl }

  C_Tb_DBinaerBefehl = 'DBEF';

  { DSfG-Binärdatei mit Antwort des gesendenden Befehln }

  C_Tb_DBinaerAntwort = 'DANT';


// 23.11.2000 Spezifische Tabellen für WiDisp32 - - - - - - - - - - - -> Anfang

  // Tabelle Inst.abh. abzurufenden Datenelemente
  C_Tb_PInstDef     = 'PInstDef';

  C_Tf_PInstDef_InstanzTyp   = 'InstanzTyp';       // str1
  C_Tf_PInstDef_DEA          = 'DEA';              // str5
  C_Tf_PInstDef_Status       = 'Status';           // smallint
  C_TI_PInstDef_ixTyp_DEA    = 'ixTyp_DEA';        // Primärindex

  // Tabelle immer abzurufenden Datenelemente
  C_Tb_PInstConst   = 'PInstCon';

  C_Tf_PInstConst_DEA        = 'DEA';              // str5

  // Tabelle der Instanz-Datenelemente
  C_Tb_PDEWerte     = 'PDEWerte';

  C_Tf_PDEWerte_InstanzId    = 'InstanzId';        // int
  C_Tf_PDEWerte_DEA          = 'DEA';              // str5
  C_Tf_PDEWerte_Wert         = 'Wert';             // str40
  C_TI_PDEWerte_ixInst_DEA   = 'ixInst_DEA';       // Primärindex

  // Tabelle Bus-Standard-Belegung
  C_Tb_PBelegung    = 'PBelegung';

  C_Tf_PBelegung_InstanzAdr  = 'InstanzAdr';      // str1
  C_Tf_PBelegung_InstanzTyp  = 'InstanzTyp';      // str1
  C_Tf_PBelegung_Hersteller  = 'Hersteller';      // str40
  C_Tf_PBelegung_GeraeteTyp  = 'GeraeteTyp';      // str40
  C_Tf_PBelegung_DEL_ada     = 'DEL_ada';         // str5
  C_Tf_PBelegung_QuellAdr    = 'QuellAdr';        // str10

  // Tabelle Dokumentenverwaltung
  C_Tb_PDokumente    = 'PDokumente';

  C_Tf_PDokumente_StationsId = 'StationsId';      // integer
  C_Tf_PDokumente_DocType    = 'DocType';         // integer
  C_Tf_PDokumente_Document   = 'Document';        // blob

  C_TI_PDokumente_ixId_Type  = 'ixId_Type';       // Primärindex

  C_TC_PDokumente_BusAnalyse = $11;
  C_TC_PDokumente_IzAnalyse  = $12;
  C_TC_PDokumente_AdrAnalyse = $13;
  C_TC_PDokumente_FzAnalyse  = $14;
  C_TC_PDokumente_MsAnalyse  = $15;
  C_TC_PDokumente_AvAnalyse  = $16;

// 23.11.2000 Spezifische Tabellen für WiDisp32 - - - - - - - - - - - -> Ende

// 12.01.2001 Spezifische Tabellen für DSfGMom - - - - - - - - - - - -> Anfang

  // Haupttabelle stationsabhängig abzurufenden Datenelemente
  C_Tb_DMomStationMain                 = 'DMomStationMain';

  C_Tf_DMomStationMain_StationMomIndex = 'StationMomIndex';   // integer
  C_Tf_DMomStationMain_StationsId      = 'StationsId';        // integer
  C_Tf_DMomStationMain_Bezeichnung     = 'Bezeichnung';       // str40

  // Untertabelle stationsabhängig abzurufenden Datenelemente
  C_Tb_DMomStationSub                  = 'DMomStationSub';

  C_Tf_DMomStationSub_StationMomIndex  = 'StationMomIndex';   // integer
  C_Tf_DMomStationSub_InstanzAdresse   = 'InstanzAdresse';    // str1
  C_Tf_DMomStationSub_DEA              = 'DEA';               // str5

  // Haupttabelle instanzabhängig abzurufenden Datenelemente
  C_Tb_DMomInstMain              = 'DMomInstMain';

  C_Tf_DMomInstMain_InstMomIndex = 'InstMomIndex';   // integer
  C_Tf_DMomInstMain_InstanzTyp   = 'InstanzTyp';     // str1
  C_Tf_DMomInstMain_Bezeichnung  = 'Bezeichnung';    // str40

  // Untertabelle instanzabhängig abzurufenden Datenelemente
  C_Tb_DMomInstSub               = 'DMomInstSub';

  C_Tf_DMomInstSub_InstMomIndex  = 'InstMomIndex';   // integer
  C_Tf_DMomInstSub_DEA           = 'DEA';            // str5

  // Haupttabelle Parametrierungs-Journal
  C_Tb_DParamJrnMain             = 'DParamJrnMain';

  C_Tf_DParamJrnMain_DParamIndex = 'DParamIndex';   // integer
  C_Tf_DParamJrnMain_StationsId  = 'StationsId';    // integer
  C_Tf_DParamJrnMain_Anwender    = 'Anwender';      // str40
  C_Tf_DParamJrnMain_DatumZeit   = 'DatumZeit';     // DateTime

  // Untertabelle Parametrierungs-Journal
  C_Tb_DParamJrnSub              = 'DParamJrnSub';

  C_Tf_DParamJrnSub_DParamIndex  = 'DParamIndex';  // integer
  C_Tf_DParamJrnSub_InstanzAdresse = 'InstanzAdresse';    // str1
  C_Tf_DParamJrnSub_DEA          = 'DEA';           // str5
  C_Tf_DParamJrnSub_WertAlt      = 'WertAlt';       // str40
  C_Tf_DParamJrnSub_WertNeu      = 'WertNeu';       // str40

  // Haupttabelle Datenelement-Werte-Definitionen         // 18.04.2001
  C_Tb_DDeaValueDefMain          = 'DDeaValueDefMain';

  C_Tf_DDeaValueDefMain_DEA      = 'DEA';           // str5
  C_Tf_DDeaValueDefMain_GerTypNr = 'GerTypNr';      // SmallInt
  C_Tf_DDeaValueDefMain_DefIndex = 'DefIndex';      // integer

  C_TI_DDeaValueDefMain_ixDEA_Nr = 'ixDEA_Nr';      // Primärindex

  // Untertabelle Datenelement-Werte-Definitionen         // 18.04.2001
  C_Tb_DDeaValueDefSub           = 'DDeaValueDefSub';

  C_Tf_DDeaValueDefSub_DefIndex  = 'DefIndex';      // integer
  C_Tf_DDeaValueDefSub_DefText   = 'DefText';       // str40
  C_Tf_DDeaValueDefSub_DefValue  = 'DefValue';      // str40

  // Funktionsmaskenstruktur WIESER-Geräte          // 18.04.2001
  C_Tb_ERZ2200P             = 'WGerTree';

  C_Tf_ERZ2200P_GerTypNr    = 'GerTypNr';      // SmallInt
  C_Tf_ERZ2200P_RefString   = 'RefString';    // str5
  C_Tf_ERZ2200P_DEA         = 'DEA';          // str5
  C_Tf_ERZ2200P_Bezeichnung = 'Bezeichnung';  // str40

  C_TI_ERZ2200P_ixRefStr    = 'ixRefString';  // Primärindex
  C_TI_ERZ2200P_ixDEA       = 'ixDEA';        // Sekundärindex

// 12.01.2001 Spezifische Tabellen für DSfGMom - - - - - - - - - - - -> Ende

  // Tabelle von Geräterbildern  // 12.11.2001
  C_Tb_GerBild              = 'GerBild';

  C_Tf_GerBild_GerName      = 'GerName';      // str10
  C_Tf_GerBild_Bild         = 'DEA';          // blob

  C_Tb_DSfGStat             = 'DSfgStat.db';

  C_Tf_DSGStat_Nummer       = 'Nummer';       // 04.07.2002
  C_Tf_DSGStat_Text         = 'Text';

  C_Tb_DSG10St              = 'DSG10St.db';

  C_Tf_DSG10St_Nummer       = 'Nummer';
  C_Tf_DSG10St_Text         = 'Text';

  C_Tb_MRGStat              = 'MRGStat.db';

  // Tabelle der Datenelemente für AKA mit Bereichzuordnung // 06.02.2002
  C_Tb_AKAParams             = 'AKAParams';

  C_Tf_AKAParams_DEA         = 'DEA';            { str5 }
  C_Tf_AKAParams_Bezeichnung = 'Bezeichnung';    { str40 }
  C_Tf_AKAParams_MomTyp      = 'MomTyp';         { smallint }
  C_Tf_AKAParams_StreamNr    = 'StreamNr';       { smallint }
  C_Tf_AKAParams_Speichern   = 'Speichern';      { boolean }

  // Tabelle der virtuellen AKA-Archive // 06.02.2002
  C_Tb_AKAArchive            = 'AKAArchive';

  C_Tf_AKAArchive_InstId     = 'InstId';         { integer }
  C_Tf_AKAArchive_DEA        = 'DEA';            { str5 }
  C_Tf_AKAArchive_Zeit       = 'Zeit';           { datetime }
  C_Tf_AKAArchive_Wert       = 'Wert';           { float }


  // Tabelle der Definitionen der virtuellen AKA-Archive // 06.02.2002
  C_Tb_AKAArchivDef          = 'AKAArchivDef';

  C_Tf_AKAArchivDef_InstTyp  = 'InstTyp';        { str1 }
  C_Tf_AKAArchivDef_DEA      = 'DEA';            { str5 }
  C_Tf_AKAArchivDef_Bezeichnung = 'Bezeichnung'; { str40 }


  { Konfigurations- und Definitionstabellen für kavernenbezogene DSfG-Daten-Konvertierung
    (EWE, Speicher Huntorf): }

  { Tabelle für Zuordnung InstanzId, Archivgruppe -> Messstrecken-Nummer }  
  CDBKavMessung = 'KavMessung.db';

  C_KavMessung_InstanzId_Quelle = 'InstanzId_Quelle';
  C_KavMessung_Archivgruppe     = 'Archivgruppe';
  C_KavMessung_MessungNr        = 'MessungNr';

  { Konfigurationstabelle zur Hinterlegung der Archivgruppen und Archivkanälen
    mit Fahrwegsinformationen einer Instanz }
  CDBKavFahrweg = 'KavFahrweg.db';

  C_KavFahrweg_InstanzId_Quelle = 'InstanzId_Quelle';
  C_KavFahrweg_Archivgruppe_FW  = 'Archivgruppe_FW';
  C_KavFahrweg_Archivkanal_FW   = 'Archivkanal_FW';
  C_KavFahrweg_FWInfo_Nr        = 'FWInfo_Nr';

  { Tabelle für InstanzIds der virtuellen Kavernen-Instanzen }
  CDBKavInstanz = 'KavInstanz.db';

  C_KavInstanz_InstanzId_Quelle  = 'InstanzId_Quelle';
  C_KavInstanz_KaverneNr         = 'KaverneNr';
  C_KavInstanz_InstanzId_Kaverne = 'InstanzId_Kaverne';
                                                      

  { Tabellen für IEC-Leitwartenkopplung }
  DBIecDatenMRG      = 'DATENMRG.DB';
  DBIecMeldDatenMRG  = 'MELDDATENMRG.DB';
  DBIecDatenDSfG     = 'DATENDSFG.DB';
  DBIecMeldDatenDSfG = 'MELDDATENDSFG.DB';

  DBIecKonfig = 'KONFIGIEC.DB';
  C_IecKonfig_GeraeteArt       = 'GeraeteArt';
  C_IecKonfig_MRGKennung       = 'MRGKennung';
  C_IecKonfig_DSfGStationID    = 'DSfGStationID';
  C_IecKonfig_DSfGInstanzID    = 'DSfGInstanzID';
  C_IecKonfig_DSfGArchivNr     = 'DSfGArchivNr';
  C_IecKonfig_KanalNr          = 'KanalNr';
  C_IecKonfig_Kanaltyp         = 'Kanaltyp';
  C_IecKonfig_IEC_LinienNr     = 'IEC_LinienNr';
  C_IecKonfig_IEC_ASDU         = 'IEC_ASDU';
  C_IecKonfig_IEC_InfoObj_high = 'IEC_InfoObj_high';
  C_IecKonfig_IEC_InfoObj_low  = 'IEC_InfoObj_low';

  DBIecMeldKonfig    = 'MELDKONFIGIEC.DB';
  C_IecMeldKonfig_GeraeteArt       = 'GeraeteArt';
  C_IecMeldKonfig_MRGKennung       = 'MRGKennung';
  C_IecMeldKonfig_DSfGStationID    = 'DSfGStationID';
  C_IecMeldKonfig_DSfGInstanzID    = 'DSfGInstanzID';
  C_IecMeldKonfig_DSfGLogbuchNr    = 'DSfGLogbuchNr';
  C_IecMeldKonfig_MNrAllg          = 'MNrAllg';
  C_IecMeldKonfig_IEC_LinienNr     = 'IEC_LinienNr';
  C_IecMeldKonfig_IEC_ASDU         = 'IEC_ASDU';
  C_IecMeldKonfig_IEC_InfoObj_high = 'IEC_InfoObj_high';
  C_IecMeldKonfig_IEC_InfoObj_low  = 'IEC_InfoObj_low';


{------------------------------------------------------------------------------}
  { Tabellen für MDE-Transferprogramm }
  { Tabelle für PDA-Vorgaben }

  C_Tb_PDAVorgaben = 'PDAVorgaben.db';

  C_Tf_PDAVorgaben_MrgId        = 'MrgId';
  C_Tf_PDAVorgaben_Importziel   = 'Importziel';
  C_Tf_PDAVorgaben_Datentyp     = 'Datentyp';
  C_Tf_PDAVorgaben_Soll_Von_DZ  = 'Soll_Von_DZ';
  C_Tf_PDAVorgaben_Ist_Von_DZ   = 'Ist_Von_DZ';
  C_Tf_PDAVorgaben_Ist_Bis_DZ   = 'Ist_Bis_DZ';
  C_Tf_PDAVorgaben_Ist_Anzahl   = 'Ist_Anzahl';
  C_Tf_PDAVorgaben_Fehlergruppe = 'Fehlergruppe';
  C_Tf_PDAVorgaben_Fehlercode   = 'Fehlercode';

  C_TI_PDAVorgaben_ix = 'ixMrgIdZielDatentyp';        // Primärindex


{------------------------------------------------------------------------------}
  { Wical-Tabellen }

  { Wical-Stammdaten-Tabellen }

  { Messstellen-Tabelle }

  C_Tb_AwMst = 'awmst';

  C_Tf_AwMst_MstLfdNr       = 'mstlfdnr';
  C_Tf_AwMst_MstNummer      = 'mstnummer';
  C_Tf_AwMst_MstName        = 'mstname';
  C_Tf_AwMst_AwGruppe       = 'awgruppe';
  C_Tf_AwMst_MstKennung     = 'mstkennung';
  C_Tf_AwMst_MstTyp         = 'msttyp';
  C_Tf_AwMst_mstlinktoclass = 'mstlinktoclass';
  C_Tf_AwMst_ordnungsnr     = 'ordnungsnr';
  C_Tf_AwMst_mststatus      = 'mststatus';
  C_Tf_AwMst_stationsname   = 'stationsname';
  C_Tf_AwMst_plz            = 'plz';
  C_Tf_AwMst_ort            = 'ort';
  C_Tf_AwMst_strasse        = 'strasse';

  { Bit-Werte für Feld 'mststatus' }
  C_mststatus_MDE = $04;  { Messstelle ist MDE-Messstelle }


  { Kanal-Tabelle }

  C_Tb_AwKan = 'awkan';

  C_Tf_AwKan_AwkLfdNr      = 'awklfdnr';
  C_Tf_AwKan_MstLfdNr      = 'mstlfdnr';
  C_Tf_AwKan_Name          = 'name';
  C_Tf_AwKan_Formel        = 'formel';
  C_Tf_AwKan_Einheit       = 'einheit';
  C_Tf_AwKan_Typ           = 'typ';
  C_Tf_AwKan_Nachkomma     = 'nachkomma';
  C_Tf_AwKan_Faktor        = 'faktor';
  C_Tf_AwKan_Konstante     = 'konstante';
  C_Tf_AwKan_AuswerteArt   = 'auswerteart';
  C_Tf_AwKan_OrdnungsNr    = 'ordnungsnr';
  C_Tf_AwKan_Spaltenbreite = 'spaltenbreite';
  C_Tf_AwKan_FormatIndex   = 'formatindex';
  C_Tf_AwKan_KanalLink     = 'kanallink';
  C_Tf_AwKan_KanalInfo     = 'kanalinfo';
  C_Tf_AwKan_mgroesse      = 'mgroesse';
  C_Tf_AwKan_schiene       = 'schiene';

  { Werte für Feld 'Formel' }
  C_AwKanFormel_MRGKanal = 0;  { Kanal ist MRG-Kanal }
  C_AwKanFormel_Manuell  = 2;  { Kanal ist Manu-Kanal, z.B. MDE-Manu-Kanal }

  { Bit-Werte für Feld 'AuswerteArt' }
  C_AwKanAwArt_MDE = $2000;    { Manu-Kanal ist MDE-Kanal }


  { MRG-Kanal-Tabelle }

  C_Tb_AwMrgKan = 'awmrgkan';

  C_Tf_AwMrgKan_AwkLfdNr   = 'awklfdnr';
  C_Tf_AwMrgKan_Mrgkennung = 'mrgkennung';
  C_Tf_AwMrgKan_MrgKanalNr = 'mrgkanalnr';
  C_Tf_AwMrgKan_TarifZone  = 'tarifzone';


  { Tabellen für WiCal-Anbindung an MDE-System }

  { Messgrößen-Tabelle }

  C_Tb_Messgr = 'messgroessen';

  C_Tf_Messgr_MgId     = 'mgid';
  C_Tf_Messgr_Kennz    = 'kennzeichen';
  C_Tf_Messgr_Bez      = 'bezeichnung';
  C_Tf_Messgr_Typ      = 'typ';
  C_Tf_Messgr_Einh     = 'einheit';
  C_Tf_Messgr_KS_MDE   = 'kommastellen_mde';
  C_Tf_Messgr_MgId_MDE = 'mgid_mde';

  { Infocode-Tabelle }

  C_Tb_MDE_Infocode = 'mde_infocode';

  C_Tf_MDE_Infocode_Nr   = 'infocodenr';
  C_Tf_MDE_Infocode_Text = 'infocodetext';

  { Infocodejournal-Tabelle }

  C_Tb_MDE_InfocodeJrn = 'mde_infocodejrn';

  C_Tf_MDE_InfocodeJrn_icid       = 'icid';
  C_Tf_MDE_InfocodeJrn_icdt       = 'icdt';
  C_Tf_MDE_InfocodeJrn_mstid      = 'mstid';
  C_Tf_MDE_InfocodeJrn_schiene    = 'schiene';
  C_Tf_MDE_InfocodeJrn_kennung    = 'kennung';
  C_Tf_MDE_InfocodeJrn_infocodenr = 'infocodenr';


  { WiCal-Archivdaten-Tabellen }

  { Archivdaten-Tabelle für nicht-periodische Werte }

  C_Tb_WiCalArchiv_GA = 'ga';  { periodische Archivdaten }
  C_Tb_WiCalArchiv_GD = 'gd';  { nicht-periodische Archivdaten }

  C_Tf_WiCalArchiv_DT     = 'dt';
  C_Tf_WiCalArchiv_Wert1  = 'wert1';
  C_Tf_WiCalArchiv_Wert2  = 'wert2';
  C_Tf_WiCalArchiv_Status = 'status';

  { Bit-Werte für Feld 'Status' }
  C_WiCalArchStatus_WertVorhanden = $8000;    { Wert ist vorhanden }


{--------------------- Allgemeine DSfG-Konstanten -----------------------------}

  { Liste aller möglichen DSfG-Busadressen }

  CMaxDSfGTeilnehmer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';

  { DSfG-Telegramm }

  CMaxDSfGTelegrammLaenge = 200;   { max. Bytelänge des DSfG-Anforderungstelegramms;
                                     17.02.2003, WW: von 7500 heruntergesetzt auf 512
                                     -> theoretisch 8k, in der Praxis funktioniert es aber
                                        nur mit ca. 500 Byte zuverlässig !
                                     08.04.2003, WW: nochmal heruntergesetzt auf 200
                                     -> damit wird die businterne Blockung auf 256 Byte
                                        vermieden, welche in den DSfG-DFÜs bis Version 03/2003
                                        zu Timeouts beim Warten auf Daten führen kann ! }

  { Zuordnung: DSfG-Busadresse -> DEL der Logbuchgruppen }

  CLogbuchDEL: array['A'..'_'] of string [4] =
    ('cbaa','cbab','cbac','cbad','cbae','cbaf','cbag','cbah','cbai','cbaj',
     'cbba','cbbb','cbbc','cbbd','cbbe','cbbf','cbbg','cbbh','cbbi','cbbj',
     'cbca','cbcb','cbcc','cbcd','cbce','cbcf','cbcg','cbch','cbci','cbcj',
     'cbda');

  { DSfG-Telegrammfelder und ihre Element-Kennziffern
   (vgl. Technische Regeln G485: Anhang A, Deklarationsanteil (DCL)) }

  C_DID =     0;
  C_TID =     1;
  C_BLO =     2;
  C_BNR =     4;
  C_DNO =     8;
  C_NTY =    16;
  C_DFO =    32;
  C_DEB =    64;
  C_ZAE =   128;

  C_PAS =   256;
  C_DTY =   512;
  C_ABS =  1024;
  C_EMF =  2048;
  C_TDA =  4096;
  C_TTI =  8192;
  C_PTB = 16384;

  C_DID_DataSimple = C_DID + C_TID + C_BLO + C_BNR + C_DNO + C_NTY + C_DFO + C_DEB + C_ZAE;          { DID..ZAE -> 255 }


  { Offsetwerte zur Bestimmung von DSfG-Archivgruppe/kanal aus der Datenelement-Adresse }

  C_INT_DELGRUPPEOFFSET =  96;                                            { ' }
  C_INT_DELKANALOFFSET  = 101;                                            { e }

  { für kavernenbezogene DSfG-Archivdatenkonvertierung: Toleranz-Zeitbereich zwischen
    Eintrag im Fahrwegkanal und in den Datenarchiven: }
  C_FW_Zeitfenster = 5;  // Standardwert in s

  { Sommerzeit/Winterzeit }

  CMEZ  = 'M';
  CMESZ = 'S';

{---------------------- Konstanten für digitale Signatur ----------------------}

  { Signatur-Verifizierungsstati }
  C_SigVerifyState_None        = 0;  // Keine Signatur vorhanden
  C_SigVerifyState_Valid       = 1;  // Signatur gültig
  C_SigVerifyState_Invalid     = 2;  // Signatur ungültig
  C_SigVerifyState_NotVerified = 3;  // Signatur wurde nicht verifiziert
  C_SigVerifyState_PubKeyMissing = 4;  // Signatur vorhanden, öffentlicher Schlüssel für Verifizierung fehlt (nur für Gas-X)

{-------------------------- Allgemeine Konstanten -----------------------------}

  CNotAllowedChars_FileName = '\/:*?"<>|';  { in Windows-Dateinamen nicht erlaubte Zeichen }

  C_To_Kelvin = 273.15; { Nullpunkt der Celsius-Skala in Kelvin }

  // Filename für Nachholen von zur Wiederholung anstehenden SMS
  C_Filename_GetSavedSMS  = 'GetSavedSMS.txt';  // 05.11.2010  WN


type
  { Allgemein: }

  { WICOM-Abrufarten }
  TAbrufart = (aa_automatisch, aa_manuell, aa_momentan, aa_ruf, aa_rufreakt, aa_rueckruf,
               aa_konflesen, aa_dfue_momentan);

  { Record mit Informationen für Abruf einer MRG-/DSfG-Station }
  TAbrufData = record
    StationId: integer;
    Datentypen: integer;
    DatenVon: TDateTime;
    DatenBis: TDateTime;
    Anrufversuch: integer;
    Erfolgreich: boolean;
    Keine_weiteren_Versuche: boolean;
  end;

  { Record mit Ergebnisdaten einer Parametrierung (MRG, DSfG oder DSfG-DFÜ }
  TParaEinstellResultData = record
    ParaTyp: string;
    BAdr: string;
    ParaAdr: string;
    WertAlt: string;
    WertNeu: string;
  end;

  { Record mit Verbindungsinformationen }
  TVerbInfoData = record
    DZ_VerbindungSteht: TDateTime;
    DZ_Login: TDateTime;
  end;

  
  { MRG: }

  { Record mit Informationen über konvertierte/aufbereitete Daten (MRG-Messwerte/Tagessätze) }
  TDataKonvInfo = record
    NeueDaten: boolean;    { true, wenn mind. 1 neuer Datensatz konvertiert wurde }
    DatenFehlen: boolean;  { true, wenn mit mind. 1 "Fehlend"-Satz aufgefüllt wurde }
    SollVon: TDateTime;    { Soll-Von-Datum/Zeit, z.B. lt. Abfragebefehl }
    SollBis: TDateTime;    { Soll-Bis-Datum/Zeit, z.B. lt. Abfragebefehl }
    IstVon: TDateTime;     { Datum/Zeit des ersten neu konvertierten Datensatzes }
    IstBis: TDateTime;     { Datum/Zeit des letzten neu konvertierten Datensatzes }
  end;

  { Record mit Informationen zur MRG-Zeitsynchronisation }
  TZeitSyncInfoData = record
    DZ_Server: TDateTime;
    DZ_Geraet: TDateTime;
  end;

  { Record mit automatisch ermittelten Verbindungsparametern }
  TVerbAutoDetectData = record
    ModemTyp: string;
    PasswortNr: integer;
  end;


  { DSfG: }

  { Record mit Konfigurationsdaten einer DSfG-DFÜ-Instanz }
  TDSfGDfueKonfigData = record
    EAdr_Dfue: char;
    Hersteller: string;
    ProgName: string;
    Version: string;
    Extensionmode: byte;  // 31.01.2017, WW
    Wieser_Teilnehmer: string;
    Wieser_Adresse: array [1..5] of string [1];
    Wieser_Fabriknummer: string;  // 02.10.2009, WW
    Wieser_Baujahr: string;  // 02.10.2009, WW
    Wieser_NG_Typ: string;  // 21.02.2012, WW
    Wieser_NG_Version: string;  // 21.02.2012, WW
    Wieser_NG_Build: string;  // im Format yyyymmddhhmmss; 21.02.2012, WW
    Wieser_NG_Flashgroesse: string;  // 21.02.2012, WW
    Wieser_NG_SysConfig: string;  // 21.02.2012, WW
  end;

  { Rohdaten-Struktur eines DSfG-Datenelements }
  TDSfGRohRec = record
    Adresse: string;
    Wert   : string;
    UTime  : string;
    OrdNr  : string;
    Status : string;
    CRC    : string;
  end;

  { Record mit Informationen zum Firmware-Update eines Geräts }
  TFwUpdateInfoData = record
    Version_neu: string;  // Neue FW-Version im Gerät (nach dem Update)
    Build_neu: string;    // Neue FW-Build-Information im Gerät (nach dem Update)
  end;

implementation

end.


