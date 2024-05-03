{******************************************************************************}
{* Unit: XML-Konstanten für WK22-Abrufsystem                                  *}
{* 20.11.2003 WW                                                              *}
{ 24.02.11  GD  XML-Ersatzzeichen in Parametertexten					                 }
{* 30.01.12  WW  XML-Ersatzzeichen für '&' korrigiert   				              *}
{* 18.07.12  GD  XML-Ersatzzeichen für '&' bei Decode an Anfang gesetzt       *}
{******************************************************************************}
unit WXmlConst;

interface

const

  C_XmlTagNameStart  = '<name>';
  C_XmlTagValueStart = '<value>';
  C_XmlTagValueEnd   = '</value>';

  C_wieser_answer = 'wieser_answer';


  { XML-Konstanten mit Ersatzzeichen: }

  { Allgemein }
  C_lt_Subst   = '&lt;';       // Ersatz für: <
  C_gt_Subst   = '&gt;';       // Ersatz für: >
  C_quot_Subst = '&quot;';     // Ersatz für: "
  C_apos_Subst = '&apos;';     // Ersatz für: '
  C_amp_Subst  = '&amp;amp;';  // Ersatz für: &     amp; ergänzt; 30.01.2012, WW

  C_WieserStart = C_lt_Subst + 'wieser' + C_gt_Subst;
  C_WieserEnd   = C_lt_Subst + '/wieser' + C_gt_Subst;

  C_ZeitSynclisteStart = C_lt_Subst + 'timesync_list' + C_gt_Subst;
  C_ZeitSynclisteEnd   = C_lt_Subst + '/timesync_list' + C_gt_Subst;
  C_DZ_ServerStart     = C_lt_Subst + 'ts_server' + C_gt_Subst;
  C_DZ_ServerEnd       = C_lt_Subst + '/ts_server' + C_gt_Subst;

  C_ErrorgroupStart    = C_lt_Subst + 'errorgroup' + C_gt_Subst;
  C_ErrorgroupEnd      = C_lt_Subst + '/errorgroup' + C_gt_Subst;
  C_ErrorcodeStart     = C_lt_Subst + 'errorcode' + C_gt_Subst;
  C_ErrorcodeEnd       = C_lt_Subst + '/errorcode' + C_gt_Subst;

  C_TransparentAntwStart = C_lt_Subst + 'transparentresp' + C_gt_Subst;
  C_TransparentAntwEnd   = C_lt_Subst + '/transparentresp' + C_gt_Subst;

  C_WieserLogStart      = 'wieser_log';
  C_WieserLogEnd        = '/member';

  C_FwUpdListeStart  = C_lt_Subst + 'fwupd_list' + C_gt_Subst;
  C_FwUpdListeEnd    = C_lt_Subst + '/fwupd_list' + C_gt_Subst;

  C_ConnInfoBlockStart = C_lt_Subst + 'conninfo' + C_gt_Subst;
  C_ConnInfoBlockEnd   = C_lt_Subst + '/conninfo' + C_gt_Subst;
  C_DZ_ConnectStart    = C_lt_Subst + 'ts_connect' + C_gt_Subst;
  C_DZ_ConnectEnd      = C_lt_Subst + '/ts_connect' + C_gt_Subst;
  C_DZ_LoginStart      = C_lt_Subst + 'ts_login' + C_gt_Subst;
  C_DZ_LoginEnd        = C_lt_Subst + '/ts_login' + C_gt_Subst;

  C_RawDataListStart = C_lt_Subst + 'rawdata_list' + C_gt_Subst;
  C_RawDataListEnd   = C_lt_Subst + '/rawdata_list' + C_gt_Subst;
  C_RawDataStart     = C_lt_Subst + 'rawdata';
  C_RawDataEnd       = C_lt_Subst + '/rawdata' + C_gt_Subst;
  C_RawDataEncode    = 'encode=';

  C_TraceLogStart = C_lt_Subst + 'traceLog' + C_gt_Subst;
  C_TraceLogEnd   = C_lt_Subst + '/traceLog' + C_gt_Subst;

  { Erweiterte Ergebnis-Rückgaben für Gas-X: }
  C_ReturnExtStart   = C_lt_Subst + 'returnext ';
  C_ReturnExtGroup   = 'group=';
  C_ReturnExtCode    = 'code=';
  C_ReturnExtAddInfo = 'addinfo=';

  { MRG: }
  C_MRGBlockStart         = C_lt_Subst + 'mrg ';
  C_MRGBlockEnd           = C_lt_Subst + '/mrg' + C_gt_Subst;

  C_Mrg_List              = 'list';
  C_Mrg_List_Orig         = 'list_orig';
  C_Mrg_Set               = 'set';
  C_MRGStdWerte           = 'voh';
  C_MRGTagWerte           = 'mv';
  C_MRGParameter          = 'para';
  C_MRGMeldungen          = 'msg';

  C_MrgKennungKennung     = 'ident=';
  C_MrgKennungDatentypen  = 'type=';
  C_MrgKennungDatumZeit   = 'ts=';
  C_MrgKennungStatus      = 'state=';
  C_MrgKennungKanalNr     = 'cid=';
  C_MrgKennungEingangWert = 'ic=';
  C_MrgKennungKontrWert   = 'cc=';
  C_MrgKennungEingStatus  = 'icstate=';
  C_MrgKennungKontrStatus = 'ccstate=';
  C_MrgKennungAllgParamNr = 'par-id=';
  C_MrgKennungMrgParamNr  = 'mrgpar-id=';
  C_MrgKennungAllgMeldNr  = 'mcode=';
  C_MrgKennungMrgMeldNr   = 'mrgmcode=';
  C_MrgKennungMeldParaAendAllgParaNr = 'pch_parid=';
  C_MrgKennungMeldParaAendMrgParaNr  = 'pch_mrgparid=';
  C_MrgKennungMeldParaAendWertAlt    = 'pch_oldval=';
  C_MrgKennungMeldParaAendWertNeu    = 'pch_newval=';

  C_MRGParamlistStart     = C_lt_Subst+C_MRGParameter+C_Mrg_List+C_gt_Subst;
  C_MRGParamlistEnd       = C_lt_Subst+'/'+C_MRGParameter+C_Mrg_List+C_gt_Subst;
  C_MRGMeldungenStart     = C_lt_Subst+C_MRGMeldungen+C_Mrg_List+C_gt_Subst;
  C_MRGMeldungenEnd       = C_lt_Subst+'/'+C_MRGMeldungen+C_Mrg_List+C_gt_Subst;

  C_MRGDZ_MRGStart        = C_lt_Subst + 'ts_mrg' + C_gt_Subst;
  C_MRGDZ_MRGEnd          = C_lt_Subst + '/ts_mrg' + C_gt_Subst;

  C_MRGRuflisteStart = C_lt_Subst + 'cslave_list' + C_gt_Subst;
  C_MRGRuflisteEnd   = C_lt_Subst + '/cslave_list' + C_gt_Subst;

  C_ConnListeStart = C_lt_Subst + 'connlist' + C_gt_Subst;
  C_ConnListeEnd   = C_lt_Subst + '/connlist' + C_gt_Subst;
  C_ModemTypeStart = C_lt_Subst + 'modemtype' + C_gt_Subst;
  C_ModemTypeEnd   = C_lt_Subst + '/modemtype' + C_gt_Subst;
  C_PwNoStart = C_lt_Subst + 'pwno' + C_gt_Subst;
  C_PwNoEnd   = C_lt_Subst + '/pwno' + C_gt_Subst;


  { DSfG: Archivdaten, Logbücher, Datenelemente }
  C_DSfGBlockStart    = C_lt_Subst + 'dsfg' + C_gt_Subst;
  C_DSfGBlockEnd      = C_lt_Subst + '/dsfg' + C_gt_Subst;
  C_DSfGInstanzStart  = C_lt_Subst + 'instance ';
  C_DSfGInstanzEnd    = C_lt_Subst + '/instance' + C_gt_Subst;
  C_DSfGRecStart      = C_lt_Subst;
  C_DSfGRecEnd        = C_gt_Subst;
  C_DSfG_List         = 'list';
  C_DSfGArchVal       = 'archval';
  C_DSfGCurrVal       = 'curval';
  C_DSfGLogbook       = 'msg';
  C_DSfGDfuParams     = 'rdtval';
  C_DSfGAttnTelegrams = 'atttelegr';
  C_DSfGArchLog       = 'archlog';
  C_DSfGLogbookLog    = 'msglog';
  C_DSfGCurrValLog    = 'curvallog';
  C_RawfileLog        = 'rfile';

  C_DSfGRecordStart   = C_lt_Subst + '%s';
  C_DSfGRecordEnd     = '/%s' + C_gt_Subst;
  C_DSfGDataSep       = C_quot_Subst;
  C_DSfGKennungIAdr   = 'busaddr=';
  C_DSfGKennungDEA    = 'addr=';
  C_DSfGKennungOrdNr  = 'ordinal=';
  C_DSfGKennungUnixDT = 'ts=';
  C_DSfGKennungTZone  = 'tz=';  // für DSfG-SMS-Daten; 01.08.2006, WW
  C_DSfGKennungStatus = 'state=';
  C_DSfGKennungCRC    = 'checksum=';
  C_DSfGKennungMCode  = 'mcode=';
  C_DSfGKennungConvState = 'convstate=';
  C_DSfGKennungName   = 'name=';
  C_DSfGKennungSigVerifyState = 'sigverifystate=';  // 15.03.2012, WW

  { DSfG: DSfG-DFÜ-Daten }

  C_DSfGDfueWertelisteStart = C_lt_Subst + 'rdtval_list' + C_gt_Subst;
  C_DSfGDfueWertelisteEnd   = C_lt_Subst + '/rdtval_list' + C_gt_Subst;
  C_DSfGBasicStart           = C_lt_Subst + 'basic' + C_gt_Subst;
  C_DSfGBasicEnd             = C_lt_Subst + '/basic' + C_gt_Subst;

  C_DSfGTeilnehmerStart      = C_lt_Subst + 'activeusers' + C_gt_Subst;
  C_DSfGTeilnehmerEnd        = C_lt_Subst + '/activeusers' + C_gt_Subst;
  C_DSfGIAdr_nStart          = 'busaddr%d';
  C_DSfGIAdr_nEnd            = '/busaddr%d';
  C_DSfGFabrikNrStart        = C_lt_Subst + 'serialno' + C_gt_Subst;
  C_DSfGFabrikNrEnd          = C_lt_Subst + '/serialno' + C_gt_Subst;
  C_DSfGBaujahrStart         = C_lt_Subst + 'yearmanufact' + C_gt_Subst;
  C_DSfGBaujahrEnd           = C_lt_Subst + '/yearmanufact' + C_gt_Subst;
  C_DSfGTypStart             = C_lt_Subst + 'type' + C_gt_Subst;
  C_DSfGTypEnd               = C_lt_Subst + '/type' + C_gt_Subst;
  C_DSfGVersionStart         = C_lt_Subst + 'version' + C_gt_Subst;
  C_DSfGVersionEnd           = C_lt_Subst + '/version' + C_gt_Subst;
  C_DSfGBuildStart           = C_lt_Subst + 'build' + C_gt_Subst;
  C_DSfGBuildEnd             = C_lt_Subst + '/build' + C_gt_Subst;

  C_DSfGParaList_Start       = C_lt_Subst + 'para_list' + C_gt_Subst;
  C_DSfGParaList_End         = C_lt_Subst + '/para_list' + C_gt_Subst;

  C_DSfGDfuPara              = 'para';
  C_DSfGDfuNtyMask           = 'ntymask';

  C_DSfGDfueAufmTelegrStart  = C_lt_Subst + 'atttelegr_list' + C_gt_Subst;
  C_DSfGDfueAufmTelegrEnd    = C_lt_Subst + '/atttelegr_list' + C_gt_Subst;

  C_DSfGDfueRFilesStart      = C_lt_Subst + C_RawfileLog + '_list' + C_gt_Subst;
  C_DSfGDfueRFilesEnd        =
    C_lt_Subst + '/' + C_RawfileLog + '_list' + C_gt_Subst;

  C_DSfGDfuKennungParId      = 'par_id=';
  C_DSfGDfuKennungNtyDno     = 'dno=';
  C_DSfGDfuKennungNty        = 'nty=';
  C_DSfGDfuKennungTZone      = 'tz=';

  C_DSfGIdentStart           = C_lt_Subst + 'ident' + C_gt_Subst;
  C_DSfGIdentEnd             = C_lt_Subst + '/ident' + C_gt_Subst;
  C_DSfGPasswordStart        = C_lt_Subst + 'passw' + C_gt_Subst;
  C_DSfGPasswordEnd          = C_lt_Subst + '/passw' + C_gt_Subst;
  C_DSfGInstAdrStart         = C_lt_Subst + 'busaddr' + C_gt_Subst;
  C_DSfGInstAdrEnd           = C_lt_Subst + '/busaddr' + C_gt_Subst;
  C_DSfGTelNrZentraleStart   = C_lt_Subst + 'callno_cs' + C_gt_Subst;
  C_DSfGTelNrZentraleEnd     = C_lt_Subst + '/callno_cs' + C_gt_Subst;
  C_DSfGAktDatumStart        = C_lt_Subst + 'currdate' + C_gt_Subst;
  C_DSfGAktDatumEnd          = C_lt_Subst + '/currdate' + C_gt_Subst;
  C_DSfGAktZeitStart         = C_lt_Subst + 'currtime' + C_gt_Subst;
  C_DSfGAktZeitEnd           = C_lt_Subst + '/currtime' + C_gt_Subst;
  C_DSfGHerstellerStart      = C_lt_Subst + 'manufact' + C_gt_Subst;
  C_DSfGHerstellerEnd        = C_lt_Subst + '/manufact' + C_gt_Subst;
  C_DSfGProgNameStart        = C_lt_Subst + 'progname' + C_gt_Subst;
  C_DSfGProgNameEnd          = C_lt_Subst + '/progname' + C_gt_Subst;
  C_DSfGProgVersionStart     = C_lt_Subst + 'progversion' + C_gt_Subst;
  C_DSfGProgVersionEnd       = C_lt_Subst + '/progversion' + C_gt_Subst;
  C_DSfGDatumLpcStart        = C_lt_Subst + 'date_lpc' + C_gt_Subst;
  C_DSfGDatumLpcEnd          = C_lt_Subst + '/date_lpc' + C_gt_Subst;
  C_DSfGZeitLpcStart         = C_lt_Subst + 'time_lpc' + C_gt_Subst;
  C_DSfGZeitLpcEnd           = C_lt_Subst + '/time_lpc' + C_gt_Subst;
  C_DSfGInVerbNoLoginStart   = C_lt_Subst + 'cin_nologin' + C_gt_Subst;
  C_DSfGInVerbNoLoginEnd     = C_lt_Subst + '/cin_nologin' + C_gt_Subst;
  C_DSfGInVerbLoginErrStart  = C_lt_Subst + 'cin_loginerror' + C_gt_Subst;
  C_DSfGInVerbLoginErrEnd    = C_lt_Subst + '/cin_loginerror' + C_gt_Subst;
  C_DSfGOutVerbErrorStart    = C_lt_Subst + 'cout_failed' + C_gt_Subst;
  C_DSfGOutVerbErrorEnd      = C_lt_Subst + '/cout_failed' + C_gt_Subst;
  C_DSfGOutVerbNoLoginStart  = C_lt_Subst + 'cout_nologin' + C_gt_Subst;
  C_DSfGOutVerbNoLoginEnd    = C_lt_Subst + '/cout_nologin' + C_gt_Subst;
  C_DSfGOutVerbLoginErrStart = C_lt_Subst + 'cout_loginerror' + C_gt_Subst;
  C_DSfGOutVerbLoginErrEnd   = C_lt_Subst + '/cout_loginerror' + C_gt_Subst;

  C_DSfGExtensionmodeStart   = C_lt_Subst + 'extensionmode' + C_gt_Subst;  // 31.01.2017, WW
  C_DSfGExtensionmodeEnd     = C_lt_Subst + '/extensionmode' + C_gt_Subst;  // 31.01.2017, WW

  C_DSfGDZ_InstanceStart     = C_lt_Subst + 'ts_instance' + C_gt_Subst;
  C_DSfGDZ_InstanceEnd       = C_lt_Subst + '/ts_instance' + C_gt_Subst;

  { Parametrierung: }

  C_ParaAendListeStart         = C_lt_Subst + 'valchange_list' + C_gt_Subst;
  C_ParaAendListeEnd           = C_lt_Subst + '/valchange_list' + C_gt_Subst;
  C_DSfGDfueParaAendListeStart = C_lt_Subst + 'rdtvalchange_list' + C_gt_Subst;
  C_DSfGDfueParaAendListeEnd   = C_lt_Subst + '/rdtvalchange_list' + C_gt_Subst;

  C_ParaAend = 'valchange';
  C_ParaAendWertAlt = 'oldval=';
  C_ParaAendWertNeu = 'newval=';

  { DSfG-DFÜ-Parametrierung: }

  C_DSfGDfueParaAend_BAdr    = 'baddr=';
  C_DSfGDfueParaAend_ParaAdr = 'paraaddr=';

  { Rufdeaktivierung: }
  C_RufDeaktListeStart = C_lt_Subst + 'callinactive_list' + C_gt_Subst;
  C_RufDeaktListeEnd   = C_lt_Subst + '/callinactive_list' + C_gt_Subst;

function EncodeXml(const sTxtString: string): string;
function DecodeXml(const sXmlString: string): string;

implementation

uses SysUtils;

// Erzeugt aus einem Text-String einen XML-kompatiblen String
function EncodeXml(const sTxtString: string): string;
begin
  Result := StringReplace(sTxtString, '<', C_lt_Subst, [rfReplaceAll]);
  Result := StringReplace(Result, '>', C_gt_Subst, [rfReplaceAll]);
  Result := StringReplace(Result, '"', C_quot_Subst, [rfReplaceAll]);
  Result := StringReplace(Result, '''', C_apos_Subst, [rfReplaceAll]);
  Result := StringReplace(Result, '&', C_amp_Subst, [rfReplaceAll]);
end;

// Erzeugt aus einem XML-String einen Klartext-String
function DecodeXml(const sXmlString: string): string;
begin
  Result := StringReplace(sXmlString, C_amp_Subst, '&', [rfReplaceAll]);
  Result := StringReplace(Result, C_lt_Subst, '<', [rfReplaceAll]);
  Result := StringReplace(Result, C_gt_Subst, '>', [rfReplaceAll]);
  Result := StringReplace(Result, C_quot_Subst, '"', [rfReplaceAll]);
  Result := StringReplace(Result, C_apos_Subst, '''', [rfReplaceAll]);
end;

end.
