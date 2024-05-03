program GPRSSrv;

uses
  Forms,
  FMain in 'FMain.pas' {FormGPRSServerMain},
  LogFile in 'V:\DELPHI32\Soft\LogFile.pas',
  GPRSVerbList in 'GPRSVerbList.pas',
  WSocketError in 'V:\Delphi32\Soft\WSocketError.pas',
  UnixDT in 'V:\DELPHI32\Soft\UnixDT.pas',
  GPRS_Util in 'GPRS_Util.pas',
  Wsyscon in 'V:\DELPHI32Sql\Abruf\Common\Wsyscon.pas',
  GPRSKonvThr in 'GPRSKonvThr.pas',
  CRC16 in 'V:\DELPHI32\Soft\CRC16.PAS',
  GPRSConst in 'GPRSConst.pas',
  Errprc32 in 'V:\Delphi32\soft\Errprc32.pas',
  DLLUtil in 'V:\Delphi32\soft\Dllutil.pas',
  DGPRSKonv in 'V:\Delphi7\GPRS\GPRSSrv\DGPRSKonv.pas',
  DListen in 'V:\DELPHI32Sql\Abruf\Common\Dlisten.pas',
  ErrConst in 'V:\DELPHI32\Dlls\errtxt32\Errconst.pas',
  DALKonv in 'V:\DELPHI32\Abruf\afdsfg32\DALKonv.PAS',
  DSFGUTIL in 'V:\DELPHI32\Abruf\common\DSFGUTIL.PAS',
  DGPRSKonvManager in 'DGPRSKonvManager.pas',
  WMessageReg in 'V:\DELPHI32\soft\WMessageReg.pas',
  WMessageSend in 'V:\DELPHI32\soft\WMessageSend.pas',
  JournalFkt in 'V:\Delphi32Sql\abruf\almsrv32\JournalFkt.pas',
  ZBDatDb in 'V:\Delphi32Sql\abruf\Common\Zbdatdb.pas',
  Journldb in 'V:\Delphi32Sql\abruf\Common\Journldb.pas',
  ZSyncDb in 'V:\Delphi32Sql\abruf\Common\Zsyncdb.pas',
  GD_Utils in 'V:\DELPHI32\Soft\GD_Utils.pas',
  DB_Attn in 'V:\Delphi32\soft\DB_Attn.pas',
  RFilesDb in 'V:\Delphi32Sql\abruf\Common\Rfilesdb.pas',
  TelegrDb in 'V:\Delphi32Sql\abruf\Common\TelegrDb.pas',
  WComm in 'V:\DELPHI32\Abruf\common\WComm.pas',
  DJLoesch in 'V:\Delphi32Sql\abruf\Common\Djloesch.pas',
  DbDSfGParams in 'V:\Delphi32Sql\abruf\Common\DbDSfGParams.pas',
  DbDSfGMom in 'V:\Delphi32Sql\abruf\Common\DbDSfGMom.pas',
  DMomLists in 'V:\Delphi32Sql\DSfG\DSfG_Utils\DMomLists.pas',
  O_DbTermin in 'V:\Delphi32Sql\abruf\Common\O_DbTermin.pas',
  Wsysdat in 'V:\Delphi32Sql\abruf\Common\Wsysdat.pas',
  FPrgBar in 'V:\DELPHI32\soft\FPrgBar.pas' {FormProgressBar},
  My_utils in 'V:\Delphi32Sql\soft\My_utils.pas',
  Auftrgdb in 'V:\Delphi32Sql\Abruf\Common\Auftrgdb.pas',
  RufeDb in 'V:\Delphi32Sql\abruf\Common\RufeDb.pas',
  MeldKonfigDb in 'V:\Delphi32Sql\abruf\Common\MeldKonfigDb.pas',
  WResMeld in 'V:\DELPHI32\Abruf\common\WResMeld.pas',
  WResConst in 'V:\DELPHI32\Abruf\Common\WResConst.pas',
  DKavKonfigDb in 'V:\Delphi32Sql\DSfG\Kaverne\Konv\DKavKonfigDb.pas',
  DDBABRUF in 'V:\Delphi32Sql\abruf\common\DDBABRUF.PAS',
  Srvcfgini in 'V:\Delphi32Sql\abruf\Common\Srvcfgini.pas',
  IPBusyList in 'V:\Delphi32Sql\Abruf\common\IPBusyList.pas',
  IecKonfDb in 'V:\Delphi32Sql\IEC\KONFIGURATION\IecKonfDb.pas',
  IecConst in 'V:\DELPHI32Sql\Iec\Kopplung\IecConst.pas',
  DDLoesch in 'V:\Delphi32Sql\Abruf\common\DDLoesch.pas',
  Tbdsfgar in 'V:\Delphi32Sql\Abruf\common\Tbdsfgar.pas',
  TbDSfGIW in 'V:\Delphi32Sql\abruf\Common\Tbdsfgiw.pas',
  MeldungenDB in 'V:\Delphi32Sql\abruf\Common\MeldungenDB.pas',
  DALKonvDb in 'V:\Delphi32Sql\abruf\afdsfg32\DALKonvDb.pas',
  SysDaten in 'V:\Delphi32Sql\abruf\almsrv32\Sysdaten.pas',
  DDbSta in 'V:\Delphi32Sql\Abruf\Common\Ddbsta.pas',
  WErrMsg in 'V:\DELPHI32\Soft\WErrMsg.pas',
  DKurzzeitWerte in 'V:\Delphi32\Abruf\common\DKurzzeitWerte.PAS',
  GPRSSrvIniFile in 'GPRSSrvIniFile.pas',
  GPRSTelegrList in 'GPRSTelegrList.pas',
  Endewin in 'V:\DELPHI32\Soft\Endewin.pas' {EndeFenster},
  AusgabeDirList in 'AusgabeDirList.pas',
  DSG_Utils in 'V:\DELPHI32\Dsfg\DSfG_Utils\DSG_Utils.pas',
  DGPRS_KZWKonv in 'DGPRS_KZWKonv.pas',
  O_TCPIP_CustomSrv in 'V:\DELPHI32\Abruf\Common\O_TCPIP_CustomSrv.pas',
  O_Signatur in 'V:\DELPHI32\soft\O_Signatur.pas',
  O_OpcKonfDb in 'V:\Delphi7\OPC\Import_Konfig\O_OpcKonfDb.pas',
  OpcDbImportConst in 'V:\Delphi7\OPC\Import\OpcDbImportConst.pas';

{$R *.RES}

begin
  if not InitLibraryErrTxt32 then exit;       { Fehlertext-Dll initialisieren }
  try
    Application.Initialize;
    Application.Title := 'GPRS-Server';
    Application.CreateForm(TFormGPRSServerMain, FormGPRSServerMain);
  Application.Run;
  finally
    DoneLibraryErrTxt32;                           { Fehlertext-Dll freigeben }
  end;
end.
