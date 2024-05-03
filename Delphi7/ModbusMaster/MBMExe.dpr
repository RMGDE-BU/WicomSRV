program MBMExe;

uses
  Forms,
  FMainModbusMaster in 'V:\Delphi7\ModbusMaster\FMainModbusMaster.pas' {FormMainModbusMaster},
  ModbusMasterConst in 'V:\Delphi7\ModbusMaster\ModbusMasterConst.pas',
  ModbusMasterIniFile in 'V:\Delphi7\ModbusMaster\ModbusMasterIniFile.pas',
  LogFile in 'V:\DELPHI32\Soft\LogFile.pas',
  ModbusMasterThr in 'V:\Delphi7\ModbusMaster\ModbusMasterThr.pas',
  LogCom in 'V:\DELPHI32\Soft\Logcom.pas',
  WSysCon in 'V:\DELPHI32\Abruf\Common\WSysCon.pas',
  WErrMsg in 'V:\DELPHI32\Soft\WErrMsg.pas',
  ModbusCmdExec in 'V:\Delphi7\ModbusMaster\ModbusCmdExec.pas',
  ErrConst in 'V:\Delphi32\Dlls\Errtxt32\ErrConst.pas',
  WComm in 'V:\DELPHI32\Abruf\Common\WComm.pas',
  AbrufTimeoutConst in 'V:\DELPHI32\Abruf\WicomSrv\AbrufTimeoutConst.pas',
  ModbusUtil in 'V:\Delphi32\Abruf\Modbus\ModbusUtil.pas',
  UnixDT in 'V:\DELPHI32\Soft\UnixDT.pas',
  CommUtil in 'V:\DELPHI32\soft\CommUtil.pas',
  O_SerialModbus in 'V:\Delphi32\Abruf\Modbus\O_SerialModbus.pas',
  O_ModbusComm in 'V:\Delphi32\Abruf\Modbus\O_ModbusComm.pas',
  ErrTxt in 'V:\DELPHI32\DLLs\errtxt32\Errtxt.pas',
  ErrConstGasX in 'V:\DELPHI32\DLLs\errtxt32\ErrconstGasX.pas',
  ModbusMasterRes in 'V:\Delphi7\ModbusMaster\ModbusMasterRes.pas',
  MBM_WriteData in 'V:\Delphi7\ModbusMaster\MBM_WriteData.pas',
  O_TcpModbus in 'V:\Delphi32\Abruf\Modbus\O_TcpModbus.pas',
  ErrConstBasic in 'V:\DELPHI32\DLLs\errtxt32\ErrConstBasic.pas',
  GD_UTILS in 'V:\Delphi32\soft\GD_UTILS.PAS',
  ModbusMasterUtil in 'V:\Delphi7\ModbusMaster\ModbusMasterUtil.pas',
  DB_Attn in 'V:\Delphi32\soft\DB_Attn.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'WICO22 MODBUS-Master';
  Application.CreateForm(TFormMainModbusMaster, FormMainModbusMaster);
  Application.Run;
end.
