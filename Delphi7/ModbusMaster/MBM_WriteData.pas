{******************************************************************************}
{* Unit: Gelesene Modbus-Daten für externe Programme bereitstellen            *}
{* 25.02.2010 WW                                                              *}
{* 28.06.2017 WW  Message-Struktur um Funktionscode erweitert                 *}
{* 04.06.2018 WW  Timeout für Windows-Nachricht per Funktions-Übergabe        *}
{******************************************************************************}
unit MBM_WriteData;

interface

uses
  Windows, Messages, SysUtils;

const
  CMBM_Msg_Timeout_Default = 100;  // Standard-Timeout in ms für Windows-Nachricht

type
  { Struktur zum Versenden eines Registerwerts per Windows-Message (WM_COPYDATA) }
  TRegisterData_WindowsMessage = packed record
    Linie: word;  // 1 - 256: COM-Ports
    SlaveAdr: byte;
    RegisterAdr: word;
    WertTyp: string [5];
    Wert: string [255];
    FktCode: byte;  // ab 28.06.2017, WW
  end;
  PRegisterData_WindowsMessage = ^TRegisterData_WindowsMessage;


function WriteRegisterData_WindowsMessage (AHandle_Sender: HWnd;
  AClassName_Receiver: string;
  ARegisterData: TRegisterData_WindowsMessage;
  iTimeout: integer = CMBM_Msg_Timeout_Default): integer; overload;
function WriteRegisterData_WindowsMessage (
  AHandle_Sender, AHandle_Receiver: HWnd;
  ARegisterData: TRegisterData_WindowsMessage;
  iTimeout: integer = CMBM_Msg_Timeout_Default): integer; overload;

implementation

{--------------------------------------------------------------------------}
function WriteRegisterData_WindowsMessage (AHandle_Sender: HWnd;
  AClassName_Receiver: string; ARegisterData: TRegisterData_WindowsMessage;
  iTimeout: integer = CMBM_Msg_Timeout_Default): integer;
{--------------------------------------------------------------------------}
{ Registerwert weiterleiten per Windows-Message;
  Übergabe: Handle des versendenden Fensters
            Klassenname des empfangenden Fensters
            Register-Datenrecord
            Timeout für erfolgreiches Versenden in ms }
var
  hWnd: THandle;
begin
  hWnd := FindWindow (pchar (AClassName_Receiver), nil);  // Empfänger-Fenster wird über Klassenname gesucht
  if (hWnd > 0)
  then Result := WriteRegisterData_WindowsMessage(
    AHandle_Sender, hWnd, ARegisterData, iTimeout)
  else Result := 0;
end;

{--------------------------------------------------------------------------}
function WriteRegisterData_WindowsMessage (
  AHandle_Sender, AHandle_Receiver: HWnd;
  ARegisterData: TRegisterData_WindowsMessage;
  iTimeout: integer = CMBM_Msg_Timeout_Default): integer;
{--------------------------------------------------------------------------}
{ Registerwert weiterleiten per Windows-Message;
  Übergabe: Handle des versendenden Fensters
            Handle des empfangenden Fensters
            Register-Datenrecord
            Timeout für erfolgreiches Versenden in ms }
var
  PRegisterData: PRegisterData_WindowsMessage;
  cds: TCopyDataStruct;
  i : DWORD;
begin
  try
    GetMem (PRegisterData, SizeOf (TRegisterData_WindowsMessage));
    try
      PRegisterData^:=ARegisterData;

      cds.dwData:=0;  // unbenutzt
      cds.cbData:=SizeOf (TRegisterData_WindowsMessage);
      cds.lpData:=PRegisterData;

      i := 0;
      if (SendMessageTimeout(AHandle_Receiver, WM_COPYDATA, AHandle_Sender,
        Integer(@cds), SMTO_ABORTIFHUNG, iTimeout, i) = 0)  // Timeout aus Übergabe; 04.06.2018, WW
      then Result := 0
      else Result := AHandle_Receiver;
    finally
      FreeMem (PRegisterData, SizeOf (TRegisterData_WindowsMessage));
    end;
  except
    Result := 0;
  end;
end;

end.
