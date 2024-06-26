{******************************************************************************}
{* Unit: Importunit f�r USBPORT.DLL (Palm-Kommunikation �ber PALMUSBD.SYS)    *}
{* Version: 07.04.2005  WW                                                    *}
{******************************************************************************}
unit UsbPortPrc;

interface

uses
  Windows, DllUtil, ErrConst;

const
  UsbPortLibHandle: THandle = HINSTANCE_ERROR;

type
  { Struktur f�r USB-Timeouts }
  TUSB_Timeouts = packed record
    ReadTimeout: longword;    // ms
    WriteTimeout: longword;   // ms
  end;


function InitLibraryUSBPort (MsgOut: boolean = true): Boolean;

function DoneLibraryUSBPort: Boolean;

function PalmUSB_GetAttachedDevices (var DeviceCount: longword; Buffer: pchar;
  var BufferSize: longword): longword;

function PalmUSB_GetDeviceFriendlyName (DeviceName: pchar;
  FriendlyName: pchar): longword;

function PalmUSB_OpenPort (DeviceName: pchar; PalmAppCreatorID: longword): THandle;

function PalmUSB_ClosePort (PortHandle: THandle): boolean;

function PalmUSB_SetTimeouts (PortHandle: THandle;
  var USB_Timeouts: TUSB_Timeouts): longword;

function PalmUSB_GetTimeouts (PortHandle: THandle;
  var USB_Timeouts: TUSB_Timeouts): longword;

function PalmUSB_SendBytes (PortHandle: THandle; const Buffer; BufLen: longword;
  var BytesSent: longword): longword;

function PalmUSB_ReceiveBytes (PortHandle: THandle; var Buffer;
  BytesToReceive: longword; var BytesReceived: longword): longword;

function PalmUSB_RegisterDeviceInterface (hRecipient: HWND): HDEVNOTIFY;

procedure PalmUSB_UnRegisterDeviceInterface (Handle: HDEVNOTIFY);

function PalmUSB_IsPalmOSDeviceNotification (EventData: longint;
  PalmAppCreatorID: longword; DeviceName: pchar; GUID: PGUID): boolean;

implementation

type
  // USBPORT.DLL ist in C geschrieben, daher alle Funktionsaufrufe mit cdecl !
  TAttachedDevFkt = function (var DeviceCount: longword; Buffer: pchar;
    var BufferSize: longword): longword; cdecl;

  TDevFriendlyNameFkt = function (DeviceName: pchar;
    FriendlyName: pchar): longword; cdecl;

  TOpenPortFkt = function (PortName: pchar;
    PalmAppCreatorID: longword): THandle; cdecl;

  TClosePortFkt = function (PortHandle: THandle): boolean; cdecl;

  TTimeoutsFkt = function (PortHandle: THandle;
    var USB_Timeouts: TUSB_Timeouts): longword; cdecl;

  TSendBytesFkt = function (PortHandle: THandle; const Buffer; BufLen: longword;
    var BytesSent: longword): longword; cdecl;

  TReceiveBytesFkt = function (PortHandle: THandle; var Buffer;
    BytesToReceive: longword; var BytesReceived: longword): longword; cdecl;

  TRegDevFkt = function (hRecipient: HWND): HDEVNOTIFY; cdecl;

  TUnRegDevFkt = procedure (Handle: HDEVNOTIFY); cdecl;

  TDevNotificationFkt = function (EventData: longint; PalmAppCreatorID: longword;
    PortName: pchar; GUID: PGUID): boolean; cdecl;


{------------------------------------------------------------}
function InitLibraryUSBPort (MsgOut: boolean = true): Boolean;
{------------------------------------------------------------}
{ USBPORT.DLL laden;
  �bergabe: 'MsgOut' (wenn 'true' erfolgt Fehlerausgabe per Message-Dialog)
  Ergebnis: true, wenn Laden erfolgreich }
const
  CDLLName_UsbPort = 'USBPORT.DLL';
begin
  Result := (InitDll (GetDLLPath (CDLLName_UsbPort), UsbPortLibHandle, MsgOut));
end;

{-----------------------------------}
function DoneLibraryUSBPort: Boolean;
{-----------------------------------}
{ USBPORT.DLL freigeben;
  Ergebnis: true, wenn Freigeben erfolgreich }
begin
  Result := DoneDll (UsbPortLibHandle);
end;

{----------------------------------------------------------------------------}
function PalmUSB_GetAttachedDevices (var DeviceCount: longword; Buffer: pchar;
  var BufferSize: longword): longword;
{----------------------------------------------------------------------------}
{ �ber USB angeschlossene Palm-Handhelds ermitteln;
  R�ckgabe: 'DeviceCount' (Anzahl der angeschlossenen Palms)
            'Buffer' (systeminterner Palm-USB-Devicename)
            'BufferSize' (L�nge des Devicenamens)
  Ergebnis: Fehlercode 'PALMUSBERR_NoError' (OK) bzw. 'PALMUSBERR_...' (Fehler)
            aus ErrConst.pas
  Anmerkung: "trial-and-error" mit USBPORT.DLL, Version 4.4
  -> Funktion liefert in 'DeviceCount' die richtige Anzahl, bei mehreren
     angeschlossenen USB-Palms in 'Buffer' jedoch "irgendeinen" Palm zur�ck
     (evtl. der erste in der Registry gefundene ?). Funktionalit�t von USBPORT.DLL
     ist nur auf eine Kommunikation mit einem einzelnen USB-Palm ausgelegt.  }
var
  p: TAttachedDevFkt;
begin
  Result := PALMUSBERR_DLLAccess;  // Default
  try
    if (UsbPortLibHandle <> 0) and (UsbPortLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (UsbPortLibHandle, 'PalmUsbGetAttachedDevices');
      if (@p <> nil) then
        Result := p (DeviceCount, Buffer, BufferSize);
    end;
  except
    // Result ist PALMUSBERR_DLLAccess
  end;
end;

{--------------------------------------------------------}
function PalmUSB_GetDeviceFriendlyName (DeviceName: pchar;
  FriendlyName: pchar): longword;
{--------------------------------------------------------}
{ liefert zum systeminternen Devicenamen eines USB-Palm den Anzeigenamen (Klartext);
  �bergabe: 'DeviceName' (systeminterner Palm-USB-Devicenname)
  R�ckgabe: 'FriendlyName' (Anzeigename im Klartext)
  Ergebnis: Fehlercode 'PALMUSBERR_NoError' (OK) bzw. 'PALMUSBERR_...' (Fehler)
            aus ErrConst.pas }
var
  p: TDevFriendlyNameFkt;
begin
  Result := PALMUSBERR_DLLAccess;  // Default
  try
    if (UsbPortLibHandle <> 0) and (UsbPortLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (UsbPortLibHandle, 'PalmUsbGetDeviceFriendlyName');
      if (@p <> nil) then
        Result := p (DeviceName, FriendlyName);
    end;
  except
    // Result ist PALMUSBERR_DLLAccess
  end;
end;

{---------------------------------------------------------------------------------}
function PalmUSB_OpenPort (DeviceName: pchar; PalmAppCreatorID: longword): THandle;
{---------------------------------------------------------------------------------}
{ USB-Port zu einem Palm-Handheld �ffnen;
  �bergabe: 'DeviceName'(systeminterner Devicename, �ber den der Port zum USB-Palm
                         angesprochen werden kann)
            'PalmAppCreatorID' (0: Port �ffnen ohne Pr�fung der Creator-ID der
                                   Palm-Gegenstellen-Applikation;
                                > 0: Port �ffnen mit Pr�fung der Creator-ID)
  Ergebnis: Handle auf den Palm-USB-Port (OK) bzw. INVALID_HANDLE_VALUE (Fehler) }
var
  p: TOpenPortFkt;
begin
  Result := INVALID_HANDLE_VALUE;  // Default
  try
    if (UsbPortLibHandle <> 0) and (UsbPortLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (UsbPortLibHandle, 'PalmUsbOpenPort');
      if (@p <> nil) then
        Result := p (DeviceName, PalmAppCreatorID);
    end;
  except
    // Result ist INVALID_HANDLE_VALUE
  end;
end;

{--------------------------------------------------------}
function PalmUSB_ClosePort (PortHandle: THandle): boolean;
{--------------------------------------------------------}
{ USB-Port zu einem Palm-Handheld schlie�en;
  �bergabe: 'PortHandle' (Handle auf einen Palm-USB-Port)
  Ergebnis: true, wenn Schlie�en des USB-Ports erfolgreich }
var
  p: TClosePortFkt;
begin
  Result := false;  // Default
  try
    if (UsbPortLibHandle <> 0) and (UsbPortLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (UsbPortLibHandle, 'PalmUsbClosePort');
      if (@p <> nil) then
        Result := p (PortHandle);
    end;
  except
    // Result ist false
  end;
end;

{------------------------------------------------}
function PalmUSB_SetTimeouts (PortHandle: THandle;
  var USB_Timeouts: TUSB_Timeouts): longword;
{------------------------------------------------}
{ Sende- und Empfangs-Timeouts f�r Palm-USB-Kommunikation setzen;
  �bergabe: 'USB_Timeouts'
  Ergebnis: Fehlercode 'PALMUSBERR_NoError' (OK) bzw. 'PALMUSBERR_...' (Fehler)
            aus ErrConst.pas
  Anmerkung: "trial-and-error" mit USBPORT.DLL, Version 4.4
  -> bei Timeouts 1..100 ms wird DLL-intern fest 100 ms verwendet. Timeouts > 100 ms
  werden dagegen 1:1 �bernommen. Timeout 0 ms wird ebenfalls akzeptiert.
  -> Tests mit verschiedenen Read-/Write-Timeouts haben zu keinerlei Verbesserung
     bei der Vermeidung von PALMUSBERR_RecvTimedOut-R�ckgaben der Funktion
     'PalmUSB_ReceiveBytes' gef�hrt; 19.04.2006 WW }
var
  p: TTimeoutsFkt;
begin
  Result := PALMUSBERR_DLLAccess;  // Default
  try
    if (UsbPortLibHandle <> 0) and (UsbPortLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (UsbPortLibHandle, 'PalmUsbSetTimeouts');
      if (@p <> nil) then
        Result := p (PortHandle, USB_Timeouts);
    end;
  except
    // Result ist PALMUSBERR_DLLAccess
  end;
end;

{------------------------------------------------}
function PalmUSB_GetTimeouts (PortHandle: THandle;
  var USB_Timeouts: TUSB_Timeouts): longword;
{------------------------------------------------}
{ Sende- und Empfangs-Timeouts f�r Palm-USB-Kommunikation lesen;
  R�ckgabe: 'USB_Timeouts'
  Ergebnis: Fehlercode 'PALMUSBERR_NoError' (OK) bzw. 'PALMUSBERR_...' (Fehler)
            aus ErrConst.pas }
var
  p: TTimeoutsFkt;
begin
  Result := PALMUSBERR_DLLAccess;  // Default
  try
    if (UsbPortLibHandle <> 0) and (UsbPortLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (UsbPortLibHandle, 'PalmUsbGetTimeouts');
      if (@p <> nil) then
        Result := p (PortHandle, USB_Timeouts);
    end;
  except
    // Result ist PALMUSBERR_DLLAccess
  end;
end;

{------------------------------------------------------------------------------}
function PalmUSB_SendBytes (PortHandle: THandle; const Buffer; BufLen: longword;
  var BytesSent: longword): longword;
{------------------------------------------------------------------------------}
{ Daten an Palm-USB-Port senden;
  �bergabe: 'PortHandle' (Handle auf einen Palm-USB-Port)
            'Buffer' (Puffer f�r zu versendende Zeichen)
            'BufLen' (Anzahl der zu versendenden Zeichen)
  R�ckgabe: 'BytesSent' (Anzahl der tats�chlich versendeten Zeichen)
  Ergebnis: Fehlercode 'PALMUSBERR_NoError' (OK) bzw. 'PALMUSBERR_...' (Fehler)
            aus ErrConst.pas }
var
  p: TSendBytesFkt;
begin
  Result := PALMUSBERR_DLLAccess;  // Default
  try
    if (UsbPortLibHandle <> 0) and (UsbPortLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (UsbPortLibHandle, 'PalmUsbSendBytes');
      if (@p <> nil) then
        Result := p (PortHandle, Buffer, BufLen, BytesSent);
    end;
  except
    // Result ist PALMUSBERR_DLLAccess
  end;
end;

{-----------------------------------------------------------------}
function PalmUSB_ReceiveBytes (PortHandle: THandle; var Buffer;
  BytesToReceive: longword; var BytesReceived: longword): longword;
{-----------------------------------------------------------------}
{ Daten von Palm-USB-Port empfangen;
  �bergabe: 'PortHandle' (Handle auf einen Palm-USB-Port)
            'Buffer' (Puffer f�r zu empfangende Zeichen)
            'BytesToReceive' (max. Anzahl an Zeichen, die gelesen werden sollen)
  R�ckgabe: 'BytesReceived' (Anzahl der tats�chlich gelesenen Zeichen)
  Ergebnis: Fehlercode 'PALMUSBERR_NoError' (OK) bzw. 'PALMUSBERR_...' (Fehler)
            aus ErrConst.pas }
var
  p: TReceiveBytesFkt;

begin
  Result := PALMUSBERR_DLLAccess;  // Default
  try
    if (UsbPortLibHandle <> 0) and (UsbPortLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (UsbPortLibHandle, 'PalmUsbReceiveBytes');
      if (@p <> nil) then
        Result := p (PortHandle, Buffer, BytesToReceive, BytesReceived);
    end;
  except
    // Result ist PALMUSBERR_DLLAccess
  end;
end;

{----------------------------------------------------------------------}
function PalmUSB_RegisterDeviceInterface (hRecipient: HWND): HDEVNOTIFY;
{----------------------------------------------------------------------}
{ Anmelden f�r Windows-Benachrichtigung bei Ger�tewechsel (alle m�glichen Ger�te);
  �bergabe: 'hRecipient' (Fenster-Handle)
  Ergebnis:  Zeiger auf Benachrichtigungshandle (OK) bzw. nil (Fehler) }
var
  p: TRegDevFkt;
begin
  Result := nil;  // Default
  try
    if (UsbPortLibHandle <> 0) and (UsbPortLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (UsbPortLibHandle, 'PalmUsbRegisterDeviceInterface');
      if (@p <> nil) then
        Result := p (hRecipient);
    end;
  except
    // Result ist nil
  end;
end;

{---------------------------------------------------------------}
procedure PalmUSB_UnRegisterDeviceInterface (Handle: HDEVNOTIFY);
{---------------------------------------------------------------}
{ von der Windows-Ger�tewechsel-Benachrichtigung abmelden;
  �bergabe: 'Handle' (Zeiger auf Benachrichtigungshandle) }
var
  p: TUnRegDevFkt;
begin
  if (UsbPortLibHandle <> 0) and (UsbPortLibHandle <> HINSTANCE_ERROR) then begin
    @p := GetProcAddress (UsbPortLibHandle, 'PalmUsbUnRegisterDeviceInterface');
    if (@p <> nil) then
      p (Handle);
  end;
end;

{---------------------------------------------------------------------}
function PalmUSB_IsPalmOSDeviceNotification (EventData: longint;
  PalmAppCreatorID: longword; DeviceName: pchar; GUID: PGUID): boolean;
{---------------------------------------------------------------------}
{ pr�fen, ob ein Palm-USB-Ger�tewechsel erfolgt ist;
  �bergabe: 'EventData' (Zeiger auf Ereignis-Daten)
            'PalmAppCreatorID' (mit/ohne Pr�fung der Creator-ID der Palm-Gegenstellen-
                                Applikation, siehe PalmUSB_OpenPort)
            'DeviceName' (systeminterner Palm-USB-Devicenname)
            'GUID' (Zeiger auf GUID)
  Ergebnis: true, wenn Palm-USB-Ger�tewechsel erfolgt ist }
var
  p: TDevNotificationFkt;
begin
  Result := false;  // Default
  try
    if (UsbPortLibHandle <> 0) and (UsbPortLibHandle <> HINSTANCE_ERROR) then begin
      @p := GetProcAddress (UsbPortLibHandle, 'PalmUsbIsPalmOSDeviceNotification');
      if (@p <> nil) then
        Result := p (EventData, PalmAppCreatorID, DeviceName, GUID);
    end;
  except
    // Result ist false
  end;
end;

end.
