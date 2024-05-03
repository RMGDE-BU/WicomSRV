{------------------------------------------------------------------------------}
{ Utilities für Taskbarhandling                                                }
{                                                                              }
{ 24.11.2000  GD  Neu                                                          }
{ 09.10.2004  GD  Erweitert um Ändern                                          }
{ 16.07.2008  GD  Taskbar-Größe                                                }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, RMG Messtechnik GmbH 2008               }
{------------------------------------------------------------------------------}
unit TaskBar_Utils;

interface

uses
  Windows, ShellApi, SysUtils, Forms;

function TaskBarAddIcon(
  iMsgConst, iIconHdl: LongWord; iWndHdl: HWND; sTip: string): boolean;
function TaskBarModifyIcon(
  iMsgConst, iIconHdl: LongWord; iWndHdl: HWND; sTip: string): boolean;
function TaskBarRemoveIcon(iWndHdl: HWND): boolean;
function GetTaskbarHeight: integer;  // 16.07.2008
function GetTaskbarWidth: integer;   // 16.07.2008

implementation

{----------------------------------------------------}
function TaskBarAddIcon(
  iMsgConst, iIconHdl: LongWord; iWndHdl: HWND; sTip: string): boolean;
{----------------------------------------------------}
var
  pNID : TNOTIFYICONDATA;
begin
  pNID.cbSize := sizeof(TNOTIFYICONDATA); // Größenangabe der Struktur
  pNID.Wnd := iWndHdl;                    // Handle des Message-Empfängers
  pNID.uID := 1;                          // ID beliebig
  pNID.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;  // siehe Tabelle
  pNID.uCallbackMessage := iMsgConst;     // Message
  pNID.hIcon := iIconHdl;                 // Iconhandle
  StrCopy(pNID.szTip, PChar(sTip));       // Tooltiptext
  Result := Shell_NotifyIcon(NIM_ADD, @pNID);       // Registrieren ...
end;

{----------------------------------------------------}
function TaskBarModifyIcon(
  iMsgConst, iIconHdl: LongWord; iWndHdl: HWND; sTip: string): boolean;
{----------------------------------------------------}
var
  pNID : TNOTIFYICONDATA;
begin
  pNID.cbSize := sizeof(TNOTIFYICONDATA); // Größenangabe der Struktur
  pNID.Wnd := iWndHdl;                    // Handle des Message-Empfängers
  pNID.uID := 1;                          // ID beliebig
  pNID.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;  // siehe Tabelle
  pNID.uCallbackMessage := iMsgConst;     // Message
  pNID.hIcon := iIconHdl;                 // Iconhandle
  StrCopy(pNID.szTip, PChar(sTip));       // Tooltiptext
  Result := Shell_NotifyIcon(NIM_MODIFY, @pNID);    // Ändern ...
end;

{----------------------------------------------------}
function TaskBarRemoveIcon(iWndHdl: HWND): boolean;
{----------------------------------------------------}
var
  pNID : TNOTIFYICONDATA;
begin
  pNID.cbSize := sizeof(TNOTIFYICONDATA);
  pNID.Wnd := iWndHdl;
  pNID.uID := 1;
  Result := Shell_NotifyIcon(NIM_DELETE, @pNID);
end;

// Gibt Höhe der Taskbar zurück
// Rückgabe: Höhe der Taskbar
//----------------------------------------------------
function GetTaskbarHeight: integer;
//----------------------------------------------------
var
  SysTray: Windows.HWND;
  Rect: TRect;
begin
  Result := -1;
  SysTray := FindWindow('Shell_TrayWnd', nil);
  If SysTray <> INVALID_HANDLE_VALUE then begin
    If GetWindowRect(SysTray, Rect) then begin
      if (Rect.Top <= 1) then Result := Rect.Bottom // Taskbar ist oben *)
      else Result := Screen.Height - Rect.Top;
    end;
  end;
end;

// Gibt Weite der Taskbar zurück
// Rückgabe: Weite der Taskbar
//----------------------------------------------------
function GetTaskbarWidth: integer;
//----------------------------------------------------
var
  SysTray: Windows.HWND;
  Rect: TRect;
begin
  Result := -1;
  SysTray := FindWindow('Shell_TrayWnd', nil);
  If SysTray <> INVALID_HANDLE_VALUE then begin
    If GetWindowRect(SysTray, Rect) then begin
      Result := Rect.Right - Rect.Left;
    end;
  end;
end;

end.
