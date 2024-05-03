{ **************************************************************************
  *                                                                        *
  *                        S h e l l L i n k                               *
  *                                                                        *
  **************************************************************************

  File:      unit ShellLink
  Author:    Thorsten Vitt, © 1998-99
  Version:   1.03 from 1999-08-18
  Requires:  Delphi 3.0, Windows 95 or later (or Windows NT 4.0 or later???)
  Contents:
    - class TShellLink
        Author:   Thorsten Vitt, © 1999
        Version:  1.03 from 1999-08-18
        Offers easy access to shell links

  History: 1998       | V. 1.0  | Created

           1999-02-25 | V. 1.01 | Added HotkeyToShortcut, ShortcutToHotkey
                                  functions & ShortcutVCL property

           1999-05-25 | V. 1.02 | Added DescriptiveName property

           1999-08-18 | V. 1.03 | Fixed TShellLink.Load and .Save methods
                                  (thanks to Peter Haas)


}
unit ShellLink;

interface

uses Windows, SysUtils, Classes, Menus, Commctrl, ShellAPI, ActiveX, ShlObj;

const
  slResolveNoUI = -1;              // used by TShellLink.Resolve

type
  TShellLink = class (TObject)
    private
      hRes: HRESULT;  // result of calls to COM functions
      // needed for properties:
      FLinkFile: String;
      procedure SetPath(const Value: String);
      function GetPath: String;
      procedure SetArguments(const Value: String);
      function GetArguments: String;
      procedure SetShowCmd(const Value: Integer);
      function GetShowCmd: Integer;
      procedure SetDescription(const Value: String);
      function GetDescription: String;
      function GetDescriptiveName: string;
      procedure SetHotkey(const Value: WORD);
      function GetHotkey: WORD;
      procedure SetShortcutVCL(const Value: TShortcut);
      function GetShortcutVCL: TShortcut;
      procedure SetIconPath(const Value: String);
      function GetIconPath: String;
      procedure SetIconIndex(const Value: Integer);
      function GetIconIndex: Integer;
      procedure SetWorkingDirectory(const Value: String);
      function GetWorkingDirectory: String;

    protected
      slI: IShellLink;     // das intern verwendete IShellLink-Interface
      pfI: IPersistFile;   // das intern verwendete IPersistFile-Interface
    public
      constructor Create; virtual;
      destructor Destroy; override;

      // The name of the .lnk-file accessed by this object
      property LinkFile: String read FLinkFile write FLinkFile;

      // Shell Link Properties: ------------------------------------------------
      property Path: String read GetPath write SetPath;
      property Arguments: String read GetArguments write SetArguments;
      property ShowCmd: Integer read GetShowCmd write SetShowCmd;
      property Description: String read GetDescription write SetDescription;

      // DescriptiveName is either the Description or,  if Description is '',
      // the DisplayName of the current link file.
      property DescriptiveName: string read GetDescriptiveName;

      // Hotkey is the shortcut in the original IShellLink format.
      property Hotkey: WORD read GetHotkey write SetHotkey;

      // ShortcutVCL is in the format as used by THotkey & TMenuItem.
      property ShortcutVCL: TShortcut read GetShortcutVCL write SetShortcutVCL;
      property IconPath: String read GetIconPath write SetIconPath;
      property IconIndex: Integer read GetIconIndex write SetIconIndex;
      property WorkingDirectory: String read GetWorkingDirectory write SetWorkingDirectory;

      property ComResult: HRESULT read hRes;  // result of calls to COM functions

      // .lnk file functions: --------------------------------------------------
      function Load: Boolean;
      function LoadFromFile(const ALinkFile: String): Boolean;
      function Save: Boolean;
      function SaveToFile(const ALinkFile: String): Boolean;

      function Resolve(const UIHwnd, TimeOut: Integer): Boolean;
      // resolves the shell link
  end;

{ ******************************************************
  HotkeyToShortcut
  Converts a Hotkey, as returned by IShellLink.GetHotkey, to a Shortcut, as
  used in THotkey control. Added 1999-02-25
}
function HotkeyToShortcut(const Hotkey: Word): TShortcut;

{ ******************************************************
  ShortcutToHotkey
  This procedure converts a TShortcut, as returned by THotkey, to a
  Hotkey Word as needed by TShellLink's Hotkey property. Added 1999-02-25 18:16:37
}
function ShortcutToHotkey(const Shortcut: TShortCut): Word;

implementation

constructor TShellLink.Create;
begin
  inherited;
  CoInitialize(nil);     // initializes COM-engine
  hRes := CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
                             IID_IShellLinkA, slI);  // create IShellLink interface
  pfI := slI as IPersistFile;
end;

destructor TShellLink.Destroy;
begin
  pfI := nil; slI := nil;
  CoUninitialize;
end;

function TShellLink.GetPath;
var
  a: array[0..MAX_PATH] of Char;
  fd: TWin32FindData;
begin
  hRes := slI.GetPath(@a, MAX_PATH, fd, 0);
  Result := String(a);
end;

procedure TShellLink.SetPath;
begin
  hRes := slI.SetPath(PChar(Value));
end;

function TShellLink.GetArguments;
var
  a: array[0..MAX_PATH] of Char;
begin
  hRes := slI.GetArguments(@a, MAX_PATH);
  Result := String(a);
end;

procedure TShellLink.SetArguments;
begin
  hRes := slI.SetArguments(PChar(Value));
end;

function TShellLink.GetShowCmd;
begin
  slI.GetShowCmd(Result);
end;

procedure TShellLink.SetShowCmd;
begin
  slI.SetShowCmd(Value);
end;

function TShellLink.GetDescription;
var
  a: array[0..MAX_PATH] of Char;
begin
  hRes := slI.GetDescription(@a, MAX_PATH);
  Result := String(a);
end;

procedure TShellLink.SetDescription;
begin
  hRes := slI.SetDescription(PChar(Value));
end;

function TShellLink.GetDescriptiveName: string;
var
  shfi: TSHFileInfo;
begin
  Result := Description;
  if Result = '' then begin
    FillChar(shfi, SizeOf(shfi), 0);
    SHGetFileInfo(PChar(LinkFile), 0, shfi, SizeOf(shfi), SHGFI_DISPLAYNAME);
    Result := shfi.szDisplayName;
  end;
end;

function TShellLink.GetHotkey;
begin
  hRes := slI.GetHotkey(Result);
end;

procedure TShellLink.SetHotkey;
begin
  hRes := slI.SetHotkey(Value);
end;

function TShellLink.GetShortcutVCL;
begin
  GetShortcutVCL := HotkeyToShortcut(Hotkey);
end;

procedure TShellLink.SetShortcutVCL;
begin
  Hotkey := ShortcutToHotkey(Value);
end;

function TShellLink.GetIconPath;
var
  a: array[0..MAX_PATH] of Char;
  i: Integer;
begin
  hRes := slI.GetIconLocation(@a, MAX_PATH, i);
  Result := String(a);
end;

procedure TShellLink.SetIconPath;
begin
  hRes := slI.SetIconLocation(PChar(Value), IconIndex);
end;

function TShellLink.GetIconIndex;
var
  a: array[0..MAX_PATH] of Char;
begin
  hRes := slI.GetIconLocation(@a, MAX_PATH, Result);
end;

procedure TShellLink.SetIconIndex;
begin
  hRes := slI.SetIconLocation(PChar(IconPath), Value);
end;

function TShellLink.GetWorkingDirectory;
var
  a: array[0..MAX_PATH] of Char;
begin
  hRes := slI.GetWorkingDirectory(@a, MAX_PATH);
  Result := String(a);
end;

procedure TShellLink.SetWorkingDirectory;
begin
  hRes := slI.SetWorkingDirectory(PChar(Value));
end;


function TShellLink.Load: Boolean;
var
  pszLongFileName: PWideChar;
begin
  GetMem(pszLongFileName, Length(LinkFile) * 2 + 4);
  try
    StringToWideChar(LinkFile, pszLongFileName, Length(LinkFile) * 2 + 2);
    Result := Succeeded(pfI.Load(pszLongFileName, 0));
  finally
    FreeMem(pszLongFileName);
  end;
end;

function TShellLink.LoadFromFile;
begin
  LinkFile := ALinkFile;
  Result := Load;
end;


function TShellLink.Save: Boolean;
var
  pszLongFileName: PWideChar;
begin
  GetMem(pszLongFileName, Length(LinkFile) * 2 + 4);
  try
    StringToWideChar(LinkFile, pszLongFileName, Length(LinkFile) * 2 + 2);
    Result := Succeeded(pfI.Save(pszLongFileName, True));
  finally
    FreeMem(pszLongFileName);
  end;
end;

function TShellLink.SaveToFile(const ALinkFile: String): Boolean;
begin
  LinkFile := ALinkFile;
  SaveToFile := Save;
end;

function TShellLink.Resolve(const UIHwnd, TimeOut: Integer): Boolean;
var Flags: Integer; Handle: HWND;
begin
  if UIHwnd = slResolveNoUI then begin
    Handle := 0;
    LongRec(Flags).Hi := TimeOut;
    LongRec(Flags).Lo := SLR_NO_UI;
  end else begin
    Handle := UIHWnd;
    Flags := 0;
  end;

  Result := Succeeded(slI.Resolve(Handle, Flags));
end;

{ ******************************************************
  ShortcutToHotkey
  This procedure converts a TShortcut, as returned by THotkey, to a
  Hotkey Word as needed by TShellLink's Hotkey property
}
function ShortcutToHotkey(const Shortcut: TShortCut): Word;
var
  Key: Word;
  x: Byte;
  Shift: TShiftState;
begin
  ShortcutToKey(Shortcut, Key, Shift);

// (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble);
  x := 0;
  if ssShift in Shift then x := x or HOTKEYF_SHIFT;
  if ssAlt in Shift then x := x or HOTKEYF_ALT;
  if ssCtrl in Shift then x := x or HOTKEYF_CONTROL;
  Result := Key or (x shl 8);
end;

{ ******************************************************
  HotkeyToShortcut
  Converts a Hotkey, as returned by IShellLink.GetHotkey, to a Shortcut, as
  used in THotkey control.
}
function HotkeyToShortcut(const Hotkey: Word): TShortcut;
var
  Key: Word;
  ShiftState: TShiftState;
  x: Byte;
begin
  Key := Hotkey and not ((HOTKEYF_SHIFT or HOTKEYF_ALT or HOTKEYF_CONTROL) shl 8);
  // only use lower byte
  x := Hotkey shr 8;       // get flags
  ShiftState := [];
  if (x and HOTKEYF_SHIFT) <> 0 then Include(ShiftState, ssShift);
  if (x and HOTKEYF_ALT) <> 0 then Include(ShiftState, ssAlt);
  if (x and HOTKEYF_CONTROL) <> 0 then Include(ShiftState, ssCtrl);
  Result := ShortCut(Key, ShiftState);
end;


end.
