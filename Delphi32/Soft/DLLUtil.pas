{******************************************************************************}
{* Allgemeine Aufrufe von DLLs                                                *}
{* Änderungen:                                                                *}
{*          25.05.02  GD  'LoadWieserIni' grundlegend überarbeitet            *}
{******************************************************************************}
unit DLLUtil;

INTERFACE

uses
  Windows, SysUtils, IniFiles;

function InitDll (LibFileName: string; var LibHandle: THandle; MsgOut: boolean = true): Boolean;
function DoneDll (var LibHandle: THandle): Boolean;
function GetDLLPath (LibName: string): string;

IMPLEMENTATION

const
  WieserSection = 'Wieser';
  WieserItem    = 'WieserIni';

  CBasePath : string = '..\';

procedure LoadWieserIni;
var
  s, sDir : string;
  sProgramIni : string;

begin
  sProgramIni:=ChangeFileExt(ParamStr(0), '.INI');
  with TIniFile.Create(sProgramIni) do
  try
    s := ReadString(WieserSection, WieserItem, CBasePath);
    sDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFilePath(ParamStr(0)));
      CBasePath:=IncludeTrailingBackslash(ExpandFileName(s));
    finally
      SetCurrentDir(sDir);
    end;
  finally
    Free;
  end;
End;

function GetDLLPath (LibName: string): string;
begin
  Result:=CBasePath + Libname;
end;

function InitDll (LibFileName: string; var LibHandle: THandle; MsgOut: boolean = true): Boolean;
resourcestring
  STitle = 'Fehler beim Starten des Programms %s';
  SMsg   = 'Die erforderliche DLL-Datei'#13'%s'#13'wurde nicht gefunden.';
var
  S: string;
  Title: string;
  ProgFileName: string;
begin
  InitDll := False;
  if LibHandle <= HINSTANCE_ERROR then
  begin
    LibHandle := LoadLibrary (pchar (LibFileName));
    if LibHandle <= HINSTANCE_ERROR then
    begin
      if MsgOut then begin
        ProgFileName:=ExtractFileName(ParamStr(0));
        Title:=Format (STitle, [ProgFileName]);
        S:=Format (SMsg, [LibFileName]);
        MessageBox (GetActiveWindow, pchar (S), pchar (Title), MB_ICONEXCLAMATION + MB_OK);
      end;
    end
    else
      InitDll := True;
  end;
end;

function DoneDll (var LibHandle: THandle): Boolean;
begin
  DoneDll := False;
  if LibHandle > HINSTANCE_ERROR then
  begin
    FreeLibrary (LibHandle);
    LibHandle := HINSTANCE_ERROR;
    DoneDll := True;
  end;
end;

begin
  LoadWieserIni;
end.
