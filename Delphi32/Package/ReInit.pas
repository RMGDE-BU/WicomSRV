{------------------------------------------------------------------------------}
{ Reinitialisieren von Komponenten nach Resourcewechsel (Sprachumschaltung)    }
{                                                                              }
{ 17.06.2004  GD  "Neu" - extrahiert aus Delphi-Beispiel "RichEdit"            }
{ 18.10.2018  WW  ShowHelpFile                                                 }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2004, 2018                                    }
{------------------------------------------------------------------------------}
unit ReInit;

interface

uses
  Windows, ShellApi, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs;

const
  { ID's für verwendete Sprachen }
  ENGLISH = (SUBLANG_ENGLISH_UK shl 10) or LANG_ENGLISH;    // *.ENG
  ENGLISH_US = (SUBLANG_ENGLISH_US shl 10) or LANG_ENGLISH; // *.ENU
  FRENCH  = (SUBLANG_FRENCH shl 10) or LANG_FRENCH;         // *.FRA
  GERMAN  = (SUBLANG_GERMAN shl 10) or LANG_GERMAN;         // *.DEU

procedure ReinitializeForms;
function LoadNewResourceModule(Locale: LCID): Longint;
function SwitchLanguage(iLocale: LCID): boolean;
function GetLocalLanguage: LCID;

procedure ShowHelpFile (iLocale: LCID; sHelpFile_DE: string;
  sHelpFile_EN: string = 'unknown');

implementation

resourcestring
  SFile_s_NotFound = 'Datei %s wurde nicht gefunden.';


type
  TAsInheritedReader = class(TReader)
  public
    procedure ReadPrefix(var Flags: TFilerFlags; var AChildPos: Integer); override;
  end;

procedure TAsInheritedReader.ReadPrefix(var Flags: TFilerFlags; var AChildPos: Integer);
begin
  inherited ReadPrefix(Flags, AChildPos);
  Include(Flags, ffInherited);
end;

{ Gibt die verwendete Systemsprache zurück}
{ Rückgabe: ID der Systemsprache          }
{-----------------------------------------}
function GetLocalLanguage: LCID;
{-----------------------------------------}
begin
  Result := SysLocale.DefaultLCID;
end;

{ Wechselt die aktuell verwendete Sprache  }
{ ! Benötigt zugehörige RES (z.B. *.ENG) ! }
{ Parameter: ID der Sprache (Konstanten)  }
{ Rückgabe: Erfolg ja/nein                }
{-----------------------------------------}
function SwitchLanguage(iLocale: LCID): boolean;
{-----------------------------------------}
begin
  Result := (LoadNewResourceModule(iLocale) <> 0);
  if (Result) then ReinitializeForms;
end;

function SetResourceHInstance(NewInstance: Longint): Longint;
var
  CurModule: PLibModule;
begin
  CurModule := LibModuleList;
  Result := 0;
  while CurModule <> nil do
  begin
    if CurModule.Instance = HInstance then
    begin
      if CurModule.ResInstance <> CurModule.Instance then
        FreeLibrary(CurModule.ResInstance);
      CurModule.ResInstance := NewInstance;
      Result := NewInstance;
      Exit;
    end;
    CurModule := CurModule.Next;
  end;
end;

function LoadNewResourceModule(Locale: LCID): Longint;
var
  FileName: array [0..260] of char;
  P: PChar;
  LocaleName: array[0..4] of Char;
  NewInst: Longint;
begin
  GetModuleFileName(HInstance, FileName, SizeOf(FileName));
  GetLocaleInfo(Locale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
  P := PChar(@FileName) + lstrlen(FileName);
  while (P^ <> '.') and (P <> @FileName) do Dec(P);
  NewInst := 0;
  Result := 0;
  if P <> @FileName then
  begin
    Inc(P);
    if LocaleName[0] <> #0 then
    begin
      // Then look for a potential language/country translation
      lstrcpy(P, LocaleName);
      NewInst := LoadLibraryEx(FileName, 0, LOAD_LIBRARY_AS_DATAFILE);
      if NewInst = 0 then
      begin
        // Finally look for a language only translation
        LocaleName[2] := #0;
        lstrcpy(P, LocaleName);
        NewInst := LoadLibraryEx(FileName, 0, LOAD_LIBRARY_AS_DATAFILE);
      end;
    end;
  end;
  if NewInst <> 0 then
    Result := SetResourceHInstance(NewInst)
end;

function InternalReloadComponentRes(const ResName: string; HInst: THandle; var Instance: TComponent): Boolean;
var
  HRsrc: THandle;
  ResStream: TResourceStream;
  AsInheritedReader: TAsInheritedReader;
begin                   { avoid possible EResNotFound exception }
  if HInst = 0 then HInst := HInstance;
  HRsrc := FindResource(HInst, PChar(ResName), RT_RCDATA);
  Result := HRsrc <> 0;
  if not Result then Exit;
  ResStream := TResourceStream.Create(HInst, ResName, RT_RCDATA);
  try
    AsInheritedReader := TAsInheritedReader.Create(ResStream, 4096);
    try
      Instance := AsInheritedReader.ReadRootComponent(Instance);
    finally
      AsInheritedReader.Free;
    end;
  finally
    ResStream.Free;
  end;
  Result := True;
end;

function ReloadInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;

  function InitComponent(ClassType: TClass): Boolean;
  begin
    Result := False;
    if (ClassType = TComponent) or (ClassType = RootAncestor) then Exit;
    Result := InitComponent(ClassType.ClassParent);
    Result := InternalReloadComponentRes(ClassType.ClassName, FindResourceHInstance(
      FindClassHInstance(ClassType)), Instance) or Result;
  end;

begin
  Result := InitComponent(Instance.ClassType);
end;

procedure ReinitializeForms;
var
  Count: Integer;
  I: Integer;
  Form: TForm;
begin
  Count := Screen.FormCount;
  for I := 0 to Count - 1 do
  begin
    Form := Screen.Forms[I];
    ReloadInheritedComponent(Form, TForm);
  end;
end;

{----------------------------------------------------------}
procedure ShowHelpFile (iLocale: LCID; sHelpFile_DE: string;
  sHelpFile_EN: string = 'unknown');
{----------------------------------------------------------}
{ Hilfe-Datei sprachabhängig mit dem im Windows verknüpften Programm öffnen;
  Übergabe: ID der Sprache
            Name der Hilfe-Datei, deutsch
            Name der Hilfe-Datei, englisch (optional) }
const
  C_Path_Manual = '..\Manual\';  // Standardpfad für PDF-Handbücher

var
  sPath: string;
  sFile: string;
  sMsg: string;

begin
  // PDF-Handbuch sprachabhängig mit dem im Windows konfigurierten PDF-Programm
  // anzeigen; 26.04.2018, WW
  sPath := ExtractFilePath(ParamStr(0)) + C_Path_Manual;

  if (iLocale = ReInit.GERMAN) then
    sFile := sHelpFile_DE
  else if (iLocale = ReInit.ENGLISH) OR (iLocale = ReInit.ENGLISH_US) then
    sFile := sHelpFile_EN
  else
    sFile := sHelpFile_EN;

  if (not FileExists(sPath + sFile)) then
    sFile := sHelpFile_DE;

  // Datei starten
  if (FileExists(sPath + sFile)) then
    ShellExecute (Application.Handle, nil, PChar(sFile), 'open', PChar(sPath),
                  SW_ShowNormal)
  else begin
    sMsg:=Format (SFile_s_NotFound, [ExpandFileName(sPath + sFile)]);
    MessageDlg(sMsg, mtWarning, [mbOK], 0);
  end;
end;

end.
