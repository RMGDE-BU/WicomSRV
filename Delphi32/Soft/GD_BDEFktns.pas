{------------------------------------------------------------------------------}
{ Funktionen für die Borland Database Engine                                   }
{                                                                              }
{ 27.06.2006  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2006                                      }
{------------------------------------------------------------------------------}
unit GD_BDEFktns;

interface

uses
  Windows, SysUtils, Classes,
  DbTables, BDE, FileCtrl, IniFiles, ExtCtrls, Db, BdeConst, DbLogDlg;

function GDRestartBde: boolean;
function GDGetBdeAliasNames(pStrings: TStrings): boolean;
function GDDeleteBDEAlias(const sAliasName: string): boolean;
function GDConfigStdAlias(
  const sAliasName: string; const sPath: TFileName): boolean;

function GDGetBdeSettings(const sNode: string; pStrings: TStrings): boolean;
function GDSetBdeSettings(const sNode: string; pStrings: TStrings): boolean;

implementation

type
  TTrickSession = class(TSession);

{-------------------------------------------------------}
function GDRestartBde: boolean;
{-------------------------------------------------------}
var
  i   : integer;
  Env : DbiEnv;
begin
  try
    for i := 0 to Sessions.Count-1 do Sessions.Sessions[i].Close;

    if IsLibrary then DbiDLLExit;
    DbiExit;
    FillChar(Env, SizeOf(Env), 0);
    StrPLCopy(Env.szLang, SIDAPILangID, SizeOf(Env.szLang) - 1);
    DbiInit(@Env);

    for i := 0 to Sessions.Count-1 do Sessions.Sessions[i].Open;
    Result := True;
  except
    Result := False;
  end;
end;

{-------------------------------------------------------}
function GDGetBdeAliasNames(pStrings: TStrings): boolean;
{-------------------------------------------------------}
var
  pSl : TStringList;
begin
  try
    pStrings.Clear;

    pSl := TStringList.Create;
    try

      with TTrickSession.Create(nil) do
      try
        AutoSessionName := True;
        Open;
        GetAliasNames(pSl);
        Close;
      finally
        Free;
      end;
      
      pSl.Sort;
      pStrings.Assign(pSl);
    finally
      pSl.Free;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

{ Angaben zu den BDE-Einstellungen                      }
{ Parameter: Knoten, Übergabe-Liste für Informationen   }
{ Rückgabe: Erfolg ja/nein                              }
{-------------------------------------------------------}
function GDGetBdeSettings(const sNode: string; pStrings: TStrings): boolean;
{-------------------------------------------------------}
var
  pSl : TStringList;
begin
  pStrings.Clear;
  try
    pSl := TStringList.Create;
    try
      with TTrickSession.Create(nil) do
      try
        AutoSessionName := True;
        Open;
        Session.GetConfigParams(sNode, '', pSl);
        Close;
      finally
        Free;
      end;

      pSl.Sort;
      pStrings.Assign(pSl);
      Result := True;
    finally
      pSl.Free;
    end;
  except
    Result := False;
  end;
end;

{ BDE-Einstellungen überschreiben                       }
{ Parameter: Knoten, Übergabe-Liste mit Informationen   }
{ Rückgabe: Erfolg ja/nein                              }
{-------------------------------------------------------}
function GDSetBdeSettings(const sNode: string; pStrings: TStrings): boolean;
{-------------------------------------------------------}
begin
  try
    with TTrickSession.Create(nil) do
    try
      AutoSessionName := True;
      Open;
      ConfigMode := [cfmPersistent];
      ModifyConfigParams(sNode, '', pStrings);
      SaveConfigFile;
      Close;
      Result := True;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

{-------------------------------------------------------}
function GDDeleteBDEAlias(const sAliasName: string): boolean;
{-------------------------------------------------------}
var
  pSl : TStrings;
  bDel: boolean;
begin
  try
    pSl := TStringList.Create;
    try
      if (GDGetBdeAliasNames(pSl)) then begin
        if (pSl.IndexOf(sAliasName) >= 0) then begin

          with TTrickSession.Create(nil) do
          try
            AutoSessionName := True;
            Open;

            with OpenDatabase(sAliasName) do begin
              bDel := not IsSQLBased;
              Close;
            end;
            if (bDel) then begin
              DeleteAlias(sAliasName);
              SaveConfigFile;
            end;

            Close;
          finally
            Free;
          end;

          Result := (GDGetBdeAliasNames(pSl)) and (pSl.IndexOf(sAliasName) < 0);
        end
        else Result := True;
      end
      else Result := False;
    finally
      pSl.Free;
    end;
  except
    Result := False;
  end;
end;

{-------------------------------------------------------}
function GDConfigStdAlias(
  const sAliasName: string; const sPath: TFileName): boolean;
{-------------------------------------------------------}

  function GetStandardDriver(s: TFileName): string;
  begin
    s := IncludeTrailingBackslash(s);
    if (FileExists(s + '*.DB')) then Result := szPARADOX

    else if (FileExists(s + '*.DBF')) then begin
      if (FileExists(s + '*.FPT')) or (FileExists(s + '*.CDX'))
      then Result := szFOXPRO
      else Result := szDBASE;
    end

    else Result := szPARADOX;

  end;

begin
  try
    if (GDDeleteBDEAlias(sAliasName)) then begin
      Session.AddStandardAlias(sAliasName, sPath, GetStandardDriver(sPath));
      Session.SaveConfigFile;
      Result := True;
    end
    else Result := False;
  except
    Result := False;
  end;
end;

end.
