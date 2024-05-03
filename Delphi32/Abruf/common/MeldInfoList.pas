{------------------------------------------------------------------------------}
{ Bereitstellung von Meldungsverwaltungs-Infos                                 }
{                                                                              }
{ 21.10.2001  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001                                          }
{------------------------------------------------------------------------------}
unit MeldInfoList;

interface

uses
  SysUtils, Classes, DbTables,
  WSysCon;

type
  TMeldInfoList = class(TObject)
    constructor Create(sDatabaseName: string); virtual;
    destructor Destroy; override;
  private
    FDatabaseName : string;
    FMrgAutoList  : TStringList;
    FMrgManuList  : TStringList;
    FDSfGAutoList : TStringList;
    FDSfGManuList : TStringList;
    procedure FillLists;
  protected
  public
    function GeraetExistsInMeldungen(
      cGerArt: char; iGerId: integer; sAbrArt: string = ''): boolean;
  end;

implementation

{-------------------------------- TMeldInfoList -------------------------------}

{------------------------------------------------------}
constructor TMeldInfoList.Create(sDatabaseName: string);
{------------------------------------------------------}
begin
  inherited Create;

  FDatabaseName := sDatabaseName;
  FMrgAutoList := TStringList.Create;
  FMrgManuList := TStringList.Create;
  FDSfGAutoList := TStringList.Create;
  FDSfGManuList := TStringList.Create;

  FillLists;
end;

{------------------------------------------------------}
destructor TMeldInfoList.Destroy;
{------------------------------------------------------}
begin
  FMrgAutoList.Free;
  FMrgManuList.Free;
  FDSfGAutoList.Free;
  FDSfGManuList.Free;

  inherited Destroy;
end;

{ Füllt Meldungslisten                                 }
{------------------------------------------------------}
procedure TMeldInfoList.FillLists;
{------------------------------------------------------}

  function GetMeldSubList(sBenutzer, sGerArt: string): TStrings;
  begin
    if (sGerArt = C_GerArtDSfG) then begin
      if (sBenutzer = C_BenutzerAuto) then Result := FDSfGAutoList
      else if (sBenutzer = C_BenutzerManu) then Result := FDSfGManuList
      else Result := nil;
    end
    else if (sGerArt = C_GerArtMrg) then begin
      if (sBenutzer = C_BenutzerAuto) then Result := FMrgAutoList
      else if (sBenutzer = C_BenutzerManu) then Result := FMrgManuList
      else Result := nil;
    end
    else Result := nil;
  end;

var
  pSl : TStrings;
begin
  FMrgAutoList.Clear;
  FMrgManuList.Clear;
  FDSfGAutoList.Clear;
  FDSfGManuList.Clear;

  with TTable.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    TableName := ChangeFileExt(C_Tb_WMeldungen, '');
    if (not Exists) then Exit;
  finally
    Free;
  end;

  with TQuery.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    Sql.Add('SELECT DISTINCT ' + C_Tf_WMeldungen_Benutzer + ',');
    Sql.Add(C_Tf_WMeldungen_GeraeteArt + ',' + C_Tf_WMeldungen_GeraeteId);
    Sql.Add('FROM ' + ChangeFileExt(C_Tb_WMeldungen, ''));
    Open;

    while (not Eof) do begin
      pSl := GetMeldSubList(FieldByName(C_Tf_WMeldungen_Benutzer).asString,
        FieldByName(C_Tf_WMeldungen_GeraeteArt).asString);
      if (Assigned(pSl)) then
        pSl.Add(FieldByName(C_Tf_WMeldungen_GeraeteId).asString);
      Next;
    end;

    if (Active) then Close;
  finally
    Free;
  end;
end;

{ Gibt zurück, ob zu Gerät Meldungen existieren        }
{ Parameter: Geräteart, -Id, Abrufart (''=Alle)        }
{------------------------------------------------------}
function TMeldInfoList.GeraetExistsInMeldungen(
  cGerArt: char; iGerId: integer; sAbrArt: string = ''): boolean;
{------------------------------------------------------}
begin
  Result := False;  // Default

  case cGerArt of
    C_GerartDSfG: begin
                    if (sAbrArt <> C_BenutzerManu) and (not Result)
                    then Result := FDSfGAutoList.IndexOf(IntToStr(iGerId)) >= 0;
                    if (sAbrArt <> C_BenutzerAuto) and (not Result)
                    then Result := FDSfGManuList.IndexOf(IntToStr(iGerId)) >= 0;
                  end;
    C_GerartMrg:  begin
                    if (sAbrArt <> C_BenutzerManu) and (not Result)
                    then Result := FMrgAutoList.IndexOf(IntToStr(iGerId)) >= 0;
                    if (sAbrArt <> C_BenutzerAuto) and (not Result)
                    then Result := FMrgManuList.IndexOf(IntToStr(iGerId)) >= 0;
                  end;
  end;
end;

end.
