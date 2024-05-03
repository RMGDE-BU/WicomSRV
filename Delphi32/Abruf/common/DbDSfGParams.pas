{------------------------------------------------------------------------------}
{ Zugriffe auf gespeicherte DSfG-Datenelemente                                 }
{                                                                              }
{ 06.04.2001  GD  Neu                                                          }
{ 05.09.2001  GD  Ersatzwertliste                                              }
{ 20.02.2002  GD  Bugfix                                                       }
{ 12.03.2002  GD  HasDatenelemente                                             }
{ 08.11.2002  GD  Anpassung an SQL Server                                      }
{ 05.05.2003  GD  Vor Löschen prüfen, ob Tabellen vorhanden sind               }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2003                                    }
{------------------------------------------------------------------------------}
unit DbDSfGParams;

interface

uses
  Classes, SysUtils, WTables, WSysCon, GD_Utils;

type
  TDSfGParams = class(TObject)
    constructor Create(sDatabaseName: string); virtual;
    destructor Destroy; override;
  private
    FTable : TTableExt;
    FOpened : boolean;
    FDatabaseName : string;
    procedure SetDatabaseName(Value: string);
  protected
    procedure InitComponents(bState: boolean); virtual;
    property Table: TTableExt read FTable;
  public
    function HasDatenelemente(iInstId: integer): boolean;  // 12.03.2002
    function WriteDatenelemente(
      iInstId : integer; pList: TStrings; bDelete: boolean = True): boolean;
    function ReadDatenelemente(
      iInstId: integer; sDeaVon: string = ''; sDeaBis: string = ''): TStrings;
    function GetErsatzwert(sDea, sValue: string): string;
    function GetErsatzwertList: TStrings;  // 05.09.2001
    function GetStatWithParams: TStrings;
    function Delete(iInstId: integer): boolean;
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property Opened: boolean read FOpened;
  end;

var
  DSfGParams : TDSfGParams;

implementation

{ Konstructor                                        }
{ Parameter: Datenbankname                           }
{----------------------------------------------------}
constructor TDSfGParams.Create(sDatabaseName: string);
{----------------------------------------------------}
begin
  inherited Create;
  FDatabaseName := sDatabaseName;
  FOpened := False;  // Flag, ob Object erfolgreich geöffnet wurdeS

  InitComponents(True);
end;

{----------------------------------------------------}
destructor TDSfGParams.Destroy;
{----------------------------------------------------}
begin
  InitComponents(False);

  inherited Destroy;
end;

{ Initialisieren/Freigeben der internen Komponenten  }
{ Parameter: T-Initialisieren,F-Freigeben            }
{----------------------------------------------------}
procedure TDSfGParams.InitComponents(bState: boolean);
{----------------------------------------------------}
begin
  if (bState) then begin
    if (not Assigned(FTable)) then begin
      FTable := TTableExt.Create(nil);
      FTable.DatabaseName := Self.DatabaseName;
      FTable.TableName := C_Tb_PDEWerte;
    end;
    FOpened := FTable.Exists;
    if (FOpened) then begin
      FTable.IndexDefs.Update;
      FTable.IndexName := FTable.IndexDefs[0].Name;
    end;
  end
  else begin
    if (Assigned(FTable)) then begin
      if (FTable.Active) then FTable.Close;
      FTable.Free;
      FTable := nil;
    end;
    FOpened := False;
  end;
end;

{ Setzt den Datenbanknamen neu                       }
{ Parameter: Name der Datenbank                      }
{----------------------------------------------------}
procedure TDSfGParams.SetDatabaseName(Value: string);
{----------------------------------------------------}
begin
  if (Value <> DatabaseName) then begin
    FDatabaseName:= Value;
    InitComponents(False);
    InitComponents(True);
  end;
end;

{ Sind Datenelemente für eine Instanz gespeichert ?  }
{ Parameter: Instanz-Id                              }
{ Rüchgabe: Datenelemente vorhanden ja/nein          }
{----------------------------------------------------}
function TDSfGParams.HasDatenelemente(iInstId: integer): boolean;
{----------------------------------------------------}
begin
  Result := False; // Default

  if (Opened) then
  with TQueryExt.Create(nil) do
  try
    DatabaseName := (Self.DatabaseName);
    Sql.Add('SELECT A.' + C_Tf_PDEWerte_DEA);
    Sql.Add('FROM ' + C_Tb_PDEWerte + ' A');
    Sql.Add('WHERE A.' + C_Tf_PDEWerte_InstanzId + '=' + IntToStr(iInstId));
    Open;
    try
      Result := (not Eof);
    finally
      if (Active) then Close;
    end;
  finally
    Free;
  end;
end;

{ Liste mit Datenelementen in Datenbank eintragen    }
{ Parameter: Instanz-ID, DE-Liste                    }
{ Rüchgabe: Erfolg Ja/Nein                           }
{----------------------------------------------------}
function TDSfGParams.WriteDatenelemente(
  iInstId : integer; pList: TStrings; bDelete: boolean = True): boolean;
{----------------------------------------------------}
var
  i           : integer;
  sDEA, sWert : string;
begin
  Result := False;  // Default

  if (Opened) then
  try

    // Bisherige Einträge löschen
    if (bDelete) then
      with TQueryExt.Create(nil) do
      try
        DatabaseName := (Self.DatabaseName);
        Sql.Add('DELETE FROM ' + C_Tb_PDEWerte);
        Sql.Add('WHERE ' + C_Tf_PDEWerte_InstanzId + '=' + IntToStr(iInstId));
        ExecSql;
      finally
        Free;
      end;

    // Schleife über alle Datenelemente und Daten in Datenbank schreiben
    with FTable do begin
      if (not Active) then Open;
      for i := 0 to pList.Count-1 do begin
        sDEA := GetStringPart(pList[i], 1);
        sWert := GetStringPart(pList[i], 2);
        if (FindKey([iInstId, sDEA]))
        then Edit
        else begin
          Append;
          FieldByName(C_Tf_PDEWerte_InstanzId).asInteger := iInstId;
          FieldByName(C_Tf_PDEWerte_DEA).asString := sDEA;
        end;
        FieldByName(C_Tf_PDEWerte_Wert).asString := sWert;
        Post;
      end;
      if (Active) then Close;
    end;

    Result := True;
  except
  // Result ist bereits False
  end;
end;

{ Liste mit Datenelementen für eine Instanz holen    }
{ Parameter: Instanz-Id, 1. + letztes Datenelement   }
{ Rüchgabe: Liste mit Datenelementen                 }
{----------------------------------------------------}
function TDSfGParams.ReadDatenelemente(iInstId: integer;
  sDeaVon : string = ''; sDeaBis: string = ''): TStrings;
{----------------------------------------------------}
begin
  Result := TStringList.Create;

  if (Opened) then
  with TQueryExt.Create(nil) do
  try
    DatabaseName := (Self.DatabaseName);
    Sql.Add('SELECT A.' + C_Tf_PDEWerte_DEA + ',');
    Sql.Add('A.' + C_Tf_PDEWerte_Wert + ', C.' + C_Tf_DDeaValueDefSub_DefText);
    Sql.Add('FROM ' + C_Tb_PDEWerte + ' A');
    Sql.Add('LEFT JOIN ' + C_Tb_DDeaValueDefMain + ' B');
    Sql.Add('ON (A.' + C_Tf_PDEWerte_DEA +
      ' = B.' + C_Tf_DDeaValueDefMain_DEA + ')');
    Sql.Add('LEFT JOIN ' + C_Tb_DDeaValueDefSub + ' C');
    Sql.Add('ON (B.' + C_Tf_DDeaValueDefMain_DefIndex +
      ' = C.' + C_Tf_DDeaValueDefSub_DefIndex + ')');
    Sql.Add('WHERE A.' + C_Tf_PDEWerte_InstanzId + '=' + IntToStr(iInstId));
    Sql.Add('AND ((C.' + C_Tf_DDeaValueDefSub_DefValue +
      ' = A.' + C_Tf_PDEWerte_Wert + ')');
    Sql.Add('OR (C.' + C_Tf_DDeaValueDefSub_DefValue + ' IS NULL))');
    if (sDeaVon <> '') then begin
      if (sDeaBis = '')
      then Sql.Add('AND (A.' + C_Tf_PDEWerte_DEA + ' = ''' + sDeaVon + ''')')
      else Sql.Add('AND (A.' + C_Tf_PDEWerte_DEA + ' BETWEEN ''' + sDeaVon +
        ''' AND ''' + sDeaBis + ''')');
    end;
    Open;
    try
      while (not Eof) do begin
        Result.Add(FieldByName(C_Tf_PDEWerte_DEA).asString + Chr(us) +
          FieldByName(C_Tf_PDEWerte_Wert).asString + Chr(us) +
          FieldByName(C_Tf_DDeaValueDefSub_DefText).asString);
        Next;
      end;
    finally
      if (Active) then Close;
    end;
  finally
    Free;
  end;
end;

{ Liste mit Stationen mit gespeicherten Parametern   }
{ Rüchgabe: Liste mit Stationen                      }
{----------------------------------------------------}
function TDSfGParams.GetStatWithParams: TStrings;
{----------------------------------------------------}
begin
  Result := TStringList.Create;

  if (Opened) then
  with TQueryExt.Create(nil) do
  try
    DatabaseName := Self.DatabaseName;
    Sql.Add('SELECT A.' + C_DTF_Station_Stationsname);
    Sql.Add('FROM ' + C_DTB_Station + ' A,');
    Sql.Add(C_DTB_Instanz + ' B,');
    Sql.Add(C_Tb_PDEWerte + ' C');
    Sql.Add('WHERE A.' + C_DTF_Station_StationId + '= B.' +
      C_DTF_Instanz_StationId);
    Sql.Add('AND B.' + C_DTF_Instanz_InstanzId + '= C.' +
      C_Tf_PDEWerte_InstanzId);
    Open;

    while (not Eof) do begin // Listenvergleich schneller als 'DISTINCT','ORDER'
      if (Result.IndexOf(FieldByName(C_DTF_Station_Stationsname).asString) < 0)
      then Result.Add(FieldByName(C_DTF_Station_Stationsname).asString);
      Next;
    end;
    TStringList(Result).Sort;

    if (Active) then Close;
  finally
    Free;
  end;
end;

{ Alle Datenelementen einer Instanz löschen          }
{ Parameter: Instanz-ID                              }
{ Rückgabe: Erfolg Ja/Nein                           }
{----------------------------------------------------}
function TDSfGParams.Delete(iInstId: integer): boolean;
{----------------------------------------------------}
begin
  Result := False;  // Default

  try

    with TTableExt.Create(nil) do   // 05.05.2003
    try
      DatabaseName := Self.DatabaseName;
      TableName := C_Tb_PDEWerte;
      Result := not Exists;
    finally
      Free;
    end;

    if (not Result) and (Opened) then
      with TQueryExt.Create(nil) do
      try
        DatabaseName := (Self.DatabaseName);
        Sql.Add('DELETE FROM ' + C_Tb_PDEWerte);
        Sql.Add('WHERE ' + C_Tf_PDEWerte_InstanzId + '=' + IntToStr(iInstId));
        ExecSql;
        Result := True;
      finally
        Free;
      end;

  except
    // Result ist bereits False
  end;
end;

{ Gibt Ersatz-(Anzeige)Wert für DEA/Wert zurück      }
{ Parameter: DEA, Wert                               }
{ Rückgabe: Ersatzwert                               }
{----------------------------------------------------}
function TDSfGParams.GetErsatzwert(sDea, sValue: string): string;
{----------------------------------------------------}
begin
  Result := sValue;  // Default

  with TTableExt.Create(nil) do  // 20.02.2002
  try
    DatabaseName := (Self.DatabaseName);
    TableName := C_Tb_DDeaValueDefMain;
    if (not Exists) then Exit;
  finally
    Free;
  end;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := (Self.DatabaseName);
    Sql.Add('SELECT A.' + C_Tf_DDeaValueDefSub_DefText);
    Sql.Add('FROM ' + C_Tb_DDeaValueDefSub + ' A,' + C_Tb_DDeaValueDefMain + ' B');
    Sql.Add('WHERE B.' + C_Tf_DDeaValueDefMain_DEA + ' = ''' + sDea + '''');
    Sql.Add('AND A.' + C_Tf_DDeaValueDefSub_DefIndex +
      ' = B.' + C_Tf_DDeaValueDefMain_DefIndex);
    Sql.Add('AND A.' + C_Tf_DDeaValueDefSub_DefValue + ' = ''' + sValue + '''');
    Open;
    if (not Eof) then Result := Fields[0].asString;
    if (Active) then Close;
  finally
    Free;
  end;
end;

{ Gibt Ersatz-(Anzeige)Wertliste für DEA/Wert zurück }
{ Rückgabe: Ersatzwertliste (DEA|Wert|Ersatzwert)    }
{----------------------------------------------------}
function TDSfGParams.GetErsatzwertList: TStrings; // 05.09.2001
{----------------------------------------------------}
begin
  Result := TStringList.Create;

  with TTableExt.Create(nil) do  // 20.02.2002
  try
    DatabaseName := (Self.DatabaseName);
    TableName := C_Tb_DDeaValueDefMain;
    if (not Exists) then Exit;
  finally
    Free;
  end;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := (Self.DatabaseName);
    Sql.Add('SELECT A.' + C_Tf_DDeaValueDefMain_DEA + ',');
    Sql.Add('B.' + C_Tf_DDeaValueDefSub_DefValue + ',');
    Sql.Add('B.' + C_Tf_DDeaValueDefSub_DefText);
    Sql.Add('FROM ' + C_Tb_DDeaValueDefMain + ' A,' + C_Tb_DDeaValueDefSub + ' B');
    Sql.Add('WHERE A.' + C_Tf_DDeaValueDefMain_DefIndex +
      ' = B.' + C_Tf_DDeaValueDefSub_DefIndex);
    Sql.Add('ORDER BY ' + C_Tf_DDeaValueDefMain_DEA + ',' +
      C_Tf_DDeaValueDefSub_DefValue);
    Open;

    while (not Eof) do begin
      Result.Add(FieldByName(C_Tf_DDeaValueDefMain_DEA).asString + Chr(us) +
                 FieldByName(C_Tf_DDeaValueDefSub_DefValue).asString + Chr(us) +
                 FieldByName(C_Tf_DDeaValueDefSub_DefText).asString);
      Next;
    end;

    if (Active) then Close;
  finally
    Free;
  end;
end;

end.
