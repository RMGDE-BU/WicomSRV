{------------------------------------------------------------------------------}
{ Zugriffe auf Datenelemente-Struktur in WIESER-Geräten                        }
{                                                                              }
{ 23.04.2001  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001                                          }
{------------------------------------------------------------------------------}
unit DbDELCrossRef;

interface

uses
  Classes, SysUtils, WTables, WSysCon, DSG_Utils, GD_Utils;

type
  TERZ2200Params = class(TObject)
    constructor Create(sDatabaseName: string); virtual;
    destructor Destroy; override;
  private
    FGerTypNr : byte;
    FTable : TTableExt;
    FOpened : boolean;
    FDatabaseName : string;
    FERZ2200List : TStrings;
    procedure SetDatabaseName(Value: string);
    procedure SetGerTypNr(Value: byte);
    procedure FillEEERZ2200List;
  protected
    procedure InitComponents(bState: boolean); virtual;
    property Table: TTableExt read FTable;
  public
    function GetERZ2200List(iGerTypNr: byte): TStrings;
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property Opened: boolean read FOpened;
    property GerTypNr: byte read FGerTypNr write SetGerTypNr;
  end;

implementation

{ Konstructor                                        }
{ Parameter: Datenbankname                           }
{----------------------------------------------------}
constructor TERZ2200Params.Create(sDatabaseName: string);
{----------------------------------------------------}
begin
  inherited Create;
  FDatabaseName := sDatabaseName;
  FOpened := False;  // Flag, ob Object erfolgreich geöffnet wurdeS
  FGerTypNr := C_GTypNr_MRG2200;

  InitComponents(True);
end;

{----------------------------------------------------}
destructor TERZ2200Params.Destroy;
{----------------------------------------------------}
begin
  InitComponents(False);

  inherited Destroy;
end;

{ Initialisieren/Freigeben der internen Komponenten  }
{ Parameter: T-Initialisieren,F-Freigeben            }
{----------------------------------------------------}
procedure TERZ2200Params.InitComponents(bState: boolean);
{----------------------------------------------------}
begin
  if (bState) then begin
    if (not Assigned(FTable)) then begin
      FTable := TTableExt.Create(nil);
      FTable.DatabaseName := Self.DatabaseName;
      FTable.TableName := C_Tb_ERZ2200P;
    end;
    FOpened := FTable.Exists;
    if (not Assigned(FERZ2200List)) then begin
      FERZ2200List := TStringList.Create;
      FillEEERZ2200List;
    end;
  end
  else begin
    if (Assigned(FERZ2200List)) then begin
      FERZ2200List.Free;
      FERZ2200List := nil;
    end;

    if (Assigned(FTable)) then begin
      if (FTable.Active) then FTable.Close;
      FTable.Free;
      FTable := nil;
    end;
    FOpened := False;
  end;
end;

{ Setzt den Gerätetyp neu                            }
{ Parameter: Gerätetypnummer                         }
{----------------------------------------------------}
procedure TERZ2200Params.SetGerTypNr(Value: byte);
{----------------------------------------------------}
begin
  if (Value <> FGerTypNr) then begin
    FGerTypNr := Value;
    FillEEERZ2200List;
  end;
end;

{ Setzt den Datenbanknamen neu                       }
{ Parameter: Name der Datenbank                      }
{----------------------------------------------------}
procedure TERZ2200Params.SetDatabaseName(Value: string);
{----------------------------------------------------}
begin
  if (Value <> DatabaseName) then begin
    FDatabaseName:= Value;
    InitComponents(False);
    InitComponents(True);
  end;
end;

{ Füllt Liste mit Definitionen aus Tabelle           }
{  -> Struktur: Referenz<us>DEA<us>Bezeichnung       }
{----------------------------------------------------}
procedure TERZ2200Params.FillEEERZ2200List;
{----------------------------------------------------}
begin
  if (Assigned(FERZ2200List)) and (Opened) then
    with TQueryExt.Create(nil) do
    try
      Self.FERZ2200List.Clear;

      DatabaseName := Self.DatabaseName;
      Sql.Add('SELECT B.' + C_DTF_DSfGDef_GerTypName + ',');
      Sql.Add('A.' + C_Tf_ERZ2200P_RefString + ',');
      Sql.Add('A.' + C_Tf_ERZ2200P_DEA + ',');
      Sql.Add('A.' + C_Tf_ERZ2200P_Bezeichnung);
      Sql.Add('FROM ' + C_Tb_ERZ2200P + ' A, ' + C_DTB_DSfGDef + ' B');
      Sql.Add('WHERE A.' + C_Tf_ERZ2200P_GerTypNr +
        ' = B.' + C_DTF_DSfGDef_GerTypNr);
      if (FGerTypNr > 0) then
        Sql.Add('AND A.' + C_Tf_ERZ2200P_GerTypNr + '=' + IntToStr(FGerTypNr));
      Sql.Add('ORDER BY B.' + C_DTF_DSfGDef_GerTypName +
        ', A.' + C_Tf_ERZ2200P_RefString);
      Open;
      while (not Eof) do begin
        Self.FERZ2200List.Add(FieldByName(C_DTF_DSfGDef_GerTypName).asString +
          Chr(us) + FieldByName(C_Tf_ERZ2200P_RefString).asString +
          Chr(us) + FieldByName(C_Tf_ERZ2200P_DEA).asString +
          Chr(us) + FieldByName(C_Tf_ERZ2200P_Bezeichnung).asString);
        Next;
      end;
      if (Active) then Close;
    finally
      Free;
    end;
end;

{ Gibt Liste mit ERZ2200-Menüstruktur zurück         }
{ Rückgabe: Liste mit ERZ2200-Menüstruktur           }
{----------------------------------------------------}
function TERZ2200Params.GetERZ2200List(iGerTypNr: byte): TStrings;
{----------------------------------------------------}
begin
  Result := TStringList.Create;
  GerTypNr := iGerTypNr;
  if (Assigned(FERZ2200List)) then Result.Assign(FERZ2200List);
end;

end.
