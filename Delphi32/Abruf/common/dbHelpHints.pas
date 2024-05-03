{------------------------------------------------------------------------------}
{ Hilfetabelle                                                                 }
{                                                                              }
{ 28.05.2001  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001                                          }
{------------------------------------------------------------------------------}
unit dbHelpHints;

interface

uses
  WTables, Classes, DbTables, Db;

type
  TTbHelpHints = class(TObject)
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
    procedure SetHint(sRefString: string; pHelpText: TStrings);
    function GetHint(sRefString: string): TStrings;
    procedure SetDeaHint(sDea: string; pHelpText: TStrings);
    function GetDeaHint(sDea: string): TStrings;
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property Opened: boolean read FOpened;
  end;

implementation

const
  // Haupttabelle Datenelement-Werte-Definitionen         // 28.05.2001
  C_Tb_WHelpHints             = 'WHelpHints';

  C_Tf_WHelpHints_RefString   = 'RefString';   // str10
  C_Tf_WHelpHints_HelpText    = 'HelpText';    // blob

  C_TI_WHelpHints_ixRefString = '';           // Primärindex

  C_HelpRef_DEAHint    = 'DH';

{ Konstructor                                        }
{ Parameter: Datenbankname                           }
{----------------------------------------------------}
constructor TTbHelpHints.Create(sDatabaseName: string);
{----------------------------------------------------}
begin
  inherited Create;
  FDatabaseName := sDatabaseName;
  FOpened := False;  // Flag, ob Object erfolgreich geöffnet wurdeS

  InitComponents(True);
end;

{----------------------------------------------------}
destructor TTbHelpHints.Destroy;
{----------------------------------------------------}
begin
  InitComponents(False);

  inherited Destroy;
end;

{ Initialisieren/Freigeben der internen Komponenten  }
{ Parameter: T-Initialisieren,F-Freigeben            }
{----------------------------------------------------}
procedure TTbHelpHints.InitComponents(bState: boolean);
{----------------------------------------------------}
begin
  if (bState) then begin
    if (not Assigned(FTable)) then begin
      FTable := TTableExt.Create(nil);
      FTable.DatabaseName := Self.DatabaseName;
      FTable.TableName := C_Tb_WHelpHints;
    end;
    FOpened := FTable.Exists;
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
procedure TTbHelpHints.SetDatabaseName(Value: string);
{----------------------------------------------------}
begin
  if (Value <> DatabaseName) then begin
    FDatabaseName:= Value;
    InitComponents(False);
    InitComponents(True);
  end;
end;

{ Schreibt neuen Hinweis in Tabelle                  }
{ Parameter: Referenzstring, Hinweis als TStrings    }
{----------------------------------------------------}
procedure TTbHelpHints.SetHint(sRefString: string; pHelpText: TStrings);
{----------------------------------------------------}
var
  pStream : TStream;
begin
  if (Opened) then begin
    if (not Table.Active) then Table.Open;
    try
      // Datensatz öffnen oder neu anlegen
      if (Table.FindKey([sRefString]))
      then Table.Edit
      else begin
        Table.Append;
        Table.FieldByName(C_Tf_WHelpHints_RefString).asString := sRefString;
      end;

      // Stringliste in BLOB als Stream ablegen
      pStream := TBlobStream.Create(
        TBlobField(Table.FieldByName(C_Tf_WHelpHints_HelpText)), bmWrite);
      try
        pHelpText.SaveToStream(pStream);
        Table.Post;
      finally
       pStream.Free;
      end;
    finally
      Table.Close;
    end;
  end;
end;

{ Liest neuen Hinweis aus Tabelle                    }
{ Parameter: Referenzstring                          }
{ Rückgabe: Hinweis als TStrings                     }
{----------------------------------------------------}
function TTbHelpHints.GetHint(sRefString: string): TStrings;
{----------------------------------------------------}
var
  pStream : TStream;
begin
  Result := TStringList.Create;

  if (Opened) then begin
    if (not Table.Active) then Table.Open;
    try
      // Datensatz öffnen
      if (Table.FindKey([sRefString])) then begin

        // Stringliste in BLOB als Stream ablegen
        pStream := TBlobStream.Create(
          TBlobField(Table.FieldByName(C_Tf_WHelpHints_HelpText)), bmRead);
        try
          Result.LoadFromStream(pStream);
        finally
         pStream.Free;
        end;

      end;
    finally
      Table.Close;
    end;
  end;
end;

{ Schreibt neuen Hinweis in Tabelle                  }
{ Parameter: Datenelementadr., Hinweis als TStrings  }
{----------------------------------------------------}
procedure TTbHelpHints.SetDeaHint(sDea: string; pHelpText: TStrings);
{----------------------------------------------------}
begin
  SetHint(C_HelpRef_DEAHint + sDea, pHelpText);
end;

{ Liest neuen Hinweis aus Tabelle                    }
{ Parameter: Datenelementadresse                     }
{ Rückgabe: Hinweis als TStrings                     }
{----------------------------------------------------}
function TTbHelpHints.GetDeaHint(sDea: string): TStrings;
{----------------------------------------------------}
begin
  Result := GetHint(C_HelpRef_DEAHint + sDea);
end;

end.
