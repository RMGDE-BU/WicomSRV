{------------------------------------------------------------------------------}
{ Spezifische Datenbankzugriffe für DSfGMomentanwertdarstellung und -parametr. }
{                                                                              }
{ 15.01.2001  GD  Neu                                                          }
{ 18.04.2001  GD  Wertedefinitionen für Datenelemente                          }
{ 05.05.2003  GD  Vor Löschen prüfen, ob Tabellen vorhanden sind               }
{                                                                              }
{ ACHTUNG : Transaktionskontrolle wird wegen guten Programierstils verwendet,  }
{           funktioniert aber bei Paradox nicht !                              }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2003                                    }
{------------------------------------------------------------------------------}
unit DbDSfGMom;

interface

uses
  Windows, SysUtils, Classes, Dialogs, DbTables, WSysCon, GD_Utils, DMomLists,
  Controls, Variants;

type
  TMyCommonMomDb = class;
  TDbMomStaDef = class;
  TDbMomInstDef = class;
  TDbMomJournal = class;
  TDbMomDeaDef = class;
  TDbDSfGMom = class;

  TMyCommonMomDb = class(TObject)
    constructor Create(
      sDatabaseName: string; bShowMessages: boolean = False); virtual;
    destructor Destroy; override;
  private
    FDatabaseName : string;
    FShowMessages : boolean;
  protected
    procedure InitDataComponents(bState: boolean); virtual;
    procedure InitThisTable(var pTable: TTable; sTableName: string);
    function GetNextIndex(sTableName, sFieldName: string): integer;
    property ShowMessages: boolean read FShowMessages;
  public
    property DatabaseName: string read FDatabaseName;
  end;

  TDbMomStaDef = class(TMyCommonMomDb)
  private
    FTbMomStatDefMain : TTable;
    FTbMomStatDefSub  : TTable;
  protected
    procedure InitDataComponents(bState: boolean); override;
  public
    function GetDeaDefList(iStaId: integer): TStaDefDeaList;
    function EntryExists(iStaId: integer; sBezeichnung: string): boolean;
    procedure DeleteMainEntry(iStaId: integer; sBezeichnung: string);
    procedure DeleteSubEntry(iStaId: integer;
      sBezeichnung: string; cInstAdr: char; sDEA: string = '');
    function NewDeaList(
      iStaId: integer; sBezeichnung: string; pList: TDSfGMomValueList): boolean;
    function Delete(iStaId: integer): boolean;
  end;

  TDbMomInstDef = class(TMyCommonMomDb)
  private
    FTbMomInstDefMain : TTable;
    FTbMomInstDefSub  : TTable;
  protected
    procedure InitDataComponents(bState: boolean); override;
  public
    procedure DeleteMainEntry(cInstTyp: char; sBezeichnung: string);
    procedure DeleteSubEntry(cInstTyp: char; sBezeichnung, sDEA: string);
    function EntryExists(cInstTyp: char; sBezeichnung: string): boolean;
    function NewDeaList(
      cInstTyp: char; sBezeichnung: string; pDeaList: TStrings): boolean;
    function GetDeaDefList(cInstTyp: char): TDeaDefList;
  end;

  TDbMomJournal = class(TMyCommonMomDb)
  private
    FTbMomJrnMain     : TTable;
    FTbMomJrnSub      : TTable;
  protected
    procedure InitDataComponents(bState: boolean); override;
  public
    function NewJournalEntry(iStaId: integer): integer;
    function NewSubJournalEntry(iIndex: integer; cInstAdr: char;
      sDea, sOldValue, sNewValue: string): boolean;
    function Delete(iStaId: integer): boolean;
    function EmptyTable: boolean;
  end;

  TDbMomDeaDef = class(TMyCommonMomDb)   // 18.04.2001
  private
    FTbDeaMain     : TTable;
    FTbDeaSub      : TTable;
  protected
    procedure InitDataComponents(bState: boolean); override;
  public
    function GetDeaDefValues(sDea: string; iGerTyp: byte = 7): TStrings;
  end;

  TDbDSfGMom = class(TObject)
    constructor Create(
      sDatabaseName: string; bShowMessages: boolean = False); virtual;
    destructor Destroy; override;
  private
    FDatabaseName : string;
    FMomStaObj   : TDbMomStaDef;
    FMomInstObj  : TDbMomInstDef;
    FMomJournal  : TDbMomJournal;
    FDeaValueDef : TDbMomDeaDef;
    FShowMessages : boolean;
    procedure InitDataComponents(bState: boolean);
  protected
    property ShowMessages: boolean read FShowMessages;
  public
    procedure DeleteInstDefEntry(cInstTyp: char; sBezeichnung, sDEA: string);
    procedure DeleteStaDefEntry(iStaId: integer; sBezeichnung: string;
      cInstAdr: char = '0'; sDEA: string = '');
    function DeleteStationsId(iStaId: integer): boolean;
    function NewDeaList(
      cInstTyp: char; sBezeichnung: string; pDeaList: TStrings): boolean;
    function NewStaList(
      iStaId: integer; sBezeichnung: string; pList: TDSfGMomValueList): boolean;
    function NewJournalEntry(iStaId: integer): integer;
    function NewSubJournalEntry(iIndex: integer; cInstAdr: char;
      sDea, sOldValue, sNewValue: string): boolean;
    function GetDeaInstDefList(cInstTyp: char): TDeaDefList;
    function GetDeaStaDefList(iStaId: integer): TStaDefDeaList;
    function GetDeaDefValues(sDea: string; iGerTyp: byte = 7): TStrings;
    property DatabaseName: string read FDatabaseName;
  end;

implementation

{--------------------------------- TMyCommonMomDb -----------------------------}

{----------------------------------------------}
constructor TMyCommonMomDb.Create(
  sDatabaseName: string; bShowMessages: boolean = False);
{----------------------------------------------}
begin
  inherited Create;

  FDatabaseName := sDatabaseName;
  FShowMessages := bShowMessages;
  InitDataComponents(True);
end;

{----------------------------------------------}
destructor TMyCommonMomDb.Destroy;
{----------------------------------------------}
begin
  InitDataComponents(False);

  inherited Destroy;
end;

{ Initialisiert eine Tabelle                  }
{ Parameter: Tabelle, Tabellenname            }
{---------------------------------------------}
procedure TMyCommonMomDb.InitThisTable(var pTable: TTable; sTableName: string);
{---------------------------------------------}
begin
  pTable := TTable.Create(nil);
  pTable.DatabaseName := DatabaseName;
  pTable.TableName := sTableName;
  if (not pTable.Exists) then begin
    if (ShowMessages) then
      MessageDlg('Benötigte Tabelle ''' + sTableName + ''' existiert nicht !',
        mtError, [mbOk], 0);
    pTable.Free;
    pTable := nil;
  end;
end;

{ Gibt nächsten Tabellenindex zurück (integer) }
{ Parameter: TabellenName, FeldName            }
{ Rückgabe: Nächster Index oder -1             }
{----------------------------------------------}
function TMyCommonMomDb.GetNextIndex(sTableName, sFieldName: string): integer;
{----------------------------------------------}
var
  v : Variant;
begin
  Result := -1;  // Vorgabe
  v := NULL;

  with TQuery.Create(nil) do
  try
    DatabaseName := Self.DatabaseName;

    Sql.Add('SELECT MAX(' + sFieldName + ') FROM ' + sTableName);
    Open;
    if (not Eof) then v := Fields[0].Value;
    Close;

    if (v = NULL) then Result := 1
    else if ((VarType(v) and varInteger) > 0) then Result := v + 1;
  finally
    Free;
  end;
end;

{ Initialisieren/Freigeben der DB-Komponenten  }
{ Parameter: True-initial.; False-freigeben    }
{----------------------------------------------}
procedure TMyCommonMomDb.InitDataComponents(bState: boolean);
{----------------------------------------------}
begin
end;

{---------------------------------- TDbMomStaDef ------------------------------}

{ Initialisieren/Freigeben der DB-Komponenten  }
{ Parameter: True-initial.; False-freigeben    }
{----------------------------------------------}
procedure TDbMomStaDef.InitDataComponents(bState: boolean);
{----------------------------------------------}
begin
  if (bState) then begin
    InitThisTable(FTbMomStatDefMain, C_Tb_DMomStationMain);
    InitThisTable(FTbMomStatDefSub, C_Tb_DMomStationSub);
  end
  else begin
    if (Assigned(FTbMomStatDefMain)) then FTbMomStatDefMain.Free;
    if (Assigned(FTbMomStatDefSub)) then FTbMomStatDefSub.Free;
  end;
end;

{ Prüft, ob Eintrag mit Bezeichnung existiert  }
{ Parameter: StationsId, Eintragsbezeichnung   }
{ Rückgabe: Existiert Ja/Nein                  }
{----------------------------------------------}
function TDbMomStaDef.EntryExists(iStaId: integer; sBezeichnung: string): boolean;
{----------------------------------------------}
begin
  Result := False;  // Default

  if (Assigned(FTbMomStatDefMain)) then
    with TQuery.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;
      Sql.Add('SELECT * FROM ' + C_Tb_DMomStationMain);
      Sql.Add('WHERE ' + C_Tf_DMomStationMain_StationsId + '=' + IntToStr(iStaId));
      Sql.Add('AND LOWER(' + C_Tf_DMomStationMain_Bezeichnung + ')=''' +
        LowerCase(sBezeichnung) + '''');
      Open;
      Result := (not Eof);
      Close;
    finally
      Free;
    end;
end;

{ Trägt neue Liste mit Datenelementen ein      }
{ Parameter: InstanzTyp, Bezeichnung, Liste    }
{ Rückgabe: Erfolg Ja/Nein                     }
{----------------------------------------------}
function TDbMomStaDef.NewDeaList(
  iStaId: integer; sBezeichnung: string; pList: TDSfGMomValueList): boolean;
{----------------------------------------------}
var
  pDatabase : TDatabase;
  iIndex    : integer;
  bTransact : boolean;
  i         : integer;
begin
  Result := False;  // Vorgabe

  if (Assigned(FTbMomStatDefMain)) then
    try
      if (pList.Count = 0) then begin
        MessageDlg('Keine Datenelemente definert', mtWarning, [mbOk], 0);
      end
      else begin
    // Prüfen, ob Eintrag bereits existiert
        if (EntryExists(iStaId, sBezeichnung))
        then begin
          if (MessageDlg('Eintrag ''' + sBezeichnung + ''' existiert für ' +
            'StationsId ''' + IntToStr(iStaId) + ''' bereits !'#13#10#13#10 +
            'Überschreiben ?', mtConfirmation, [mbYes, mbCancel], 0) = mrCancel)
          then Exit
          else DeleteMainEntry(iStaId, sBezeichnung);
        end;

        if (not FTbMomStatDefMain.Active) then FTbMomStatDefMain.Open;
        if (not FTbMomStatDefSub.Active) then FTbMomStatDefSub.Open;
        try
    // Transaktion starten
          pDatabase := FTbMomStatDefMain.Database;
          pDatabase.TransIsolation := tiDirtyRead;
          bTransact := pDatabase.InTransaction;
          if (not bTransact) then pDatabase.StartTransaction;
          try
      // Nächsten Index ermitteln
            iIndex :=
              GetNextIndex(C_Tb_DMomStationMain, C_Tf_DMomStationMain_StationMomIndex);
            if (iIndex <= 0) then Exit;

      // Eintrag in Hauptabelle
            FTbMomStatDefMain.Append;
            FTbMomStatDefMain.FieldByName(
              C_Tf_DMomStationMain_StationMomIndex).asInteger := iIndex;
            FTbMomStatDefMain.FieldByName(
              C_Tf_DMomStationMain_StationsId).asInteger := iStaId;
            FTbMomStatDefMain.FieldByName(
              C_Tf_DMomStationMain_Bezeichnung).asString := sBezeichnung;
            FTbMomStatDefMain.Post;

      // Einträge in Subtabelle
            for i := 0 to pList.Count-1 do begin
              FTbMomStatDefSub.Append;
              FTbMomStatDefSub.FieldByName(
                C_Tf_DMomStationSub_StationMomIndex).asInteger := iIndex;
              FTbMomStatDefSub.FieldByName(C_Tf_DMomStationSub_InstanzAdresse).
                asString := pList.InstanzAdresse[i];
              FTbMomStatDefSub.FieldByName(
                C_Tf_DMomStationSub_DEA).asString := pList.DEAdresse[i];
              FTbMomStatDefSub.Post;
            end;

            if (not bTransact) then pDatabase.Commit;

            Result := True;

          finally
            if (not bTransact) and (pDatabase.InTransaction) then
              pDatabase.Rollback;
          end;
        finally
          FTbMomStatDefMain.Close;
          FTbMomStatDefSub.Close;
        end;

      end;
    except
      on E: Exception do begin
        raise Exception.Create(
         'Fehler beim Eintragen der Datenelemente aufgetreten !'#13#10#13#10 +
         E.Message);
      end;
    end;
end;

{ Holt Liste mit sta.spez. Datenelementen      }
{ Parameter: Stations-Id                       }
{ Rückgabe: Liste mit sta.spez. Datenelementen }
{----------------------------------------------}
function TDbMomStaDef.GetDeaDefList(iStaId: integer): TStaDefDeaList;
{----------------------------------------------}
var
  pSl : TDeaDefList;
  sName  : string;
  iIndex : integer;

  procedure AddEintrag(iIndex: integer);
  var
    p : TOneDeaDefList;
    c : char;
  begin
    with TQuery.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;
      Sql.Add('SELECT ' + C_Tf_DMomStationSub_InstanzAdresse + ',');
      Sql.Add(C_Tf_DMomStationSub_DEA);
      Sql.Add('FROM ' + C_Tb_DMomStationSub);
      Sql.Add('WHERE ' + C_Tf_DMomStationSub_StationMomIndex + '=' +
        IntToStr(iIndex));
      Open;

      // Schleife über alle möglichen Instanzadressen
      p := TOneDeaDefList.Create;
      try
        for c := 'A' to '_' do begin
          p.Clear;
          p.Bezeichnung := c;
          First;
          while (not Eof) do begin
            if (FieldByName(C_Tf_DMomStationSub_InstanzAdresse).asString[1] = c)
            then p.Add(FieldByName(C_Tf_DMomInstSub_DEA).asString);
            Next;
          end;
          if (p.Count > 0) then pSl.NeuerEintrag(p);
        end;
      finally
        p.Free;
      end;

      Close;
    finally
      Free;
    end;
  end;

begin
  Result := TStaDefDeaList.Create;

  if (Assigned(FTbMomStatDefMain)) then
    with TQuery.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;
      Sql.Add('SELECT ' + C_Tf_DMomStationMain_StationMomIndex + ',');
      Sql.Add(C_Tf_DMomStationMain_Bezeichnung);
      Sql.Add('FROM ' + C_Tb_DMomStationMain);
      Sql.Add('WHERE ' + C_Tf_DMomStationMain_StationsId + '=' + IntToStr(iStaId));

      Open;

      pSl := TDeaDefList.Create;
      try

        while (not Eof) do begin
          pSl.Clear;
          sName := FieldByName(C_Tf_DMomStationMain_Bezeichnung).asString;
          iIndex := FieldByName(C_Tf_DMomStationMain_StationMomIndex).asInteger;
          AddEintrag(iIndex);
          Result.AddDeaDefList(pSl, sName);
          Next;
        end;

      finally
        pSl.Free;
      end;

      Close;
    finally
      Free;
    end;
end;

{ Löscht einen anw.def. Ordner                 }
{ Parameter: StationsId, Ordnerbezeichnung     }
{----------------------------------------------}
procedure TDbMomStaDef.DeleteMainEntry(iStaId: integer; sBezeichnung: string);
{----------------------------------------------}
var
  iIndex : integer;
begin
  if (Assigned(FTbMomStatDefMain)) then
    with TQuery.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;

    // Index ermitteln
      Sql.Add('SELECT ' + C_Tf_DMomStationMain_StationMomIndex);
      Sql.Add('FROM ' + C_Tb_DMomStationMain);
      Sql.Add('WHERE ' + C_Tf_DMomStationMain_StationsId + '=' + IntToStr(iStaId));
      Sql.Add('AND LOWER(' + C_Tf_DMomStationMain_Bezeichnung + ')=''' +
        LowerCase(sBezeichnung) + '''');
      Open;
      if (Eof) then Exit else iIndex := Fields[0].asInteger;
      Close;

    // Subeinträge löschen
      Sql.Clear;
      Sql.Add('DELETE FROM ' + C_Tb_DMomStationSub);
      Sql.Add('WHERE ' + C_Tf_DMomStationSub_StationMomIndex + '=' +
        IntToStr(iIndex));
      ExecSql;

    // Haupteintrag löschen
      Sql.Clear;
      Sql.Add('DELETE FROM ' + C_Tb_DMomStationMain);
      Sql.Add('WHERE ' + C_Tf_DMomStationMain_StationMomIndex + '=' +
        IntToStr(iIndex));
      ExecSql;

    finally
      if (Active) then Close;
      Free;
    end;
end;

{ Löscht einen anw.def. Parameter              }
{ Parameter: StationsId, Ordnerbezeichnung,    }
{            InstanzAdresse, DEA oder ''       }
{----------------------------------------------}
procedure TDbMomStaDef.DeleteSubEntry(iStaId: integer;
  sBezeichnung: string; cInstAdr: char; sDEA: string = '');
{----------------------------------------------}
var
  iIndex : integer;
begin
  if (Assigned(FTbMomStatDefMain)) then
    with TQuery.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;

    // Index ermitteln
      Sql.Add('SELECT ' + C_Tf_DMomStationMain_StationMomIndex);
      Sql.Add('FROM ' + C_Tb_DMomStationMain);
      Sql.Add('WHERE ' + C_Tf_DMomStationMain_StationsId + '=' + IntToStr(iStaId));
      Sql.Add('AND LOWER(' + C_Tf_DMomStationMain_Bezeichnung + ')=''' +
        LowerCase(sBezeichnung) + '''');
      Open;
      if (Eof) then Exit else iIndex := Fields[0].asInteger;
      Close;

    // Subeintrag löschen
      Sql.Clear;
      Sql.Add('DELETE FROM ' + C_Tb_DMomStationSub);
      Sql.Add('WHERE ' + C_Tf_DMomStationSub_StationMomIndex + '=' +
        IntToStr(iIndex));
      Sql.Add('AND ' + C_Tf_DMomStationSub_InstanzAdresse + '=''' + cInstAdr + '''');
      if (sDEA <> '') then
        Sql.Add('AND ' + C_Tf_DMomStationSub_DEA + '=''' + sDEA + '''');
      ExecSql;

    finally
      if (Active) then Close;
      Free;
    end;
end;

{ Löscht alle Definitionen für eine Station    }
{ Parameter: StationsId                        }
{ Rückgabe: Erfolg Ja/Nein                     }
{----------------------------------------------}
function TDbMomStaDef.Delete(iStaId: integer): boolean;
{----------------------------------------------}
begin
  Result := not Assigned(FTbMomStatDefMain);  // Default  // 05.05.2003

  if (not Result) then
    try
      with TQuery.Create(nil) do
      try
        DatabaseName := Self.DatabaseName;

      // Subeinträge löschen
        Sql.Clear;
        Sql.Add('DELETE FROM ' + C_Tb_DMomStationSub);
        Sql.Add('WHERE (' + C_Tf_DMomStationSub_StationMomIndex + ' IN (');
        Sql.Add('  SELECT ' + C_Tf_DMomStationMain_StationMomIndex);
        Sql.Add('  FROM ' + C_Tb_DMomStationMain);
        Sql.Add('  WHERE ' + C_Tf_DMomStationMain_StationsId + '=' +
          IntToStr(iStaId) + '))');
        ExecSql;

      // Haupteinträge löschen
        Sql.Clear;
        Sql.Add('DELETE FROM ' + C_Tb_DMomStationMain);
        Sql.Add('WHERE ' + C_Tf_DMomStationMain_StationsId + '=' +
           IntToStr(iStaId));
        ExecSql;

        Result := True;
      finally
        if (Active) then Close;
        Free;
      end;

    except
      Result := False;
    end;
end;

{--------------------------------- TDbMomInstDef ------------------------------}

{ Initialisieren/Freigeben der DB-Komponenten  }
{ Parameter: True-initial.; False-freigeben    }
{----------------------------------------------}
procedure TDbMomInstDef.InitDataComponents(bState: boolean);
{----------------------------------------------}
begin
  if (bState) then begin
    InitThisTable(FTbMomInstDefMain, C_Tb_DMomInstMain);
    InitThisTable(FTbMomInstDefSub, C_Tb_DMomInstSub);
  end
  else begin
    if (Assigned(FTbMomInstDefMain)) then FTbMomInstDefMain.Free;
    if (Assigned(FTbMomInstDefSub)) then FTbMomInstDefSub.Free;
  end;
end;

{ Prüft, ob Eintrag mit Bezeichnung existiert  }
{ Parameter: InstanzTyp, Eintragsbezeichnung   }
{ Rückgabe: Existiert Ja/Nein                  }
{----------------------------------------------}
function TDbMomInstDef.EntryExists(cInstTyp: char; sBezeichnung: string): boolean;
{----------------------------------------------}
begin
  Result := False;  // Default

  if (Assigned(FTbMomInstDefMain)) then
    with TQuery.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;
      Sql.Add('SELECT * FROM ' + C_Tb_DMomInstMain);
      Sql.Add('WHERE ' + C_Tf_DMomInstMain_InstanzTyp + '=''' + cInstTyp + '''');
      Sql.Add('AND LOWER(' + C_Tf_DMomInstMain_Bezeichnung + ')=''' +
        LowerCase(sBezeichnung) + '''');
      Open;
      Result := (not Eof);
      Close;
    finally
      Free;
    end;
end;

{ Trägt neue Liste mit Datenelementen ein      }
{ Parameter: InstanzTyp, Bezeichnung, Liste    }
{ Rückgabe: Erfolg Ja/Nein                     }
{----------------------------------------------}
function TDbMomInstDef.NewDeaList(
  cInstTyp: char; sBezeichnung: string; pDeaList: TStrings): boolean;
{----------------------------------------------}
var
  pDatabase : TDatabase;
  iIndex    : integer;
  bTransact : boolean;
  i         : integer;
begin
  Result := False;  // Vorgabe
  if (Assigned(FTbMomInstDefMain)) then
    try
      if (EntryExists(cInstTyp, sBezeichnung))
      then begin
        MessageDlg('Eintrag ''' + sBezeichnung + ''' existiert für ' +
          'Instanztyp ''' + cInstTyp + ''' bereits !', mtError, [mbOk], 0);
      end
      else if ((pDeaList.Count = 1) and (pDeaList[0] = '')) then begin
        MessageDlg('Keine Datenelemente definert', mtWarning, [mbOk], 0);
      end
      else begin

        if (not FTbMomInstDefMain.Active) then FTbMomInstDefMain.Open;
        if (not FTbMomInstDefSub.Active) then FTbMomInstDefSub.Open;
        try
    // Transaktion starten
          pDatabase := FTbMomInstDefMain.Database;
          pDatabase.TransIsolation := tiDirtyRead;
          bTransact := pDatabase.InTransaction;
          if (not bTransact) then pDatabase.StartTransaction;
          try
      // Nächsten Index ermitteln
            iIndex :=
              GetNextIndex(C_Tb_DMomInstMain, C_Tf_DMomInstMain_InstMomIndex);
            if (iIndex <= 0) then Exit;

      // Eintrag in Hauptabelle
            FTbMomInstDefMain.Append;
            FTbMomInstDefMain.FieldByName(
              C_Tf_DMomInstMain_InstMomIndex).asInteger := iIndex;
            FTbMomInstDefMain.FieldByName(
              C_Tf_DMomInstMain_InstanzTyp).asString := cInstTyp;
            FTbMomInstDefMain.FieldByName(
              C_Tf_DMomInstMain_Bezeichnung).asString := sBezeichnung;
            FTbMomInstDefMain.Post;

      // Einträge in Subtabelle
            for i := 0 to pDeaList.Count-1 do begin
              FTbMomInstDefSub.Append;
              FTbMomInstDefSub.FieldByName(
                C_Tf_DMomInstSub_InstMomIndex).asInteger := iIndex;
              FTbMomInstDefSub.FieldByName(
                C_Tf_DMomInstSub_DEA).asString := pDeaList[i];
              FTbMomInstDefSub.Post;
            end;

            if (not bTransact) then pDatabase.Commit;

            Result := True;

          finally
            if (not bTransact) and (pDatabase.InTransaction) then
              pDatabase.Rollback;
          end;
        finally
          FTbMomInstDefMain.Close;
          FTbMomInstDefSub.Close;
        end;

      end;
    except
      on E: Exception do begin
        raise Exception.Create(
         'Fehler beim Eintragen der Datenelemente aufgetreten !'#13#10 +
          #13#10);
      end;
    end;
end;

{ Holt Liste mit instspez. Datenelementen      }
{ Parameter: InstanzTyp,                       }
{ Rückgabe: Liste mit instspez. Datenelementen }
{----------------------------------------------}
function TDbMomInstDef.GetDeaDefList(cInstTyp: char): TDeaDefList;
{----------------------------------------------}
var
  pSl: TOneDeaDefList;

  procedure AddEintrag(iIndex: integer);
  begin
    with TQuery.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;
      Sql.Add('SELECT ' + C_Tf_DMomInstSub_DEA);
      Sql.Add('FROM ' + C_Tb_DMomInstSub);
      Sql.Add('WHERE ' + C_Tf_DMomInstSub_InstMomIndex + '=' + IntToStr(iIndex));
      Open;
      while (not Eof) do begin
        pSl.Add(FieldByName(C_Tf_DMomInstSub_DEA).asString);
        Next;
      end;
      Close;
    finally
      Free;
    end;
  end;

begin
  Result := TDeaDefList.Create;

  if (Assigned(FTbMomInstDefMain)) then
    with TQuery.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;
      Sql.Add('SELECT ' + C_Tf_DMomInstMain_InstMomIndex + ',');
      Sql.Add(C_Tf_DMomInstMain_Bezeichnung);
      Sql.Add('FROM ' + C_Tb_DMomInstMain);
      Sql.Add('WHERE ' + C_Tf_DMomInstMain_InstanzTyp + '=''' + cInstTyp + '''');

      Open;

      pSl := TOneDeaDefList.Create;
      try

        while (not Eof) do begin
          pSl.Bezeichnung := FieldByName(C_Tf_DMomInstMain_Bezeichnung).asString;
          AddEintrag(FieldByName(C_Tf_DMomInstMain_InstMomIndex).asInteger);
          Result.NeuerEintrag(pSl);
          pSl.Clear;
          Next;
        end;

      finally
        pSl.Free;
      end;
      Close;
    finally
      Free;
    end;
end;

{ Löscht einen anw.def. Ordner                 }
{ Parameter: InstanzTyp, Ordnerbezeichnung     }
{----------------------------------------------}
procedure TDbMomInstDef.DeleteMainEntry(cInstTyp: char; sBezeichnung: string);
{----------------------------------------------}
var
  iIndex : integer;
begin
  if (Assigned(FTbMomInstDefMain)) then
    with TQuery.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;

    // Index ermitteln
      Sql.Add('SELECT ' + C_Tf_DMomInstMain_InstMomIndex);
      Sql.Add('FROM ' + C_Tb_DMomInstMain);
      Sql.Add('WHERE ' + C_Tf_DMomInstMain_InstanzTyp + '=''' + cInstTyp + '''');
      Sql.Add('AND LOWER(' + C_Tf_DMomInstMain_Bezeichnung + ')=''' +
        LowerCase(sBezeichnung) + '''');
      Open;
      if (Eof) then Exit else iIndex := Fields[0].asInteger;
      Close;

    // Subeinträge löschen
      Sql.Clear;
      Sql.Add('DELETE FROM ' + C_Tb_DMomInstSub);
      Sql.Add('WHERE ' + C_Tf_DMomInstSub_InstMomIndex + '=' + IntToStr(iIndex));
      ExecSql;

    // Haupteintrag löschen
      Sql.Clear;
      Sql.Add('DELETE FROM ' + C_Tb_DMomInstMain);
      Sql.Add('WHERE ' + C_Tf_DMomInstMain_InstMomIndex + '=' + IntToStr(iIndex));
      ExecSql;

    finally
      if (Active) then Close;
      Free;
    end;
end;

{ Löscht einen anw.def. Parameter              }
{ Parameter: InstanzTyp, Ordnerbezeichnung, DEA}
{----------------------------------------------}
procedure TDbMomInstDef.DeleteSubEntry(
  cInstTyp: char; sBezeichnung, sDEA: string);
{----------------------------------------------}
var
  iIndex : integer;
begin
  if (Assigned(FTbMomInstDefMain)) then
    with TQuery.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;

    // Index ermitteln
      Sql.Add('SELECT ' + C_Tf_DMomInstMain_InstMomIndex);
      Sql.Add('FROM ' + C_Tb_DMomInstMain);
      Sql.Add('WHERE ' + C_Tf_DMomInstMain_InstanzTyp + '=''' + cInstTyp + '''');
      Sql.Add('AND LOWER(' + C_Tf_DMomInstMain_Bezeichnung + ')=''' +
        LowerCase(sBezeichnung) + '''');
      Open;
      if (Eof) then Exit else iIndex := Fields[0].asInteger;
      Close;

    // Subeintrag löschen
      Sql.Clear;
      Sql.Add('DELETE FROM ' + C_Tb_DMomInstSub);
      Sql.Add('WHERE ' + C_Tf_DMomInstSub_InstMomIndex + '=' + IntToStr(iIndex));
      Sql.Add('AND ' + C_Tf_DMomInstSub_DEA + '=''' + sDEA + '''');
      ExecSql;

    finally
      if (Active) then Close;
      Free;
    end;
end;

{--------------------------------- TDbMomJournal ------------------------------}

{ Initialisieren/Freigeben der DB-Komponenten  }
{ Parameter: True-initial.; False-freigeben    }
{----------------------------------------------}
procedure TDbMomJournal.InitDataComponents(bState: boolean);
{----------------------------------------------}
begin
  if (bState) then begin
    InitThisTable(FTbMomJrnMain, C_Tb_DParamJrnMain);
    InitThisTable(FTbMomJrnSub, C_Tb_DParamJrnSub);
  end
  else begin
    if (Assigned(FTbMomJrnMain)) then FTbMomJrnMain.Free;
    if (Assigned(FTbMomJrnSub)) then FTbMomJrnSub.Free;
  end;
end;

{ Erzeugt einen neuen Journal-Haupteintrag     }
{ Parameter: StationsId                        }
{ Rückgabe: Indexnummer des Eintrags oder -1   }
{----------------------------------------------}
function TDbMomJournal.NewJournalEntry(iStaId: integer): integer;
{----------------------------------------------}
begin
  Result := -1;  // Default

  if (Assigned(FTbMomJrnMain)) then
    try
      if (not FTbMomJrnMain.Active) then FTbMomJrnMain.Open;
      try
        FTbMomJrnMain.Append;
        FTbMomJrnMain.FieldByName(C_Tf_DParamJrnMain_DParamIndex).asInteger :=
          GetNextIndex(C_Tb_DParamJrnMain, C_Tf_DParamJrnMain_DParamIndex);
        FTbMomJrnMain.FieldByName(C_Tf_DParamJrnMain_StationsId).asInteger :=
          iStaId;
       FTbMomJrnMain.FieldByName(C_Tf_DParamJrnMain_Anwender).asString :=
          GetMyUserName;
       FTbMomJrnMain.FieldByName(C_Tf_DParamJrnMain_DatumZeit).asDateTime :=
          Now;
        FTbMomJrnMain.Post;
        Result := FTbMomJrnMain.FieldByName(
          C_Tf_DParamJrnMain_DParamIndex).asInteger;
      finally
        FTbMomJrnMain.Close;
      end;
    except
      Result := -1;
    end;
end;

{ Erzeugt einen neuen Journal-Untereintrag     }
{ Parameter: Index, InstanzAdresse, DEA        }
{            alter Wert, neuer Wert            }
{ Rückgabe: Erfolg Ja/Nein                     }
{----------------------------------------------}
function TDbMomJournal.NewSubJournalEntry(iIndex: integer; cInstAdr: char;
  sDea, sOldValue, sNewValue: string): boolean;
{----------------------------------------------}
begin
  Result := False;  // Default

  if (Assigned(FTbMomJrnMain)) and (Assigned(FTbMomJrnSub)) then
  try
    // Prüfen, ob Index existiert
    if (not FTbMomJrnMain.Active) then FTbMomJrnMain.Open;
    try
      if (not FTbMomJrnMain.FindKey([iIndex])) then Exit;
    finally
      FTbMomJrnMain.Close;
    end;
    // Eintragen
    if (not FTbMomJrnSub.Active) then FTbMomJrnSub.Open;
    try
      FTbMomJrnSub.Append;
      FTbMomJrnSub.FieldByName(C_Tf_DParamJrnSub_DParamIndex).asInteger :=
        iIndex;
      FTbMomJrnSub.FieldByName(C_Tf_DParamJrnSub_InstanzAdresse).asString :=
        cInstAdr;
      FTbMomJrnSub.FieldByName(C_Tf_DParamJrnSub_DEA).asString :=
        sDea;
      FTbMomJrnSub.FieldByName(C_Tf_DParamJrnSub_WertAlt).asString :=
        sOldValue;
      FTbMomJrnSub.FieldByName(C_Tf_DParamJrnSub_WertNeu).asString :=
        sNewValue;
      FTbMomJrnSub.Post;
      Result := True;
    finally
      FTbMomJrnSub.Close;
    end;
  except
    Result := False;
  end;
end;

{ Löscht alle Journaleinträge einer Station    }
{ Parameter: StationsId                        }
{ Rückgabe: Erfolg Ja/Nein                     }
{----------------------------------------------}
function TDbMomJournal.Delete(iStaId: integer): boolean;
{----------------------------------------------}
begin
  Result := not Assigned(FTbMomJrnMain);  // Default  // 05.05.2003

  if (not Result) then
    try
      with TQuery.Create(nil) do
      try
        DatabaseName := Self.DatabaseName;

      // Subeinträge löschen
        Sql.Clear;
        Sql.Add('DELETE FROM ' + C_Tb_DParamJrnSub);
        Sql.Add('WHERE (' + C_Tf_DParamJrnSub_DParamIndex + ' IN (');
        Sql.Add('  SELECT ' + C_Tf_DParamJrnMain_DParamIndex);
        Sql.Add('  FROM ' + C_Tb_DParamJrnMain);
        Sql.Add('  WHERE ' + C_Tf_DParamJrnMain_StationsId + '=' +
          IntToStr(iStaId) + '))');
        ExecSql;

      // Haupteinträge löschen
        Sql.Clear;
        Sql.Add('DELETE FROM ' + C_Tb_DParamJrnMain);
        Sql.Add('WHERE ' + C_Tf_DParamJrnMain_StationsId + '=' +
           IntToStr(iStaId));
        ExecSql;

        Result := True;
      finally
        if (Active) then Close;
        Free;
      end;

    except
      Result := False;
    end;
end;

{ Löscht alle Journaleinträge                  }
{ Rückgabe: Erfolg Ja/Nein                     }
{----------------------------------------------}
function TDbMomJournal.EmptyTable: boolean;
{----------------------------------------------}
begin
  Result := False;

  if (Assigned(FTbMomJrnMain)) and (Assigned(FTbMomJrnSub)) then begin
    if (FTbMomJrnMain.Active) then FTbMomJrnMain.Close;
    FTbMomJrnMain.EmptyTable;
    if (FTbMomJrnSub.Active) then FTbMomJrnSub.Close;
    FTbMomJrnSub.EmptyTable;
    Result := True;
  end;
end;

{--------------------------------- TDbMomDeaDef ------------------------------}

{ Initialisieren/Freigeben der DB-Komponenten  }
{ Parameter: True-initial.; False-freigeben    }
{----------------------------------------------}
procedure TDbMomDeaDef.InitDataComponents(bState: boolean);
{----------------------------------------------}
begin
  if (bState) then begin
    InitThisTable(FTbDeaMain, C_Tb_DDeaValueDefMain);
    InitThisTable(FTbDeaSub, C_Tb_DDeaValueDefSub);
  end
  else begin
    if (Assigned(FTbDeaMain)) then FTbDeaMain.Free;
    if (Assigned(FTbDeaSub)) then FTbDeaSub.Free;
  end;
end;

{ Gibt Definitionen zu einer DEA zurück        }
{ Parameter: DEA, Gerätetyp-Nummer             }
{ Rückgabe: Liste der Definitionen             }
{----------------------------------------------}
function TDbMomDeaDef.GetDeaDefValues(sDea: string; iGerTyp: byte = 7): TStrings;
{----------------------------------------------}
begin
  Result := TStringList.Create;


  if (Assigned(FTbDeaMain)) then
    with TQuery.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;
      Sql.Add('SELECT B.' + C_Tf_DDeaValueDefSub_DefText + ',');
      Sql.Add('CAST(B.' + C_Tf_DDeaValueDefSub_DefValue + ' AS INTEGER) AS Feld1');
      Sql.Add('FROM ' + C_Tb_DDeaValueDefMain + ' A,' + C_Tb_DDeaValueDefSub + ' B');
      Sql.Add('WHERE A.' + C_Tf_DDeaValueDefMain_DEA + '=''' + sDea + '''');
      Sql.Add('AND ((A.' + C_Tf_DDeaValueDefMain_GerTypNr + '=' + IntToStr(iGerTyp) + ')');
      Sql.Add('OR (A.' + C_Tf_DDeaValueDefMain_GerTypNr + '= 0))');
      Sql.Add('AND B.' + C_Tf_DDeaValueDefSub_DefIndex + ' = A.' +
        C_Tf_DDeaValueDefMain_DefIndex);
      Sql.Add('ORDER BY Feld1');
      Open;
      while (not Eof) do begin
        Result.Add(Fields[0].asString + Chr(us) + Fields[1].asString);
        Next;
      end;
      Close;
   finally
     Free;
   end;
end;

{---------------------------------- TDbDSfGMom --------------------------------}

{----------------------------------------------}
constructor TDbDSfGMom.Create(
  sDatabaseName: string; bShowMessages: boolean = False);
{----------------------------------------------}
begin
  inherited Create;

  FDatabaseName := sDatabaseName;
  InitDataComponents(True);
end;

{----------------------------------------------}
destructor TDbDSfGMom.Destroy;
{----------------------------------------------}
begin
  InitDataComponents(False);

  inherited Destroy;
end;

{ Initialisieren/Freigeben der DB-Komponenten  }
{ Parameter: True-initial.; False-freigeben    }
{----------------------------------------------}
procedure TDbDSfGMom.InitDataComponents(bState: boolean);
{----------------------------------------------}
begin
  if (bState) then begin
    FMomStaObj := TDbMomStaDef.Create(DatabaseName, ShowMessages);
    FMomInstObj := TDbMomInstDef.Create(DatabaseName, ShowMessages);
    FMomJournal := TDbMomJournal.Create(DatabaseName, ShowMessages);
    FDeaValueDef := TDbMomDeaDef.Create(DatabaseName, ShowMessages);
  end
  else begin
    if (Assigned(FMomStaObj)) then FMomStaObj.Free;
    if (Assigned(FMomInstObj)) then FMomInstObj.Free;
    if (Assigned(FMomJournal)) then FMomJournal.Free;
    if (Assigned(FDeaValueDef)) then FDeaValueDef.Free;
  end;
end;

{ Trägt neue Liste mit Datenelementen ein      }
{ Parameter: InstanzTyp, Bezeichnung, Liste    }
{ Rückgabe: Erfolg Ja/Nein                     }
{----------------------------------------------}
function TDbDSfGMom.NewDeaList(
  cInstTyp: char; sBezeichnung: string; pDeaList: TStrings): boolean;
{----------------------------------------------}
begin
  Result := FMomInstObj.NewDeaList(cInstTyp, sBezeichnung, pDeaList);
end;

{ Trägt neue Liste mit Datenelementen ein      }
{ Parameter: StationsId, Bezeichnung, Liste    }
{ Rückgabe: Erfolg Ja/Nein                     }
{----------------------------------------------}
function TDbDSfGMom.NewStaList(
 iStaId: integer; sBezeichnung: string; pList: TDSfGMomValueList): boolean;
{----------------------------------------------}
begin
  Result := FMomStaObj.NewDeaList(iStaId, sBezeichnung, pList);
end;

{ Holt die instspez. DEA-Definitionen          }
{ Parameter: Instanztyp                        }
{ Rückgabe: Liste mit instspez. DEA-Def.       }
{----------------------------------------------}
function TDbDSfGMom.GetDeaInstDefList(cInstTyp: char): TDeaDefList;
{----------------------------------------------}
begin
  Result := FMomInstObj.GetDeaDefList(cInstTyp);
end;

{ Holt die stationsspez. DEA-Definitionen      }
{ Parameter: StationsId                        }
{ Rückgabe: Liste mit stationsspez. DEA-Def.   }
{----------------------------------------------}
function TDbDSfGMom.GetDeaStaDefList(iStaId: integer): TStaDefDeaList;
{----------------------------------------------}
begin
  Result := FMomStaObj.GetDeaDefList(iStaId);
end;

{ Löscht einen anw.def. inst.spez. Eintrag     }
{ Parameter: InstanzTyp, Ordnerbezeichnung, DEA}
{   Bei sDEA = '' wird der Ordner gelöscht     }
{----------------------------------------------}
procedure TDbDSfGMom.DeleteInstDefEntry(
  cInstTyp: char; sBezeichnung, sDEA: string);
{----------------------------------------------}
begin
  if (sDEA = '')
    then FMomInstObj.DeleteMainEntry(cInstTyp, sBezeichnung)
    else FMomInstObj.DeleteSubEntry(cInstTyp, sBezeichnung, sDEA);
end;

{ Löscht einen anw.def. inst.spez. Eintrag     }
{ Parameter: InstanzTyp, Ordnerbezeichnung,    }
{            InstanzAdresse, DEA               }
{----------------------------------------------}
procedure TDbDSfGMom.DeleteStaDefEntry(iStaId: integer; sBezeichnung: string;
  cInstAdr: char = '0'; sDEA: string = '');
{----------------------------------------------}
begin
  if (cInstAdr = '0')
    then FMomStaObj.DeleteMainEntry(iStaId, sBezeichnung)
    else FMomStaObj.DeleteSubEntry(iStaId, sBezeichnung, cInstAdr, sDEA);
end;

{ Erzeugt einen neuen Journal-Haupteintrag     }
{ Parameter: StationsId                        }
{ Rückgabe: Indexnummer des Eintrags oder -1   }
{----------------------------------------------}
function TDbDSfGMom.NewJournalEntry(iStaId: integer): integer;
{----------------------------------------------}
begin
  Result := FMomJournal.NewJournalEntry(iStaId);
end;

{ Erzeugt einen neuen Journal-Untereintrag     }
{ Parameter: Index, InstanzAdresse, DEA        }
{            alter Wert, neuer Wert            }
{ Rückgabe: Erfolg Ja/Nein                     }
{----------------------------------------------}
function TDbDSfGMom.NewSubJournalEntry(iIndex: integer; cInstAdr: char;
  sDea, sOldValue, sNewValue: string): boolean;
{----------------------------------------------}
begin
  Result := FMomJournal.NewSubJournalEntry(
    iIndex, cInstAdr, sDea, sOldValue, sNewValue);
end;

{ Gibt Definitionen zu einer DEA zurück        }
{ Parameter: DEA, Gerätetyp-Nummer             }
{ Rückgabe: Liste der Definitionen             }
{----------------------------------------------}
function TDbDSfGMom.GetDeaDefValues(sDea: string; iGerTyp: byte = 7): TStrings;
{----------------------------------------------}
begin
  Result := FDeaValueDef.GetDeaDefValues(sDea, iGerTyp);
end;

{ Löscht MOM-Spezifischen Einträge einer Stat. }
{ Parameter: Stations-ID                       }
{----------------------------------------------}
function TDbDSfGMom.DeleteStationsId(iStaId: integer): boolean;
{----------------------------------------------}
begin
  Result := True;  // Vorgabe

  if (not FMomStaObj.Delete(iStaId)) then Result := False;
  if (not FMomJournal.Delete(iStaId)) then Result := False;
end;

end.
