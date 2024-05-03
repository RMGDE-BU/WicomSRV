{******************************************************************************}
{* Unit: erweitertes Tabellenobjekt                                           *}
{* 09.03.1999 WW                                                              *}
{                                                                              }
{ 20.04.1999 GD Erweiterung für TQuery - ExecSql und Open                      }
{ 29.01.2001 WW TTableExt.CreateTable mit Exists-Prüfung                       }
{ 13.03.2003 GD Explizites Setzen des PrivateDir von Session                   }
{ 25.03.2003 GD Bei Wiederholen von Tabellenoperationen auf EXISTS prüfen      }
{ 27.01.2004 GD EXISTS überschrieben, Update von IIndexinfos in eigener proc   }
{ 18.10.2004 GD PrivateDir nicht bei DLLs                                      }
{ 11.10.2005 GD PrivateDir-Funktionen für allgemeinen Gebrauch veröffentlicht  }
{ 10.11.2005 GD "GetExists" um Abfangen "Tabelle geöffnett" erweitert          }
{ 01.03.2006 GD TTableExt um erweiterte "IndexDefs" bereichert         	       }
{ 13.02.2006 GD Einkommentierbare zusätzliche Debugs                           }
{ 27.07.2009 GD Transaktionskontrolle                                          }
{ 19.08.2009 GD "PrivateDir" allgemeingültig setzen                            }
{ 19.12.2012 WW TADOQueryExt                                                   }
{ 03.07.2013 WW IFNDEF NO_BDETEMPDIR                                           }
{******************************************************************************}
Unit WTables;

INTERFACE

uses
//LogFile, WSysCon, // 13.02.2007
  SysUtils, Windows, Classes, DB, DBTables, BDE, IniFiles, ADODB,
  T_Zeit, FileCtrl, WStream, Novell;

type

  { Tabellenöffnungsmodi }

  TOpenMode = (o_shared, o_exclusive);


  { erweitertes TTable-Objekt }

  TTableExt = class (TTable)
  private
    MaxZugriffVersuche     : integer;     { max. Anzahl von Zugriffsversuchen }
    MaxZugriffVerzoegerung : integer; { max. Wartezeit in ms bis zum nächsten Zugriffsversuch }
    FTransact              : boolean; // Transaktion aktiv?  // 27.07.2009
    function OpenTable (OpenMode: TOpenMode): boolean;
  protected
    function GetExists: boolean;
    function GetIndexDefs: TIndexDefs;
    procedure SetIndexDefs(pValue: TIndexDefs);
  public
    constructor Create (AOwner: TComponent); override;
    function OpenShared: boolean;
    function OpenExclusive: boolean;
    function SafeUpdateIndexDefs: boolean;
    function SafeUpdateFieldDefs: boolean;
    procedure Close;
    function CreateTable: boolean;
    function EmptyTable: boolean;
    function DeleteTable: boolean;

    procedure SetMaxZugriffVersuche (Versuche: integer);
    procedure SetMaxZugriffVerzoegerung (Verzoegerung: integer);
    property Exists: boolean read GetExists;
    property IndexDefs: TIndexDefs read GetIndexDefs write SetIndexDefs;
  end;

  { erweitertes TQuery-Objekt }

  TQueryExt = class (TQuery)
  private
    MaxZugriffVersuche    : integer;     { max. Anzahl von Zugriffsversuchen }
    MaxZugriffVerzoegerung: integer; { max. Wartezeit in ms bis zum nächsten Zugriffsversuch }
  public
    constructor Create (anOwner: TComponent); override;
    function ExecSql: boolean;
    function Open: boolean;
    procedure SetMaxZugriffVersuche (Versuche: integer);
    procedure SetMaxZugriffVerzoegerung (Verzoegerung: integer);
  end;

  { erweitertes TADOQuery-Objekt }

  TADOQueryExt = class (TADOQuery)
  private
    MaxZugriffVersuche    : integer;     { max. Anzahl von Zugriffsversuchen }
    MaxZugriffVerzoegerung: integer; { max. Wartezeit in ms bis zum nächsten Zugriffsversuch }
  public
    constructor Create (anOwner: TComponent); override;
    function ExecSql: integer;
    function Open: boolean;
    procedure SetMaxZugriffVersuche (Versuche: integer);
    procedure SetMaxZugriffVerzoegerung (Verzoegerung: integer);
  end;


function GetBDEPrivateDir: string;  // 19.08.2009
function WTablesGetTempDir(
  bWieser: boolean = False; bCheckUnique: boolean = True): string;     // 13.03.2003
procedure WTablesInsertToDeleteFile(sFileName: TFileName = '');   // 13.03.2003
procedure WTablesDeleteDirTempFiles;        // 13.03.2003

IMPLEMENTATION

Const
  CMaxZugriffVersuche     =   80;  { 02.07.2003, WW/SM: auf 80 erhöht wg. Problem im DSfG-Abrufmodul
                                     beim Öffnen der WJournal.db, um neuen Satz einzutragen (-> Bayerngas, Gamess)
                                     bis 12.10.2000: 40, davor: 100 }
  CMaxZugriffVerzoegerung = 1000;

  C_DirDeleteFile         = 'WDelDir.DAT';
  C_Debug                 = False;  // 13.02.2007

var
  FPrivateTempDir : TFileName;

{-----------------------------------------------------------------------}
procedure WriteDebugLog (APath: string; ALogFilename: string; S: string);  // 13.02.2007
{-----------------------------------------------------------------------}
(*
var
  CLF: TCustomLogFile;
*)
begin
(*
  if (IsDebugFlag) then begin
    CLF:=TCustomLogFile.Create (APath, ALogFilename, false);
    try
     CLF.Write (S);
    finally
      CLF.Free;
    end;
  end;
*)  
end;

{------------------------------------------------}
constructor TTableExt.Create (AOwner: TComponent);
{------------------------------------------------}
begin
  inherited Create (AOwner);
  SetMaxZugriffVersuche (CMaxZugriffVersuche);
  SetMaxZugriffVerzoegerung (CMaxZugriffVerzoegerung);
  FTransact := False;
  Randomize;
end;

{-------------------------------------}
function TTableExt.GetExists: boolean;
{-------------------------------------}
var
//  H : HDBICur;
//  E : DBIResult;
  i : integer;
begin
  Result := Active;
  if Result or (TableName = '') then Exit;
  SetDBFlag(dbfTable, True);
  try
    if (Database.IsSQLBased) then begin
      with TQuery.Create(nil) do
      try
        DatabaseName := Self.DatabaseName;
        SessionName := Self.SessionName;
        Sql.Add('SELECT * FROM ' + ChangeFileExt(TableName, ''));
        Sql.Add('WHERE 1 = 0');
        try
          Open;
          Result := True;
          Close;
        except
          on E:Exception do begin
            Result := False;
            if (E is EDBEngineError) then begin
              for i := 0 to (E as EDBEngineError).ErrorCount-1 do
                if ((E as EDBEngineError).Errors[0].ErrorCode = DBIERR_FILELOCKED)
                then begin
                  Result := True;
                  Break;
                end;
            end;
          end;
        end;
      finally
        Free;
      end;
    end
    else Result := FileExists(GetFileName);
  finally
    SetDBFlag(dbfTable, False);
  end;
end;

{-------------------------------------}
function TTableExt.OpenShared: boolean;
{-------------------------------------}
{ Tabelle nicht exklusiv öffnen, andere Anwendungen können Tabelle öffnen;
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  Result:=OpenTable (o_shared);
end;

{----------------------------------------}
function TTableExt.OpenExclusive: boolean;
{----------------------------------------}
{ Tabelle exklusiv öffnen, andere Anwendungen können Tabelle nicht öffnen;
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  Result:=OpenTable (o_exclusive);
end;

{----------------------------------------------------------}
function TTableExt.OpenTable (OpenMode: TOpenMode): boolean;
{----------------------------------------------------------}
{ Tabelle öffnen;
  Übergabe: OpenMode (exclusive/shared)
  Ergebnis: true, wenn Tabelle spätestens nach MaxZugriffVersuche geöffnet werden konnte }
var
  i : integer;
  s : string;
begin
  Result:=false;
  Exclusive:=OpenMode = o_exclusive;

  // Falls SQL und keine Transaktion -> Transaktion starten
  i:=0;
  repeat
    inc (i);
    try
      Open;
      if (OpenMode = o_exclusive) then begin
        FTransact := (Database.IsSQLBased) and (not Database.InTransaction);
        if (FTransact) then Database.StartTransaction;
      end;
      Result := True;
    except
      on E:EDatabaseError do begin
        if (C_Debug) then begin
          s := E.Message;
          if (i = 1) then WriteDebugLog(Session.NetFileDir, 'WTables.err',
            'TTableExt.OpenTable (' + TableName + ' - ' +
            IntToStr(Integer(OpenMode)) + '): ' + s);
        end;
        Delay (Random (MaxZugriffVerzoegerung));   { keine weiteren gleichzeitigen Zugriffe }
      end;
    end;
  until (not Exists) or (Result) OR (i >= MaxZugriffVersuche);

  if not Result then Exclusive:=False;
  if (C_Debug) and (not Result) then begin
    if (not Exists) then s := 'No such table';
    WriteDebugLog(Session.NetFileDir, 'WTables.err',
      'TTableExt.OpenTable  (' + TableName + ' - ' +
      IntToStr(Integer(OpenMode)) + ') FALSE: ' + s);
  end;
end;

{----------------------------------------------------------}
function TTableExt.SafeUpdateIndexDefs: boolean;
{----------------------------------------------------------}
var
  i: integer;
  s: string;
begin
  Result := false;
  i := 0;
  repeat
    Inc(i);
    try
      IndexDefs.Update;
      Result:=true;
    except
      on E:EDatabaseError do begin
        if (C_Debug) then begin
          s := E.Message;
          if (i = 1) then
            WriteDebugLog(Session.NetFileDir, 'WTables.err',
              'TTableExt.SafeUpdateIndexDefs (' + TableName + '): ' + s);
        end;
        Delay (Random (MaxZugriffVerzoegerung));   { keine weiteren gleichzeitigen Zugriffe }
      end;
    end;
  until (not Exists) or (Result) OR (i >= MaxZugriffVersuche);
  if (C_Debug) and (not Result) then begin
    if (not Exists) then s := 'No such table';
    WriteDebugLog(Session.NetFileDir, 'WTables.err',
      'TTableExt.SafeUpdateIndexDefs  (' + TableName + ') FALSE: ' + s);
  end;
end;

{----------------------------------------------------------}
function TTableExt.SafeUpdateFieldDefs: boolean;
{----------------------------------------------------------}
var
  i : integer;
  s : string;
begin
  Result:=false;
  i:=0;
  repeat
    inc (i);
    try
      FieldDefs.Update;
      Result:=true;
    except
      on E:EDatabaseError do begin
        if (C_Debug) then begin
          s := E.Message;
          if (i = 1) then WriteDebugLog(Session.NetFileDir, 'WTables.err',
            'TTableExt.SafeUpdateFieldDefs: ' + s);
        end;
        Delay (Random (MaxZugriffVerzoegerung));   { keine weiteren gleichzeitigen Zugriffe }
      end;
    end;
  until (not Exists) or (Result) OR (i >= MaxZugriffVersuche);
  if (C_Debug) and (not Result) then begin
    if (not Exists) then s := 'No such table';
    WriteDebugLog(Session.NetFileDir, 'WTables.err',
      'TTableExt.SafeUpdateFieldDefs  (' + TableName + ') FALSE: ' + s);
  end;
end;

{------------------------}
procedure TTableExt.Close;
{------------------------}
begin
  if (Assigned(Database)) and (FTransact) and (Database.InTransaction) then
  try
    Database.Commit;
  except
    Database.Rollback;
  end;

  inherited Close;

  Exclusive := False;
end;

{--------------------------------------}
function TTableExt.CreateTable: boolean;
{--------------------------------------}
{ Tabelle neu anlegen;
  Ergebnis: true, wenn Tabelle spätestens nach MaxZugriffVersuche angelegt werden konnte }
var
  i : integer;
  s : string;
begin
  Result:=false;
  i:=0;
  repeat
    inc (i);
    try
      { zwischendurch auf Vorhandensein prüfen, damit nicht von anderem Prozeß
        erzeugte Tabelle überschrieben wird  WW 29.01.2001 }
      if not Exists then
        inherited CreateTable;
      Result:=true;
    except
      on E:EDatabaseError do begin
        if (C_Debug) then begin
          s := E.Message;
          if (i = 1) then WriteDebugLog(Session.NetFileDir, 'WTables.err',
            'TTableExt.CreateTable: ' + s);
        end;
        Delay (Random (MaxZugriffVerzoegerung));   { keine weiteren gleichzeitigen Zugriffe }
      end;
    end;
  until Result OR (i >= MaxZugriffVersuche);
  if (not Result) and (C_Debug) then WriteDebugLog(Session.NetFileDir,
    'WTables.err', 'TTableExt.CreateTable FALSE: ' + s);
end;

{-------------------------------------}
function TTableExt.EmptyTable: boolean;
{-------------------------------------}
{ Tabelle leeren;
  Ergebnis: true, wenn Tabelle spätestens nach MaxZugriffVersuche geleert werden konnte }
var
  i: integer;
begin
  Result:=false;
  i:=0;
  repeat
    inc (i);
    try
      inherited EmptyTable;
      Result:=true;
    except
      on E:EDatabaseError do begin
        if (C_Debug) then begin
          if (i = 1) then WriteDebugLog(Session.NetFileDir, 'WTables.err',
            'TTableExt.EmptyTable: ' + E.Message);
        end;
        Delay (Random (MaxZugriffVerzoegerung));   { keine weiteren gleichzeitigen Zugriffe }
      end;
    end;
  until (not Exists) or (Result) OR (i >= MaxZugriffVersuche);
  if (not Result) and (C_Debug) then WriteDebugLog(Session.NetFileDir,
    'WTables.err', 'TTableExt.EmptyTable: FALSE');
end;

{--------------------------------------}
function TTableExt.DeleteTable: boolean;
{--------------------------------------}
{ Tabelle löschen;
  Ergebnis: true, wenn Tabelle spätestens nach MaxZugriffVersuche gelöscht werden konnte }
var
  i: integer;
begin
  Result:=false;
  i:=0;
  repeat
    inc (i);
    try
      inherited DeleteTable;
      Result:=true;

      if (Assigned(Session.FindDatabase(DatabaseName))) then
        Session.FindDatabase(DatabaseName).FlushSchemaCache(TableName);
    except
      on E:EDatabaseError do begin
        if (C_Debug) then begin
          if (i = 1) then WriteDebugLog(Session.NetFileDir, 'WTables.err',
            'TTableExt.DeleteTable: ' + E.Message);
        end;
        Delay (Random (MaxZugriffVerzoegerung));   { keine weiteren gleichzeitigen Zugriffe }
      end;
    end;
  until (not Exists) or (Result) OR (i >= MaxZugriffVersuche);
  Result := (not Exists);
  if (not Result) and (C_Debug) then WriteDebugLog(Session.NetFileDir,
    'WTables.err', 'TTableExt.DeleteTable: FALSE');
end;

{------------------------------------------------------------}
procedure TTableExt.SetMaxZugriffVersuche (Versuche: integer);
{------------------------------------------------------------}
begin
  MaxZugriffVersuche:=Versuche;
end;

{--------------------------------------------------------------------}
procedure TTableExt.SetMaxZugriffVerzoegerung (Verzoegerung: integer);
{--------------------------------------------------------------------}
begin
  MaxZugriffVerzoegerung:=Verzoegerung;
end;


{---------------------------------------------------}
function TTableExt.GetIndexDefs: TIndexDefs;
{---------------------------------------------------}
(*
var
  i : integer;
  b : boolean;
  s : string;
*)  
begin
(*
  try
    if (Assigned(inherited IndexDefs)) and (inherited IndexDefs.Count = 0) then
    begin
      i := 0;
      b := False;
      repeat
        Inc(i);
        try
          inherited IndexDefs.Update;
          b := True;
        except
          on E:EDatabaseError do begin
            if (C_Debug) then begin
              s := E.Message;
              if (i = 1) then
                WriteDebugLog(Session.NetFileDir, 'WTables.err',
                  'TTableExt.GetIndexDefs (' + TableName + '): ' + s);
            end;
            Delay(Random (MaxZugriffVerzoegerung));   { keine weiteren gleichzeitigen Zugriffe }
          end;
        end;
      until (b) OR (i >= MaxZugriffVersuche);
      if (C_Debug) and (not b) then begin
        if (not Exists) then s := 'No such table';
        WriteDebugLog(Session.NetFileDir, 'WTables.err',
          'TTableExt.SafeUpdateIndexDefs  (' + TableName + ') FALSE: ' + s);
      end;
    end;
  except
  end;
*)
  try
    Result := inherited IndexDefs;
  except
    on E:Exception do begin
      if (C_Debug) then WriteDebugLog(Session.NetFileDir, 'WTables.err',
          'TTableExt.GetIndexDefs: ' + E.Message);
      Result := nil;
    end;
  end;
end;

{---------------------------------------------------}
procedure TTableExt.SetIndexDefs(pValue: TIndexDefs);
{---------------------------------------------------}
begin
  try
    inherited IndexDefs := pValue;
  except
    on E:Exception do begin
      if (C_Debug) then WriteDebugLog(Session.NetFileDir, 'WTables.err',
          'TTableExt.SetIndexDefs: ' + E.Message);
    end;
  end;
end;

{------------------------------- TQueryExt ------------------------------------}


{------------------------------------------------}
constructor TQueryExt.Create (anOwner: TComponent);
{------------------------------------------------}
begin
  inherited Create(anOwner);
  SetMaxZugriffVersuche(CMaxZugriffVersuche);
  SetMaxZugriffVerzoegerung(CMaxZugriffVerzoegerung);
  Randomize;
end;

{ Aktualisierungs-SQL ausführen                  }
{ Rückgabe: TRUE - konnte durchgeführt werden    }
{------------------------------------------------}
function TQueryExt.ExecSql: boolean;
{------------------------------------------------}
var
  i : integer;
  s : string;
begin
  result:= False;
  i:=0;
  repeat
    inc (i);
    try
      inherited ExecSql;
      result:= True;
    except
      on E:EDatabaseError do begin
        if (C_Debug) then begin
          s := E.Message;
          if (i = 1) then WriteDebugLog(Session.NetFileDir, 'WTables.err',
            'TQueryExt.ExecSql (' + Sql[0] + '...): ' + s);
        end;
        Delay (Random (MaxZugriffVerzoegerung));   { keine weiteren gleichzeitigen Zugriffe }
      end;
    end;
  until result or (i >= MaxZugriffVersuche);
  if (not Result) and (C_Debug) then WriteDebugLog(Session.NetFileDir,
    'WTables.err', 'TQueryExt.ExecSql (' + Sql[0] + '...) FALSE: ' + s);
end;

{ Abfrage-SQL ausführen                          }
{ Rückgabe: TRUE - konnte durchgeführt werden    }
{------------------------------------------------}
function TQueryExt.Open: boolean;
{------------------------------------------------}
var
  i : integer;
  s : string;
begin
  result:= False;
  i:=0;
  repeat
    inc (i);
    try
      inherited Open;
      result:= True;
    except
      on E:EDatabaseError do begin
        if (C_Debug) then begin
          s := E.Message;
          if (i = 1) then WriteDebugLog(Session.NetFileDir, 'WTables.err',
            'TQueryExt.Open (' + Sql[0] + '...): ' + s);
        end;
        Delay (Random (MaxZugriffVerzoegerung));   { keine weiteren gleichzeitigen Zugriffe }
      end;
    end;
  until result or (i >= MaxZugriffVersuche);
  if (not Result) and (C_Debug) then WriteDebugLog(Session.NetFileDir,
    'WTables.err', 'TQueryExt.Open (' + Sql[0] + '...) FALSE: ' + s);
end;

{------------------------------------------------------------}
procedure TQueryExt.SetMaxZugriffVersuche (Versuche: integer);
{------------------------------------------------------------}
begin
  MaxZugriffVersuche:=Versuche;
end;

{--------------------------------------------------------------------}
procedure TQueryExt.SetMaxZugriffVerzoegerung (Verzoegerung: integer);
{--------------------------------------------------------------------}
begin
  MaxZugriffVerzoegerung:=Verzoegerung;
end;


{------------------------------ TADOQueryExt ----------------------------------}

{----------------------------------------------------}
constructor TADOQueryExt.Create (anOwner: TComponent);
{----------------------------------------------------}
begin
  inherited Create(anOwner);
  SetMaxZugriffVersuche(CMaxZugriffVersuche);
  SetMaxZugriffVerzoegerung(CMaxZugriffVerzoegerung);
  Randomize;
end;

{ Aktualisierungs-SQL ausführen                  }
{ Rückgabe: -1 - konnte nicht durchgeführt werden}
{------------------------------------------------}
function TADOQueryExt.ExecSql: integer;
{------------------------------------------------}
var
  i : integer;
  s : string;
begin
  result:=-1;
  i:=0;
  repeat
    inc (i);
    try
      result:=inherited ExecSql;
    except
      on E:Exception do begin
        if (C_Debug) then begin
          s := E.Message;
          if (i = 1) then WriteDebugLog(Session.NetFileDir, 'WTables.err',
            'TADOQueryExt.ExecSql (' + Sql[0] + '...): ' + s);
        end;
        Delay (Random (MaxZugriffVerzoegerung));   { keine weiteren gleichzeitigen Zugriffe }
      end;
    end;
  until (result > -1) or (i >= MaxZugriffVersuche);
  if (Result = -1) and (C_Debug) then WriteDebugLog(Session.NetFileDir,
    'WTables.err', 'TADOQueryExt.ExecSql (' + Sql[0] + '...) FALSE: ' + s);
end;

{ Abfrage-SQL ausführen                          }
{ Rückgabe: TRUE - konnte durchgeführt werden    }
{------------------------------------------------}
function TADOQueryExt.Open: boolean;
{------------------------------------------------}
var
  i : integer;
  s : string;
begin
  result:= False;
  i:=0;
  repeat
    inc (i);
    try
      inherited Open;
      result:= True;
    except
      on E:EDatabaseError do begin
        if (C_Debug) then begin
          s := E.Message;
          if (i = 1) then WriteDebugLog(Session.NetFileDir, 'WTables.err',
            'TADOQueryExt.Open (' + Sql[0] + '...): ' + s);
        end;
        Delay (Random (MaxZugriffVerzoegerung));   { keine weiteren gleichzeitigen Zugriffe }
      end;
    end;
  until result or (i >= MaxZugriffVersuche);
  if (not Result) and (C_Debug) then WriteDebugLog(Session.NetFileDir,
    'WTables.err', 'TADOQueryExt.Open (' + Sql[0] + '...) FALSE: ' + s);
end;

{------------------------------------------------------------}
procedure TADOQueryExt.SetMaxZugriffVersuche (Versuche: integer);
{------------------------------------------------------------}
begin
  MaxZugriffVersuche:=Versuche;
end;

{--------------------------------------------------------------------}
procedure TADOQueryExt.SetMaxZugriffVerzoegerung (Verzoegerung: integer);
{--------------------------------------------------------------------}
begin
  MaxZugriffVerzoegerung:=Verzoegerung;
end;
      

{--------------------------------------------------------------------}
function WTablesGetTempDir(
  bWieser: boolean = False; bCheckUnique: boolean = True): string;     // 13.03.2003
{--------------------------------------------------------------------}
var
  Buffer : array[0..MAX_PATH - 1] of Char;
  i      : integer;
  sPath  : string;
begin
  SetString(Result, Buffer, GetTempPath(SizeOf(Buffer), Buffer));
  Result := IncludeTrailingBackslash(Result) +
    'WIESER\';
  if (not bWieser) then begin
    i := StrToInt(FormatDateTime('zsn', Now));
    sPath := IncludeTrailingBackslash(
      Copy(ExtractFileName(ParamStr(0)), 1, 3) + IntToStr(i));
    if (bCheckUnique) then begin
      while (DirectoryExists(Result + sPath)) do begin      // 31.01.2007
        Inc(i);
        sPath := IncludeTrailingBackslash(
          Copy(ExtractFileName(ParamStr(0)), 1, 3) + IntToStr(i));
      end;
    end;
    Result := Result + sPath;
    ForceDirectories(Result);
    Delay(10);                                            // 31.01.2007
    if (FPrivateTempDir = '') then FPrivateTempDir := Result;
  end
  else ForceDirectories(Result);
end;

{--------------------------------------------------------------------}
procedure WTablesInsertToDeleteFile(sFileName: TFileName = '');   // 13.03.2003
{--------------------------------------------------------------------}
var
  s       : TFileName;
  bOpened : boolean;
begin
  if (sFileName = '') then sFileName := FPrivateTempDir;
  try
    s := WTablesGetTempDir(True) + C_DirDeleteFile;
    if (FileExists(s)) then begin
      with TTextFileStreamExt.Create(
        s, fmOpenReadWrite+fmShareExclusive, bOpened) do
      try
        if (bOpened) then begin
          Seek(0, soFromEnd);
          WriteLn(sFileName);
        end;
      finally
        if (bOpened) then Free;
      end;
    end;
  except
  end;
end;

{--------------------------------------------------------------------}
procedure WTablesDeleteDirTempFiles;        // 13.03.2003
{--------------------------------------------------------------------}
var
  s, sRest : string;
  pSR      : TSearchRec;
  bOpened  : boolean;
  c        : char;
begin
  try
    s := WTablesGetTempDir(True) + C_DirDeleteFile;
    if (FileExists(s)) then begin
      with TTextFileStreamExt.Create(
        s, fmOpenReadWrite+fmShareExclusive, bOpened) do
      try
        if (bOpened) then begin
          sRest := '';
          // Gespeicherte Pfade löschen
          while (Position < Size) do begin
            ReadLn(s);
            if (DirectoryExists(s)) then begin
              if (SysUtils.FindFirst(
                  IncludeTrailingBackslash(s) + '*.*', 0, pSR) = 0) then
              try
                SysUtils.DeleteFile(
                  IncludeTrailingBackslash(s) + pSR.Name);
                while (SysUtils.FindNext(pSR) = 0) do
                  SysUtils.DeleteFile(
                    IncludeTrailingBackslash(s) + pSR.Name);
              finally
                SysUtils.FindClose(pSr);
              end;
              if (not RemoveDir(s)) then sRest := sRest + s + #13#10;
            end;
          end;

          // Nicht gelöschte Pfade zurückschreiben
          Position := 0;
          c := #0;
          if (sRest <> '') then WriteLn(sRest) else Write(c, 1);
          Size := Position;
        end;
      finally
        if (bOpened) then Free;
      end;
    end;
  except
  end;
end;

// Vervollständigt ggf. relativen Pfad mit Applikationspfad (ohne Prüfung)
// Parameter: Pfad-/Dateiname
// Rückgabe: vervollständigter Pfad
//----------------------------------------------------
function GDExpandFilePath(const sFileName: TFileName): string; // 19.08.2009
//----------------------------------------------------
begin
  Result := Trim(sFileName);
  if (Pos('.', Result) = 1) then
    Result := ExtractFilePath(ParamStr(0)) + Result;
end;

// Gibt ggf. vorgegebenes Verzeichnsi für BDE-PrivateDir aus WIESER.INI zurück
//--------------------------------------------------------------------
function GetBDEPrivateDir: string;  // 19.08.2009
//--------------------------------------------------------------------
begin
  with TIniFile.Create(ChangeFileExt(ParamStr(0), '.INI')) do
  try
    with TIniFile.Create(GDExpandFilePath(ReadString('Wieser', 'WieserIni',
      ExtractFilePath(ParamStr(0)) + '..\')) + 'Wieser.INI') do
    try
      Result := ReadString('Wieser', 'BDEPRIVATEDIR', '');
    finally
      Free;
    end;
  finally
    Free;
  end;
  Result := GDExpandFilePath(Result)
end;

{$IFNDEF NO_BDETEMPDIR} // 03.07.2013, WW
initialization
  FPrivateTempDir := GetBDEPrivateDir; // 19.08.2009
  if (FPrivateTempDir = '') or (not DirectoryExists(FPrivateTempDir)) then begin
    FPrivateTempDir := '';
    WTablesDeleteDirTempFiles;
    if (not isLibrary) and (Session.PrivateDir = '') then
      Session.PrivateDir := WTablesGetTempDir;  // 18.10.2004
  end
  else SetCurrentDir(FPrivateTempDir);

finalization
  if (not DirectoryExists(GetBDEPrivateDir)) then begin  // 19.08.2009
    if (not RemoveDir(FPrivateTempDir)) then
      WTablesInsertToDeleteFile;  // 13.03.2003
  end;
{$ENDIF}

end.
