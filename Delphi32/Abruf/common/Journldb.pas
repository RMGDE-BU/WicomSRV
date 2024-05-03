{**************************************************************************************}
{* Unit: Tabellenzugriffe auf WJOURNAL.DB (Abrufjournal) und WFEHLER.DB (Abruffehler) *}
{                                                                              }
{* 11.01.1999 WW                                                              *}
{ 29.04.1999  GD  Erweitert um GetAktuelleFehler                               }
{ 14.02.2001  GD  Queries auf TQueryExt umgestellt ('GetJournal', 'GetRecord') }
{                 Queries auf vor Freigeben schließen                          }
{ 29.04.2002  GD  'GetJournal' um freien Text erweitert - Neu: Fehlertxt.db    }
{ 24.05.2002  GD  WJFehler immer anlegen lassen, Prüfung auf ErrTxt32          }
{  (C) Karl Wieser GmbH 1999, 2001                                             }
{------------------------------------------------------------------------------}
Unit JournlDb;

INTERFACE

Uses
  SysUtils, DBTables, DB, ZBDatDb, ZSyncDB, ErrConst, WSysCon, Db_Attn, dialogs,
  Windows,
  WTables, RFilesDB, ErrPrc32, TelegrDb;

Const

  CDBWJournal = 'WJournal.db';
  CDBWJFehler = 'WJFehler.db';

  { Tabelle 'WJournal.db' }

  C_WJournal_JournalId     = 'JournalId';     { fortlaufende Nummer }
  C_WJournal_GeraeteArt    = 'GeraeteArt';    { MRG, DSfG, LAKS etc. }
  C_WJournal_GeraeteId     = 'GeraeteId';     { GeräteId in Stammdaten, z.B. MrgId }
  C_WJournal_Kennung       = 'Kennung';       { Kennung, unabhängig, ob Gerät in Stammdaten }
  C_WJournal_Abrufart      = 'Abrufart';      { Automatik, manuell, Rufentgegennahme etc. }
  C_WJournal_Datentypen    = 'Datentypen';    { Meßwerte, Meldungen etc. }
  C_WJournal_ComNr         = 'ComNr';         { physikalische Schnittstelle: Com1, Com2 etc. }
  C_WJournal_Quittiert     = 'Quittiert';     { Eintrag quittiert (Anzeige) }
  C_WJournal_DZVerbAufbau  = 'DZVerbAufbau';  { Zeitpunkt, wenn Verbindung aufgebaut wird }
  C_WJournal_DZVerbSteht   = 'DZVerbSteht';   { Zeitpunkt, wenn Verbindung steht }
  C_WJournal_DZLoggedIn    = 'DZLoggedIn';    { Zeitpunkt, wenn erfolgreich eingeloggt }
  C_WJournal_DZVerbEnde    = 'DZVerbEnde';    { Zeitpunkt, wenn Verbindung beendet wird }

  { Tabelle 'WJFehler.db' }

  C_WJFehler_JournalId = 'JournalId';     { JournalId aus WJournal.db }
  C_WJFehler_Klasse    = 'Klasse';        { Fehlerklasse: z.B. Warnung, Fehler }
  C_WJFehler_Gruppe    = 'Gruppe';        { Fehlergruppe: z.B. Fehler bei Stammdatenzugriff }
  C_WJFehler_Fehler    = 'Fehler';        { Fehler: z.B. STA.DB kann nicht geöffnet werden }
  C_WJFehler_DatumZeit = 'DatumZeit';     { Zeitpunkt, zu dem Fehler aufgetreten ist }

  { Tabelle 'JrnErrorTxt.db' }

  C_Tb_JrnErrorTxt = 'JrnErrorTxt';

  C_Tf_JrnErrorTxt_ErrGrp  = 'ErrGrp';    { integer }
  C_Tf_JrnErrorTxt_ErrId   = 'ErrId';     { integer }
  C_Tf_JrnErrorTxt_ErrText = 'ErrText';   { str255 }

type

  { Objekt für Zugriff auf WJournal.db }

  TJournalDB = class (TObject)
  private
    FilePath: TFileName;
    JournalTable: TTableExt;
    procedure CreateJournalDB;
    procedure InitTbFehlerText;   // 29.04.2002
  public
    constructor Create (AFilePath: TFileName);
    destructor Destroy; override;
    function WriteNewJournal (GeraeteArt: string; GeraeteId: integer; Kennung: string;
                              Abrufart: string; Datentypen: integer; ComNr: integer): integer;
    procedure UpdateJournal (JournalId: integer; GeraeteId: integer;
                             DZFieldName: string; Kennung: string; Datentypen: integer);
    function GetRecordCount: integer;
    function GetJournal(aQuery: TQueryExt; GerId: integer;
      GerArt, AbrArt: string;
      Status, Quittiert,DatenBereiche, ZeitSync: byte;
      ComPort: integer;
      DatumVon, DatumBis: TDateTime; sText: string = ''): boolean;
    function GetRecord(aQuery: TQueryExt; JournalId: integer): boolean; // 14.02.2001
    function DeleteRecord(JournalId: integer): boolean;
    function DeleteRecordsBefore(JournalId: integer): boolean;
    function DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
    function QuittRecord(JournalId: integer): boolean;
    function QuittAllRecordsFromQuery(aQuery: TQuery): boolean;
    function Reorganize (AnzahlTage: integer): boolean;
    procedure ReorganizeByStationsabrufe (Geraeteart: string; GeraeteId: integer; AnzahlAbrufe: integer);
    function DeleteJournal (Geraeteart: string; GeraeteId: integer): boolean;
    function GetFirstId: integer;
    function GetLastId: integer;
    function GetPriorId(aJournalId: integer): integer;
    function GetNextId(aJournalId: integer): integer;
    function GetLastRuf(aQuery: TQuery): boolean;
    function GetRufeSeit(aQuery: TQuery; seit: TDateTime): boolean;
    function GetAkuelleFehler(aComPort: integer): byte;
  end;

  { Objekt für Zugriff auf WJFehler.db }

  TJFehlerDB = class (TObject)
  private
    FilePath: TFileName;
    procedure CreateFehlerDB;
  public
    FehlerTable: TTableExt;
    constructor Create (AFilePath: TFileName);
    destructor Destroy; override;
    function DeleteRecords(JournalId: integer): boolean;
    function DeleteRecordsBefore(JournalId: integer): boolean;
    function DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
    procedure WriteFehler (JournalId: integer; Klasse: integer; Gruppe: integer; Fehler: integer);
    function IdRecordCount(JournalId: integer): integer;
    function IdFehlerCount(JournalId: integer): integer;
    function IdWarnungCount(JournalId: integer): integer;
    function IdReadFehlerUndWarnungen(var aQuery: TQuery; JournalId: integer): boolean;
  end;

IMPLEMENTATION

Const
  LastReorganizeDate: TDateTime = 0;

{ TJournalDB }

{---------------------------------------------------}
constructor TJournalDB.Create (AFilePath: TFileName);
{---------------------------------------------------}
begin
  inherited Create;
  FilePath:=AFilePath;
  JournalTable:=TTableExt.Create (nil);
  JournalTable.DatabaseName:=FilePath;
  JournalTable.TableName:=CDBWJournal;
  if not FileExists(FilePath + CDBWJournal) then
    CreateJournalDB;                            { Tabelle automatisch anlegen }

  if not FileExists(FilePath + CDBWJFehler) then  // 24.05.2002
    with TJFehlerDB.Create(FilePath) do Free;

  InitTbFehlerText;   // 29.04.2002
end;

{----------------------------}
Destructor TJournalDB.Destroy;
{----------------------------}
begin
  JournalTable.Free;
  inherited Destroy;
end;

{-----------------------------------}
procedure TJournalDB.CreateJournalDB;
{-----------------------------------}
{ Journal-Tabelle anlegen }
begin
  with JournalTable.FieldDefs do begin
    Clear;
    Add(C_WJournal_JournalId, ftAutoInc, 0, false);
    Add(C_WJournal_GeraeteArt, ftString, 10, false);
    Add(C_WJournal_GeraeteId, ftInteger, 0, false);
    Add(C_WJournal_Kennung, ftString, 14, false);
    Add(C_WJournal_Abrufart, ftString, 10, false);
    Add(C_WJournal_Datentypen, ftInteger, 0, false);
    Add(C_WJournal_ComNr, ftSmallInt, 0, false);
    Add(C_WJournal_Quittiert, ftBoolean, 0, false);
    Add(C_WJournal_DZVerbAufbau, ftDateTime, 0, false);
    Add(C_WJournal_DZVerbSteht, ftDateTime, 0, false);
    Add(C_WJournal_DZLoggedIn, ftDateTime, 0, false);
    Add(C_WJournal_DZVerbEnde, ftDateTime, 0, false);
  end;
  with JournalTable.IndexDefs do begin
    Clear;
    Add('PJournalId', C_WJournal_JournalId, [ixPrimary,ixUnique]);            { Primärindex }
  end;
  JournalTable.CreateTable;
end;

{-----------------------------------}
procedure TJournalDB.InitTbFehlerText;   // 29.04.2002
{-----------------------------------}
var
  i, iGrp, iFehlerMin, iFehlerMax : integer;
  ss, se                          : string;
  FTbFehlerText                   : TTableExt;
  bInitErrTxt                     : boolean;
begin
  { Ggf. ErrTxt initialisieren }
  bInitErrTxt := (ErrLibHandle = HINSTANCE_ERROR);
  if (bInitErrTxt) then InitLibraryErrTxt32;  // 24.05.2002

  FTbFehlerText := TTableExt.Create(nil);
  try

    FTbFehlerText.DatabaseName := FilePath;
    FTbFehlerText.TableName := C_Tb_JrnErrorTxt;

    // Ggf. Tabelle erstellen
    if (not FTbFehlerText.Exists) then begin
      with FTbFehlerText.FieldDefs do begin
        Add(C_Tf_JrnErrorTxt_ErrGrp, ftInteger, 0, True);
        Add(C_Tf_JrnErrorTxt_ErrId, ftInteger, 0, True);
        Add(C_Tf_JrnErrorTxt_ErrText, ftString, 255, False);
      end;
      with FTbFehlerText.IndexDefs do begin
        Add('ixGrpId', C_Tf_JrnErrorTxt_ErrGrp + ';' + C_Tf_JrnErrorTxt_ErrId,
          [ixPrimary,ixUnique]);            { Primärindex }
      end;
      FTbFehlerText.CreateTable;
    end;

    FTbFehlerText.Open;
    try

      // Schleife über alle vorhandenen Fehlernummern und Gruppen - ggf. eintragen
      with TQuery.Create(nil) do
      try
        DatabaseName := FilePath;
        Sql.Add('SELECT ' + C_WJFehler_Gruppe + ',');
        Sql.Add('MIN(' + C_WJFehler_Fehler + '),');
        Sql.Add('MAX(' + C_WJFehler_Fehler + ')');
        Sql.Add('FROM ' + CDBWJFehler);
        Sql.Add('GROUP BY ' + C_WJFehler_Gruppe);
        Open;

        while (not Eof) do begin
          iGrp := Fields[0].asInteger;
          iFehlerMin := Fields[1].asInteger;
          iFehlerMax := Fields[2].asInteger;
          for i := iFehlerMin to iFehlerMax do
            if (not FTbFehlerText.FindKey([iGrp, i])) then begin
              ss := GetStatusText(iGrp);
              se := GetErrorText(iGrp, i);
              FTbFehlerText.AppendRecord([iGrp, i, ss + ' ' + se]);
            end;
          Next;
        end;

        Close;
      finally
        Free;
      end;

    finally
      FTbFehlerText.Close;
    end;

  finally
    FTbFehlerText.Free;
    if (bInitErrTxt) then DoneLibraryErrTxt32;  // 24.05.2002
  end;
end;
{---------------------------------------------------------------------------------}
function TJournalDB.WriteNewJournal (GeraeteArt: string; GeraeteId: integer;
                                     Kennung: string; Abrufart: string;
                                     Datentypen: integer; ComNr: integer): integer;
{---------------------------------------------------------------------------------}
{ neuen Journaleintrag in Journal-Tabelle einfügen;
  Übergabe: GeraeteArt
            GeraeteId
            Kennung
            Abrufart
            Datentypen
            ComNr
  Ergebnis: 0, falls einfügen nicht möglich
            sonst JournalId des neuen Eintrags }
begin
  Result:=0;
  if JournalTable.OpenExclusive then begin                        { Exklusiv wegen Zählfeld }
    try
      JournalTable.InsertRecord ([nil, GeraeteArt, GeraeteId, Kennung, Abrufart, Datentypen,
                                  ComNr, false, Now]);
      Result:=JournalTable.FieldByName (C_WJournal_JournalId).AsInteger;
    finally
      JournalTable.Close;
    end;
    { Trigger-Datei für Journal schreiben }
    WriteNewTime(FilePath + CDBWJournal);
  end;  { if OpenExclusive }
end;

{---------------------------------------------------------------------------------------------}
procedure TJournalDB.UpdateJournal (JournalId: integer; GeraeteId: integer;
                                    DZFieldName: string; Kennung: string; Datentypen: integer);
{---------------------------------------------------------------------------------------------}
{ Eintrag mit JournalId in der Journal-Tabelle aktualisieren;
  Übergabe: JournalId
            GeraeteId
            DZFieldName (Name des zu aktualisierenden Datum/Zeit-Felds)
            Kennung
            Datentypen }
begin
  if JournalId > 0 then begin
    if JournalTable.OpenShared then begin
      try
        if JournalTable.FindKey ([JournalId]) then begin
          JournalTable.Edit;
          if GeraeteId > 0 then
            JournalTable.FieldByName (C_WJournal_GeraeteId).AsInteger:=GeraeteId;
          if (DZFieldName = C_WJournal_DZVerbSteht) OR (DZFieldName = C_WJournal_DZLoggedIn) OR
             (DZFieldName = C_WJournal_DZVerbEnde) then
            JournalTable.FieldByName (DZFieldName).AsDateTime:=Now;
          if Kennung <> '' then
            JournalTable.FieldByName (C_WJournal_Kennung).AsString:=Kennung;
          if Datentypen > 0 then
            JournalTable.FieldByName (C_WJournal_Datentypen).AsInteger:=Datentypen;
          JournalTable.Post;
        end;
      finally
        JournalTable.Close;
      end;
      { Trigger-Datei schreiben }
      WriteNewTime(FilePath + CDBWJournal);
    end;
  end;
end;

{ Gibt die erste Id aus der WJournal.db zurück   }
{----------------------------------------------------------}
function TJournalDB.GetFirstId: integer;
{----------------------------------------------------------}
begin
  Result:=-1;
  if JournalTable.OpenShared then begin
    try
      JournalTable.first;
      result:= JournalTable.FieldByName(C_WJournal_JournalId).asInteger;
    finally
      JournalTable.Close;
    end;
  end;
end;

{ Gibt die letzte Id aus der WJournal.db zurück   }
{----------------------------------------------------------}
function TJournalDB.GetLastId: integer;
{----------------------------------------------------------}
begin
  Result:=-1;
  if JournalTable.OpenShared then begin
    try
      JournalTable.last;
      result:= JournalTable.FieldByName(C_WJournal_JournalId).asInteger;
    finally
      JournalTable.Close;
    end;
  end;
end;

{ Gibt die vorige Id aus der WJournal.db zurück   }
{----------------------------------------------------------}
function TJournalDB.GetPriorId(aJournalId: integer): integer;
{----------------------------------------------------------}
begin
  Result:=-1;
  if JournalTable.OpenShared then begin
    try
      if JournalTable.FindKey([aJournalId]) then begin
        JournalTable.Prior;
        result:= JournalTable.FieldByName(C_WJournal_JournalId).asInteger;
      end
      else result:= -1;
    finally
      JournalTable.Close;
    end;
  end;
end;

{ Gibt die nächste Id aus der WJournal.db zurück   }
{----------------------------------------------------------}
function TJournalDB.GetNextId(aJournalId: integer): integer;
{----------------------------------------------------------}
begin
  Result:=-1;
  if JournalTable.OpenShared then begin
    try
      if JournalTable.FindKey([aJournalId]) then begin
        JournalTable.Next;
        result:= JournalTable.FieldByName(C_WJournal_JournalId).asInteger;
      end
      else result:= -1;
    finally
      JournalTable.Close;
    end;
  end;
end;

{ Gibt die Anzahl der Einträge in der WJournal.db zurück   }
{ Rückgabe: RecordCount; -1 bei Fehler                     }
{----------------------------------------------------------}
function TJournalDB.GetRecordCount: integer;
{----------------------------------------------------------}
begin
  try
    JournalTable.OpenShared;
    result:= JournalTable.RecordCount;
  except
    if JournalTable.Active then JournalTable.Close;
    result:= -1;
    if IsDebugFlag then showMessage('Journal - GetRecordCount');
    exit;
  end;
  if JournalTable.Active then JournalTable.Close;
end;

{ Gibt die über die Übergabe-Parameter angeforderte Ein-   }
{ träge in einem Query zurück                              }
{ Parameter: aQuery: Übergabequery; GerId: Id der Station  }
{                                   (-1 - Alle)            }
{       Status: 0 - Alle; 1 - OK; 2 - W; 3 - W+F; 4 - F    }
{       GerArt, AbrArt:                                    }
{          Strings für Geräte- und Abrufart ('-1' - Alle)  }
{       Quittiert: 0 - Alle; 1 - Nein; 2 - Ja              }
{       DatumVon/Bis: Zeitbereich, 0 - Alle                }
{       DatenBereiche: 0 - Alle; 1 - OK; 2 - n. OK         }
{       ZeitSync: 0 - Alle; 1 - ohne; 2 - mit              }
{       ComPort: 0 - Alle; 1-9 - COM-Nummer                }
{ Rückgabe: True - alles i.O.                              }
{----------------------------------------------------------}
function TJournalDB.GetJournal(aQuery: TQueryExt; GerId: integer;
  GerArt, AbrArt: string;
  Status, Quittiert, DatenBereiche, ZeitSync: byte;
  ComPort: integer;
  DatumVon, DatumBis: TDateTime; sText: string = ''): boolean;
{----------------------------------------------------------}
var
  isSelection, isFirstAnd : boolean;
  s                       : string;
begin
  result:= False; { default }
  { Testen, ob eine Selektion vorliegt }
  if (GerId > -1) or (Status > 0) or (GerArt <> '-1') or (AbrArt <> '-1') or
     (Quittiert > 0) or (trunc(DatumVon) > 0) or (trunc(DatumBis) > 0) or
     (ZeitSync > 0) or (ComPort > 0)
  then isSelection:= true else isSelection:= false;
  try
    aQuery.Close;
    aQuery.DatabaseName:= JournalTable.DatabaseName;
    with aQuery.Sql do begin
      clear;
      Add('SELECT DISTINCT');
      Add('A.' + C_WJournal_JournalId + ',');
      Add('A.' + C_WJournal_GeraeteArt + ',');
      Add('A.' + C_WJournal_GeraeteId + ',');
      Add('A.' + C_WJournal_Kennung + ',');
      Add('A.' + C_WJournal_Abrufart + ',');
      Add('A.' + C_WJournal_Datentypen + ',');
      Add('A.' + C_WJournal_ComNr + ',');
      Add('A.' + C_WJournal_Quittiert + ',');
      Add('A.' + C_WJournal_DZVerbAufbau + ',');
      Add('A.' + C_WJournal_DZVerbSteht + ',');
      Add('A.' + C_WJournal_DZLoggedIn + ',');
      Add('A.' + C_WJournal_DZVerbEnde+ ',');
      Add('COUNT(B.' + C_WJFehler_JournalId + ') Status,');
      Add('Min(B.' + C_WJFehler_Klasse + ') Warnung,'); { Neu für F/W-Unterscheidung }
      Add('Max(B.' + C_WJFehler_Klasse + ') Fehler'); { Neu für F/W-Unterscheidung }
      Add('FROM "' + CDBWJournal + '" A');
      Add('LEFT JOIN "' + CDBWJFehler + '" B');
      Add('ON A.' + C_WJournal_JournalId + '= B.' + C_WJFehler_JournalId);
      if isSelection then begin
        Add('WHERE');
        isFirstAnd:= true;
        if (GerId > -1) then begin
          if not isFirstAnd then Add('AND') else isFirstAnd:= false;
          Add(C_WJournal_GeraeteId + '= :GId');
          aQuery.ParamByName('GId').asInteger:= GerId;
        end;
        if (Status > 0) then begin
          if not isFirstAnd then Add('AND') else isFirstAnd:= false;
          if (Status = 1) then begin                { Ruf i.O. }
            Add('A.' + C_WJournal_JournalId + ' NOT IN');
            Add('(SELECT ' + C_WJFehler_JournalId);
            Add('FROM "' + CDBWJFehler + '")');
          end
          else if (Status = 2) then begin           { Warnungen }
            Add('(A.' + C_WJournal_JournalId + '=' + 'B.' + C_WJournal_JournalId);
            Add('AND B.' + C_WJFehler_Klasse + '=' + IntToStr(FK_WARNUNG) + ')');
          end
          else if (Status = 3) then begin           { Warnungen und Fehler }
            Add('(A.' + C_WJournal_JournalId + '=' + 'B.' + C_WJournal_JournalId);
            Add('AND (B.' + C_WJFehler_Klasse + '=' + IntToStr(FK_WARNUNG));
            Add('OR B.' + C_WJFehler_Klasse + '=' + IntToStr(FK_Fehler) + '))');
          end
          else if (Status = 4) then begin           { Fehler }
            Add('(A.' + C_WJournal_JournalId + '=' + 'B.' + C_WJournal_JournalId);
            Add('AND B.' + C_WJFehler_Klasse + '=' + IntToStr(FK_Fehler) + ')');
          end;
        end;
        if (Quittiert > 0) then begin
          if not isFirstAnd then Add('AND') else isFirstAnd:= false;
          if (Quittiert = 1) then begin
            Add('A.' + C_WJournal_Quittiert + '= :isFalse');
            aQuery.ParamByName('isFalse').asBoolean:= False;
          end
          else if (Quittiert = 2) then begin
            Add('A.' + C_WJournal_Quittiert + '= :isTrue');
            aQuery.ParamByName('isTrue').asBoolean:= True;
          end;
        end;
        if (ZeitSync > 0) then begin
          if not isFirstAnd then Add('AND') else isFirstAnd:= false;
          if (ZeitSync = 1) then begin
            Add('A.' + C_WJournal_JournalId + ' NOT IN');
            Add('(SELECT ' + C_WJZSync_JournalId);
            Add('FROM "' + CDBWJZSync + '")');
          end
          else if (ZeitSync = 2) then begin
            Add('A.' + C_WJournal_JournalId + ' IN');
            Add('(SELECT ' + C_WJZSync_JournalId);
            Add('FROM "' + CDBWJZSync + '")');
          end;
        end;
        if (ComPort > 0) then begin
          if not isFirstAnd then Add('AND') else isFirstAnd:= false;
          Add('A.' + C_WJournal_ComNr + '= :Com');
          aQuery.ParamByName('Com').asInteger:= ComPort;
        end;
        if (DatenBereiche > 0) then begin
          if not isFirstAnd then Add('AND') else isFirstAnd:= false;
          if (DatenBereiche = 1) then begin
            Add('A.' + C_WJournal_JournalId + ' IN');
            Add('(SELECT ' + C_MJZBDaten_JournalId);
            Add('FROM "' + CDBMJZBDaten + '"');
            Add('WHERE ' + C_MJZBDaten_SollVon + '= ' + C_MJZBDaten_IstVon);
            Add('AND ' + C_MJZBDaten_SollBis + '= ' + C_MJZBDaten_IstBis + ')');
          end
          else if (DatenBereiche = 2) then begin
            Add('A.' + C_WJournal_JournalId + ' IN');
            Add('(SELECT ' + C_MJZBDaten_JournalId);
            Add('FROM "' + CDBMJZBDaten + '"');
            Add('WHERE (' + C_MJZBDaten_SollVon + '<> ' + C_MJZBDaten_IstVon);
            Add('OR ' + C_MJZBDaten_SollBis + '<> ' + C_MJZBDaten_IstBis);
            Add('OR ' + C_MJZBDaten_IstVon + ' IS NULL');
            Add('OR ' + C_MJZBDaten_IstBis + ' IS NULL)');
            Add('AND (' + C_MJZBDaten_SollVon + ' <= ' + C_MJZBDaten_SollBis + '))');
          end;
        end;
        if (GerArt <> '-1') then begin
          if not isFirstAnd then Add('AND') else isFirstAnd:= false;
          Add(C_WJournal_GeraeteArt + '= :GArt');
          aQuery.ParamByName('GArt').asString:= GerArt;
        end;
        if (AbrArt <> '-1') then begin
          if not isFirstAnd then Add('AND') else isFirstAnd:= false;
          if (AbrArt = C_AbrArtMomStart) then Add('(');
          Add(C_WJournal_Abrufart + '= :AArt');
          aQuery.ParamByName('AArt').asString:= AbrArt;
          if (AbrArt = C_AbrArtMomStart) then
            Add('OR ' + C_WJournal_Abrufart + '= "' + C_AbrArtMomStop + '")');
        end;
        if (trunc(DatumVon) > 0) then begin
          if not isFirstAnd then Add('AND') else isFirstAnd:= false;
          Add(C_WJournal_DZVerbAufbau + '>= :Von');
          aQuery.ParamByName('Von').asDateTime:= DatumVon;
        end;
        if (trunc(DatumBis) > 0) then begin
          if not isFirstAnd then Add('AND') else isFirstAnd:= false;
          Add(C_WJournal_DZVerbAufbau + '<= :Bis');
          aQuery.ParamByName('Bis').asDateTime:= DatumBis;
        end;
        if (Trim(sText) <> '') then begin  // 29.04.2002
          if not isFirstAnd then Add('AND') {else isFirstAnd:= false};
          // Widcards in SQL-Standard ändern
          s := Trim(StringReplace(sText, '*', '%', [rfReplaceAll]));
          s := '%' + StringReplace(s, '?', '_', [rfReplaceAll]) + '%';
          // SQL-Abfrage über Detailtabelle
          Add('A.' + C_WJournal_JournalId + ' IN');
          Add('(SELECT X.' + C_WJFehler_JournalId);
          Add('FROM "' + CDBWJFehler + '" X,' + C_Tb_JrnErrorTxt + ' Y');
          Add('WHERE X.' + C_WJFehler_Gruppe + ' = Y.' + C_Tf_JrnErrorTxt_ErrGrp);
          Add('AND X. ' + C_WJFehler_Fehler + ' = Y.' + C_Tf_JrnErrorTxt_ErrId);
          Add('AND Y.' + C_Tf_JrnErrorTxt_ErrText + ' LIKE "' + s + '")');
        end;
      end;
//      Add('ORDER BY A.' + C_WJournal_DZVerbAufbau);
      Add('GROUP BY');
      Add('A.' + C_WJournal_JournalId + ',');
      Add('A.' + C_WJournal_GeraeteArt + ',');
      Add('A.' + C_WJournal_GeraeteId + ',');
      Add('A.' + C_WJournal_Kennung + ',');
      Add('A.' + C_WJournal_Abrufart + ',');
      Add('A.' + C_WJournal_Datentypen + ',');
      Add('A.' + C_WJournal_ComNr + ',');
      Add('A.' + C_WJournal_Quittiert + ',');
      Add('A.' + C_WJournal_DZVerbAufbau + ',');
      Add('A.' + C_WJournal_DZVerbSteht + ',');
      Add('A.' + C_WJournal_DZLoggedIn + ',');
      Add('A.' + C_WJournal_DZVerbEnde);
    end;
    aQuery.Open;
    result:= True;
  except
    if IsDebugFlag then showMessage('Journal - GetJournal');
    Abort;  // 14.02.2001
  end;
end;

{ Gibt einen Eintrag in einem Query zurück                 }
{ Parameter: aQuery: Übergabequery;                        }
{            JournalId des Records                         }
{ Rückgabe: True - alles i.O.                              }
{----------------------------------------------------------}
function TJournalDB.GetRecord(aQuery: TQueryExt; JournalId: integer): boolean;
{----------------------------------------------------------}
begin
  try
    aQuery.Close;
    aQuery.DatabaseName:= JournalTable.DatabaseName;
    with aQuery.Sql do begin
      clear;
      Add('SELECT * FROM "' + CDBWJournal + '"');
      Add('WHERE ' + C_WJournal_JournalId + '= :JId');
      aQuery.ParamByName('JId').asInteger:= JournalId;
    end;
    aQuery.Open;
    if aQuery.RecordCount = 1 then result:= true else result:= false; { default }
  except
    if IsDebugFlag then ShowMessage('Journal - GetRecord');
    result:= false;
    Abort; // 14.02.2001
  end;
end;

{ Löscht einen Eintrag aus der Journaltabelle              }
{ Parameter:  JournalId des Records                        }
{ Rückgabe: True - alles i.O.                              }
{----------------------------------------------------------}
function TJournalDB.DeleteRecord(JournalId: integer): boolean;
{----------------------------------------------------------}
begin
  Result:=false;
  if JournalTable.OpenShared then begin
    try
      if JournalTable.FindKey([JournalId]) then begin
        JournalTable.Delete;
        Result:=true;
      end;
    except
      JournalTable.Close;
      exit;
    end;
    JournalTable.Close;

    { Trigger-Datei schreiben }
    WriteNewTime(FilePath + CDBWJournal);
  end;
end;

{ Löscht Einträge mit kleinerer JournalId als                       }
{ die übergebene aus der Journal-Tabelle                            }
{ Parameter:  JournalId des Records                                 }
{ Rückgabe: True - alles i.O.                                       }
{-------------------------------------------------------------------}
function TJournalDB.DeleteRecordsBefore(JournalId: integer): boolean;
{--------------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= JournalTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBWJournal + '"');
      Add('WHERE ' +  C_WJournal_JournalId + ' < ' + IntToStr(JournalId));
    end;
    q.ExecSql;
    result:= true;
  except
    q.free;
    result:= false;
    exit;
  end;
  if (q.Active) then q.Close; // 14.02.2001
  q.free;

  { Trigger-Datei schreiben }
  WriteNewTime(FilePath + CDBWJournal);
end;

{ Löscht alle übergebenen Einträge der Journaltabelle      }
{ Parameter:  Query mit zu löschenden Einträgen            }
{             Query ist offen                              }
{ Rückgabe: True - alles i.O.                              }
{----------------------------------------------------------}
function TJournalDB.DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
{----------------------------------------------------------}
var
  JournalId: integer;
begin
  Result:=true;
  if aQuery.RecordCount > 0 then begin
    Result:=false;
    if JournalTable.OpenShared then begin
      try
        aQuery.first;
        while not aQuery.Eof do begin
          JournalId:=aQuery.FieldByName(C_WJournal_JournalId).asInteger;
          if JournalTable.FindKey([JournalId]) then
            JournalTable.Delete;
          aQuery.Next;
        end;
        Result:=true;
      except
        JournalTable.Close;
        exit;
      end;
      JournalTable.Close;

      { Trigger-Datei schreiben }
      WriteNewTime(FilePath + CDBWJournal);
    end;
  end;
end;

{ Quittiert einen Eintrag in der Journaltabelle            }
{ Parameter:  JournalId des Records                        }
{ Rückgabe: True - alles i.O.                              }
{----------------------------------------------------------}
function TJournalDB.QuittRecord(JournalId: integer): boolean;
{----------------------------------------------------------}
begin
  Result:=false;
  if JournalTable.OpenShared then begin
    try
      if JournalTable.FindKey ([JournalId]) then begin
        JournalTable.Edit;
        JournalTable.FieldByName (C_WJournal_Quittiert).AsBoolean:=true;
        JournalTable.Post;
        Result:=true;
      end;
    except
      JournalTable.Close;
      exit;
    end;
    JournalTable.Close;

    { Trigger-Datei schreiben }
    WriteNewTime(FilePath + CDBWJournal);
  end;
end;


{ Quittiert alle übergebenen Einträge der Journaltabelle   }
{ Parameter:  Query mit zu quittierenden Einträgen         }
{             Query ist offen                              }
{ Rückgabe: True - alles i.O.                              }
{----------------------------------------------------------}
function TJournalDB.QuittAllRecordsFromQuery(aQuery: TQuery): boolean;
{----------------------------------------------------------}
var
  JournalId: integer;
begin
  Result:=true;
  if aQuery.RecordCount > 0 then begin
    Result:=false;
    if JournalTable.OpenShared then begin
      try
        aQuery.first;
        while not aQuery.Eof do begin
          JournalId:=aQuery.FieldByName(C_WJournal_JournalId).asInteger;
          if JournalTable.FindKey([JournalId]) then begin
            JournalTable.Edit;
            JournalTable.FieldByName (C_WJournal_Quittiert).AsBoolean:=true;
            JournalTable.Post;
          end;
          aQuery.Next;
        end;
        Result:=true;
      except
        JournalTable.Close;
        exit;
      end;
      JournalTable.Close;

      { Trigger-Datei schreiben }
      WriteNewTime(FilePath + CDBWJournal);
    end;
  end;
end;

{------------------------------------------------------------}
function TJournalDB.Reorganize (AnzahlTage: integer): boolean;
{------------------------------------------------------------}
{ Journaltabelle incl. Detailtabellen reorganisieren, d.h. Einträge, die mehr als AnzahlTage
  zurückliegen, einmal täglich aus Tabelle löschen;
  Übergabe: AnzahlTage (Wenn = 0, dann keine Organisation der Tabelle als Rundpuffer
  Ergebnis: true, wenn Reorganisation durchgeführt wurde }
var
  JournalId: integer;
  FehlerDB: TJFehlerDB;
  WJZSyncDB: TWJZSyncDB;
  WJRFilesDB: TWJRFilesDB;
  MJZBDatenDB: TMJZBDatenDB;
  DJZBDatenDB: TDJZBDatenDB;
  DJTelegrDB: TDJTelegrDB;
  JDatum: TDateTime;

begin
  Result:=false;
  if (AnzahlTage > 0) AND (Date > LastReorganizeDate) then begin
    if JournalTable.OpenShared then begin
      try
        Result:=true;
        LastReorganizeDate:=Date;
        JournalId := -1;
        while not JournalTable.Eof do begin
          JDatum := JournalTable.FieldByName(C_WJournal_DZVerbAufbau).AsDateTime;
          if (Date - Int (JDatum)) < AnzahlTage then begin
            JournalId := JournalTable.FieldByName(C_WJournal_JournalId).AsInteger;
            Break;
          end;
          JournalTable.Next;
        end;
      finally
        JournalTable.Close;
      end;

      if JournalId > -1 then begin
        FehlerDB:=TJFehlerDB.Create (FilePath);
        try
          { alle Einträge mit kleinerer JournalId aus WJFehler.db löschen: }
          FehlerDB.DeleteRecordsBefore (JournalId);
        finally
          FehlerDB.Free;
        end;

        WJZSyncDB:=TWJZSyncDB.Create (FilePath);
        try
          { alle Einträge mit kleinerer JournalId aus WJZSync.db löschen: }
          WJZSyncDB.DeleteRecordsBefore (JournalId);
        finally
          WJZSyncDB.Free;
        end;

        WJRFilesDB:=TWJRFilesDB.Create (FilePath);
        try
          { alle Einträge mit kleinerer JournalId aus WJRFiles.db löschen: }
          WJRFilesDB.DeleteRecordsBefore (JournalId);
        finally
          WJRFilesDB.Free;
        end;

        MJZBDatenDB:=TMJZBDatenDB.Create (FilePath);
        try
          { alle Einträge mit kleinerer JournalId aus MJZBDat.db löschen: }
          MJZBDatenDB.DeleteRecordsBefore (JournalId);
        finally
          MJZBDatenDB.Free;
        end;

        DJZBDatenDB:=TDJZBDatenDB.Create (FilePath);
        try
          { alle Einträge mit kleinerer JournalId aus DJZBDat.db löschen: }
          DJZBDatenDB.DeleteRecordsBefore (JournalId);
        finally
          DJZBDatenDB.Free;
        end;

        DJTelegrDB:=TDJTelegrDB.Create (FilePath);
        try
          { alle Einträge mit kleinerer JournalId aus DJTELEGR.db löschen (erweitert 16.04.2003, WW): }
          DJTelegrDB.DeleteRecordsBefore (JournalId);
        finally
          DJTelegrDB.Free;
        end;

        { alle Einträge mit kleinerer JournalId aus WJournal.db löschen: }
        DeleteRecordsBefore (JournalId);
      end;   { if JournalId > -1 }
    end;  { if OpenShared }
  end;
end;

{--------------------------------------------------------------------------------------------------------------}
procedure TJournalDB.ReorganizeByStationsabrufe (Geraeteart: string; GeraeteId: integer; AnzahlAbrufe: integer);
{--------------------------------------------------------------------------------------------------------------}
{ bestimmte Journaldetailtabellen nach Stationsabrufen reorganisieren, d.h. alle Einträge außer denen, der letzten
  n Abrufe löschen; Grund für diese zusätzliche Reorganisation ist die in diesen Tabellen gespeicherte immense, nicht
  mehr handlebare Datenmenge, die nach dem standardmäßigen Reorganisieren nach Tagen (procedure Reorganize) weiterhin
  besteht.
  betroffene Tabellen: DJZBDAT.DB  (nur DSfG)
                       WJRFILES.DB (bislang nur von DSfG genutzt)
  Übergabe: Geraeteart
            GeraeteId
            AnzahlAbrufe (Wenn = 0, dann keine Organisation der Tabellen als Rundpuffer }
var
  WJRFilesDB: TWJRFilesDB;
  DJZBDatenDB: TDJZBDatenDB;
  JournalId: integer;

begin
  if AnzahlAbrufe > 0 then begin
    if JournalTable.OpenShared then begin
      try
        { Filter auf Geräteart, GeräteId und Automatik-/Manuelle Abrufe: }
        JournalTable.Filter:='('+ C_WJournal_GeraeteArt + ' = ''' + Geraeteart +''') AND '+
                             '('+ C_WJournal_GeraeteId + ' = ' + IntToStr(GeraeteId) +') AND '+
                             '(('+ C_WJournal_Abrufart + ' = ''' + C_AbrArtAuto +''') OR '+
                             ' ('+ C_WJournal_Abrufart + ' = ''' + C_AbrArtManu +'''))';
        JournalTable.Filtered:=true;
        try
          JournalTable.Last;
          JournalTable.MoveBy ((AnzahlAbrufe)*-1);   { auf die zu löschende JournalId positionieren }
          if not JournalTable.Bof then
            JournalId := JournalTable.FieldByName(C_WJournal_JournalId).AsInteger
          else
            JournalId:=-1;
        finally
          JournalTable.Filtered:=false;
        end;
      finally
        JournalTable.Close;
      end;

      if JournalId > -1 then begin
        if Geraeteart = C_GerArtDSfG then begin
          { alle Einträge mit JournalId aus DJZBDat.db löschen
            (bis 10.4.2002 wurden auch alle davorliegenden Datensätze in den
             Detailtabellen mitgelöscht und dabei fehlerhafterweise auch
             Datensätze anderer Stationen mit !): }
          DJZBDatenDB:=TDJZBDatenDB.Create (FilePath);
          try
            DJZBDatenDB.DeleteRecords (JournalId);  // 10.04.2002 WW
          finally
            DJZBDatenDB.Free;
          end;
        end;

        { alle Einträge mit JournalId aus WJRFiles.db löschen: }
        WJRFilesDB:=TWJRFilesDB.Create (FilePath);
        try
          WJRFilesDB.DeleteRecords (JournalId);     // 10.04.2002 WW
        finally
          WJRFilesDB.Free;
        end;
      end;

      { Journal-Trigger-Datei nicht schreiben, Journal.db und WFehler.db wurden nicht verändert }

    end;  { if OpenShared }
  end;
end;

{----------------------------------------------------------------------------------}
function TJournalDB.DeleteJournal (Geraeteart: string; GeraeteId: integer): boolean;
{----------------------------------------------------------------------------------}
{ Alle Einträge für Geräteart und Geräte-ID aus Journaltabelle und Detailtabellen löschen;
  Übergabe: Geraeteart
            GeraeteId
  Ergebnis: true, wenn Journal erfolgreich gelöscht wurde }
var
  JournalId: integer;
  FehlerDB: TJFehlerDB;
  WJZSyncDB: TWJZSyncDB;
  WJRFilesDB: TWJRFilesDB;
  MJZBDatenDB: TMJZBDatenDB;
  DJZBDatenDB: TDJZBDatenDB;
  DJTelegrDB: TDJTelegrDB;

begin
  Result:=false;
  if JournalTable.OpenShared then begin
    try
      Result:=true;
      FehlerDB:=TJFehlerDB.Create (FilePath);
      try
        WJZSyncDB:=TWJZSyncDB.Create (FilePath);
        try
          WJRFilesDB:=TWJRFilesDB.Create (FilePath);
          try
            MJZBDatenDB:=TMJZBDatenDB.Create (FilePath);
            try
              DJZBDatenDB:=TDJZBDatenDB.Create (FilePath);
              try
                DJTelegrDB:=TDJTelegrDB.Create (FilePath);
                try
                  { Filter auf Geräteart und GeräteId: }
                  JournalTable.Filter:='('+ C_WJournal_GeraeteArt + ' = ''' + Geraeteart +''') AND '+
                                       '('+ C_WJournal_GeraeteId + ' = ' + IntToStr(GeraeteId) +')';
                  JournalTable.Filtered:=true;
                  try
                    while not JournalTable.Eof do begin
                      JournalId := JournalTable.FieldByName(C_WJournal_JournalId).AsInteger;
                      { Einträge mit kleinerer JournalId aus WJFehler.db löschen: }
                      if not FehlerDB.DeleteRecords (JournalId) then
                        Result:=false;
                      { Einträge mit kleinerer JournalId aus WJZSync.db löschen: }
                      if not WJZSyncDB.DeleteRecord (JournalId) then
                        Result:=false;
                      { Einträge mit JournalId aus WJRFiles.db löschen: }
                      if not WJRFilesDB.DeleteRecords (JournalId) then
                        Result:=false;
                      { nur MRG: Einträge mit JournalId aus MJZBDat.db löschen: }
                      if Geraeteart = C_GerArtMrg then begin
                        if not MJZBDatenDB.DeleteRecord (JournalId) then
                          Result:=false;
                      end;
                      { nur DSfG: }
                      if Geraeteart = C_GerArtDSfG then begin
                        { Einträge mit JournalId aus DJZBDat.db löschen: }
                        if not DJZBDatenDB.DeleteRecords (JournalId) then
                          Result:=false;
                        { Einträge mit JournalId aus DJTELEGR.db löschen (erweitert 16.04.2003, WW): }
                        if not DJTelegrDB.DeleteRecords (JournalId) then
                          Result:=false;
                      end;

                      JournalTable.Delete;
                    end;  { while }
                  finally
                    DJTelegrDB.Free;
                  end;
                finally
                  JournalTable.Filtered:=false;
                end;
              finally
                DJZBDatenDB.Free;
              end;
            finally
              MJZBDatenDB.Free;
            end;
          finally
            WJRFilesDB.Free;
          end;
        finally
          WJZSyncDB.Free;
        end;
      finally
        FehlerDB.Free;
      end;
    finally
      JournalTable.Close;
    end;

    { Trigger-Datei für Journal schreiben }
    WriteNewTime(FilePath + CDBWJournal);
  end;  { if OpenShared }
end;


{ Gibt letzten Ruf in Query zurück                 }
{ Parameter: Übergabequery                         }
{ Rückgabe: TRUE - Alles i.O.                      }
{--------------------------------------------------}
function TJournalDB.GetLastRuf(aQuery: TQuery): boolean;
{--------------------------------------------------}
begin
  result:= False; { default }
  try
    aQuery.Close;
    aQuery.DatabaseName:= JournalTable.DatabaseName;
    with aQuery.Sql do begin
      clear;
      Add('SELECT * FROM "' + CDBWJournal + '"');
      Add('WHERE ' + C_WJournal_Abrufart + '= :Abrufart');
      Add('ORDER BY ' + C_WJournal_DZVerbAufbau);
      aQuery.ParamByName('Abrufart').asString:= C_AbrArtRuf;
    end;
    aQuery.Open;
    if aQuery.RecordCount > 0 then result:= true;
  except
    if IsDebugFlag then ShowMessage('Journal - GetLastRuf');
    exit;
  end;
end;

{ Gibt Rufe seit 'seit' in Query zurück            }
{ Parameter: Übergabequery, Startzeitpunkt         }
{ Rückgabe: TRUE - Alles i.O.                      }
{--------------------------------------------------}
function TJournalDB.GetRufeSeit(aQuery: TQuery; seit: TDateTime): boolean;
{--------------------------------------------------}
begin
  result:= False; { default }
  try
    aQuery.Close;
    aQuery.DatabaseName:= JournalTable.DatabaseName;
    with aQuery.Sql do begin
      clear;
      Add('SELECT * FROM "' + CDBWJournal + '"');
      Add('WHERE ' + C_WJournal_Abrufart + '= :Abrufart');
      Add('AND ' + C_WJournal_DZVerbAufbau + '> :Zeit');
      Add('ORDER BY ' + C_WJournal_DZVerbAufbau);
      aQuery.ParamByName('Abrufart').asString:= C_AbrArtRuf;
      aQuery.ParamByName('Zeit').asDateTime:= seit;
    end;
    aQuery.Open;
    if aQuery.RecordCount > 0 then result:= true;
  except
    if IsDebugFlag then ShowMessage('Journal - GetRufeSeit');
    exit;
  end;
end;

{ Gibt an, ob beim letzten Eintrag der übergebenen     }
{ ComNr ein Fehler aufgetreten ist                     }
{ Rückgabe: 0 - Kein Fehler, 1 - Warnung, 2 - Fehler   }
{------------------------------------------------------}
function TJournalDB.GetAkuelleFehler(aComPort: integer): byte;
{------------------------------------------------------}
var
  q        : TQueryExt;
  JId      : integer;
  Fehler   : TJFehlerDB;
begin
  result:= 3; { Default }
  JId:= -1;   { Default für JournalId }
  q:= TQueryExt.Create(nil);
  try
    q.DatabaseName:= JournalTable.DatabaseName;
  { 1.: Einträge der ComNr holen }
    q.Sql.Add('SELECT ' + C_WJournal_JournalId + ', ' + C_WJournal_DZVerbAufbau);
    q.Sql.Add('FROM "' + CDBWJournal + '"');
    q.Sql.Add('WHERE ' + C_WJournal_ComNr + '= ' + IntToStr(aComPort));
    if q.Open then if q.RecordCount > 0 then begin
      q.Last;
      JId:= q.FieldByName(C_WJournal_JournalId).asInteger;
    end;
  finally
    if (q.Active) then q.Close;  // 14.02.2001
    q.Free;
  end;

  { 2.: Fehler und Warnungen ermitteln }
  if JId > -1 then begin
    Fehler:= TJFehlerDB.Create(FilePath);
    try
      if Fehler.IdFehlerCount(JId) > 0 then result:= 2
      else if Fehler.IdWarnungCount(JId) > 0 then result:= 1
      else result:= 0;
    finally
      Fehler.Free;
    end;
  end;
end;


{ TJFehlerDB }

{---------------------------------------------------}
constructor TJFehlerDB.Create (AFilePath: TFileName);
{---------------------------------------------------}
begin
  inherited Create;
  FilePath:=AFilePath;
  FehlerTable:=TTableExt.Create (nil);
  FehlerTable.DatabaseName:=FilePath;
  FehlerTable.TableName:=CDBWJFehler;
  if not FileExists(FilePath + CDBWJFehler) then
    CreateFehlerDB;                             { Tabelle automatisch anlegen }
end;

{----------------------------}
Destructor TJFehlerDB.Destroy;
{----------------------------}
begin
  FehlerTable.Free;
  inherited Destroy;
end;

{----------------------------------}
procedure TJFehlerDB.CreateFehlerDB;
{----------------------------------}
{ Fehler-Tabelle anlegen }
begin
  with FehlerTable.FieldDefs do begin
    Clear;
    Add(C_WJFehler_JournalId, ftInteger, 0, false);
    Add(C_WJFehler_Klasse, ftSmallInt, 0, false);
    Add(C_WJFehler_Gruppe, ftSmallInt, 0, false);
    Add(C_WJFehler_Fehler, ftSmallInt, 0, false);
    Add(C_WJFehler_DatumZeit, ftDateTime, 0, false);
  end;
  FehlerTable.IndexDefs.Clear;
  FehlerTable.CreateTable;
end;

{-----------------------------------------------------------------------------------}
procedure TJFehlerDB.WriteFehler (JournalId: integer;
                                  Klasse: integer; Gruppe: integer; Fehler: integer);
{-----------------------------------------------------------------------------------}
{ Eintrag in Fehler-Tabelle einfügen;
  Übergabe: JournalId }
begin
  if JournalId > 0 then begin
    if FehlerTable.OpenShared then begin
      try
        FehlerTable.AppendRecord ([JournalId, Klasse, Gruppe, Fehler, Now]);
      finally
        FehlerTable.Close;
      end;
      { Trigger-Datei für Journal schreiben }
      WriteNewTime(FilePath + CDBWJournal);
    end;
  end;
end;

{ Löscht Einträge zu einer Id aus der Fehlertabelle        }
{ Parameter:  JournalId der Records                        }
{ Rückgabe: True - alles i.O.                              }
{----------------------------------------------------------}
function TJFehlerDB.DeleteRecords(JournalId: integer): boolean;
{----------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= FehlerTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBWJFehler + '"');
      Add('WHERE ' +  C_WJFehler_JournalId + '= ' + IntToStr(JournalId));
    end;
    q.ExecSql;
    result:= true;
  except
    q.free;
    result:= false;
    exit;
  end;
  if (q.Active) then q.Close;  // 14.02.2001
  q.free;
end;

{ Löscht Einträge mit kleinerer JournalId als                       }
{ die übergebene aus der Journal-Fehler-Tabelle                     }
{ Parameter:  JournalId                                             }
{ Rückgabe: True - alles i.O.                                       }
{-------------------------------------------------------------------}
function TJFehlerDB.DeleteRecordsBefore(JournalId: integer): boolean;
{-------------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= FehlerTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBWJFehler + '"');
      Add('WHERE ' +  C_WJFehler_JournalId + '< ' + IntToStr(JournalId));
    end;
    q.ExecSql;
    result:= true;
  except
    q.free;
    result:= false;
    exit;
  end;
  if (q.Active) then q.Close;  // 14.02.2001
  q.free;
end;

{ Löscht alle übergebenen Einträge der Fehlertabelle       }
{ Parameter:  Query mit zu löschenden Einträgen            }
{             Query ist offen                              }
{ Rückgabe: True - alles i.O.                              }
{----------------------------------------------------------}
function TJFehlerDB.DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
{----------------------------------------------------------}
begin
  try
    if aQuery.RecordCount > 0 then begin
      aQuery.first;
      while not aQuery.Eof do begin
        DeleteRecords (aQuery.FieldByName(C_WJournal_JournalId).asInteger);
        aQuery.Next;
      end;
    end;
    result:= true;
  except
    result:= false;
    exit;
  end;
end;

{ Gibt die Anzahl der Einträge für eine Id zurück        }
{ Parameter: JournalId                                   }
{ Rückgabe: Anzahl der zugehörigen Einträge              }
{--------------------------------------------------------}
function TJFehlerDB.IdRecordCount(JournalId: integer): integer;
{--------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.Close;
    q.DataBaseName:= FehlerTable.DatabaseName;
    q.Sql.Clear;
    q.Sql.Add('SELECT COUNT(' + C_WJFehler_JournalId + ')');
    q.Sql.Add('FROM "' + CDBWJFehler + '"');
    q.Sql.Add('WHERE ' + C_WJFehler_JournalId + '= :JId');
    q.ParamByName('JId').asInteger:= JournalId;
    q.open;
    result:= q.fields[0].asInteger;
  except
    q.free;
    if IsDebugFlag then showMessage('Fehler - IdRecCount');
    result:= 0;
    exit;
  end;
  if (q.Active) then q.Close;  // 14.02.2001
  q.free;
end;

{ Gibt die Anzahl der Fehler für eine Id zurück          }
{ Parameter: JournalId                                   }
{ Rückgabe: Anzahl der zugehörigen Fehler-Einträge       }
{--------------------------------------------------------}
function TJFehlerDB.IdFehlerCount(JournalId: integer): integer;
{--------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.Close;
    q.DataBaseName:= FehlerTable.DatabaseName;
    q.Sql.Clear;
    q.Sql.Add('SELECT COUNT(' + C_WJFehler_JournalId + ')');
    q.Sql.Add('FROM "' + CDBWJFehler + '"');
    q.Sql.Add('WHERE ' + C_WJFehler_JournalId + '= :JId');
    q.Sql.Add('AND ' + C_WJFehler_Klasse + '= ' + IntToStr(FK_FEHLER));
    q.ParamByName('JId').asInteger:= JournalId;
    q.open;
    result:= q.fields[0].asInteger;
  except
    q.free;
    if IsDebugFlag then showMessage('Journal - IdFehlerCount');
    result:= 0;
    exit;
  end;
  if (q.Active) then q.Close;  // 14.02.2001
  q.free;
end;

{ Gibt die Anzahl der Warnungen für eine Id zurück       }
{ Parameter: JournalId                                   }
{ Rückgabe: Anzahl der zugehörigen Waqrnungs-Einträge    }
{--------------------------------------------------------}
function TJFehlerDB.IdWarnungCount(JournalId: integer): integer;
{--------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.Close;
    q.DataBaseName:= FehlerTable.DatabaseName;
    q.Sql.Clear;
    q.Sql.Add('SELECT COUNT(' + C_WJFehler_JournalId + ')');
    q.Sql.Add('FROM "' + CDBWJFehler + '"');
    q.Sql.Add('WHERE ' + C_WJFehler_JournalId + '= :JId');
    q.Sql.Add('AND ' + C_WJFehler_Klasse + '= ' + IntToStr(FK_WARNUNG));
    q.ParamByName('JId').asInteger:= JournalId;
    q.open;
    result:= q.fields[0].asInteger;
  except
    q.free;
    if IsDebugFlag then showMessage('Journal - IdWarnungCount');
    result:= 0;
    exit;
  end;
  if (q.Active) then q.Close;  // 14.02.2001
  q.free;
end;

{ Gibt die Fehler und Warnungen zurück                   }
{ Parameter: ÜbergabequeryJournalId                      }
{ Rückgabe: True - Aktion erfolgreich                    }
{--------------------------------------------------------}
function TJFehlerDB.IdReadFehlerUndWarnungen(var aQuery: TQuery; JournalId: integer): boolean;
{--------------------------------------------------------}
begin
  try
    with aQuery do begin
      Close;
      DataBaseName:= FehlerTable.DatabaseName;
      Sql.Clear;
      Sql.Add('SELECT * FROM "' + CDBWJFehler + '"');
      Sql.Add('WHERE ' + C_WJFehler_JournalId + '= ' + IntToStr(JournalId));
      aQuery.Open;
    end;
    result:= true;
  except
    if IsDebugFlag then showMessage('Journal - IdRead ...');
    result:= false;
  end;
end;

end.
