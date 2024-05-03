{******************************************************************************}
{* Unit: Zugriff auf DSfG-Archiv-Tabellen (Index-, Satz-, Wert-, Hex-Tabellen)*}
{* 29.11.1999 WW                                                              *}
{* 12.02.2002 GD Erweitert um Funktion zum Austausch von Auto- und Manu-Daten *}
{*               Erweitert um Funktion zum direkten Eintragen von Daten       *}
{* 04.07.2002 GD Erweitert um Konstanten für Text-Kanäle                      *}
{* 26.02.2003 WW TTbDArchiv jetzt von TDSfGDataKonvert abgeleitet             *}
{* 17.04.2003 WW neue Methode 'Get_ArchivDaten_By_Zeit_OrdNr' ersetzt die     *}
{*               bisherigen Daten-Zugriffsmethoden ('_By_Zeit', '_By_OrdNr')  *}
{******************************************************************************}
unit TbDSfGAr;

interface

uses
  Windows, Forms, Classes, Db, DbTables, SysUtils, WTables, GD_Utils, DALKonv;

const
  { Index-Tabelle für DSfG-Archiv }
  C_TbDIndex            = 'Index.db';

  C_TfDIndex_Index      = 'Index';           { smallint }
  C_TfDIndex_Kennung    = 'Kennung';         { str14 }
  C_TfDIndex_StatNr     = 'StatNr';          { smallint }
  C_TfDIndex_ArchGrpNr  = 'ArchivGruppenNr'; { smallint }
  C_TfDIndex_Instanz    = 'Instanz';         { smallint }
  C_TfDIndex_Geraetetyp = 'Geraetetyp';      { str3 }

  { DSfG-Satz-Tabelle }
  C_TbDSatz             = 'Satz';

  C_TfDSatz_ReferenzNr  = 'ReferenzNr';      { int }
  C_TfDSatz_Index       = 'Index';           { smallint }
  C_TfDSatz_OrdNr       = 'Ordnungsnummer';  { int }
  C_TfDSatz_Datum       = 'Datum';           { date }
  C_TfDSatz_Zeit        = 'Zeit';            { time }
  C_TfDSatz_Status      = 'Status';          { str10 }
  C_TfDSatz_Zeitzone    = 'Zeitzone';        { str1 }

  CSDatOrd = 'SDatOrd';                      { Sekundärindex }

  { DSfG-Wert-Tabelle }
  C_TbDWert              = 'W';

  C_TfDWert_ReferenzNr   = 'ReferenzNr';      { int }
  C_TfDWert_Kanaltyp     = 'Kanaltyp';        { str2 }
  C_TfDWert_Werteart     = 'Werteart';        { str2 }
  C_TfDWert_Wert         = 'Wert';            { num }
  C_TfDWert_Status       = 'Status';          { str16 }
  C_TfDWert_CRC          = 'CRC';             { str10 }

  { DSfG-Hex-Tabelle }
  C_TbDHex              = 'H';

  C_TfDHex_ReferenzNr   = 'ReferenzNr';      { int }
  C_TfDHex_Kanaltyp     = 'Kanaltyp';        { str2 }
  C_TfDHex_Werteart     = 'Werteart';        { str2 }
  C_TfDHex_Wert         = 'Wert';            { str10 }
  C_TfDHex_Status       = 'Status';          { str16 }
  C_TfDHex_CRC          = 'CRC';             { str10 }

  { DSfG-Textkanal-Tabelle }
  C_TbDText             = 'T';               // 04.07.2002

  C_TfDText_ReferenzNr  = 'ReferenzNr';      { int }
  C_TfDText_Kanaltyp    = 'Kanaltyp';        { str2 }
  C_TfDText_Werteart    = 'Werteart';        { str2 }
  C_TfDText_Wert        = 'Wert';            { str40 }
  C_TfDText_Status      = 'Status';          { str16 }
  C_TfDText_CRC         = 'CRC';             { str10 }

  { Kanaltypen }

  kt_MW = 'MW';         { Meßwert }
  kt_ZS = 'ZS';         { Zählerstand }
  kt_ZW = 'ZW';         { Zählwert }
  kt_ST = 'ST';         { Status }
  kt_FW = 'FW';         { Fahrweg }
  kt_TX = 'TX';         { Text }             // 04.07.2002

type

  { Objekt für Zugriff auf Satz-, Werte- und Hex-Tabellen }

  TTbDArchiv = class(TDArchivLogbuchKonv)
  protected
    ArchivPath: TFileName;
    StationId: integer;
    tbDSatz: TTableExt;
    tbDWert: TTableExt;
    tbDHex: TTableExt;
    tbDText: TTableExt;  // 04.07.2002
    procedure SetTableNames (Index, Kanal: integer);
    procedure CreatetbDSatz (ASatzTable: TTableExt);
    procedure CreatetbDWert (AWertTable: TTableExt);
    procedure CreatetbDHex (AHexTable: TTableExt);
    procedure CreatetbDText (ATextTable: TTableExt);
  public
    constructor Create (AArchivPath: TFileName; AStationId: integer);
    destructor Destroy; override;
    function GetLetztWertOrdNr_DatumZeit (InstanzId, ArchivGrp, Kanal: integer;
                                          Kanaltyp: string;
                                          var OrdNr: integer;
                                          var DatumZeit: TDateTime): boolean;
    function Get_ArchivDaten_By_Zeit_OrdNr (InstanzId, ArchivGrp, Kanal: integer;
                                            var ergebnis: TQuery;
                                            VonDatumZeit, BisDatumZeit: TDateTime;
                                            OrdNrVon: integer = -1;
                                            OrdNrBis: integer = -1): boolean;
    function InsertArchivdatenList(   // 12.02.2002
      iStaId, iInstId, iAGNr: integer; pSlDaten: TStrings): boolean;
  end;

procedure CreateIndexDB(Pfad: TFileName);
function Get_DSfGDatenIndex(Pfad: TFilename; StationID, InstanzID,
  GruppenID: integer; Eintragen: boolean): integer;
function ReplaceAutoManu(   // 12.02.2002
  sAuto, sManu: TFileName; iStaId, iInstId, iAGNr: integer): boolean;

implementation

{ Funktionen für Zugriff auf DSfG-Index-Tabelle }

{---------------------------------------}
procedure CreateIndexDB(Pfad: TFileName);
{---------------------------------------}
{ Index-Tabelle anlegen }
var
  IndexTable: TTableExt;

begin
  IndexTable:=TTableExt.Create(nil);
  try
    IndexTable.DatabaseName:=Pfad;
    IndexTable.TableName:=C_TbDIndex;
    with IndexTable.FieldDefs do begin
      Clear;
      Add(C_TfDIndex_Index, ftSmallInt, 0, false);
      Add(C_TfDIndex_Kennung, ftString, 14, false);
      Add(C_TfDIndex_StatNr, ftSmallInt, 0, false);
      Add(C_TfDIndex_ArchGrpNr, ftSmallInt, 0, false);
      Add(C_TfDIndex_Instanz, ftSmallInt, 0, false);
      Add(C_TfDIndex_Geraetetyp, ftString, 3, false);
    end;
    with IndexTable.IndexDefs do begin
      Clear;
      Add('PIndex', C_TfDIndex_Index, [ixPrimary, ixUnique]);
    end;
    IndexTable.CreateTable;
  finally
    IndexTable.Free;
  end;
end;

{------------------------------------------------------------------------------------------------------------------}
function Get_DSfGDatenIndex(Pfad: TFilename; StationID, InstanzID, GruppenID: integer; Eintragen: boolean): integer;
{------------------------------------------------------------------------------------------------------------------}
{ Übergabe: Pfad der Index-Tabelle;
            StationsID, InstanzID, ArchivgruppenID;
            Eintragen: true = neuer Eintrag für StationsID, InstanzID, ArchivgruppenID
                       false = Rückgabe -1, wenn fehlend
  Rückgabe im Erfolgsfall: Indexnummer, sonst -1 }
type
  PMerkfeld = ^TMerkfeld;
  TMerkfeld = array[1..9999] of boolean;

var
  Idx: integer;
  gefunden: boolean;
  GrpNr,InstNr,StatNr: integer;
  Merkfeld: PMerkfeld;
  IndexTable: TTableExt;

begin
  Result:=-1;
  Merkfeld:=New(PMerkfeld);
  try
    for Idx:=Low(Merkfeld^) to High(Merkfeld^) do Merkfeld^[Idx]:=false;
    Idx:=-1;
    Pfad:=ExpandFilename(Pfad);
    IndexTable:=TTableExt.Create(nil);
    try
      IndexTable.DatabaseName:=Pfad;
      IndexTable.TableName:=C_TbDIndex;
      { Tabelle anlegen, wenn nicht vorhanden: }
      if not IndexTable.Exists then CreateIndexDB(Pfad);
      gefunden:=false;
      if IndexTable.OpenShared then begin
        try
          IndexTable.First;
          while not IndexTable.EOF do begin
            Idx:=IndexTable.FieldByName(C_TfDIndex_Index).AsInteger;
            InstNr:=IndexTable.FieldByName(C_TfDIndex_Instanz).AsInteger;
            StatNr:=IndexTable.FieldByName(C_TfDIndex_StatNr).AsInteger;
            GrpNr:=IndexTable.FieldByName(C_TfDIndex_ArchGrpNr).AsInteger;
            Merkfeld^[Idx]:=true;                               { Nummer belegt }
            if (StationID = StatNr) AND (GruppenID = GrpNr) AND (InstanzID = InstNr) then begin       { Eintrag gefunden }
              gefunden:=true;
              Break;
            end;
            IndexTable.Next;
          end;

          if (not gefunden) AND Eintragen then begin         { Eintrag einfügen }
            Idx:=1;
            while Merkfeld^[Idx] do
              inc(Idx);                           { nächste freie Nummer suchen }
            IndexTable.AppendRecord([Idx, nil, StationID, GruppenID, InstanzID, nil]);
            gefunden:=true;
          end;
        finally
          IndexTable.Close;
        end;
      end;
    finally
      IndexTable.Free;
    end;
  finally
    Dispose(Merkfeld);
  end;
  if (Idx > 0) AND gefunden then Result:=Idx;
end;

{ Fügt Daten aus einem manuellen Abruf in die Automatikdaten ein           }
{ Vorherige Daten der Archivgruppe werden gelöscht                         }
{ Parameter: Dateipfade für Auto- und Manu-Daten, StationsId, InstanzId,   }
{            Archivgruppennummer                                           }
{ Rückgabe: Erfolg ja/nein                                                 }
{--------------------------------------------------------------------------}
function ReplaceAutoManu(
  sAuto, sManu: TFileName; iStaId, iInstId, iAGNr: integer): boolean;
{--------------------------------------------------------------------------}
var
  i, iIndexA, iIndexM : integer;
  pSearchRec          : TSearchRec;
  pSl                 : TStrings;
  s                   : string;
begin
  Result := False;  // Default;

  // Index aus jeweiliger INDEX.DB ermitteln
  iIndexM := Get_DSfGDatenIndex(sManu, iStaId, iInstId, iAGNr, False);
  if (iIndexM < 0) then Exit;
  iIndexA := Get_DSfGDatenIndex(sAuto, iStaId, iInstId, iAGNr, True);
  if (iIndexA < 0) then Exit;

  pSl := TStringList.Create;  // Liste zum Aufnehmen der betroffenen Dateinamen
  try
    // Bisher vorhandene Automatikdaten löschen

    // Satz- und Archivdateien
    if (FindFirst(IncludeTrailingBackslash(sAuto) + '????' +
      Format('%.4d', [iIndexA]) + '.*', faAnyFile, pSearchRec) = 0) then
    try
      pSl.Add(IncludeTrailingBackslash(sAuto) + pSearchRec.Name);
      while (FindNext(pSearchRec) = 0) do
        pSl.Add(IncludeTrailingBackslash(sAuto) + pSearchRec.Name);
    finally
      FindClose(pSearchRec);
    end;

    // Dateien löschen
    for i := 0 to pSl.Count-1 do DeleteFile(pSl[i]);
    pSl.Clear;

    // Vorhandene manuelle Daten erfassen

    // Satz- und Archivdateien
    if (FindFirst(IncludeTrailingBackslash(sManu) + '????' +
      Format('%.4d', [iIndexM]) + '.*', faAnyFile, pSearchRec) = 0) then
    try
      pSl.Add(IncludeTrailingBackslash(sManu) + pSearchRec.Name);
      while (FindNext(pSearchRec) = 0) do
        pSl.Add(IncludeTrailingBackslash(sManu) + pSearchRec.Name);
    finally
      FindClose(pSearchRec);
    end;

    // Dateien kopieren
    for i := 0 to pSl.Count-1 do begin
      s := ExtractFileName(pSl[i]);
      s := Copy(s, 1, 4) + Format('%.4d', [iIndexA]) + Copy(s, 9, Length(s)-8);
      s := IncludeTrailingBackslash(sAuto) + s;
      CopyFile(PChar(pSl[i]), PChar(s), True);
    end;

    // Index-Feld anpassen
    if (FileExists(C_TbDSatz + Format('%.4d', [iIndexA]) + '.DB')) then
      with TQuery.Create(nil) do
      try
        DatabaseName := sAuto;
        Sql.Text := 'UPDATE ' + C_TbDSatz + Format('%.4d', [iIndexA]) + ' A ' +
          'SET A."' + C_TfDSatz_Index + '" = ' + IntToStr(iIndexA);
        ExecSQL;
      finally
        Free;
      end;

    Result := True;
  finally
    pSl.Free;
  end;
end;

{ TTbDArchiv }

{--------------------------------------------------------------------------}
constructor TTbDArchiv.Create (AArchivPath: TFileName; AStationId: integer);
{--------------------------------------------------------------------------}
begin
  inherited Create;
  ArchivPath:=AArchivPath;
  StationId:=AStationId;
  tbDSatz:=TTableExt.Create(nil);
  tbDSatz.DataBaseName:=ArchivPath;
  tbDSatz.TableName:='';
  tbDWert:=TTableExt.Create(nil);
  tbDWert.DataBaseName:=ArchivPath;
  tbDWert.TableName:='';
  tbDHex:=TTableExt.Create(nil);
  tbDHex.DataBaseName:=ArchivPath;
  tbDHex.TableName:='';
  tbDText:=TTableExt.Create(nil);
  tbDText.DataBaseName:=ArchivPath;
  tbDText.TableName:='';
end;

{----------------------------}
destructor TTbDArchiv.Destroy;
{----------------------------}
begin
  tbDHex.Free;
  tbDWert.Free;
  tbDText.Free;
  tbDSatz.Free;
  inherited Destroy;
end;

{---------------------------------------------------------}
procedure TTbDArchiv.SetTableNames (Index, Kanal: integer);
{---------------------------------------------------------}
{ weist den Tabellen-Variablen die Tabellennamen anhand von Index und Kanal zu }
begin
  if Kanal > 0 then begin                                                             { ArchivKanal }
    tbDSatz.TableName:=Format(C_TbDSatz+'%.4d.db',[Index]);                           { Satztabelle }
    tbDWert.TableName:=Format(C_TbDWert+'%.3d',[Kanal])+Format('%.4d.db',[Index]);    { Werttabelle }
    tbDHex.TableName:=Format(C_TbDHex+'%.3d',[Kanal])+Format('%.4d.db',[Index]);      { Hex-Tabelle }
    tbDText.TableName:=Format(C_TbDText+'%.3d',[Kanal])+Format('%.4d.db',[Index]);    { Text-Tabelle }
  end;
end;

{---------------------------------------------------------}
procedure TTbDArchiv.CreatetbDSatz (ASatzTable: TTableExt);
{---------------------------------------------------------}
{ Tabelle mit Satz-Tabellenstruktur anlegen;
  Übergabe: Tabelle }
begin
  with ASatzTable.FieldDefs do begin
    Clear;
    Add(C_TfDSatz_ReferenzNr, ftInteger, 0, false);
    Add(C_TfDSatz_Index, ftSmallInt, 0, false);
    Add(C_TfDSatz_OrdNr, ftInteger, 0, false);
    Add(C_TfDSatz_Datum, ftDate, 0, false);
    Add(C_TfDSatz_Zeit, ftTime, 0, false);
    Add(C_TfDSatz_Status, ftString, 10, false);
    Add(C_TfDSatz_Zeitzone, ftString, 1, false);
  end;
  with ASatzTable.IndexDefs do begin
    Clear;
    Add('PRefNr', C_TfDSatz_ReferenzNr, [ixPrimary,ixUnique]);                                            { Primärindex }
    Add(CSDatOrd, C_TfDSatz_Datum+';'+C_TfDSatz_Zeit+';'+C_TfDSatz_OrdNr, [ixCaseInsensitive]);         { Sekundärindex }
  end;
  ASatzTable.CreateTable;
end;

{---------------------------------------------------------}
procedure TTbDArchiv.CreatetbDWert (AWertTable: TTableExt);
{---------------------------------------------------------}
{ Tabelle mit Wert-Tabellenstruktur anlegen;
  Übergabe: Tabelle }
begin
  with AWertTable.FieldDefs do begin
    Clear;
    Add(C_TfDWert_ReferenzNr, ftInteger, 0, false);
    Add(C_TfDWert_Kanaltyp, ftString, 2, false);
    Add(C_TfDWert_Werteart, ftString, 2, false);
    Add(C_TfDWert_Wert, ftFloat, 0, false);
    Add(C_TfDWert_Status, ftString, 16, false);
    Add(C_TfDWert_CRC, ftString, 10, false);
  end;
  with AWertTable.IndexDefs do begin
    Clear;
    Add('PRefNr', C_TfDWert_ReferenzNr, [ixPrimary, ixUnique]);                                           { Primärindex }
  end;
  AWertTable.CreateTable;
end;

{-------------------------------------------------------}
procedure TTbDArchiv.CreatetbDHex (AHexTable: TTableExt);
{-------------------------------------------------------}
{ Tabelle mit Hex-Tabellenstruktur anlegen;
  Übergabe: Tabelle }
begin
  with AHexTable.FieldDefs do begin
    Clear;
    Add(C_TfDHex_ReferenzNr, ftInteger, 0, false);
    Add(C_TfDHex_Kanaltyp, ftString, 2, false);
    Add(C_TfDHex_Werteart, ftString, 2, false);
    Add(C_TfDHex_Wert, ftString, 10, false);
    Add(C_TfDHex_Status, ftString, 16, false);
    Add(C_TfDHex_CRC, ftString, 10, false);
  end;
  with AHexTable.IndexDefs do begin
    Clear;
    Add('PRefNr', C_TfDHex_ReferenzNr, [ixPrimary, ixUnique]);                                            { Primärindex }
  end;
  AHexTable.CreateTable;
end;

{---------------------------------------------------------}
procedure TTbDArchiv.CreatetbDText (ATextTable: TTableExt);
{---------------------------------------------------------}
{ Tabelle mit Hex-Tabellenstruktur anlegen;
  Übergabe: Tabelle }
begin
  with ATextTable.FieldDefs do begin
    Clear;
    Add(C_TfDText_ReferenzNr, ftInteger, 0, false);
    Add(C_TfDText_Kanaltyp, ftString, 2, false);
    Add(C_TfDText_Werteart, ftString, 2, false);
    Add(C_TfDText_Wert, ftString, 40, false);
    Add(C_TfDText_Status, ftString, 16, false);
    Add(C_TfDText_CRC, ftString, 10, false);
  end;
  with ATextTable.IndexDefs do begin
    Clear;
    Add('PRefNr', C_TfDText_ReferenzNr, [ixPrimary, ixUnique]);                                            { Primärindex }
  end;
  ATextTable.CreateTable;
end;

{------------------------------------------------------------------------------------}
function TTbDArchiv.GetLetztWertOrdNr_DatumZeit (InstanzId, ArchivGrp, Kanal: integer;
                                                 Kanaltyp: string;
                                                 var OrdNr: integer;
                                                 var DatumZeit: TDateTime): boolean;
{------------------------------------------------------------------------------------}
{ ermittelt Ordnungsnummer und Datum und Zeit des letzten Eintrags der
  Wert/Hex-Tabelle (referenziert über Feld 'ReferenzNr' mit Satztabelle);
  Rückgabe: Ordnungsnummer und Datum/Zeit des letzten Eintrags
  Ergebnis: false, wenn noch keine Daten vorhanden sind }
var
  Idx: integer;
  RefNr: integer;

begin
  Result:=false;
  DatumZeit:=0;
  Idx:=Get_DSfGDatenIndex(ArchivPath, StationId, InstanzId, ArchivGrp, false);             { Index für Satz-Tabelle ermitteln }
  if Idx <= 0 then exit;                                                                     { noch kein Index vergeben }

  SetTableNames (Idx, Kanal);

  if Kanaltyp <> kt_ST then begin                                                       { bei Status-Kanal: Hex-Tabelle }
    if not tbDWert.Exists then exit;                                                     { Wert-Tabelle nicht vorhanden }
    if tbDWert.OpenShared then begin
      try
        RefNr:=-1;                                                                                        { Vorbelegung }
        if tbDWert.RecordCount > 0 then begin                                        { wenn mind. 1 Datensatz enthalten }
          tbDWert.Last;
          RefNr:=tbDWert.FieldByName (C_TfDWert_ReferenzNr).AsInteger;
        end;
      finally
        tbDWert.Close;
      end;
    end else
      exit;
  end
  else begin
    if not tbDHex.Exists then exit;                                                       { Hex-Tabelle nicht vorhanden }
    if tbDHex.OpenShared then begin
      try
        RefNr:=-1;                                                                                        { Vorbelegung }
        if tbDHex.RecordCount > 0 then begin                                         { wenn mind. 1 Datensatz enthalten }
          tbDHex.Last;
          RefNr:=tbDHex.FieldByName (C_TfDHex_ReferenzNr).AsInteger;
        end;
      finally
        tbDHex.Close;
      end;
    end else
      exit;
  end;

  if RefNr > -1 then begin
    if not tbDSatz.Exists then exit;                                                     { Satz-Tabelle nicht vorhanden }
    if tbDSatz.OpenShared then begin
      try
        if tbDSatz.FindKey ([RefNr]) then begin
          OrdNr:=tbDSatz.FieldByName (C_TfDSatz_OrdNr).AsInteger;
          DatumZeit:=tbDSatz.FieldByName (C_TfDSatz_Datum).AsDateTime +
                     tbDSatz.FieldByName (C_TfDSatz_Zeit).AsDateTime;
          Result:=true;
        end;
      finally
        tbDSatz.Close;
      end;
    end;
  end;
end;

{---------------------------------------------------------------------------------------}
function TTbDArchiv.Get_ArchivDaten_By_Zeit_OrdNr (InstanzId, ArchivGrp, Kanal: integer;
                                                   var ergebnis: TQuery;
                                                   VonDatumZeit, BisDatumZeit: TDateTime;
                                                   OrdNrVon: integer = -1;
                                                   OrdNrBis: integer = -1): boolean;
{---------------------------------------------------------------------------------------}
{ liefert Archivdaten für übergebenen Zeitbereich (optional zusätzlich auch für
  Ordnungsnummern-Bereich) in Query zurück;
  Übergabe: InstanzId
            Archivgruppen-Nummer
            Archivkanal-Nummer
            von/bis-Zeitbereich
            von/bis-Ordnungsnummernbereich (optional)
  Rückgabe: Ergebnis-Query
  Ergebnis: false, wenn keine Daten für angeforderten Bereich vorhanden sind }
var
  Idx: integer;
  Anzahl: integer;
  vd, bd: TDateTime;
  vz, bz: string;
  Hex: boolean;

begin
  Result:=false;
  Hex:=false;                    { Vorbelegung: Daten kommen aus Wert-Tabelle }
  Idx:=Get_DSfGDatenIndex(ArchivPath, StationId, InstanzId, ArchivGrp, false);
  if Idx <= 0 then exit;

  SetTableNames (Idx, Kanal);
  if not tbDWert.Exists then begin
    Hex:=true;
    if not tbDHex.Exists then exit;
  end;
  Anzahl:=0;

  vd:=int(VonDatumZeit);
  vz:=FormatDateTime('hh":"nn":"ss',VonDatumZeit);
  bd:=int(BisDatumZeit);
  bz:=FormatDateTime('hh":"nn":"ss',BisDatumZeit);

  try
    if ergebnis <> nil then begin
      with ergebnis do begin
        Databasename:=ArchivPath;
        Close;
        sql.Clear;
        if not Hex then begin                        { Daten aus Wert-Tabelle }
          sql.Add('Select s.'+ C_TfDSatz_ReferenzNr +',s.'+ C_TfDSatz_Datum+
                  ',s.'+ C_TfDSatz_Zeit+',s.'+C_TfDSatz_Zeitzone+',s.'+C_TfDSatz_OrdNr +
                  ',w.'+ C_TfDWert_Wert+',w.'+ C_TfDWert_Status);
          sql.Add('From "'+tbdSatz.Tablename+'" s,"'+tbdWert.Tablename+'" w');
          sql.Add('Where (s.'+ C_TfDSatz_ReferenzNr+' = w.'+ C_TfDWert_ReferenzNr+')');
        end
        else begin                                    { Daten aus Hex-Tabelle }
          sql.Add('Select s.'+ C_TfDSatz_ReferenzNr +',s.'+ C_TfDSatz_Datum+
                  ',s.'+ C_TfDSatz_Zeit+',s.'+C_TfDSatz_Zeitzone+',s.'+C_TfDSatz_OrdNr +
                  ',h.'+ C_TfDHex_Wert+',h.'+ C_TfDHex_Status);
          sql.Add('From "'+tbdSatz.Tablename+'" s,"'+tbdHex.Tablename+'" h');
          sql.Add('Where (s.'+ C_TfDSatz_ReferenzNr+' = h.'+ C_TfDHex_ReferenzNr+')');
        end;
        { Zeit-Bereich: }
        Sql.Add(' AND ((s.'+C_TfDSatz_Datum+'>:vd AND s.'+C_TfDSatz_Datum+' < :bd) OR ');
        if vd <> bd then
          sql.Add('(s.'+C_TfDSatz_Datum+' = :vd AND s.'+ C_TfDSatz_Zeit+' >= "'+vz+'") OR ')
        else
          sql.Add('(s.'+C_TfDSatz_Datum+' = :vd AND s.'+ C_TfDSatz_Zeit+' >= "'+vz+'") AND ');
        sql.Add('(s.'+C_TfDSatz_Datum+' = :bd AND s.'+ C_TfDSatz_Zeit+' <= "'+bz+'"))');
        { Ordnungsnummern-Bereich: }
        if (OrdNrVon > -1) AND (OrdNrBis > -1) then
          sql.Add(' AND (s.'+C_TfDSatz_OrdNr+' >= :Ov AND s.'+ C_TfDSatz_OrdNr+' <= :Ob)');

        ParamByname('VD').AsDate:=VD;
        ParamByname('BD').AsDate:=BD;
        if (OrdNrVon > -1) AND (OrdNrBis > -1) then begin
          ParamByname('Ov').AsInteger:=OrdNrVon;
          ParamByname('Ob').AsInteger:=OrdNrBis;
        end;
        Open;
      end;
      Anzahl:=ergebnis.RecordCount;
    end;
  finally
    if Anzahl > 0 then
      Result:=true
    else
      Result:=true;
  end;
end;

{ Trägt (virtuelle) Archivdaten aus Liste in Archivtabellen ein            }
{ Parameter: StationsId, InstanzId, Archivgruppe, Liste mit Daten          }
{   Struktur: ONr oder leer<us>Unixtime<us>Kanal1<us>..<us>Kanaln          }
{ ACHTUNG: Es wird zeitlich angehängt                                      }
{ Rückgabe: Erfolg ja/nein                                                 }
{--------------------------------------------------------------------------}
function TTbDArchiv.InsertArchivdatenList(   // 12.02.2002
  iStaId, iInstId, iAGNr: integer; pSlDaten: TStrings): boolean;
{--------------------------------------------------------------------------}
var
  i, j, iStart, iIndex, iONr, iRef, iO1, iR1 : integer;
  iUnix                         : longint;
  dt                            : TDateTime;
  s                             : string;
  pTable                        : TTableExt;
  pTableList                    : TList;
  b                             : boolean;
begin
  Result := False;  // Default
  if (pSlDaten.Count = 0) then Exit;

  iIndex := Get_DSfGDatenIndex(ArchivPath, iStaId, iInstId, iAGNr, True);
  if (iIndex < 0) then Exit;

  pTableList := TList.Create;  // Liste zum Aufnehmen der Wertetabellen
  try

    // Tabellen in Tabellenliste aufnehmen
    b := False;  // Gibt es überhaupt Einträge ?
    for i := 2+1 to 2+21 do  // Schleife über 21 mögliche Kanäle
      if (Trim(GetStringPart(pSlDaten[0], i)) <> '')
      then begin
        b := True;  // Es gibt Einträge !
        pTable := TTableExt.Create(nil);
        pTable.DatabaseName := ArchivPath;
        pTable.TableName := C_TbDWert + Format('%.3d%.4d', [i-2, iIndex]);

        with tbDWert do begin
          if (Active) then Close;
          DatabaseName := ArchivPath;
          TableName := pTable.TableName;
          if (not Exists) then CreatetbDWert (tbDWert);
        end;

        pTable.Open;
        pTableList.Add(pTable);
      end
      else pTableList.Add(nil);
      if (not b) then Exit;

    try

      with tbDSatz do begin
        if (Active) then Close;
        DatabaseName := ArchivPath;
        TableName := C_TbDSatz + Format('%.4d', [iIndex]);
        if (not Exists) then CreatetbDSatz (tbDSatz);

        Open;

        // Ersten neuen Eintrag herausfinden
        iStart := pSlDaten.Count;
        iO1 := 0;
        iR1 := 0;
        if (not Eof) then begin
          Last;
          for i := 0 to pSlDaten.Count-1 do begin
            iUnix := StrToIntDef('$' + Trim(GetStringPart(pSlDaten[i], 2)), -1);
            if (iUnix < 0) then Continue
            else begin
              dt := Trunc(FieldByName(C_TfDSatz_Datum).asDateTime) +
                Frac(FieldByName(C_TfDSatz_Zeit).asDateTime);
              if (dt < UnixToDateTime(iUnix)) then begin
                iStart := i;
                iR1 := FieldByName(C_TfDSatz_ReferenzNr).asInteger;
                iO1 := FieldByName(C_TfDSatz_OrdNr).asInteger;
                Break;
              end;
            end;
          end;
        end
        else iStart := 0;

        // Neue Einträge in Satztabelle schreiben
        iRef := iR1;
        iONr := iO1;
        for i := iStart to pSlDaten.Count-1 do begin
          // Referenznummer
          Inc(iRef);
          // Ordnungsnummer
          s := Trim(GetStringPart(pSlDaten[i], 1));
          if (s = '') then Inc(iONr) else iONr := StrToInt(s);
          // Datum/Zeit
          iUnix := StrToIntDef('$' + Trim(GetStringPart(pSlDaten[i], 2)), -1);
          if (iUnix < 0) then Continue;
          dt := UnixToDateTime(iUnix);

          Append;
          FieldByName(C_TfDSatz_ReferenzNr).asInteger := iRef;
          FieldByName(C_TfDSatz_Index).asInteger := iIndex;
          FieldByName(C_TfDSatz_OrdNr).asInteger := iONr;
          FieldByName(C_TfDSatz_Datum).asDateTime := Trunc(dt);
          FieldByName(C_TfDSatz_Zeit).asDateTime := Frac(dt);
          Post;

          for j := 0 to pTableList.Count-1 do
            if (Assigned(pTableList[j])) then begin
              s := Trim(GetStringPart(pSlDaten[i], j+3));
              if (s <> '') then
                with TTableExt(pTableList[j]) do begin
                  Append;
                  FieldByName(C_TfDWert_ReferenzNr).asInteger := iRef;
                  FieldByName(C_TfDWert_Wert).AsFloat := WStrToFloatDef(s, 0);
                  Post;
                end;
            end;
        end;

        Close;
      end;

    finally
      for i := 0 to pTableList.Count-1 do
        if (Assigned(pTableList[i])) then
          with TTableExt(pTableList[i]) do begin
            Close;
            Free;
          end;
    end;

  finally
    pTableList.Free
  end;
end;

end.

