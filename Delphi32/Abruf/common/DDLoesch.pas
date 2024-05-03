{******************************************************************************}
{* Unit: Lˆschen von DSfG-Daten (Auto/Manu)                                   *}
{* 03.02.2000 WW                                                              *}
{* 24.10.2001 GD  - Lˆschen von Logb¸chern, + Lˆschen Zeitbereichen (D_Arch.) *}
{* 03.05.2002 GD  - Erweitert um optionalen Datenbanknamen *}
{* 04.07.2002 GD  - Vorbereitet f¸r Text-Wertetabellen                        *}
{******************************************************************************}
unit DDLoesch;

interface

uses
  SysUtils, Forms, DbTables, PathIni, TbDSfGAr, TbDSfGIw, WTables, WSysCon,
  DDbSta, MeldungenDb;


function LoescheDaten_Archivkanal (Auto: boolean; InstanzId: integer;
  ArchivNr: integer; KanalNr: integer; sArchivDatabase: string = ''): boolean;
function LoescheDaten_Archiv (Auto: boolean; InstanzId: integer;
  ArchivNr: integer; sArchivDatabase: string = ''): boolean;
// 24.10.2001 Anfang
function Loesche_ArchivDatenVonBis(
  sDatabaseName: string; iIndex: integer; dtVon, dtBis: TDateTime): boolean;
function Loesche_ArchivKanalDatenVonBis(sDatabaseName: string;
  iIndex, iKanalNr: integer; iVon, iBis: integer): boolean;
function LoescheDaten_Logbuch (Auto: boolean; InstanzId: integer; LogbuchNr: integer;
  sStammDatabase: string = ''): boolean;
function LoescheDaten_Instanz (Auto: boolean; InstanzId: integer;
  sStammDatabase: string = ''; sArchivDatabase: string = ''): boolean;
function LoescheDaten_Station (Auto: boolean; StationId: integer;
  sStammDatabase: string = ''; sArchivDatabase: string = ''): boolean;
procedure LoescheDatenelemente_Station (Auto: boolean; StationId: integer;
  sStammDatabase: string = ''; sArchivDatabase: string = '');

implementation

{-----------------------------------------------------------------------------------}
function LoescheDaten_Logbuch (Auto: boolean; InstanzId: integer; LogbuchNr: integer;
                               sStammDatabase: string = ''): boolean;
{-----------------------------------------------------------------------------------}
// WW, ab 16.04.2003 wieder mit Funktion: auf WMeldungen.db umgestellt }
var
  Benutzer: string;
  MeldungenDb: TMeldungenDb;
begin
  Result := True;
  if (Auto) then Benutzer:=C_BenutzerAuto else Benutzer:=C_BenutzerManu;

  { alle Eintr‰ge zu InstanzId und LogbuchNr aus Meldungen-Tabellen lˆschen: }
  if (sStammDatabase = '')
  then MeldungenDb:=TMeldungenDb.Create (PathServer.Pathname [WStammDir])
  else MeldungenDb:=TMeldungenDb.Create (sStammDatabase);

  try
    MeldungenDb.DeleteDSfGMeldungen (Benutzer, InstanzId, LogbuchNr);
  finally
    MeldungenDb.Free;
  end;
end;

{--------------------------------------------------------------------}
procedure DeleteIndexfromIndexDb (Auto: boolean; Datenindex: integer;
  sArchivDatabase: string = '');
{--------------------------------------------------------------------}
{ Eintrag aus Index.db lˆschen;
  ‹bergabe: Auto (wenn true, Eintrag aus Index-Tabelle des Auto-Verzeichnis lˆschen, sonst aus Manu)
            Datenindex }
var
  Q: TQueryExt;
begin
  Q:=TQueryExt.Create (nil);
  try
    if (sArchivDatabase = '') then begin
      if (Auto) then
        Q.DataBaseName := Pathserver.Pathname[DArchivDir]
      else
        Q.DataBaseName := Pathserver.Pathname[DManuDir];
    end
    else Q.DataBaseName := sArchivDatabase;  // 03.05.2002

    with q do begin
      Sql.Add ('DELETE FROM "' + C_TbDIndex + '" a');
      Sql.Add ('WHERE a."' + C_TfDIndex_Index + '" = ' + IntToStr (Datenindex));
      ExecSql;
    end;
  finally
    Q.Free;
  end;
end;

{------------------------------------------------------------------------------------------------}
function Loesche_ArchivkanalDaten (Auto: boolean; Datenindex: integer;
  KanalNr: integer; sArchivDatabase: string = ''): boolean;
{------------------------------------------------------------------------------------------------}
{ lˆschen der Wert- und Hex-Tabelle f¸r Datenindex, KanalNr;
  ‹bergabe: Auto (wenn true, Daten aus Auto-Verzeichnis lˆschen, sonst aus Manu)
            Datenindex (Index aus Index.db)
            Kanalnr
  Ergebnis: true, wenn alle Tabellen erfolgreich gelˆscht wurden }
var
  i: integer;
  Name, Pfad: string;
  SR: TSearchRec;

begin
  Result:=true;
  if (sArchivDatabase = '') then begin
    if (Auto) then
      Pfad:= Pathserver.Pathname[DArchivDir]
    else
      Pfad:= Pathserver.Pathname[DManuDir];
  end
  else Pfad := sArchivDatabase;  // 03.05.2002

  { Wert }
  Name:= C_TbDWert + Format('%.3d', [KanalNr]) + Format('%.4d.*', [Datenindex]);
  i:= FindFirst (Pfad + Name, faAnyFile, SR);
  while i = 0 do begin
    if not DeleteFile (Pfad + SR.Name) then
      Result:=false;
    i:= FindNext(SR);
  end;
  FindClose (SR);

  { Hex }
  Name:= C_TbDHex + Format('%.3d', [KanalNr]) + Format('%.4d.*', [Datenindex]);
  i:= FindFirst (Pfad + Name, faAnyFile, SR);
  while i = 0 do begin
    if not DeleteFile (Pfad + SR.Name) then
      Result:=false;
    i:= FindNext(SR);
  end;
  FindClose (SR);

  { Text }
  Name:= C_TbDText + Format('%.3d', [KanalNr]) + Format('%.4d.*', [Datenindex]);
  i:= FindFirst (Pfad + Name, faAnyFile, SR);
  while i = 0 do begin
    if not DeleteFile (Pfad + SR.Name) then
      Result:=false;
    i:= FindNext(SR);
  end;
  FindClose (SR);
end;

{ Lˆscht die Daten eines Kanals                                           }
{ Parameter: Datenbankname, Index aus INDEX.DB, Zeitbereich von  - bis    }
{            (eine Grenze muﬂ offen (=0) sein)                            }
{-------------------------------------------------------------------------}
function Loesche_ArchivDatenVonBis (
  sDatabaseName: string; iIndex: integer; dtVon, dtBis: TDateTime): boolean;
{-------------------------------------------------------------------------}
var
  sTbSatz    : string;
  iVon, iBis : integer;
  i          : byte;
begin
  Result := False;  // Default
  iVon := 0;
  iBis := 0;
  if (dtVon > 0) and (dtBis > 0) then Exit;   // Eine Richtung muﬂ offen (=0) sein

  try
    with TTableExt.Create(nil) do
    try
      Result := True;
      DatabaseName := sDatabaseName;
      sTbSatz:= C_TbDSatz + Format('%.4d', [iIndex]);
      TableName := sTbSatz;
      if (Exists) then begin
        with TQuery.Create(nil) do
        try
          DatabaseName := sDatabaseName;
          Sql.Add('SELECT ' + C_TfDSatz_ReferenzNr);
          Sql.Add('FROM ' + sTbSatz);
          if (dtVon > 0) then begin
            Sql.Add('WHERE ' + C_TfDSatz_Datum + ' >= :VON');
            ParamByName('VON').asDateTime := Trunc(dtVon);
          end
          else if (dtBis > 0) then begin
            Sql.Add('WHERE ' + C_TfDSatz_Datum + ' <= :BIS');
            ParamByName('BIS').asDateTime :=
              Trunc(dtBis) + EncodeTime(23, 59, 59, 0);
          end;
          Sql.Add('ORDER BY ' + C_TfDSatz_ReferenzNr);
          Open;

          if (not IsEmpty) then begin
            if (dtVon > 0) then iVon := Fields[0].asInteger
            else if (dtBis > 0) then begin
              Last;
              iBis := Fields[0].asInteger;
            end;
          end;

          Close;
        finally
          Free;
        end;

        if (iVon > 0) or (iBis > 0) then begin  // Referenznummern vorhanden

          // Kan‰le lˆschen
          for i := 1 to 21 do begin  // max. 21 Kan‰le
            if (not Loesche_ArchivKanalDatenVonBis(
              sDatabaseName, iIndex, i, iVon, iBis)) then Result := False;
          end;

          // Eintr‰ge in Satztabelle lˆschen
          if (Result) then
            with TQueryExt.Create(nil) do
            try
              DatabaseName := sDatabaseName;
              Sql.Add('DELETE FROM ' + sTbSatz);
              if (iVon > 0) then
                Sql.Add('WHERE ' + C_TfDSatz_ReferenzNr + ' >= ' + IntToStr(iVon))
              else if (iBis > 0) then
              Sql.Add('WHERE ' + C_TfDSatz_ReferenzNr + ' <= ' + IntToStr(iBis));
              ExecSql;
            finally
              Free;
            end;

          // Ggf. Satztabelle und Eintrag in Index.db lˆschen
          Open;
          if (IsEmpty) then begin
            Close;
            Result := (DeleteTable);
            with TQueryExt.Create(nil) do // Aus Index.DB lˆschen
            try
              DatabaseName := sDatabaseName;
              Sql.Add('DELETE FROM "' + C_TbDIndex + '" A');
              Sql.Add('WHERE A."' + C_TfDIndex_Index + '" = ' + IntToStr(iIndex));
              ExecSql;
            finally
              Free;
            end;
          end
          else Close;

        end;    // If Referenznummern vorhanden
      end;    // TableExists

    finally
      Free;   // TTableExt
    end;
  except
     Result := False;
  end;
end;

{ Lˆscht die Daten eines Kanals                                           }
{ Parameter: Datenbankname, Index aus INDEX.DB, Kanalnummer, Referenz von }
{            - bis (eine Grenze muﬂ offen (=0) sein                       }
{-------------------------------------------------------------------------}
function Loesche_ArchivKanalDatenVonBis(sDatabaseName: string;
  iIndex, iKanalNr: integer; iVon, iBis: integer): boolean;
{-------------------------------------------------------------------------}
var
  sTbWert : string;
begin
  Result := False;  // Default
  if (iVon > 0) and (iBis > 0) then Exit;   // Eine Richtung muﬂ offen (=0) sein
  Result := True;

  try
    with TTable.Create(nil) do
    try
      DatabaseName := sDatabaseName;
      sTbWert:= C_TbDWert + Format('%.3d%.4d', [iKanalNr, iIndex]);
      TableName := sTbWert;
      if (not Exists) then begin
        sTbWert:= C_TbDHex + Format('%.3d%.4d', [iKanalNr, iIndex]);
        TableName := sTbWert;
        if (not Exists) then begin
          sTbWert:= C_TbDText + Format('%.3d%.4d', [iKanalNr, iIndex]);
          TableName := sTbWert;
          if (not Exists) then sTbWert := '';
        end;
      end;
    finally
      Free;
    end;

    if (sTbWert <> '') then begin
      with TQueryExt.Create(nil) do
      try
        DatabaseName := sDatabaseName;
        Sql.Add('DELETE FROM ' + sTbWert);
        if (iVon > 0) then
          Sql.Add('WHERE ' + C_TfDWert_ReferenzNr + ' >= ' + IntToStr(iVon))
        else if (iBis > 0) then
        Sql.Add('WHERE ' + C_TfDWert_ReferenzNr + ' <= ' + IntToStr(iBis));
        ExecSql;
      finally
        Free;
      end;

      with TTableExt.Create(nil) do
      try
        DatabaseName := sDatabaseName;
        TableName := sTbWert;
        Open;
        if (IsEmpty) then begin
          Close;
          Result := (DeleteTable);
        end
        else Close;
      finally
        Free;
      end;
    end;
  except
     Result := False;
  end;
end;

{-------------------------------------------------------------------------}
function Loesche_ArchivDaten (Auto: boolean; Datenindex: integer;
  sArchivDatabase: string = ''): boolean;
{-------------------------------------------------------------------------}
{ lˆschen der Satz-, Wert-, und Hex-Tabellen
  ‹bergabe: Auto (wenn true, Daten aus Auto-Verzeichnis lˆschen, sonst aus Manu)
            Datenindex (Index aus Index.db)
  Ergebnis: true, wenn alle Tabellen erfolgreich gelˆscht wurden }
var
  i: integer;
  Name, Pfad: string;
  SR: TSearchRec;

begin
  Result:=true;
  if (sArchivDatabase = '') then begin
    if (Auto) then
      Pfad:= Pathserver.Pathname[DArchivDir]
    else
      Pfad:= Pathserver.Pathname[DManuDir];
  end
  else Pfad := sArchivDatabase;  // 03.05.2002

  { Wert }
  Name:= C_TbDWert + '???' + Format('%.4d.*', [Datenindex]);
  i:= FindFirst (Pfad + Name, faAnyFile, SR);
  while i = 0 do begin
    if not DeleteFile (Pfad + SR.Name) then
      Result:=false;
    i:= FindNext(SR);
  end;
  FindClose (SR);

  { Text }
  Name:= C_TbDText + '???' + Format('%.4d.*', [Datenindex]);
  i:= FindFirst (Pfad + Name, faAnyFile, SR);
  while i = 0 do begin
    if not DeleteFile (Pfad + SR.Name) then
      Result:=false;
    i:= FindNext(SR);
  end;
  FindClose (SR);

  { Hex }
  Name:= C_TbDHex + '???' + Format('%.4d.*', [Datenindex]);
  i:= FindFirst (Pfad + Name, faAnyFile, SR);
  while i = 0 do begin
    if not DeleteFile (Pfad + SR.Name) then
      Result:=false;
    i:= FindNext(SR);
  end;
  FindClose (SR);

  { Satz }
  Name:= C_TbDSatz + Format('%.4d.*', [Datenindex]);
  i:= FindFirst (Pfad + Name, faAnyFile, SR);
  while i = 0 do begin
    if not DeleteFile (Pfad + SR.Name) then
      Result:=false;
    i:= FindNext (SR);
  end;
  FindClose (SR);
end;

{------------------------------------------------------------------------------------------------------------------}
function LoescheDaten_Archivkanal (Auto: boolean; InstanzId: integer;
  ArchivNr: integer; KanalNr: integer; sArchivDatabase: string = ''): boolean;
{------------------------------------------------------------------------------------------------------------------}
{ lˆschen von Daten f¸r einen Archivkanal;
  ‹bergabe: Auto (wenn true, Daten aus Auto-Verzeichnis lˆschen, sonst aus Manu)
            InstanzId
            ArchivNr
            KanalNr
  Ergebnis: true, wenn Daten des Archivkanals erfolgreich gelˆscht wurden }
var
  Pfad: TFileName;
  Datenindex: integer;
  Q: TQueryExt;

begin
  Result:=true;
  if (sArchivDatabase = '') then begin
    if Auto then
      Pfad:=Pathserver.Pathname[DArchivDir]
    else
      Pfad:=Pathserver.Pathname[DManuDir];
  end
  else Pfad := sArchivDatabase;   // 03.05.2002

  { Wert- und Hex-Tabelle lˆschen: }
  Q:=TQueryExt.Create (nil);
  try
    Q.DataBaseName:=Pfad;
    if FileExists (Pfad + C_TbDIndex) then begin
      with Q do begin
        Sql.Clear;
        Sql.Add ('SELECT ' + C_TfDIndex_Instanz + ', ');
        Sql.Add ('a."' + C_TfDIndex_Index + '"');
        Sql.Add ('FROM "' + C_TbDIndex + '" a');
        Sql.Add ('WHERE ' + C_TfDIndex_Instanz + ' = ' + IntToStr(InstanzId) + ' AND ');
        Sql.Add (C_TfDIndex_ArchGrpNr + ' = ' + IntToStr(ArchivNr));
        Open;
        First;
        while not Eof do begin
          Datenindex:= FieldByName(C_TfDIndex_Index).asInteger;
          if not Loesche_ArchivkanalDaten (Auto, Datenindex, KanalNr, Pfad) then
            Result:=false;
          { Eintrag aus Index.db hier nicht lˆschen, es kˆnnten noch andere zu Datenindex gehˆrende Archivkanal-
            daten existieren ! }
          Next;
        end;
      end;
    end;
  finally
    Q.Free;
  end;
end;

{-------------------------------------------------------------------------------------------}
function LoescheDaten_Archiv (Auto: boolean; InstanzId: integer;
  ArchivNr: integer; sArchivDatabase: string = ''): boolean;
{-------------------------------------------------------------------------------------------}
{ lˆschen von Daten f¸r eine Archivgruppe;
  ‹bergabe: Auto (wenn true, Daten aus Auto-Verzeichnis lˆschen, sonst aus Manu)
            InstanzId
            ArchivNr
  Ergebnis: true, wenn alle Daten der Archivgruppe erfolgreich gelˆscht wurden }
var
  Pfad: TFileName;
  Datenindex: integer;
  Q: TQueryExt;

begin
  Result:=true;
  if (sArchivDatabase = '') then begin
    if (Auto) then
      Pfad:= Pathserver.Pathname[DArchivDir]
    else
      Pfad:= Pathserver.Pathname[DManuDir];
  end
  else Pfad := sArchivDatabase;  // 03.05.2002

  { Satz-, Wert-, und Hex-Tabellen lˆschen: }
  Q:=TQueryExt.Create (nil);
  try
    Q.DataBaseName:=Pfad;
    if FileExists (Pfad + C_TbDIndex) then begin
      with Q do begin
        Sql.Clear;
        Sql.Add ('SELECT ' + C_TfDIndex_Instanz + ', ');
        Sql.Add ('a."' + C_TfDIndex_Index + '"');
        Sql.Add ('FROM "' + C_TbDIndex + '" a');
        Sql.Add ('WHERE ' + C_TfDIndex_Instanz + ' = ' + IntToStr(InstanzId) + ' AND ');
        Sql.Add (C_TfDIndex_ArchGrpNr + ' = ' + IntToStr(ArchivNr));
        Open;
        First;
        while not Eof do begin
          Application.ProcessMessages;
          Datenindex:= FieldByName(C_TfDIndex_Index).asInteger;
          if not Loesche_ArchivDaten (Auto, Datenindex, sArchivDatabase) then
            Result:=false;
          if Result then
            DeleteIndexfromIndexDb (Auto, Datenindex, sArchivDatabase);
          Next;
        end;
      end;
    end;
  finally
    Q.Free;
  end;
end;

{-------------------------------------------------------------------------}
function LoescheDaten_Instanz (Auto: boolean; InstanzId: integer;
  sStammDatabase: string = ''; sArchivDatabase: string = ''): boolean;
{-------------------------------------------------------------------------}
{ lˆschen von Daten f¸r eine Instanz;
  ‹bergabe: Auto (wenn true, Daten aus Auto-Verzeichnis lˆschen, sonst aus Manu)
            InstanzId
  Ergebnis: true, wenn alle Daten der Instanz erfolgreich gelˆscht wurden }
var
  Pfad: TFileName;
  Datenindex: integer;
  Q: TQueryExt;
  TbDInstanzwert: TTbDInstanzwert;
  Benutzer: string;
  MeldungenDb: TMeldungenDb;

begin
  Result:=true;

  if (Auto) then Benutzer:=C_BenutzerAuto else Benutzer:=C_BenutzerManu;
  if (sArchivDatabase = '') then begin
    if (Auto)
    then Pfad:= Pathserver.Pathname[DArchivDir]
    else Pfad:= Pathserver.Pathname[DManuDir];
  end
  else begin
    Pfad := sArchivDatabase;  // 03.05.2002
  end;

  { Satz-, Wert-, Hex-Tabellen lˆschen: }
  Q:=TQueryExt.Create (nil);
  try
    Q.DataBaseName:=Pfad;
    if FileExists (Pfad + C_TbDIndex) then begin
      with Q do begin
        Sql.Clear;
        Sql.Add ('SELECT ' + C_TfDIndex_Instanz + ', ');
        Sql.Add ('a."' + C_TfDIndex_Index + '"');
        Sql.Add ('FROM "' + C_TbDIndex + '" a');
        Sql.Add ('WHERE ' + C_TfDIndex_Instanz + ' = ' + IntToStr(InstanzId));
        Open;
        First;
        while not Eof do begin
          Application.ProcessMessages;
          Datenindex:= FieldByName(C_TfDIndex_Index).asInteger;
          if not Loesche_ArchivDaten (Auto, Datenindex, sArchivDatabase) then
            Result:=false;
          if Result then
            DeleteIndexfromIndexDb (Auto, Datenindex, sArchivDatabase);
          Next;
        end;
      end;
    end;
  finally
    Q.Free;
  end;

  { alle Eintr‰ge zu InstanzId aus Meldungen-Tabellen lˆschen: }
  if (sStammDatabase = '')
  then MeldungenDb:=TMeldungenDb.Create (PathServer.Pathname [WStammDir])
  else MeldungenDb:=TMeldungenDb.Create (sStammDatabase);

  try
    MeldungenDb.DeleteDSfGMeldungen (Benutzer, InstanzId, -1);
  finally
    MeldungenDb.Free;
  end;

  { Eintr‰ge zu InstanzId aus Instanzwert-Tabelle lˆschen: }
  TbDInstanzwert:=TTbDInstanzwert.Create (Pfad);
  try
    TbDInstanzwert.Loesche_Instanzwerte (InstanzId);
  finally
    TbDInstanzwert.Free;
  end;
end;

{-------------------------------------------------------------------------}
function LoescheDaten_Station (Auto: boolean; StationId: integer;
  sStammDatabase: string = ''; sArchivDatabase: string = ''): boolean;
{-------------------------------------------------------------------------}
{ lˆschen von Daten f¸r eine Station;
  ‹bergabe: Auto (wenn true, Daten aus Auto-Verzeichnis lˆschen, sonst aus Manu)
            StationId
  Ergebnis: true, wenn alle Daten der Station erfolgreich gelˆscht wurden }
var
  Q: TQuery;
  InstanzId: integer;
  Stammdaten: TDSfGStammdaten;

begin
  Result:=true;
  Q:=TQuery.Create (nil);
  try
    if (sStammDatabase = '')
    then Stammdaten := TDSfGStammdaten.Create (PathServer.PathName[WStammDir])
    else Stammdaten:= TDSfGStammdaten.Create (sStammDatabase);  // 03.05.2002

    try
      if Stammdaten.InitTabellen then
        Stammdaten.GetInstanzQuery (StationId, Q);                     { alle zu StationId gehˆrenden InstanzIds holen }
    finally
      Stammdaten.Free;
    end;

    while not Q.Eof do begin
      InstanzId:=Q.FieldByName(C_DTF_Instanz_InstanzId).asInteger;
      if not LoescheDaten_Instanz (Auto, InstanzId, sStammDatabase, sArchivDatabase) then
        Result:=false;
      Q.next;
    end;
  finally
    Q.Free;
  end;
end;

{-------------------------------------------------------------------------}
procedure LoescheDatenelemente_Station (Auto: boolean; StationId: integer;
  sStammDatabase: string = ''; sArchivDatabase: string = '');
{-------------------------------------------------------------------------}
{ lˆschen von abgespeicherten Datenelementen f¸r eine Station;
  ‹bergabe: Auto (wenn true, Datenelemente aus Auto-Verzeichnis lˆschen, sonst aus Manu)
            StationId }
var
  Q: TQuery;
  InstanzId: integer;
  Stammdaten: TDSfGStammdaten;
  Pfad: TFileName;
  TbDInstanzwert: TTbDInstanzwert;

begin
  Q:=TQuery.Create (nil);
  try
    if (sStammDatabase = '')
    then Stammdaten:=TDSfGStammdaten.Create (PathServer.PathName[WStammDir])
    else Stammdaten:=TDSfGStammdaten.Create (sStammDatabase);
    try
      if Stammdaten.InitTabellen then
        Stammdaten.GetInstanzQuery (StationId, Q);                     { alle zu StationId gehˆrenden InstanzIds holen }
    finally
      Stammdaten.Free;
    end;

    if (sArchivDatabase = '') then begin
      if Auto then
        Pfad:=Pathserver.Pathname[DArchivDir]
      else
        Pfad:=Pathserver.Pathname[DManuDir];
    end
    else Pfad := sArchivDatabase;

    TbDInstanzwert:=TTbDInstanzwert.Create (Pfad);
    try
      while not Q.Eof do begin
        InstanzId:=Q.FieldByName(C_DTF_Instanz_InstanzId).asInteger;
        TbDInstanzwert.Loesche_Instanzwerte (InstanzId);
        Q.next;
      end;
    finally
      TbDInstanzwert.Free;
    end;
  finally
    Q.Free;
  end;
end;

end.
