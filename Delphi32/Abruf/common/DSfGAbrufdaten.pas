{------------------------------------------------------------------------------}
{ Bereitstellung von DSfGArchiv-Abrufdaten                                     }
{                                                                              }
{ 18.10.2001  GD  Neu                                                          }
{ 04.07.2002  GD  Statustext wird nicht mehr intern ermittelt                  }
{ 03.02.2003  GD  Diverses (PriorDay, NextDay, ...)                            }
{ 10.07.2003  GD  Spaltentypen ergänzt                                         }
{ 04.08.2003  GD  Überflüssige Proceduren gestrichen                           }
{ 07.10.2003  GD  Zählerstände mit AddInteger hinzufügen                       }
{ 01.12.2003  GD  Bugfix: bei Stundenwerten eine Stunde früher anfangen        }
{ 24.09.2004  GD  Einstellbar: Kanalstati als Zahl                             }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2002, 2004                                    }
{------------------------------------------------------------------------------}
unit DSfGAbrufdaten;

interface

uses
  SysUtils, Classes, DbTables, dialogs, 
  WSysCon, DSfGArchive, TbDSfGAr, GD_Utils, Abrufdaten, DStaDll, WStrUtils;

type
  TLoadSatzDataType = (lsdtDateTime, lsdtRefNr, lsdtOrdNr);

  TDSfGAbrufDaten = class(TAbrufDaten)
    constructor Create(sDatabaseName: string; iInstId, iArchNr: integer;
      pArchivInfo: TDSfGArchivInfo); virtual;
    destructor Destroy; override;
  private
    FArchivNr     : integer;
    FIndex        : integer;
    FReferenzVon  : integer;
    FReferenzBis  : integer;
    FArchivInfo   : TDSfGArchivInfo;
    FArchiv       : TArchivList;
    FDStatus      : TStrings;   // 04.07.2002
    FD10Status    : TStrings;   // 04.07.2002
    FDSfGStand    : integer;
    FStaName      : string;
    FTagesEnde    : byte;       // 04.07.2002
    FDatabaseName : string;
    FKanaele      : TByteSet;
    FStatusOk     : string;
    FStatusZahl   : boolean;    // 24.09.2004
    procedure LoadSatzAndArchivData(iLSDT: TLoadSatzDataType = lsdtDateTime;
      iVon: integer = 0; iBis: integer = 0);
    procedure CreateStundenWerte;
    procedure InitStatusLists(bState: boolean);
  protected
    procedure AutoDelete; override;
  public
    procedure Clear; override;
    function GetStatusText(sStatus: string): string;
    procedure SetStaName(sName: string);
    procedure LoadData(dtVon: TDateTime = 0; dtBis: TDateTime = 0); override;
    procedure LoadDataByRefNr(iVon, iBis: integer; bRefNr: boolean);
    property StatusListe;
    property TagesEnde: byte read FTagesEnde write FTagesEnde;
    property DatabaseName: string read FDatabaseName;
    property Kanaele: TByteSet read FKanaele write FKanaele;
    property StrStatusOk: string read FStatusOk;
    property StatusZahl: boolean write FStatusZahl;
    property ArchivGruppe: integer read FArchivNr;
    property TbSatzIndex: integer read FIndex;
  end;

implementation

resourcestring
  C_Text_Referenz = 'Referenznummer';
  C_Text_OrdNr = 'Ordnungsnr.';
  C_Text_DatumZeit = 'Datum/Zeit';
  C_Text_ZeitZone = 'Zeitzone';

const
  C_Index_Referenz   = 0;
  C_Index_DatumZeit  = 1;
  C_Index_Zeitzone   = 2;
  C_Index_OrdnungsNr = 3;

  // Offset Werte- zu den Statusspalten (4 Wertespalten - 1 Satzstatusspalte)
  C_ColDiff_StatusWert = 3;

{-------------------------------- TDSfGAbrufDaten -----------------------------}

{------------------------------------------------------}
constructor TDSfGAbrufDaten.Create(sDatabaseName: string;
  iInstId, iArchNr: integer; pArchivInfo: TDSfGArchivInfo);
{------------------------------------------------------}
begin
  inherited Create(sDatabaseName, iInstId);

  FStatusOk := '';   // Statusstring für 'OK'
  InitStatusLists(True);
  FDatabaseName := sDatabaseName;
  FArchivInfo := pArchivInfo;
  FArchivNr := iArchNr;
  FIndex := FArchivInfo.GetIndex(GerId, FArchivNr);
  FArchiv := FArchivInfo.GetArchiv(GerId, FArchivNr);
  if (not Assigned(FArchiv)) then raise Exception.Create('No data');
  if (FArchiv.DatenBis = 1) then
    FArchivInfo.LoadArchivZeitbereich(iInstId, iArchNr);
  FTagesEnde := 0;
  FReferenzVon := -1;
  FReferenzBis := -1;
  FDSfGStand := 2;
  FStatusZahl := False;  // Status als Text
  FStaName := '';
  FKanaele := [];    // Set der abzurufenden Kanäle ([] = Alle)
end;

{------------------------------------------------------}
destructor TDSfGAbrufDaten.Destroy;
{------------------------------------------------------}
begin
  InitStatusLists(False);

  inherited;
end;

{------------------------------------------------------}
procedure TDSfGAbrufDaten.Clear;
{------------------------------------------------------}
begin
  FReferenzVon := -1;
  FReferenzBis := -1;

  inherited;
end;

{ Laden/Freigeben von Daten für DSfG-Stati             }
{ Parameter: Flag (T=Init., F=Freigeben)               }
{------------------------------------------------------}
procedure TDSfGAbrufDaten.InitStatusLists(bState: boolean);
{------------------------------------------------------}

  procedure FillStatusList(var pSl: TStrings; sTbName: string);
  var
    i : integer;
  begin
    if (not Assigned(pSl)) then pSl := TStringList.Create else pSl.Clear;

    with TTable.Create(nil) do  // Tabelle vorhanden ?
    try
      DatabaseName := ClientStammdaten.DStaDatabaseName;
      TableName := sTbName;
      if (not Exists) then Exit;
    finally
      Free
    end;

    with TQuery.Create(nil) do
    try
      DatabaseName := ClientStammdaten.DStaDatabaseName;
      UniDirectional := True;
      Sql.Add('SELECT Nummer, Text FROM ' + sTbName);
      Sql.Add('ORDER BY Nummer');
      Open;

      while (not Eof) do begin
        i := FieldByName('Nummer').asInteger;
        while (pSl.Count <= i) do pSl.Add('');
        pSl[i] := FieldByName('Text').asString;
        Next;
      end;

      Close;
    finally
      Free
    end;
  end;

begin
  if (bState) then begin
    if (not Assigned(FDStatus)) then
      FillStatusList(FDStatus, ChangeFileExt(C_Tb_DSfGStat, ''));
    if (not Assigned(FD10Status)) then begin
      FillStatusList(FD10Status, ChangeFileExt(C_Tb_DSG10St, ''));
      if (FD10Status.Count > 0) then FStatusOk := FD10Status[0];
    end;
  end
  else begin
    FreeAndNil(FDStatus);
    FreeAndNil(FD10Status);
  end;
end;

{ Lädt Daten für den Archivgruppen-Index               }
{------------------------------------------------------}
procedure TDSfGAbrufDaten.LoadData(dtVon: TDateTime = 0; dtBis: TDateTime = 0);
{------------------------------------------------------}
begin
  if (not Assigned(FArchiv)) then Exit;

  if (not StundenWerte) then begin
    OldVon := DatumVon;
    OldBis := DatumBis;
  end
  else begin
    OldVon := 0;
    OldBis := 0;
    FReferenzVon := -1;
    FReferenzBis := -1;
  end;

  DatumVon := dtVon;
  DatumBis := dtBis;
  if (DatumVon = 0) then DatumVon := FArchiv.DatenVon;
  if (DatumBis = 0) then DatumBis := FArchiv.DatenBis;
  if (DatumVon = 0) or (DatumBis = 0) then Exit;

  if (dtVon = 0) then begin
    if (Frac(DatumBis) > 0)
    then DatumVon := Trunc(DatumBis) + EncodeTime(FTagesEnde, 0, 1, 0)
    else DatumVon := Trunc(DatumBis-1);
  end;

  if (StundenWerte) then DatumVon := DatumVon - EncodeTime(1, 0, 0, 0);  // 01.12.2003

  if ((OldVon = 0) and (OldBis = 0)) or
     (DatumVon < OldVon) or (DatumBis > OldBis)
  then LoadSatzAndArchivData;

  CreateStundenWerte;

  inherited;
end;

{ Lädt Satzdaten für den Archivgruppen-Index           }
{ Parameter: Kriterium, nach dem Daten selektiert w.   }
{ Rückgabe: Spalte, ab der Daten existieren oder -1    }
{------------------------------------------------------}
procedure TDSfGAbrufDaten.LoadSatzAndArchivData(
  iLSDT: TLoadSatzDataType = lsdtDateTime;
  iVon: integer = 0; iBis: integer = 0);
{------------------------------------------------------}

  // Erstellt SQL-Anweisung
  procedure CreateSql(iIndex: integer; pQuery: TQuery; pSlWTables: TStrings);
  var
    i    : integer;
    cTb  : char;
    sTbS : string;
  begin
    sTbS := Format(C_TbDSatz + '%.4d', [iIndex]);  // Name der Satztabelle

    with pQuery do begin
      // Felder der Satztabelle
      Sql.Add('SELECT A.' + C_TfDSatz_ReferenzNr + ', A.' + C_TfDSatz_OrdNr + ',');
      Sql.Add('A.' + C_TfDSatz_Datum + ', A.' + C_TfDSatz_Zeit + ',');
      Sql.Add('A.' + C_TfDSatz_Zeitzone);

      // Felder der Wertetabellen
      for i := 0 to pSlWTables.Count-1 do begin
        cTb := Chr(Ord('B') + i);
        Sql.Add(',' + cTb + '.' + C_TfDWert_Wert + ' AS Wert' + cTb + ',');
        Sql.Add(cTb + '.' + C_TfDWert_Status + ' AS Status' + cTb);
      end;

      // Tabellenname Satztabelle
      Sql.Add('FROM ' + sTbS + ' A');
      Sql.Add('LEFT JOIN ' + pSlWTables[0] + ' B ON A.' +
        C_TfDSatz_ReferenzNr + ' = B.' + C_TfDWert_ReferenzNr);

      // Tabellennamen Wertetabellen
      for i := 1 to pSlWTables.Count-1 do begin
        cTb := Chr(Ord('B') + i);
//        Sql.Add(',' + sTbS);
        Sql.Add('LEFT JOIN ' + pSlWTables[i] + ' ' + cTb + ' ON A.' +
          C_TfDSatz_ReferenzNr + ' = ' + cTb + '.' + C_TfDWert_ReferenzNr);
      end;

      case iLSDT of
        lsdtDateTime:
          begin
            Sql.Add('WHERE ((A.' + C_TfDSatz_Datum + '> :VON)');
            Sql.Add('       OR ((A.' + C_TfDSatz_Datum + '= :VON)');
            Sql.Add('           AND (A.' + C_TfDSatz_Zeit + '>= :VONZEIT)))');
            Sql.Add('AND ((A.' + C_TfDSatz_Datum + '< :BIS)');
            Sql.Add('     OR ((A.' + C_TfDSatz_Datum + '= :BIS)');
            Sql.Add('         AND (A.' + C_TfDSatz_Zeit + '<= :BISZEIT)))');
            ParamByName('VON').asDate := Trunc(DatumVon);
            ParamByName('BIS').asDate := Trunc(DatumBis);
            if (Frac(DatumVon) > 0) and (Frac(DatumVon) >= EncodeTime(0, 0, 1, 1))
              then ParamByName('VONZEIT').asTime := DatumVon - EncodeTime(0, 0, 1, 0)
              else ParamByName('VONZEIT').asTime := DatumVon;
            if (Frac(DatumBis) > 0) and (Frac(DatumBis) < EncodeTime(23, 59, 58, 999))
              then ParamByName('BISZEIT').asTime := DatumBis + EncodeTime(0, 0, 1, 0)
              else ParamByName('BISZEIT').asTime := DatumBis;
          end;
        lsdtRefNr:
          begin
            Sql.Add('WHERE A.' + C_TfDSatz_ReferenzNr + ' >= ' + IntToStr(iVon));
            Sql.Add('AND A.' + C_TfDSatz_ReferenzNr + ' <= ' + IntToStr(iBis));
          end;
        lsdtOrdNr:
          begin
            Sql.Add('WHERE A.' + C_TfDSatz_OrdNr + ' >= ' + IntToStr(iVon));
            Sql.Add('AND A.' + C_TfDSatz_OrdNr + ' <= ' + IntToStr(iBis));
          end;
        else begin
          Exit;
        end;
      end;

      if (FReferenzVon > 0) and (FReferenzBis > 0) then begin
        Sql.Add('AND ((A.' + C_TfDSatz_ReferenzNr + '<' + IntToStr(FReferenzVon) + ')');
        Sql.Add('OR (A.' + C_TfDSatz_ReferenzNr + '>' + IntToStr(FReferenzBis) + '))');
      end;

      Sql.Add('ORDER BY ' + C_TfDSatz_ReferenzNr);
    end;
  end;

var
  pQuery                     : TQuery;
  i, j, iRef, iOrd, iDZ, iZZ : integer;
  iRefVon, iRefBis, iKNr, iKTyp : integer;
  dtv, dtb                   : TDateTime;
  sTbName    : string;
  pSlKNamen, pSlTNamen       : TStrings;
  pLKNr                      : TList;
begin
  // Kanaldaten und -tabellen festlegen
  pLKNr := TList.Create;  // Liste zum Aufnehmen der Kanalnummern
  pSlKNamen := TStringList.Create;  // Liste zum Aufnehmen der Kanalnamen
  pSlTNamen := TStringList.Create;  // Liste zum Aufnehmen der Tabellennamen
  try

    with TTable.Create(nil) do
    try
      DatabaseName := Self.DatabaseName;

      // Existiert die Satztabelle ?
      TableName := Format(C_TbDSatz + '%.4d', [FIndex]);
      if (not Exists) then Exit;

      // Listen der Kanalnummern und -namen erstellen
      for i := 0 to FArchiv.Count-1 do begin
        iKNr := FArchiv.GetKanalByIndex(i)^.KanalNr;

        // Falls Kanäle vorgegeben sind: Ist Kanal enthalten ?
        if (FKanaele <> []) and (not (FArchiv.Kanal[iKNr]^.KanalNr in FKanaele))
        then Continue;

        // Existiert die Wertetabelle ?
        if (FArchiv.Kanal[iKNr]^.Kanaltyp = kt_ST) then
          sTbName := C_TbDHex + Format('%.3d%.4d', [iKNr, FIndex])
        else if (FArchiv.Kanal[iKNr]^.Kanaltyp = kt_TX) then
          sTbName := C_TbDText + Format('%.3d%.4d', [iKNr, FIndex])
        else sTbName :=
          C_TbDWert + Format('%.3d%.4d', [iKNr, FIndex]);
        TableName := Format(C_TbDSatz + '%.4d', [FIndex]);
        if (Exists) then begin
          pLKNr.Add(Pointer(iKNr));
          pSlKNamen.Add(FArchiv.GetKanalByIndex(i)^.Name);
          pSlTNamen.Add(sTbName);
        end;
      end;
    finally
      Free;
    end;

    if (pLKNr.Count = 0) then Exit;


    pQuery := TQuery.Create(nil);
    with pQuery do
    try
      DatabaseName := Self.DatabaseName;
      CreateSql(FIndex, pQuery, pSlTNamen);
      Open;

      // Referenznummern feststellen
      iRefVon := FReferenzVon;
      iRefBis := FReferenzBis;
      if (not Eof) then begin
        FReferenzVon := FieldByName(C_TfDSatz_ReferenzNr).asInteger;
        Last;
        FReferenzBis := FieldByName(C_TfDSatz_ReferenzNr).asInteger;
        // Wenn die bestehenden Referenznummern nicht anschließen, dann löschen
        if ((iRefVon <= 0) or (iRefBis <= 0) or
            (not ((FReferenzVon = iRefBis+1) or (FReferenzBis = iRefVon-1))
            )
           ) then
        begin
          iRefVon := FReferenzVon;
          iRefBis := FReferenzBis;
          dtv := DatumVon;
          dtb := DatumBis;
          Clear;
          DatumVon := dtv;
          DatumBis := dtb;
          FReferenzVon := iRefVon;
          FReferenzBis := iRefBis;
        end;
      end;

      if (Count = 0) then begin
        // Spalten für Satztabelle
        iRef := AddCol(C_Text_Referenz, C_SpaltenTyp_ReferenzNr);  // 10.07.2003
        iDZ := AddCol(C_Text_DatumZeit, C_SpaltenTyp_DatumZeit);   // 10.07.2003
        iZZ := AddCol(C_Text_ZeitZone, C_SpaltenTyp_ZeitZone);     // 10.07.2003
        iOrd := AddCol(C_Text_OrdNr, C_SpaltenTyp_OrdnungsNr);     // 10.07.2003
        StatusListe.Clear;
        StatusListe.Add(TStringList.Create);  // SatzStatus

        // Spalten für Wertekanäle
        for i := 0 to pLKNr.Count-1 do begin
          if (FArchiv.Kanal[Integer(pLKNr[i])]^.Kanaltyp = kt_MW) then
            iKTyp := C_SpaltenTyp_Messwert
          else if (FArchiv.Kanal[Integer(pLKNr[i])]^.Kanaltyp = kt_ZS) then
            iKTyp := C_SpaltenTyp_Zaehlerstand
          else if (FArchiv.Kanal[Integer(pLKNr[i])]^.Kanaltyp = kt_ST) then
            iKTyp := C_SpaltenTyp_Status
          else if (FArchiv.Kanal[Integer(pLKNr[i])]^.Kanaltyp = kt_TX) then
            iKTyp := C_SpaltenTyp_Text
          else iKTyp := 0;
          AddCol(pSlKNamen[i], iKTyp);
          StatusListe.Add(TStringList.Create);  // Kanalstatus
        end;
      end
      else begin
        iRef := 0;
        iDZ := 1;
        iZZ := 2;
        iOrd := 3;
      end;

      // Daten füllen
      First;
      if (FReferenzVon < iRefVon) then i := 0 else i := -1;
      while (not Eof) do begin
        if (i < 0) then begin
          GetCol(iRef).AddString(FieldByName(C_TfDSatz_ReferenzNr).asString);
          GetCol(iDZ).AddString(FormatDateTime(C_FormatDateTime,
            FieldByName(C_TfDSatz_Datum).asDateTime +
            FieldByName(C_TfDSatz_Zeit).asDateTime));
          GetCol(iZZ).AddString(FieldByName(C_TfDSatz_Zeitzone).asString);
          GetCol(iOrd).AddString(FieldByName(C_TfDSatz_OrdNr).asString);
          StatusListe.GetCol(0).Add('');
          for j := iOrd+1 to Count-1 do begin
            with GetCol(j) do begin
              if (SpaltenTyp = C_SpaltenTyp_Zaehlerstand) then begin
                AddInteger(Trunc(FieldByName(  // 07.10.2003
                  'Wert' + Chr(Ord('B') + (j - (iOrd+1)))).asFloat));
              end
              else AddString(FieldByName(
                'Wert' + Chr(Ord('B') + (j - (iOrd+1)))).asString);
            end;
            StatusListe.GetCol(j-C_ColDiff_StatusWert).Add(
              GetStatusText(IntToStr(StrToIntDef(
              FieldByName('Status' + Chr(Ord('B') + (j - (iOrd+1)))).asString, -1))));
          end;
        end
        else begin
          GetCol(iRef).InsertString(i, FieldByName(C_TfDSatz_ReferenzNr).asString);
          GetCol(iDZ).InsertString(i, FormatDateTime(C_FormatDateTime,
            FieldByName(C_TfDSatz_Datum).asDateTime +
            FieldByName(C_TfDSatz_Zeit).asDateTime));
          GetCol(iZZ).InsertString(i, FieldByName(C_TfDSatz_Zeitzone).asString);
          GetCol(iOrd).InsertString(i, FieldByName(C_TfDSatz_OrdNr).asString);
          StatusListe.GetCol(0).Insert(i, '');
          for j := iOrd+1 to Count-1 do begin
            GetCol(j).InsertString(i, FieldByName(
              'Wert' + Chr(Ord('B') + (j - (iOrd+1)))).asString);
            StatusListe.GetCol(j-C_ColDiff_StatusWert).Insert(
              i, GetStatusText(IntToStr(StrToIntDef(
              FieldByName('Status' + Chr(Ord('B') + (j - (iOrd+1)))).asString, -1))));
          end;
          Inc(i);
          if (i >= iRefVon) then i := -1;
        end;
        Next;
      end;

      if (Active) then Close;
    finally
      Free;
    end;
  finally
    pLKNr.Free;
    pSlKNamen.Free;
    pSlTNamen.Free;
  end;
end;

{ Entfernt überzählige Daten, passt Parameter an       }
{------------------------------------------------------}
procedure TDSfGAbrufDaten.CreateStundenWerte;
{------------------------------------------------------}

  {--------------------------------------------------}
  function GetVolleStunde(dtDatumZeit: TDateTime; var fDiff: double): TDateTime;
  {--------------------------------------------------}
  var
     h, n, s, ms  : word;
  begin
    DecodeTime(dtDatumZeit, h, n, s, ms);
    if (n >= 58) then h := h + 1
    else if (n <= 2) then h := h
    else h := 99;

    if (h < 24) then Result := Trunc(dtDatumZeit) + EncodeTime(h, 0, 0, 0)
    else if (h = 24) then Result := Trunc(dtDatumZeit) + 1
    else Result := 0;

    fDiff := Abs(Result - dtDatumZeit);
  end;

var
  i, j           : integer;
  dt, dt1        : TDateTime;
  fW, fW1, fD1, fD2 : double;
  sCompare, sDt  : string;
  pSlDateTime    : TKanalListe;
begin
  if (StundenWerte) and (Count > 3) then begin

    pSlDateTime := GetCol(C_Index_DatumZeit);

    // Alle 'nicht vollen Stunden' eleminieren - Grobsortierung
    i := 0;
    sCompare := '';
    fD1 := 1;  // = 1 Tag
    while (i < pSlDateTime.Count) do begin
      dt := GetVolleStunde(pSlDateTime.AsDateTime[i], fD2);
      if (dt = 0) then begin
        for j := 0 to Count-1 do GetCol(j).Delete(i);  // Werte
        for j := 0 to StatusListe.Count-1 do
          if (StatusListe.GetCol(j).Count > i) then
            StatusListe.GetCol(j).Delete(i);  // Stati
      end
      else begin
        // Feinjustierung
        sDt := FormatDateTime(C_FormatDateTime, dt);
        if (sDt <> sCompare) then begin
          pSlDateTime.AsDateTime[i] := dt;
          Inc(i);
          sCompare := sDt;
        end
        else begin
          if (fD2 >= fD1) then begin
            // Abweichung größer => Wert löschen
            for j := 0 to Count-1 do GetCol(j).Delete(i);  // Werte
            for j := 0 to StatusListe.Count-1 do
              if (StatusListe.GetCol(j).Count > 0) then
                StatusListe.GetCol(j).Delete(i);  // Stati
          end
          else begin
            // Abweichung kleiner => vorherigen Wert löschen
            for j := 0 to Count-1 do GetCol(j).Delete(i-1);  // Werte
            for j := 0 to StatusListe.Count-1 do
              if (StatusListe.GetCol(j).Count > i-1) then
                StatusListe.GetCol(j).Delete(i-1);  // Stati
            pSlDateTime.AsDateTime[i-1] := dt;  // Neuer gültiger Wert
          end;
        end;
        fD1 := fD2;
      end;
    end;

    // Nicht genügend Werte vorhanden
    if (pSlDateTime.Count < 2) then begin
      Clear;
      Exit;
    end;

    // Ggf. fehlende Stunden eintragen
    dt1 := GetVolleStunde(pSlDateTime.AsDateTime[0], fD2);
    i := 1;
    while (i < pSlDateTime.Count) do begin
      dt := GetVolleStunde(pSlDateTime.AsDateTime[i], fD2);
      while (dt > dt1 + EncodeTime(1, 0, 1, 0)) do begin
        dt1 := GetVolleStunde(dt1 + EncodeTime(1, 0, 0, 0), fD2);
        for j := 0 to Count-1 do GetCol(j).InsertString(i, '');
        for j := 0 to StatusListe.Count-1 do
          StatusListe.GetCol(j).Insert(i, '');  // Stati
        pSlDateTime.AsDateTime[i] := dt1;
        Inc(i);
      end;
      dt1 := dt;
      Inc(i);
    end;

    // Differenzwerte ermitteln und eintragen
    for i := C_Index_OrdnungsNr+1 to Count-1 do begin
      if (GetCol(i).SpaltenTyp <> C_SpaltenTyp_Zaehlerstand) then Continue;
      fW1 := 0;
      for j := 0 to pSlDateTime.Count-1 do begin
        fW := GetCol(i).AsFloat[j];
        if (fW > 0) and (fW1 > 0)
        then GetCol(i).AsFloat[j] := (fW-fW1)
        else GetCol(i).Strings[j] := '';
        fW1 := fW;
      end;
    end;

    // 1. Zeile löschen (Ausgangswerte)
    for i := 0 to Count-1 do GetCol(i).Delete(0);
    for i := 0 to StatusListe.Count-1 do
      if (StatusListe.GetCol(i).Count > 0) then
        StatusListe.GetCol(i).Delete(0);  // Stati
  end;
end;

{ Entfernt überzählige Daten, passt Parameter an       }
{------------------------------------------------------}
procedure TDSfGAbrufDaten.AutoDelete;
{------------------------------------------------------}
var
  i, j   : integer;
begin
  if (Count > 0) and (GetCol(0).Count > MaxRecords) then begin
    while (GetCol(0).Count > MaxRecords) and
          (GetCol(IndexDatumZeit).AsDateTime[0] < DatumVon)
    do begin
      for i := 0 to Count-1 do GetCol(i).Delete(0);
      for i := 0 to StatusListe.Count-1 do
        if (StatusListe.GetCol(i).Count > 0) then
          StatusListe.GetCol(i).Delete(0);
    end;
    while (GetCol(0).Count > MaxRecords) and
          (GetCol(IndexDatumZeit).AsDateTime[GetCol(0).Count-1] > DatumBis)
    do begin
      j := GetCol(0).Count-1;
      for i := 0 to Count-1 do GetCol(i).Delete(j);
      for i := 0 to StatusListe.Count-1 do
        if (StatusListe.GetCol(i).Count > 0) then
          StatusListe.GetCol(i).Delete(j);
    end;
  end;

  if (Count > 0) and (GetCol(0).Count > 0) then begin
    FReferenzVon := GetCol(C_Index_Referenz).AsInteger[0];
    FReferenzBis := GetCol(C_Index_Referenz).AsInteger[GetCol(0).Count-1];
    DatumVon := GetCol(C_Index_DatumZeit).AsDateTime[0];
    DatumBis := GetCol(C_Index_DatumZeit).AsDateTime[GetCol(0).Count-1];
  end
  else begin
    FReferenzVon := -1;
    FReferenzBis := -1;
    DatumVon := 0;
    DatumBis := 0;
    OldVon := 0;
    OldBis := 0;
  end;
end;

{------------------------------------------------------}
procedure TDSfGAbrufDaten.SetStaName(sName: string);
{------------------------------------------------------}
var
  i : integer;
begin
  FStaName := sName;
  with ClientStammdaten.FillDInstanzListe(FStaName) do
    for i := 0 to Count-1 do
      if (InstanzId[i] = Self.GerId) then begin
        Self.FDSfGStand := InstanzStand[i];
        Break;
      end;
end;

{------------------------------------------------------}
function TDSfGAbrufDaten.GetStatusText(sStatus: string): string;
{------------------------------------------------------}

  function ExpandResult(
    pSlStatus: TStrings; sRes: string; iIndex: integer): string;
  begin
    Result := sRes;
    if (iIndex >= 0) and (pSlStatus.Count > iIndex) then begin
      if (sRes <> '') then Result := sRes + ' | ';
      Result := Result + pSlStatus[iIndex];
    end;
  end;

const
  szNull = '00000000000000000000000000000000';
var
  i: Integer;
  Status0 : string[32];
  szStatus: string [32];
  strstatus: string [9];
  lstatus: longint;
  code : integer;
  pSlStatus : TStrings;
begin
  Result := '';

  if (FDSfGStand = 2) then pSlStatus := FD10Status else pSlStatus := FDStatus;

  szStatus := szNull;
  Status0 := sStatus;

  if (FDSfGStand = 1) then begin  { Stand bis 1996 }
    strstatus := '$' + Copy(Status0, 1, 8);
    val (strstatus, lstatus, code);
    for i := 0 to 31 do begin
      if (lstatus and (LongInt(1) shl i)) <> 0 then begin
        szStatus[32 - i] := '1';
        Result := ExpandResult(pSlStatus, Result, i);
      end;
    end; {* end for *}
  end

  else if (FDSfGStand = 2) then begin   { Stand ab 1996 }
  { Darstellung des Status beim neuen Stand als Dezimal-Zahl }
    strstatus:= copy(Status0, 1, 8);
    i:= strToIntDef(strStatus, -1);
  { GeDa }
    if (FStatusZahl)
    then Result := IntToStr(i)
    else Result := ExpandResult(pSlStatus, Result, i);
  end

  else begin  { default }
    strstatus := '$'+copy(Status0, 1, 8);
    val (strstatus, lstatus, code);
    if (FStatusZahl) then Result := IntToStr(lstatus)
    else
      for i := 0 to 31 do begin
        if (lstatus and (LongInt(1) shl i)) <> 0 then begin
          szStatus[32 - i] := '1';
          Result := ExpandResult(pSlStatus, Result, i);
        end;
    end; {* end for *}
  end;

end;

{ Lädt Archivdaten abhängig von Referenz- oder Ordnr.  }
{ Parameter: Nr. von, bis; T=Referenz-, F=Ordnungsnr.  }
{------------------------------------------------------}
procedure TDSfGAbrufDaten.LoadDataByRefNr(iVon, iBis: integer; bRefNr: boolean);
{------------------------------------------------------}
var
  iv, ib : integer;
  iLSDT  : TLoadSatzDataType;
begin
  if (not Assigned(FArchiv)) then Exit;

  iv := iVon;
  ib := iBis;
  if (bRefNr) then iLSDT := lsdtRefNr else iLSDT := lsdtOrdNr;

  if (not StundenWerte) then begin
    OldVon := DatumVon;
    OldBis := DatumBis;
  end
  else begin
    OldVon := 0;
    OldBis := 0;
    FReferenzVon := -1;
    FReferenzBis := -1;
  end;

  LoadSatzAndArchivData(iLSDT, iv, ib);


  CreateStundenWerte;

  AutoDelete;
end;

end.

