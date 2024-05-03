{------------------------------------------------------------------------------}
{ Stammdatenzugriffs-Objekt für DSfG                                           }
{                                                                              }
{ 23.10.2000  GD  Neu                                                          }
{ 28.03.2001  GD  Um Einlesen von Stammdaten erweitert                         }
{ 05.06.2001  GD  Um Funktionen für Parametrierung der DSfG-DFÜ erweitert      }
{ 18.06.2001  GD  Um Funktionen für DSfG-Export erweitert                      }
{ 07.08.2001  GD  Um Aktualisieren erweitert                                   }
{ 05.09.2001  GD  Ersatzwertliste                                              }
{ 16.11.2001  GD  Stand                                                        }
{ 13.02.2002  GD  GetArchivData                                                }
{ 12.03.2002  GD  HasDatenelemente                                             }
{ 10.06.2002  GD  Änderung beim Stammdateneinlesen                             }
{ 14.06.2002  GD  DeaList wird nur noch bei Bedarf geladen                     }
{ 14.06.2002  GD  Gerätelisten (CrossReferenz) herausgenommen                  }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, 2002                                    }
{------------------------------------------------------------------------------}
unit DWCOMDb;

interface

uses
  SysUtils, Classes, DbTables, WSysCon, Controls, Forms, GD_Utils, StdCtrls,
  DKfKonv, DDbSta, DMomLists, DListen, Dialogs, DSG_Utils, PathIni,
  DbDSfGParams, DbDELCrossRef, FAuswahlDialog;

type
  TDbDSfGDaten = class(TObject)
    constructor Create(sDatabaseName: string);
    destructor Destroy; override;
  private
    FDatabaseName : string;
    FTbStation    : TTable;
    FTbInstanz    : TTable;
    FTbArchive    : TTable;
    FTbKanaele    : TTable;
    FTbITyp       : TTable;
    FTbDatenelemente : TDSfGParams;
    FDStationsListe: TDSfGStationenList;
    FDDEAListe    : TDSfGDEAList;
    procedure InitTables(aState: boolean);
    procedure FillDStationsListe;
    procedure FillDDEAListe;
    function GetDeaList: TDSfGDEAList;  // 14.06.2002
  public
    function GetInstTypName(sInstTyp: string): string;
    function GetDStationsName(iStatId: integer): string;
    function GetDStationsId(sStationsName: string): integer;
    function GetStationsLogPort(iStationsId: integer): integer;
    procedure AktualisiereStammdaten; // 07.08.2001
    function GetKanalData(          // 18.06.2001
      iInstanzId, iArchivNr, iKanalNr: integer): TAKanaeleData;
    function GetArchivData(iInstanzId, iArchivNr: integer): TAKanaeleDataList;  // 13.02.2002
    function KennungAlreadyExists(  // 05.06.2001
      Kennung: string; InstanzId_Excl: integer; var Exists: boolean): boolean;
    function GetRufstammdaten(iStationsId: integer): TRufStammDaten;
    function FillDInstanzListe(sStationsName: string): TDSfGStationList;
    procedure WriteStammdaten(iStaId, iJrnlId: integer; sTeiln, sLoginD: string;
      sAdr_nichtloeschen: string; pInstanzListe: TInstanzDataList;
      pInstDfuListe: TInstDfuDataList; pKonfigRohdataListe: TKonfigRohdataList);
      overload;
    procedure WriteStammdaten(pList: TStrings; iStaId: integer = -1;
      bOverride: boolean = False); overload;
    function WriteDatenelemente(sStaName: string; sInstAdr: char;
      pList: TStrings; bDelete: boolean = True): boolean;
    function HasDatenelemente(sStaName: string; cAdr: char = '0'): boolean;  // 12.03.2002
    function ReadDatenelemente(sStaName: string; sInstAdr: char;
      sDeaVon : string; sDeaBis: string = ''): TStrings;
    function GetStatWithParams: TStrings;
    function GetErsatzwert(sDea, sValue: string): string;
    function GetErsatzwertList: TStrings;  // 05.09.2001
    function DeleteStaId(iStationsId: integer): boolean;
    property DStationsName[index: integer]: string read GetDStationsName;
    property DStationsListe: TDSfGStationenList read FDStationsListe;
    property DatabaseName: string read FDatabaseName;
    property DEAList: TDSfGDEAList read GetDEAList;
  end;

implementation

{-------------------------------- TDbDSfGDaten --------------------------------}

{--------------------------------------}
constructor TDbDSfGDaten.Create(sDatabaseName: string);
{--------------------------------------}
begin
  inherited Create;

  FDatabaseName := sDatabaseName;
  InitTables(True);
  FDStationsListe := TDSfGStationenList.Create;
  FillDStationsListe;
end;

{--------------------------------------}
destructor TDbDSfGDaten.Destroy;
{--------------------------------------}
begin
  InitTables(False);
  if (Assigned(FDStationsListe)) then FDStationsListe.Free;
  if (Assigned(FDDEAListe)) then FDDEAListe.Free;

  inherited;
end;

{ Tabellen initialisiert/freigeben     }
{--------------------------------------}
procedure TDbDSfGDaten.InitTables(aState: boolean);
{--------------------------------------}

  procedure InitThisTable(var pTable: TTable; sTbName: string);
  begin
    if (not Assigned(pTable)) then with pTable do begin
      pTable := TTable.Create(nil);
      DatabaseName := FDatabaseName;
      TableName := sTbName;
      if (not Exists) then
        raise Exception.Create('Tabelle ''' + sTbName + ''' nicht vorhanden !');
    end;
  end;

begin
  if (aState) then begin
    InitThisTable(FTbStation, C_DTB_Station);
    InitThisTable(FTbInstanz, C_DTB_Instanz);
    InitThisTable(FTbArchive, C_DTB_Archive);
    InitThisTable(FTbKanaele, C_DTB_AKanaele);
    InitThisTable(FTbITyp, C_DTB_ITyp);
    FTbDatenelemente := TDSfGParams.Create(FDatabaseName);
  end
  else begin
    if (Assigned(FTbStation)) then FTbStation.Free;
    if (Assigned(FTbInstanz)) then FTbInstanz.Free;
    if (Assigned(FTbArchive)) then FTbArchive.Free;
    if (Assigned(FTbKanaele)) then FTbKanaele.Free;
    if (Assigned(FTbITyp)) then FTbITyp.Free;
    if (Assigned(FTbDatenelemente)) then FTbDatenelemente.Free;
    FTbStation := nil;
    FTbInstanz := nil;
    FTbArchive := nil;
    FTbKanaele := nil;
    FTbITyp := nil;
    FTbDatenelemente := nil;
  end;
end;

{ Liste mit Datenelementen füllen      }
{ Rückgabe: Liste mit DEA-Definitionen ]
{--------------------------------------}
function TDbDSfGDaten.GetDeaList: TDSfGDEAList;  // 14.06.2002
{--------------------------------------}
begin
  if (not Assigned(FDDeaListe)) then FillDDEAListe;
  Result := FDDEAListe;
end;

{ Liste mit Datenelementen füllen      }
{--------------------------------------}
procedure TDbDSfGDaten.FillDDEAListe;
{--------------------------------------}
var
  oldCursor : TCursor;
  sDEA, sBez, sZugriff, sTyp : string;
  iGrp : SmallInt;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try

    if (Assigned(FDDEAListe))
      then FDDEAListe.Clear
      else FDDEAListe := TDSfGDEAList.Create;

  // Datenelementliste füllen
    if (FDatabaseName <> '') then with TQuery.Create(nil), Sql do
    try
      DatabaseName := Self.DatabaseName;
      Add('SELECT * FROM ' + C_Tb_DelList);
      Open;

      FDDEAListe.BeginUpdate;
      try

        while not Eof do begin
          sDEA := FieldByName(C_Tf_DelList_DEL_Adresse).asString;
          sBez := FieldByName(C_Tf_DelList_Name).asString;
          sZugriff := FieldByName(C_Tf_DelList_Zugriff).asString;
          sTyp := FieldByName(C_Tf_DelList_Typ).asString;
          if (FieldByName(C_Tf_DelList_DELGrp).isNull)
            then iGrp := 0
            else iGrp := FieldByName(C_Tf_DelList_DELGrp).asInteger;
          FDDEAListe.AddDEA(sDEA, sBez, sZugriff, sTyp, iGrp);
          Next;
        end;

      finally
        FDDEAListe.EndUpdate;
      end;

      Close;
    finally
      Free;
    end;

  finally
    Screen.Cursor := oldCursor
  end;
end;

{ Liste mit Stationsnamen/-IDs füllen  }
{--------------------------------------}
procedure TDbDSfGDaten.FillDStationsListe;
{--------------------------------------}
var
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try

    if (Assigned(FDStationsListe)) then FDStationsListe.Clear else Exit;

  // Stationsliste füllen
    if (FDatabaseName <> '') then with TQuery.Create(nil), Sql do
    try
      DatabaseName := Self.DatabaseName;
      Add('SELECT ' + C_DTF_Station_StationId + ',' + C_DTF_Station_Stationsname);
      Add('FROM ' + C_DTB_Station);
      Open;
      while not Eof do begin
        FDStationsListe.AddStation(FieldByName(C_DTF_Station_Stationsname).asString,
          FieldByName(C_DTF_Station_StationId).asInteger);
        Next;
      end;
      Close;
    finally
      Free;
    end;

  finally
    Screen.Cursor := oldCursor
  end;
end;

{ Stammdaten aktualisieren             }
{--------------------------------------}
procedure TDbDSfGDaten.AktualisiereStammdaten; // 07.08.2001
{--------------------------------------}
begin
  FillDStationsListe;
end;

{ Liste mit Instanzadr./-typen füllen  }
{ Parameter: Stations-Id               }
{--------------------------------------}
function TDbDSfGDaten.FillDInstanzListe(sStationsName: string): TDSfGStationList;
{--------------------------------------}
var
  oldCursor     : TCursor;
  pStationsList : TDSfGStationList;
begin

  Result := nil;

  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try

    if (not Assigned(FDStationsListe)) or
       (FDStationsListe.IndexOf(sStationsName) < 0) then Exit;

    pStationsList := TDSfGStationList(
      FDStationsListe.Objects[FDStationsListe.IndexOf(sStationsName)]);

    // Daten werden nur dann geholt, wenn noch nicht vorhanden
    if (pStationsList.Count = 0) then
      with TQuery.Create(nil), Sql do
      try
        DatabaseName := Self.DatabaseName;
        Add('SELECT ' + C_DTF_Instanz_InstanzId + ',');
        Add(C_DTF_Instanz_Busadresse + ',' + C_DTF_Instanz_Instanztyp + ',');
        Add(C_DTF_Instanz_Hersteller + ',' + C_DTF_Instanz_Geraetetyp + ',');
        Add(C_DTF_Instanz_FabrikNr + ',' + C_DTF_Instanz_Instanzname + ',');
        Add(C_DTF_Instanz_Stand);  // 16.11.2001
        Add('FROM ' + C_DTB_Instanz);
        Add('WHERE ' + C_DTF_Instanz_StationId + '=' +
          IntToStr(FDStationsListe.GetStationsId(sStationsName)));
        Open;
        while not Eof do begin
          FDStationsListe.AddInstanz(sStationsName,
            FieldByName(C_DTF_Instanz_InstanzId).asInteger,
            FieldByName(C_DTF_Instanz_Busadresse).asString[1],
            FieldByName(C_DTF_Instanz_Instanztyp).asString[1],
            FieldByName(C_DTF_Instanz_Hersteller).asString,
            FieldByName(C_DTF_Instanz_Geraetetyp).asString,
            FieldByName(C_DTF_Instanz_FabrikNr).asString,
            FieldByName(C_DTF_Instanz_Instanzname).asString,
            FieldByName(C_DTF_Instanz_Stand).asInteger);
          Next;
        end;
        Close;
      finally
        Free;
      end;

    Result := pStationsList;

  finally
    Screen.Cursor := oldCursor;
  end;
end;

{ Stationsnamen zu einer ID            }
{ Parameter: Stations-ID               }
{ Rückgabe: Stationsname oder ''       }
{--------------------------------------}
function TDbDSfGDaten.GetDStationsName(iStatId: integer): string;
{--------------------------------------}
var
  i : integer;
begin
  Result := '';  // Default

  for i := 0 to FDStationsListe.Count-1 do
    if (GetDStationsId(FDStationsListe[i]) = iStatId) then begin
      Result := FDStationsListe[i];
      Exit;
    end;
end;

{ Stations-ID zu einem Namen           }
{ Parameter: Stationsname              }
{ Rückgabe: Stations-ID oder -1        }
{--------------------------------------}
function TDbDSfGDaten.GetDStationsId(sStationsName: string): integer;
{--------------------------------------}
begin
  Result := FDStationsListe.GetStationsId(sStationsName);
end;

{ Gibt LogPort einer DSfG-Station zur. }
{ Parameter: StationsId              }
{ Rückgabe: LogPort oder '-1'          }
{--------------------------------------}
function TDbDSfGDaten.GetStationsLogPort(iStationsId: integer): integer;
{--------------------------------------}
begin
  Result := -1;

  with TQuery.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    Sql.Add('SELECT B.' + C_DTF_InstDfu_LogPort);
    Sql.Add('FROM "' + C_DTB_Instanz + '" A, "' + C_DTB_InstDfu + '" B');
    Sql.Add('WHERE A.' + C_DTF_Instanz_StationId + ' = ' + IntToStr(iStationsId));
    Sql.Add('AND B.' + C_DTF_InstDfu_InstanzId + ' = A.' + C_DTF_Instanz_InstanzId);
    Open;
    if (not Eof) then Result := FieldByName(C_DTF_InstDfu_LogPort).asInteger;
    Close;
  finally
    Free;
  end;
end;

{ Gibt den Instanztyp-Namen einer Instanz zurück     }
{ Parameter: Instanztyp-NTY-Kennbuchstabe            }
{ Rückgabe: Instanztyp-Name                          }
{----------------------------------------------------}
function TDbDSfGDaten.GetInstTypName(sInstTyp: string): string;
{----------------------------------------------------}
var
  bOpened : boolean;
begin
  Result := ''; // default

  if (Length(sInstTyp) > 0) then begin
    bOpened := FTbITyp.Active;
    if (not bOpened) then FTbITyp.Open;

    if (FTbITyp.FindKey([sInstTyp[1]])) then
      Result := FTbITyp.FieldByName(C_DTF_ITyp_Bezeichnung).asString;

    if (not bOpened) then FTbITyp.Close;
  end;
end;

{ In Listen enthaltene Gerätekonfigurationen in      }
{ Stammdaten-Tabellen eintragen                      }
{ Parameter: StationsId; JournalId des Abrufs;       }
{            Teilnehmer; Login-DFÜ; nicht zu         }
{            löschende Adressen; Liste mit allg.     }
{            Instanzdaten; Liste mit DFÜ-Instanz-    }
{            daten; Liste mit Reg.-Antworttelegr.    }
{----------------------------------------------------}
procedure TDbDSfGDaten.WriteStammdaten(iStaId, iJrnlId: integer;
  sTeiln, sLoginD: string; sAdr_nichtloeschen: string;
  pInstanzListe: TInstanzDataList; pInstDfuListe: TInstDfuDataList;
  pKonfigRohdataListe: TKonfigRohdataList);
{----------------------------------------------------}
begin
  Konvertiere_Konfiguration(iStaId, iJrnlId, sTeiln, sLoginD,
    sAdr_nichtloeschen, pInstanzListe, pInstDfuListe, pKonfigRohdataListe);
end;

{ In Liste enthaltene Gerätekonfigurationen in       }
{ Stammdaten-Tabellen eintragen                      }
{ Parameter: Liste mit DSfG-Telegrammen;             }
{            Stations-ID (-1 = neu);                 }
{            Flag, ob alle bisherigen Instanzen ge-  }
{            löscht werden sollen                    }
{----------------------------------------------------}
procedure TDbDSfGDaten.WriteStammdaten(pList: TStrings; iStaId: integer = -1;
  bOverride: boolean = False);
{----------------------------------------------------}
var
  i, j, iJrnlId                          : integer;
  s, sTeiln, sLoginD, sAdr_nichtloeschen : string;
  pIList                                 : TInstanzDataList;
  pIDfuList                              : TInstDfuDataList;
  pKonfRohdataList                       : TKonfigRohdataList;
  pStaData                               : TStationData;
  pIData                                 : PInstanzData;
  pKonfRohdataListObj                    : TKonfigRohdataListObj;
begin
  if (pList.Count > 0) then begin

    iJrnlId := -1;  // kann ich nichts mit anfangen ...
    sLoginD := '';  // Gibt es hier nicht (jedenfalls jetzt !)
    pIDfuList := TInstDfuDataList.Create; // Bleibt leer;

    // Ggf. neuen Stammdatensatz anlegen
    if (iStaId < 0) then begin
      with TDSfGStammDaten.Create(PathServer[WStammDir]) do
      try
        InitTabellen;
        with pStaData do begin
          StationId:=-1;
          s := '';

          // Stationsnamen ermitteln
          with TFormAuswahlDlg.Create(nil) do
          try
            DropDownStyle := csDropDown;
            AuswahlCaption[1] := 'Stationsnamen eingeben';
            AuswahlCaption[2] := 'Stationsname:';
            AuswahlList := Self.FDStationsListe;
            Auswahl := '';
            if (ShowModal = mrOk) then s := Auswahl else Exit;
          finally
            Free;
          end;

          // Stationsrecord zum Eintragen erstellen und übergeben
          Stationsname:= s;
          Automatik:=true;
          ErsterAbruf:= Date;
          LoginInstanzname:= '';
          Zweitname:= s;
          Prioritaet:=10;
        end;
        i :=
          InsertStationDataByFieldname (C_DTF_Station_Stationsname, pStaData);
        if (i = -1) then Exit else iStaId := pStaData.StationId;
      finally
        Free;
      end;
    end;

    // Teilnehmerstrings füllen
    sTeiln := '';
    sAdr_nichtloeschen := '';
    for i := 0 to pList.Count-1 do
      if (Length(pList[i]) > 0) and (Pos(pList[i][1], sTeiln) = 0) then
        sTeiln := sTeiln + pList[i][1];
    if (not bOverride) then
      for i := Ord('A') to Ord('_') do
        if (Pos(Chr(i), sTeiln) = 0) then
          sAdr_nichtloeschen := sAdr_nichtloeschen + Chr(i);

    // Instanzobjekt-Liste füllen
    pIList := TInstanzDataList.Create;
    for i := 0 to pList.Count-1 do
      if (Length(pList[i]) > 1) and (pList[i][2] <> C_D_Instanztyp_Reg) then
      begin
        with GetDatenListe(Copy(pList[i], 3, Length(pList[i])-2)) do
        try
          New(pIData);
          FillChar(pIData^, SizeOf(pIData^), 0);
          pIData^.InstanzId := -1;
          pIData^.StationId := iStaId;
          pIData^.Busadresse := pList[i][1];
          for j := 0 to Count-1 do
            if (GetStringPart(Strings[j], 1) = C_DEA_InstTyp) then
              pIData^.Instanztyp := GetStringPart(Strings[j], 2)[1]
            else if (GetStringPart(Strings[j], 1) = C_DEA_Hersteller) then
              pIData^.Hersteller := GetStringPart(Strings[j], 2)
            else if (GetStringPart(Strings[j], 1) = C_DEA_GerTyp) then
              pIData^.Geraetetyp := GetStringPart(Strings[j], 2)
            else if (GetStringPart(Strings[j], 1) = C_DEA_FabrikNr) then
              pIData^.FabrikNr := GetStringPart(Strings[j], 2)
            else if (GetStringPart(Strings[j], 1) = C_DEA_Baujahr) then
              pIData^.Baujahr := StrToIntDef(GetStringPart(Strings[j], 2), 0)
            else if (GetStringPart(Strings[j], 1) = C_DEA_GerSwVs) then
              pIData^.SoftwareVersion := GetStringPart(Strings[j], 2)
            else if (GetStringPart(Strings[j], 1) = C_DEA_Inbetriebnahme) then
              pIData^.Inbetriebnahme :=
                UnixToDateTime(StrToIntDef(GetStringPart(Strings[j], 2), 0));
                
          // Instanznamen festlegen
          case pIData^.Instanztyp[1] of
            C_D_Instanztyp_DFU : pIData^.Instanzname := C_D_Instanzname_DFU;
            C_D_Instanztyp_Gas : pIData^.Instanzname := C_D_Instanzname_Gas;
            C_D_Instanztyp_Rev : pIData^.Instanzname := C_D_Instanzname_Rev;
            C_D_Instanztyp_Prot : pIData^.Instanzname := C_D_Instanzname_Prot;
            C_D_Instanztyp_Reg : pIData^.Instanzname := C_D_Instanzname_Reg;
            C_D_Instanztyp_Strg : pIData^.Instanzname := C_D_Instanzname_Strg;
            C_D_Instanztyp_Umw : pIData^.Instanzname := C_D_Instanzname_Umw;
            C_D_Instanztyp_Wieser : pIData^.Instanzname := C_D_Instanzname_Wieser;
            C_D_Instanztyp_unbest : pIData^.Instanzname := C_D_Instanzname_unbest;
            else  pIData^.Instanzname := C_D_Instanzname_unbest;
          end;
          pIData^.Instanzname :=                           // 10.06.2002
            pIData^.Instanzname + ' [' + pIData^.Busadresse + ']';

          // Gerätetypnummer festlegen
          pIData^.GerTypNr :=
            GetGerTypNr(pIData^.Hersteller, pIData^.Geraetetyp);
            
          pIList.Add(pIData);
        finally
          Free;
        end;
      end;

    // Rohdatenliste-Liste für Registrierinstanzen füllen
    pKonfRohdataList := TKonfigRohdataList.Create;
    for i := 0 to pList.Count-1 do
      if (Length(pList[i]) > 1) and (pList[i][2] = C_D_Instanztyp_Reg) then
      begin
        pKonfRohdataListObj := TKonfigRohdataListObj.Create;
        pKonfRohdataListObj.SetData(Copy(pList[i], 3, Length(pList[i])-2));
        pKonfRohdataList.AddObject(pList[i][1], pKonfRohdataListObj);
      end;

    Konvertiere_Konfiguration(iStaId, iJrnlId, sTeiln, sLoginD,
      sAdr_nichtloeschen, pIList, pIDfuList, pKonfRohdataList);
  end;
end;

{ Liste mit Datenelementen in Datenbank eintragen    }
{ Parameter: Stationsname, Instanzadresse, DE-Liste  }
{ Rüchgabe: Erfolg Ja/Nein                           }
{----------------------------------------------------}
function TDbDSfGDaten.WriteDatenelemente(sStaName: string; sInstAdr: char;
  pList: TStrings; bDelete: boolean = True): boolean;
{----------------------------------------------------}
var
  p : TDSfGStationList;
  i, iInstId : integer;
begin
  Result := False;  // Default

  // Liste der Station holen
  p := FillDInstanzListe(sStaName);
  if (not Assigned(p)) then Exit;

  // Instanz-ID holen
  iInstId := -1;
  for i := 0 to p.Count-1 do
    if (p.InstanzAdresse[i] = sInstAdr) then begin
      iInstId := p.InstanzId[i];
      Break;
    end;
  if (iInstId < 0) then Exit;

  FTbDatenelemente.WriteDatenelemente(iInstId, pList, bDelete);
end;

{ Sind Datenelemente für diese Station vorhanden ?   }
{ Parameter: Stationsname                            }
{ Rüchgabe: Datenelemente vorhanden ja/nein          }
{----------------------------------------------------}
function TDbDSfGDaten.HasDatenelemente(
  sStaName: string; cAdr: char = '0'): boolean;  // 12.03.2002
{----------------------------------------------------}
var
  p : TDSfGStationList;
  i : integer;
begin
  Result := False;  // Default

  // Liste der Stationen holen
  p := FillDInstanzListe(sStaName);
  if (Assigned(p)) then
    for i := 0 to p.Count-1 do
      if (cAdr = '0') or (p.InstanzAdresse[i] = cAdr) then
        if (FTbDatenelemente.HasDatenelemente(p.InstanzId[i])) then begin
          Result := True;
          Break;
        end;
end;

{ Liste mit Datenelementen aus Datenbank holen       }
{ Parameter: Stationsname, Instanzadresse, DEAs      }
{ Rüchgabe: Stringliste oder nil                     }
{----------------------------------------------------}
function TDbDSfGDaten.ReadDatenelemente(sStaName: string; sInstAdr: char;
  sDeaVon : string; sDeaBis: string = ''): TStrings;
{----------------------------------------------------}
var
  p : TDSfGStationList;
  i, iInstId : integer;
begin
  Result := nil;  // Default

  // Liste der Station holen
  p := FillDInstanzListe(sStaName);
  if (not Assigned(p)) then Exit;

  // Instanz-ID holen
  iInstId := -1;
  for i := 0 to p.Count-1 do
    if (p.InstanzAdresse[i] = sInstAdr) then begin
      iInstId := p.InstanzId[i];
      Break;
    end;
  if (iInstId < 0) then Exit;

  Result := FTbDatenelemente.ReadDatenelemente(iInstId, sDeaVon, sDeaBis);
end;

{ Liste mit Stationen mit gespeicherten Parametern   }
{ Rüchgabe: Liste mit Stationen                      }
{----------------------------------------------------}
function TDbDSfGDaten.GetStatWithParams: TStrings;
{----------------------------------------------------}
begin
  Result := FTbDatenelemente.GetStatWithParams;
end;

{ Gibt Ersatz-(Anzeige)Wert für DEA/Wert zurück      }
{ Parameter: DEA, Wert                               }
{ Rückgabe: Ersatzwert                               }
{----------------------------------------------------}
function TDbDSfGDaten.GetErsatzwert(sDea, sValue: string): string;
{----------------------------------------------------}
begin
  Result := FTbDatenelemente.GetErsatzwert(sDea, sValue);
end;

{ Gibt Ersatz-(Anzeige)Wertliste für DEA/Wert zurück }
{ Rückgabe: Ersatzwertliste                          }
{----------------------------------------------------}
function TDbDSfGDaten.GetErsatzwertList: TStrings;  // 05.09.2001
{----------------------------------------------------}
begin
  Result := FTbDatenelemente.GetErsatzwertList;
end;

{ Löschte alle Tabelleneinträge zu einer Station     }
{ Parameter Od der Station                           }
{ Rückgabe: Erfolg (nur Exceptions) Ja/Nein          }
{----------------------------------------------------}
function TDbDSfGDaten.DeleteStaId(iStationsId: integer): boolean;
{----------------------------------------------------}
begin
  Result := False;  // Default;

  try
    with TDSfGStammDaten.Create(PathServer[WStammDir]) do
    try
      if (InitTabellen) then begin
        // geändert 15.04.2003, WW: löschen der Daten jetzt in 'DeleteStationData' integriert
        DeleteStation(iStationsId);
        Result := True;
      end;
    finally
      Free;
    end;
  except
   // Result ist False;
  end;
end;

{ Rufstammdaten über StationId ermitteln             }
{ Parameter: StationsId                              }
{ Rückgabe: Rufstammdaten oder 0-Record              }
{----------------------------------------------------}
function TDbDSfGDaten.GetRufstammdaten(iStationsId: integer): TRufStammDaten;
{----------------------------------------------------}
begin
  FillChar(Result, SizeOf(Result), 0);

  try
    with TDSfGStammDaten.Create(PathServer[WStammDir]) do
    try
      if (InitTabellen) then GetRufStammDaten(iStationsId, Result);
    finally
      Free;
    end;
  except
   // Result ist nil;
  end;
end;

{ Übergebene Kennung in den Stammdaten vorhanden ?   }
{ Parameter: Kennung; InstanzId (alle/>0: ausgeschl.)}
{            Rückgabevariable                        }
{ Rückgabe: True, wenn Zugriff auf Tabelle ok        }
{----------------------------------------------------}
function TDbDSfGDaten.KennungAlreadyExists(
  Kennung: string; InstanzId_Excl: integer; var Exists: boolean): boolean;
{----------------------------------------------------}
begin
  Result := False;

  try
    with TDSfGStammDaten.Create(PathServer[WStammDir]) do
    try
      if (InitTabellen) then
        Result := KennungAlreadyExists(Kennung, InstanzId_Excl, Exists);
    finally
      Free;
    end;
  except
    // Result ist bereits False
  end;
end;

{ Kanaldaten eines Kanals ermitteln                  }
{ Parameter: InstanzId, Archivnr., Kanalnr.          }
{ Rückgabe: Record mit Kanaldaten oder 0-Array       }
{----------------------------------------------------}
function TDbDSfGDaten.GetKanalData(
  iInstanzId, iArchivNr, iKanalNr: integer): TAKanaeleData;
{----------------------------------------------------}
begin
  FillChar(Result, SizeOf(Result), 0);

  try
    with TDSfGStammDaten.Create(FDatabaseName) do
    try
      if (InitTabellen) then
        if (not GetAKanaeleData(iInstanzId, iArchivNr, iKanalNr, Result, True)) then
           FillChar(Result, SizeOf(Result), 0);
    finally
      Free;
    end;
  except
    // Result ist bereits 0-Array
  end;
end;

{ Kanalinfos für einen Kanal in Liste schreiben      }
{ Parameter: InstanzId, Archivnr.                    }
{ Rückgabe: Liste mit Kanaldaten oder nil            }
{----------------------------------------------------}
function TDbDSfGDaten.GetArchivData(
  iInstanzId, iArchivNr: integer): TAKanaeleDataList;
{----------------------------------------------------}
begin
  Result := nil;

  try
    Result := TAKanaeleDataList.Create;

    with TDSfGStammDaten.Create(FDatabaseName) do
    try
      if (InitTabellen) then begin
        if (not GetArchivKanalList(iInstanzId, iArchivNr, Result, True)) then
           FreeAndNil(Result);
      end;
    finally
      Free;
    end;
  except
    // Result ist bereits nil
  end;
end;

end.
