{------------------------------------------------------------------------------}
{ Bereitstellung von DSfGArchiv-Infos                                          }
{                                                                              }
{ 11.10.2001  GD  Neu                                                          }
{ 20.02.2002  GD  Überarbeitet                                                 }
{ 07.07.2002  GD  Sortierung geändert                                          }
{ 03.02.2003  GD  Erweitert um Tagesinformationen                              }
{ 17.02.2003  GD  Bugfix (1. Indexeintrag unterschlagen)                       }
{ 07.07.2003  GD  Archivweises aktualisieren                                   }
{ 26.02.2004  GD  Beschleunigtes "Erstsichten"                                 }
{ 08.06.2004  GD  Bugfix in SQL-Anweisung (2 x MIN)                            }
{ 12.01.2006  GD  Erweiterung der Kanalinformationen    	               }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2006                                    }
{------------------------------------------------------------------------------}
unit DSfGArchive;

interface

uses
  SysUtils, Classes, Db, DBTables, dialogs,
  WSysCon, GD_Utils, DMomLists, DListen, TbDSfGAr, DatenInfo;

type
  TIndexList = class(TStringList)
    destructor Destroy; override;
  private
    function GetInstanzId(iIndex: integer): integer;
    function GetArchivNr(iIndex: integer): integer;
  protected
  public
  public
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    procedure AddIndex(iIndex, iInstId, iArchNr: integer);
    property InstanzId [iIndex: integer]: integer read GetInstanzId;
    property ArchivNr [iIndex: integer]: integer read GetArchivNr;
  end;

  TArchivList = class(TStringList)
    constructor Create(iIndex, iArchivNr: integer); virtual;
    destructor Destroy; override;
  private
    FIndex       : integer;
    FArchivNr    : integer;
    FDatenVon    : TDateTime;
    FDatenBis    : TDateTime;
    FRefVon      : integer;    // 20.02.2002
    FRefBis      : integer;    // 20.02.2002
    FDatenInfoList : TDatenInfoList;  // 03.02.2003
    FKanalSet    : set of byte;
    function GetHasDaten: boolean;
    function GetKanal(iKanalNr: integer): PAKanaeleData;
  protected
  public
    function GetKanalByIndex(iIndex: integer): PAKanaeleData;
    procedure AddKanal(iKanalNr: integer; sName, sKanaltyp, sWerteart: string;
      cEAdr: char; sQuellDEL: string; iKommastellen: byte;
      fAnzeigeMin, fAnzeigeMax: double);
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    property MyIndex: integer read FIndex;
    property ArchivNr: integer read FArchivNr;
    property HasDaten: boolean read GetHasDaten;
    property DatenVon: TDateTime read FDatenVon write FDatenVon;
    property DatenBis: TDateTime read FDatenBis write FDatenBis;
    property RefVon: integer read FRefVon write FRefVon;
    property RefBis: integer read FRefBis write FRefBis;
    property Kanal [iKanalNr: integer]: PAKanaeleData read GetKanal;
    property DatenInfoList: TDatenInfoList read FDatenInfoList;
  end;

  TInstIdList = class(TStringList)
    constructor Create(iInstId: integer); virtual;
    destructor Destroy; override;
  private
    FInstanzId   : integer;
    FDatenVon    : TDateTime;
    FDatenBis    : TDateTime;
    function GetHasDaten: boolean;
    function GetArchiv(iArchivNr: integer): TArchivList;
  protected
  public
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    procedure AddArchiv(iIndex, iArchivNr: integer);
    function GetArchivByIndex(iIndex: integer): TArchivList;
    property InstanzId: integer read FInstanzId;
    property Archiv [iArchivNr: integer]: TArchivList read GetArchiv;
    property DatenVon: TDateTime read FDatenVon write FDatenVon;
    property DatenBis: TDateTime read FDatenBis write FDatenBis;
    property HasDaten: boolean read GetHasDaten;
  end;

  TDSfGArchivInfo = class(TStringList)
    constructor Create(sDatabaseName: string; iTagesEnde: byte = 0); virtual;
    destructor Destroy; override;
  private
    FDatabaseName : string;
    FStaDatabase  : string;
    FTagesEnde    : byte;
    procedure ReadSatzTables(pList: TIndexList; pSlTables: TStrings);
    procedure ReadKanalTable(pSlTables: TStrings);
    procedure ReadIndexTable;
    procedure AddInstanz(iInstId: integer);
    function GetInstanz(iInstanzId: integer): TInstIdList;
  protected
  public
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    procedure Aktualisieren;
    procedure AktualisiereArchiv(iInstId, iAgNr: integer);  // 07.07.2003
    function GetIndex(const iInstId, iArchNr: integer): integer;
    function GetArchiv(iInstanzId, iArchivNr: integer): TArchivList;
    procedure LoadArchivZeitbereich(const iInstId, iArchNr: integer);
    procedure LoadDataInfoList(const iInstId, iArchNr: integer);
    function GetArchivDatenVon(const iInstId, iArchNr: integer): TDateTime;
    function GetArchivDatenBis(const iInstId, iArchNr: integer): TDateTime;
    function GetInstanzDatenVon(const iInstId: integer): TDateTime;
    function GetInstanzDatenBis(const iInstId: integer): TDateTime;
    function GetInstanzByIndex(iIndex: integer): TInstIdList;
    property StammDatabase: string write FStaDatabase;
    property Instanz [iInstanzId: integer]: TInstIdList read GetInstanz;
  end;

implementation

type
  TStr20 = string[20];
  PStr20 = ^TStr20;

{------------------------------------------------------}
function GetTableList(sDatabaseName: string; sMask: string = ''): TStringList;
{------------------------------------------------------}
var
  i : integer;
begin
  Result := TStringList.Create;

  Session.GetTableNames(sDatabaseName, sMask, False, False, Result);
  for i := 0 to Result.Count-1 do  // ggf. User entfernen
    if (ExtractFileExt(Result[i]) <> '')
    then Result[i] := Copy(ExtractFileExt(Result[i]), 2, 20)
    else Break;
end;


{---------------------------------- TIndexList --------------------------------}

{------------------------------------------------------}
destructor TIndexList.Destroy;
{------------------------------------------------------}
begin
  Clear;

  inherited;
end;

{------------------------------------------------------}
procedure TIndexList.Delete(iIndex: integer);
{------------------------------------------------------}
begin
  if (Assigned(Objects[iIndex])) then Dispose(PStr20(Objects[iIndex]));

  inherited;
end;

{------------------------------------------------------}
procedure TIndexList.Clear;
{------------------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do
    if (Assigned(Objects[i])) then Dispose(PStr20(Objects[i]));

  inherited;
end;

{ Fügt neuen Index hinzu                               }
{ Parameter: Index aus INDEX.DB, InstanzId, Archivnr.  }
{------------------------------------------------------}
procedure TIndexList.AddIndex(iIndex, iInstId, iArchNr: integer);
{------------------------------------------------------}
var
  p : PStr20;
begin
  if (IndexOf(IntToStr(iIndex)) < 0) then begin
    New(p);
    p^ := IntToStr(iInstId) + Chr(us) + IntToStr(iArchNr);
    AddObject(IntToStr(iIndex), TObject(p));
  end;
end;

{ Gibt InstanzId zu einem Index zurück                 }
{ Parameter: Index aus INDEX.DB                        }
{ Rückgabe: InstanzId                                  }
{------------------------------------------------------}
function TIndexList.GetInstanzId(iIndex: integer): integer;
{------------------------------------------------------}
var
  i : integer;
begin
  i := IndexOf(IntToStr(iIndex));
  if (i >= 0)
  then Result := StrToIntDef(GetStringPart(PStr20(Objects[i])^, 1), -1)
  else Result := -1;
end;

{ Gibt Archivnr zu einem Index zurück                  }
{ Parameter: Index aus INDEX.DB                        }
{ Rückgabe: Archivnr                                   }
{------------------------------------------------------}
function TIndexList.GetArchivNr(iIndex: integer): integer;
{------------------------------------------------------}
var
  i : integer;
begin
  i := IndexOf(IntToStr(iIndex));
  if (i >= 0)
  then Result := StrToIntDef(GetStringPart(PStr20(Objects[i])^, 2), -1)
  else Result := -1;
end;

{---------------------------------- TArchivList -------------------------------}

{------------------------------------------------------}
constructor TArchivList.Create(iIndex, iArchivNr: integer);
{------------------------------------------------------}
begin
  inherited Create;

  FIndex := iIndex;
  FArchivNr := iArchivNr;
  FDatenVon := 0;
  FDatenbis := 0;
  FKanalSet := [];
  FDatenInfoList := TDatenInfoList.Create('');
end;

{------------------------------------------------------}
destructor TArchivList.Destroy;
{------------------------------------------------------}
begin
  Clear;
  FDatenInfoList.Free;

  inherited;
end;

{------------------------------------------------------}
procedure TArchivList.Delete(iIndex: integer);
{------------------------------------------------------}
begin
  if (Assigned(Objects[iIndex])) then Dispose(PAKanaeleData(Objects[iIndex]));

  inherited;
end;

{------------------------------------------------------}
procedure TArchivList.Clear;
{------------------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do
    if (Assigned(Objects[i])) then Dispose(PAKanaeleData(Objects[i]));

  inherited;
end;

{ Fügt neuen Kanal hinzu                               }
{ Parameter: Kanalnummer, Kanalname, Kanaltyp, Werte-  }
{            art, Quelladresse, Quelldatenelementadr.  }
{------------------------------------------------------}
procedure TArchivList.AddKanal(iKanalNr: integer;
  sName, sKanaltyp, sWerteart: string; cEAdr: char; sQuellDEL: string;
  iKommastellen: byte; fAnzeigeMin, fAnzeigeMax: double);
{------------------------------------------------------}
var
  p : PAKanaeleData;
begin
  if (not (iKanalNr in FKanalSet)) then begin
    New(p);
    p^.KanalNr := iKanalNr;
    p^.ArchivNr := ArchivNr;
    p^.Name := sName;
    p^.Kanaltyp := sKanaltyp;
    p^.Werteart := sWerteart;
    p^.EAdr := cEAdr;
    p^.QuellDEL := sQuellDEL;
    p^.Kommastellen := iKommastellen;
    p^.AnzeigeMin := fAnzeigeMin;
    p^.AnzeigeMax := fAnzeigeMax;
    AddObject(IntToStr(iKanalNr), TObject(p));
  end;
end;

{ Gibt Kanaldaten zurück                               }
{ Parameter: Kanalnummer                               }
{ Rückgabe: Zeiger auf Kanaldaten oder nil             }
{------------------------------------------------------}
function TArchivList.GetKanal(iKanalNr: integer): PAKanaeleData;
{------------------------------------------------------}
var
  i : integer;
begin
  Result := nil;  // Default

  i := IndexOf(IntToStr(iKanalNr));
  if (i >= 0) and (Assigned(Objects[i])) then
    Result := PAKanaeleData(Objects[i]);
end;

{ Gibt Kanaldaten zurück                               }
{ Parameter: Index in der Liste                        }
{ Rückgabe: Zeiger auf Kanaldaten oder nil             }
{------------------------------------------------------}
function TArchivList.GetKanalByIndex(iIndex: integer): PAKanaeleData;
{------------------------------------------------------}
begin
  Result := nil;  // Default

  if (Assigned(Objects[iIndex])) then
    Result := PAKanaeleData(Objects[iIndex]);
end;

{------------------------------------------------------}
function TArchivList.GetHasDaten: boolean;
{------------------------------------------------------}
begin
  Result := (FDatenVon > 1) and (FDatenBis > 1);
end;


{---------------------------------- TInstIdList -------------------------------}

{------------------------------------------------------}
constructor TInstIdList.Create(iInstId: integer);
{------------------------------------------------------}
begin
  inherited Create;

  FInstanzId := iInstId;
  FDatenVon := 0;
  FDatenbis := 0;
end;

{------------------------------------------------------}
destructor TInstIdList.Destroy;
{------------------------------------------------------}
begin
  Clear;

  inherited;
end;

{------------------------------------------------------}
procedure TInstIdList.Delete(iIndex: integer);
{------------------------------------------------------}
begin
  if (Assigned(Objects[iIndex])) then Objects[iIndex].Free;

  inherited;
end;

{------------------------------------------------------}
procedure TInstIdList.Clear;
{------------------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do if (Assigned(Objects[i])) then Objects[i].Free;

  inherited;
end;

{------------------------------------------------------}
procedure TInstIdList.AddArchiv(iIndex, iArchivNr: integer);
{------------------------------------------------------}
var
  pArchiv : TArchivList;
begin
  if (IndexOf(IntToStr(iArchivNr)) < 0) then begin
    pArchiv := TArchivList.Create(iIndex, iArchivNr);
    AddObject(IntToStr(iArchivNr), pArchiv);
  end;
end;

{------------------------------------------------------}
function TInstIdList.GetArchiv(iArchivNr: integer): TArchivList;
{------------------------------------------------------}
var
  i : integer;
begin
  i := IndexOf(IntToStr(iArchivNr));
  if (i >= 0) then Result := TArchivList(Objects[i]) else Result := nil;
end;

{------------------------------------------------------}
function TInstIdList.GetArchivByIndex(iIndex: integer): TArchivList;
{------------------------------------------------------}
begin
  Result := TArchivList(Objects[iIndex]);
end;
{------------------------------------------------------}
function TInstIdList.GetHasDaten: boolean;
{------------------------------------------------------}
begin
  Result := (FDatenVon > 1) and (FDatenBis > 1);
end;


{------------------------------- TDSfGSArchivInfo -----------------------------}

{------------------------------------------------------}
constructor TDSfGArchivInfo.Create(sDatabaseName: string; iTagesEnde: byte = 0);
{------------------------------------------------------}
begin
  inherited Create;

  FDatabaseName := sDatabaseName;
  FStaDatabase := '';
  FTagesEnde := iTagesEnde;
end;

{------------------------------------------------------}
destructor TDSfGArchivInfo.Destroy;
{------------------------------------------------------}
begin
  Clear;

  inherited;
end;

{------------------------------------------------------}
procedure TDSfGArchivInfo.Delete(iIndex: integer);
{------------------------------------------------------}
begin
  if (Assigned(Objects[iIndex])) then Objects[iIndex].Free;

  inherited;
end;

{------------------------------------------------------}
procedure TDSfGArchivInfo.Clear;
{------------------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do if (Assigned(Objects[i])) then Objects[i].Free;

  inherited;
end;

{ Fügt neue Instanz in die Liste ein                   }
{ Parameter: Instanzid                                 }
{------------------------------------------------------}
procedure TDSfGArchivInfo.AddInstanz(iInstId: integer);
{------------------------------------------------------}
var
  pInstanz : TInstIdList;
begin
  if (IndexOf(IntToStr(iInstId)) < 0) then begin
    pInstanz := TInstIdList.Create(iInstId);
    AddObject(IntToStr(iInstId), pInstanz);
  end;
end;

{------------------------------------------------------}
function TDSfGArchivInfo.GetInstanz(iInstanzId: integer): TInstIdList;
{------------------------------------------------------}
var
  i : integer;
begin
  i := IndexOf(IntToStr(iInstanzId));
  if (i >= 0) then Result := TInstIdList(Objects[i]) else Result := nil;
end;

{------------------------------------------------------}
function TDSfGArchivInfo.GetArchiv(
  iInstanzId, iArchivNr: integer): TArchivList;
{------------------------------------------------------}
var
  pInstList : TInstIdList;
begin
  Result := nil;  // Default

  pInstList := Instanz[iInstanzId];
  if (Assigned(pInstList)) then Result := pInstList.Archiv[iArchivNr];
end;

{------------------------------------------------------}
function TDSfGArchivInfo.GetInstanzByIndex(iIndex: integer): TInstIdList;
{------------------------------------------------------}
begin
  Result := TInstIdList(Objects[iIndex]);
end;

{------------------------------------------------------}
function TDSfGArchivInfo.GetIndex(const iInstId, iArchNr: integer): integer;
{------------------------------------------------------}
var
  pInst : TInstIdList;
  pArch : TArchivList;
begin
  Result := -1;  // Default

  pInst := Instanz[iInstId];
  if (not Assigned(pInst)) then Exit;

  pArch := pInst.Archiv[iArchNr];
  if (not Assigned(pArch)) then Exit;

  Result := pArch.MyIndex;
end;

{------------------------------------------------------}
function TDSfGArchivInfo.GetArchivDatenVon(
  const iInstId, iArchNr: integer): TDateTime;
{------------------------------------------------------}
var
  pInst : TInstIdList;
  pArch : TArchivList;
begin
  Result := 0;  // Default

  pInst := Instanz[iInstId];
  if (not Assigned(pInst)) then Exit;

  pArch := pInst.Archiv[iArchNr];
  if (not Assigned(pArch)) then Exit;

  if (pArch.DatenVon = 1) then begin
    LoadArchivZeitbereich(iInstId, iArchNr);
    if (pInst.DatenVon <= 1) or (pInst.DatenVon > pArch.DatenVon) then
      pInst.DatenVon := pArch.DatenVon;
    if (pInst.DatenBis < pArch.DatenBis) then pInst.DatenBis := pArch.DatenBis;
  end;

  Result := pArch.DatenVon;
end;

{------------------------------------------------------}
function TDSfGArchivInfo.GetArchivDatenBis(
  const iInstId, iArchNr: integer): TDateTime;
{------------------------------------------------------}
var
  pInst : TInstIdList;
  pArch : TArchivList;
begin
  Result := 0;  // Default

  pInst := Instanz[iInstId];
  if (not Assigned(pInst)) then Exit;

  pArch := pInst.Archiv[iArchNr];
  if (not Assigned(pArch)) then Exit;

  if (pArch.DatenBis = 1) then begin
    LoadArchivZeitbereich(iInstId, iArchNr);
    if (pInst.DatenVon <= 1) or (pInst.DatenVon > pArch.DatenVon) then
      pInst.DatenVon := pArch.DatenVon;
    if (pInst.DatenBis < pArch.DatenBis) then pInst.DatenBis := pArch.DatenBis;
  end;

  Result := pArch.DatenBis;
end;

{------------------------------------------------------}
function TDSfGArchivInfo.GetInstanzDatenVon(const iInstId: integer): TDateTime;
{------------------------------------------------------}
var
  pInst : TInstIdList;
begin
  Result := 0;  // Default

  pInst := Instanz[iInstId];
  if (not Assigned(pInst)) then Exit;

  Result := pInst.DatenVon;
end;

{------------------------------------------------------}
function TDSfGArchivInfo.GetInstanzDatenBis(const iInstId: integer): TDateTime;
{------------------------------------------------------}
var
  pInst : TInstIdList;
begin
  Result := 0;  // Default

  pInst := Instanz[iInstId];
  if (not Assigned(pInst)) then Exit;

  Result := pInst.DatenBis;
end;

{ Liest die Archivdaten in die Liste ein               }
{------------------------------------------------------}
procedure TDSfGArchivInfo.Aktualisieren;
{------------------------------------------------------}
begin
  Clear;
  ReadIndexTable;
end;

{ Aktualisiert Archivdaten einer Instanz               }
{ Parameter: Instanz-ID, Archivgruppennummer           }
{------------------------------------------------------}
procedure TDSfGArchivInfo.AktualisiereArchiv(iInstId, iAgNr: integer);
{------------------------------------------------------}
var
  pArchiv  : TArchivList;
begin
  pArchiv := GetArchiv(iInstId, iAgNr);
  if (not Assigned(pArchiv)) then begin
    Aktualisieren;
    pArchiv := GetArchiv(iInstId, iAgNr);
    if (Assigned(pArchiv)) then LoadArchivZeitbereich(iInstId, iAgNr);
  end
  else LoadArchivZeitbereich(iInstId, iAgNr);
end;

{ Liest die Indextabelle in die Liste ein              }
{------------------------------------------------------}
procedure TDSfGArchivInfo.ReadIndexTable;
{------------------------------------------------------}
var
  pIndexList : TIndexList;
  pSlTables  : TStrings;
begin
  with TTable.Create(nil) do
  try
     DatabaseName := FDatabaseName;
     TableName := C_DTF_Index_Index;
     if (not Exists) then Exit;
  finally
    Free;
  end;

  with TQuery.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    Sql.Add('SELECT A."' + C_DTF_Index_Index + '",' + C_DTF_Index_Instanz + ',');
    Sql.Add(C_DTF_Index_ArchivGruppenNr);
    Sql.Add('FROM "' + C_DTB_Index + '" A');
    Sql.Add('ORDER BY ' + C_DTF_Index_Instanz);
    Open;

    // Indices der Satztabellen mit zugehörigen Wertetabellen herausfinden
    pSlTables := GetTableList(FDatabaseName);
    pIndexList := TIndexList.Create;
    try
      while (not Eof) do begin
        pIndexList.AddIndex(FieldByName(C_DTF_Index_Index).asInteger,
          FieldByName(C_DTF_Index_Instanz).asInteger,
          FieldByName(C_DTF_Index_ArchivGruppenNr).asInteger);
        Next;
      end;
      ReadSatzTables(pIndexList, pSlTables);
      if (FStaDatabase <> '') then ReadKanalTable(pSlTables);
    finally
      pIndexList.Free;
    end;
    Close;
  finally
    Free;
  end;
end;

{ Liest die Satztabelle in ein                         }
{ Parameter: Indexliste (Verknüpfung Indizes-Instanzen)}
{------------------------------------------------------}
procedure TDSfGArchivInfo.ReadSatzTables(pList: TIndexList; pSlTables: TStrings);
{------------------------------------------------------}
var
  i, k, iIndex, iIId, iANr : integer;
  s : string;
  pInstanz : TInstIdList;
  pArchiv  : TArchivList;
  pSlIndices : TStrings;
begin
  pSlIndices := TStringList.Create;
  try

    // Liste mit Indexnummern erstellen
    pSlIndices.Assign(pSlTables);
    // Index herausfiltern und sortieren
    for i := pSlIndices.Count-1 downto 0 do
      pSlIndices[i] := Copy(pSlIndices[i], 5, 4);
    TStringList(pSlIndices).Sort;
    // Doppelte Indizes herausnehmen
    s := '0000';
    k := 1;   // Zähler für Index (muss min. 2x da sein (1x Satz, 1x Wert))
    for i := pSlIndices.Count-1 downto 0 do
      if (pSlIndices[i] = s) then pSlIndices.Delete(i)
      else begin
        if (k = 0) then begin
          pSlIndices.Delete(i);
          Inc(k);
        end
        else begin
          s := pSlIndices[i];
          k := 0;
        end;
      end;

    for i := 0 to pList.Count-1 do begin
      iIndex := StrToIntDef(pList[i], 0);
      iIId := pList.InstanzId[iIndex];
      iANr := pList.ArchivNr[iIndex];

      if (pSlIndices.IndexOf(Format('%.4d', [iIndex])) >= 0) then begin  // 17.02.2003
        AddInstanz(iIId);
        pInstanz := Instanz[iIId];
        if (not Assigned(pInstanz)) then Continue;
        pInstanz.DatenVon := 1;
        pInstanz.DatenBis := 1;

        pInstanz.AddArchiv(iIndex, iANr);
        pArchiv := pInstanz.Archiv[iANr];
        if (not Assigned(pArchiv)) then Continue;
        pArchiv.DatenVon := 1;
        pArchiv.DatenBis := 1;
      end;
    end;
  finally
    pSlIndices.Free;
  end;
end;

{ Lädt den Zeitbereich für ein Archiv                  }
{ Parameter: InstanzId, Archivnummer                   }
{------------------------------------------------------}
procedure TDSfGArchivInfo.LoadArchivZeitbereich(const iInstId, iArchNr: integer);
{------------------------------------------------------}
var
  pInstanz     : TInstIdList;
  pArchiv      : TArchivList;
  dtVon, dtBis : TDateTime;
  i            : integer;
  sTbName      : string;
begin
  pInstanz := Instanz[iInstId];
  if (not Assigned(pInstanz)) then Exit;

  pArchiv := pInstanz.Archiv[iArchNr];
  if (not Assigned(pArchiv)) then Exit;

  with TTable.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    TableName :=
      C_TbDSatz + Format('%.4d', [GetIndex(iInstId, iArchNr)]);
    if (not Exists) then begin
      sTbName := '';
      pArchiv.DatenVon := 0;
      pArchiv.DatenBis := 0;
      pInstanz.DatenVon := 0;
      pInstanz.DatenBis := 0;
      for i := 0 to pInstanz.Count-1 do begin
        dtVon := pInstanz.GetArchivByIndex(i).DatenVon;
        dtBis := pInstanz.GetArchivByIndex(i).DatenVon;
        if (pInstanz.DatenVon = 0) or
           ((dtVon > 0) and (dtVon < pInstanz.DatenVon))
        then pInstanz.DatenVon := dtVon;
        if (pInstanz.DatenBis = 0) or
           ((dtBis > 0) and (dtBis > pInstanz.DatenBis))
        then pInstanz.DatenBis := dtBis;
      end;
      Exit;
    end
    else sTbName := TableName;
  finally
    Free;
  end;

  if (sTbName <> '') then
    with TQuery.Create(nil) do
    try
      DatabaseName := FDatabaseName;
      Sql.Add('SELECT * FROM ' + sTbName);
      Sql.Add('ORDER BY ' + C_TfDSatz_Datum + ', ' + C_TfDSatz_Zeit);
      Open;
      if (not Eof) then begin
        dtVon := FieldByName(C_TfDSatz_Datum).asDateTime +
          FieldByName(C_TfDSatz_Zeit).asDateTime;
        Last;
        dtBis := FieldByName(C_TfDSatz_Datum).asDateTime +
          FieldByName(C_TfDSatz_Zeit).asDateTime;

        pArchiv.DatenVon := dtVon;
        pArchiv.DatenBis := dtBis;

        if (pInstanz.DatenVon = 0) or
           ((dtVon > 0) and (dtVon < pInstanz.DatenVon))
        then pInstanz.DatenVon := dtVon;
        if (pInstanz.DatenBis = 0) or
           ((dtBis > 0) and (dtBis > pInstanz.DatenBis))
        then pInstanz.DatenBis := dtBis;
      end;
      Close;

      Sql.Clear;
      Sql.Add('SELECT MIN(' + C_TfDSatz_ReferenzNr + '),');
      Sql.Add('MAX(' + C_TfDSatz_ReferenzNr + ')');  // 08.06.2004
      Sql.Add('FROM ' + sTbName);
      Open;
      if (not Eof) then begin
        pArchiv.RefVon := Fields[0].asInteger;
        pArchiv.RefBis := Fields[1].asInteger;
      end;
      Close;
    finally
      Free;
    end;
end;

{ Lädt die Dateninfoliste für ein Archiv               }
{ Parameter: InstanzId, Archivnummer                   }
{------------------------------------------------------}
procedure TDSfGArchivInfo.LoadDataInfoList(const iInstId, iArchNr: integer);
{------------------------------------------------------}
var
  pInstanz      : TInstIdList;
  pArchiv       : TArchivList;
  dt            : TDateTime;
  iAktTag, iRef : integer;
  fTEnde        : double;
begin
  pInstanz := Instanz[iInstId];
  if (not Assigned(pInstanz)) then Exit;

  pArchiv := pInstanz.Archiv[iArchNr];
  if (not Assigned(pArchiv)) then Exit;

  if (pArchiv.DatenVon > 0) and (pArchiv.DatenBis > 0) then
    with TQuery.Create(nil) do
    try
      DatabaseName := FDatabaseName;
      Sql.Add('SELECT * FROM ' + C_TbDSatz +
        Format('%.4d', [GetIndex(iInstId, iArchNr)]));
      Sql.Add('ORDER BY ' + C_TfDSatz_Datum + ', ' + C_TfDSatz_Zeit);
      Open;
      if (not Eof) then begin
        dt := Trunc(FieldByName(C_TfDSatz_Datum).asDateTime) +
          Frac(FieldByName(C_TfDSatz_Zeit).asDateTime);
        iRef := FieldByName(C_TfDSatz_ReferenzNr).asInteger;
        fTEnde := FTagesEnde/24; // Tagesende-Hilfsvariable
        iAktTag := Trunc(dt - fTEnde);

        pArchiv.DatenInfoList.InsertRecord(iAktTag, iRef, False); //26.02.2004
        while (not Eof) do begin
          dt := Trunc(FieldByName(C_TfDSatz_Datum).asDateTime) +
            Frac(FieldByName(C_TfDSatz_Zeit).asDateTime);
          if (Trunc(dt - fTEnde) <> iAktTag) then begin
            pArchiv.DatenInfoList.InsertRecord(iAktTag, iRef, False);  // Letzter Tageseintrag
            iAktTag := Trunc(dt - fTEnde);
            iRef := FieldByName(C_TfDSatz_ReferenzNr).asInteger;
            pArchiv.DatenInfoList.InsertRecord(iAktTag, iRef, False);  // Nächster Tageseintrag
          end
          else iRef := FieldByName(C_TfDSatz_ReferenzNr).asInteger;
          Next;
        end;
        pArchiv.DatenInfoList.RebuildInfoLists;

      end;
      Close;
    finally
      Free;
    end;
end;

{ Ordnet den Archiven Kanalinfos zu                    }
{------------------------------------------------------}
procedure TDSfGArchivInfo.ReadKanalTable(pSlTables: TStrings);
{------------------------------------------------------}
var
  pInstanz : TInstIdList;
  pArchiv  : TArchivList;
  i, iIId, iKNr, iANr : integer;
  sQDea            : string;
  c                : char;
  pSl              : TStrings;
begin
  pSl := TStringList.Create;
  with TQuery.Create(nil) do
  try
    // Tabellenliste ohne Vorbuchstabe erstellen
    pSl.Assign(pSlTables);
    for i := 0 to pSl.Count-1 do pSl[i] := Copy(pSl[i], 2, 7);

    DatabaseName := FStaDatabase;
    UniDirectional := True;
    Sql.Add('SELECT A.*, B.*');
    Sql.Add('FROM ' + C_DTB_AKanaele + ' A');
    Sql.Add('LEFT JOIN ' + C_Tb_Werteart + ' B ON A.' +
      C_DTF_AKanaele_Werteart + ' = B.' + C_Tf_Werteart_Kurzname);
    Sql.Add('ORDER BY A.' + C_DTF_AKanaele_InstanzID + ', A.' +
      C_DTF_AKanaele_ArchivNr + ', A.' + C_DTF_AKanaele_KanalNr);  // 04.07.2002
    Open;

    iIId := -1;
    iANr := -1;
    pInstanz := nil;
    pArchiv := nil;
    while (not Eof) do begin

      // Instanz in Archivinfo vorhanden ?
      if (iIId <> FieldByName(C_DTF_AKanaele_InstanzID).asInteger) then begin
        iIId := FieldByName(C_DTF_AKanaele_InstanzID).asInteger;
        pInstanz := Self.Instanz[iIId];
        iANr := -1;
      end;
      if (Assigned(pInstanz)) then begin
        // Archiv in Archivinfo vorhanden ?
        if (iANr <> FieldByName(C_DTF_AKanaele_ArchivNr).asInteger) then begin
          iANr := FieldByName(C_DTF_AKanaele_ArchivNr).asInteger;
          pArchiv := pInstanz.Archiv[iANr];
        end;

        if (Assigned(pArchiv)) then begin

          // Kanal in Datenbank vorhanden ?
          iKNr := FieldByName(C_DTF_AKanaele_KanalNr).asInteger;
          if (pSl.IndexOf(Format('%.3d%.4d', [iKNr, pArchiv.MyIndex])) < 0) then
            iKNr := -1;

          if (iKNr > 0) then begin
            if (Assigned(FindField(C_DTF_AKanaele_QuellDEL)))
            then sQDea := FieldByName(C_DTF_AKanaele_QuellDEL).asString
            else sQDea := '';  // Feld in älteren Versionen nicht vorhanden
            if (Trim (FieldByName(C_DTF_AKanaele_EADR).asString) = '')
            then c := '0'
            else c := FieldByName(C_DTF_AKanaele_EADR).asString[1];
            pArchiv.AddKanal(iKNr, FieldByName(C_DTF_AKanaele_Name).asString,
              FieldByName(C_DTF_AKanaele_Kanaltyp).asString,
              FieldByName(C_DTF_AKanaele_Werteart).asString, c, sQDea,
              FieldByName(C_Tf_Werteart_Kommastellen).asInteger,
              FieldByName(C_Tf_Werteart_AnzeigeMin).asFloat,
              FieldByName(C_Tf_Werteart_AnzeigeMax).asFloat);
          end;
        end;
      end;

      Next;
    end;
    if (Active) then Close;
  finally
    Free;
    pSl.Free;
  end;
end;

end.
