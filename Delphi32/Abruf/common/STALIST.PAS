{------------------------------------------------------------------------------}
{ Stammdaten-Speicher-Komponente                                               }
{                                                                              }
{ 26.06.2001  GD  Neu                                                          }
{ 01.08.2001  GD  Erweiterung um Sichtlevel                                    }
{ 29.04.2002  GD  Erweitert um Steuerung, Sortierungskriterien erweitert       }
{ 08.08.2002  GD  Unit DSfGStationen eleminiert                                }
{ 31.02.2003  GD  Archivliste speichert bereits Quelladresse                   }
{ 01.09.2003  GD  Bugfix: Auch Archive ohne Kanal 1 anzeigen                   }
{ 02.10.2003  GD  Neue Instanztypen integriert (M, Q)                          }
{ 28.09.2004  GD  Kanäle jetzt auch bei MRG                                    }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2004                                    }
{------------------------------------------------------------------------------}
unit StaList;

interface

uses
  SysUtils, DbTables, Db, Classes, ComCtrls,
  WSysCon, WTables, DMomLists, GD_Utils, DSG_Utils, DListen, MrgStd;

type
  TListClass = class of TStringIdList;

  TStringIdList = class;
  TStringIdListWithSubList = class;

  TStationsList = class;
  TInstanzList = class;
  TArchiveList = class;
  TKanaeleList = class;
  TLogbuchList = class;
  TBuildStaTree = class;

  TStaSortOrder = (ssoId, ssoName);
  TStaZuordnung = (
    szoRegistry, szoQuelle, szoArchReg_LogQuell, szoArchQuell_LogReg, szoAll);

  TStringIdList = class(TStringList)
    constructor Create; virtual;
    destructor Destroy; override;
  private
  protected
    function GetString(iIndex: integer): Str40; virtual;
    function GetId(iIndex: integer): integer; virtual;
  public
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    function Add(const sText: string): integer; override;
    function AddObject(const sText: string; pObject: TObject): integer; override;
    function AddString(
      iId: integer; sText: string; sGTyp: char = C_GerArtDSfG): integer; virtual;
    function AddInstanzValue(iId, iInstId: integer; sText: string;
      sGTyp: char = C_GerArtDSfG): integer; virtual;
    property MyString [iIndex: integer]: Str40 read GetString;
    property MyId [iIndex: integer]: integer read GetId;
  end;

  TStringIdListWithSubList = class(TStringIdList)
    constructor Create; override;
    destructor Destroy; override;
  private
    FSubList   : TList;
  protected
    FListClass : TListClass;
  public
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    function Add(const sText: string): integer; override;
  end;

  TStationsList = class(TStringIdListWithSubList)
    constructor Create; override;
  private
    FDatabaseName : string;
    FQuellArchive : TStaZuordnung;
    FSortOrder    : TStaSortOrder;
    FGerTypen     : TCharSet;
    FSichtLevel   : TDSfGSichtLevel;
    function GetInstanzList(iStaId: integer): TInstanzList;
    procedure FillMRGStationen;
    procedure FillDSfGStationen;
    procedure FillGruppen;
    procedure FillInstanzen;
    procedure FillArchive;
    procedure RearrangeArchive;
    procedure FillKanaele;
    procedure FillLogbuecher;
    procedure RearrangeLogbuecher;
    procedure SetDatabaseName(sDatabaseName: string);
  protected
  public
    property InstanzList [iStaId: integer]: TInstanzList read GetInstanzList;
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property QuellArchive: TStaZuordnung read FQuellArchive write FQuellArchive;
    property SortOrder: TStaSortOrder read FSortOrder write FSortOrder;
    property GerTypen: TCharSet read FGerTypen write FGerTypen;
    property SichtLevel: TDSfGSichtLevel read FSichtLevel write FSichtLevel;
  end;

  TInstanzList = class(TStringIdListWithSubList)
    constructor Create; override;
    destructor Destroy; override;
  private
    FLogbuchList : TList;
    function GetArchiveList(iInstId: integer): TArchiveList;
    function GetLogbuchList(iInstId: integer): TLogbuchList;
  protected
    function GetInstTyp(iIndex: integer): char; virtual;
    function GetInstName(iIndex: integer): Str40; virtual;
    function GetInstAdr(iIndex: integer): char; virtual;
  public
    function Add(const sText: string): integer; override;
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    property ArchiveList [iInstId: integer]: TArchiveList read GetArchiveList;
    property LogbuchList [iInstId: integer]: TLogbuchList read GetLogbuchList;
    property InstanzTyp [iIndex: integer]: char read GetInstTyp;
    property InstanzName [iIndex: integer]: Str40 read GetInstName;
    property InstanzAdresse [iIndex: integer]: char read GetInstAdr;
  end;

  TArchiveList = class(TStringIdListWithSubList)
    constructor Create; override;
  private
    function GetKanaeleList(iArchId: integer): TKanaeleList;
    procedure SetKanaeleList(iArchId: integer; pKanaele: TKanaeleList);
  protected
  public
    property KanaeleList [iArchId: integer]: TKanaeleList
      read GetKanaeleList write SetKanaeleList;
  end;

  TKanaeleList = class(TStringIdList)
    constructor Create; override;
  private
    FQuellAdresse : char;
  protected
  public
    property QuellAdresse: char read FQuellAdresse write FQuellAdresse;
  end;

  TLogbuchList = class(TStringIdList)
  private
    function GetLogbuchName(iIndex: integer): Str40; virtual;
    function GetQuellAdr(iIndex: integer): char; virtual;
  protected
  public
    property LogbuchName [iIndex: integer]: Str40 read GetLogbuchName;
    property QuellAdresse [iIndex: integer]: char read GetQuellAdr;
  end;

  TBuildStaTree = class(TObject)
    constructor Create(pStaTree: TTreeView; sDatabaseName: string;
      pMSStammdaten: TMSStammdaten = nil);
    destructor Destroy; override;
  private
    FDatabaseName : string;
    FTree         : TTreeView;
    FInstTypen    : TCharSet;
    FGerTypen     : TCharSet;
    FQuellArchive : TStaZuordnung;
    FSortOrder    : TStaSortOrder;
    FSichtLevel   : TDSfGSichtLevel;
    FMSStammdaten : TMSStammdaten;
    FMSStmCreated : boolean;
    function WriteStationen(pStationen: TStationsList; pRootNode: TTreeNode;
      sGTyp: char): integer;
    procedure WriteInstanzen(pInstanzen: TInstanzList; pRootNode: TTreeNode);
    procedure WriteArchive(
      pInstanzen: TInstanzList; pArchive: TArchiveList; pRootNode: TTreeNode);
    procedure WriteLogbuecher(pLogbuecher: TLogbuchList; pRootNode: TTreeNode);
    procedure WriteKanaele(pKanaele: TKanaeleList; pRootNode: TTreeNode); overload;
    procedure WriteKanaele(pRootNode: TTreeNode); overload;
    procedure FillMRGKennungen;
  protected
  public
    procedure BuildTree;
    property InstTypen: TCharSet read FInstTypen write FInstTypen;
    property GerTypen: TCharSet read FGerTypen write FGerTypen;
    property QuellArchive: TStaZuordnung read FQuellArchive write FQuellArchive;
    property SortOrder: TStaSortOrder read FSortOrder write FSortOrder;
    property SichtLevel: TDSfGSichtLevel read FSichtLevel write FSichtLevel;
  end;

  TBuildDSfGStationenList = class(TObject)
    constructor Create(pDSfGStaList: TStrings; sDatabaseName: string);
  private
    FDatabaseName  : string;
    FDSfGStationen : TStrings;
    function FillDSfGStationen: TQueryExt;
    function FillDSfGInstanzen: TQueryExt;
  protected
  public
    procedure Aktualisieren;
  end;

  TBuildMRGStationenList = class(TObject)
    constructor Create(pMRGStaList: TStrings; sDatabaseName: string);
  private
    FDatabaseName  : string;
    FMRGStationen : TStrings;
  protected
  public
    procedure Aktualisieren;
  end;

implementation

{------------------------------- TStringIdList --------------------------------}

{----------------------------------------------}
constructor TStringIdList.Create;
{----------------------------------------------}
begin
  inherited Create;
end;

{----------------------------------------------}
destructor TStringIdList.Destroy;
{----------------------------------------------}
begin
  Clear;

  inherited Destroy;
end;

{----------------------------------------------}
procedure TStringIdList.Delete(iIndex: integer);
{----------------------------------------------}
begin
  Dispose(PStaRecord(Objects[iIndex]));

  inherited Delete(iIndex);
end;

{----------------------------------------------}
procedure TStringIdList.Clear;
{----------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do Dispose(PStaRecord(Objects[i]));

  inherited Clear;
end;

{----------------------------------------------}
function TStringIdList.Add(const sText: string): integer;
{----------------------------------------------}
var
  p : PStaRecord;
  i : integer;
begin
  i := StrToIntDef(sText, -1);

  if (i >= 0) then begin
    Result := inherited Add(Format('%.8d', [i]));
    New(p);
    p^.GTyp:=#0;
    p^.GName:='';
    p^.InstId:=0;
    p^.MyId := i;
    Objects[Result] := TObject(p);
  end
  else Result := -1;
end;

{----------------------------------------------}
function TStringIdList.AddObject(const sText: string; pObject: TObject): integer;
{----------------------------------------------}
begin
  Result := Add(sText);
  PStaRecord(Objects[Result])^.GTyp := PStaRecord(pObject)^.GTyp;
  PStaRecord(Objects[Result])^.GName := PStaRecord(pObject)^.GName;
  PStaRecord(Objects[Result])^.InstId := PStaRecord(pObject)^.InstId;
  PStaRecord(Objects[Result])^.MyId := PStaRecord(pObject)^.MyId;
end;

{----------------------------------------------}
function TStringIdList.AddString(
  iId: integer; sText: string; sGTyp: char = C_GerArtDSfG): integer;
{----------------------------------------------}
begin
  Result := Add(Format('%.8d', [iId]));
  PStaRecord(Objects[Result])^.GTyp := sGTyp;
  PStaRecord(Objects[Result])^.GName := sText;
  PStaRecord(Objects[Result])^.MyId := iId;
end;

{----------------------------------------------}
function TStringIdList.AddInstanzValue(
  iId, iInstId: integer; sText: string; sGTyp: char = C_GerArtDSfG): integer;
{----------------------------------------------}
begin
  Result := AddString(iId, sText, sGTyp);
  PStaRecord(Objects[Result])^.InstId := iInstId;
end;

{----------------------------------------------}
function TStringIdList.GetString(iIndex: integer): Str40;
{----------------------------------------------}
begin
  Result := PStaRecord(Objects[iIndex])^.GName;
end;

{----------------------------------------------}
function TStringIdList.GetId(iIndex: integer): integer;
{----------------------------------------------}
begin
  Result := StrToIntDef(Strings[iIndex], -1);
end;

{------------------------ TStringIdListWithSubList ----------------------------}

{----------------------------------------------}
constructor TStringIdListWithSubList.Create;
{----------------------------------------------}
begin
  inherited Create;

  FSubList := TList.Create;
  FListClass := TStringIdList;
end;

{----------------------------------------------}
destructor TStringIdListWithSubList.Destroy;
{----------------------------------------------}
var
  i : integer;
begin
  for i := FSubList.Count-1 downto 0 do TStringIdList(FSubList.Items[i]).Free;
  FSubList.Free;
  FSubList := nil;

  inherited Destroy;
end;

{----------------------------------------------}
procedure TStringIdListWithSubList.Delete(iIndex: integer);
{----------------------------------------------}
begin
  if (Assigned(FSubList)) then begin
    TStringIdList(FSubList[iIndex]).Free;
    FSubList.Delete(iIndex);
  end;

  inherited Delete(iIndex);
end;

{----------------------------------------------}
procedure TStringIdListWithSubList.Clear;
{----------------------------------------------}
var
  i : integer;
begin
  if (Assigned(FSubList)) then begin
    for i := FSubList.Count-1 downto 0 do TStringIdList(FSubList[i]).Free;
    FSubList.Clear;
  end;

  inherited Clear;
end;

{----------------------------------------------}
function TStringIdListWithSubList.Add(const sText: string): integer;
{----------------------------------------------}
var
  p : TStringIdList;
begin
  Result := inherited Add(sText);

  if (Result >= 0) then begin
    p := FListClass.Create;
    FSubList.Add(p);
  end;
end;

{------------------------------ TStationsList ---------------------------------}

{----------------------------------------------}
constructor TStationsList.Create;
{----------------------------------------------}
begin
  inherited Create;

  FDatabaseName := '';
  FQuellArchive := szoQuelle;  // Archive werden ihren Datenquellen zugeordnet
  FSortOrder := ssoName;  // Stationen werden nach dem Namen sortiert
  FListClass := TInstanzList;
  FGerTypen := [];        // Ggf. Einschränkung der anzuz. Gerätetypen ([]=Alle)
  FSichtLevel := dslAll;  // Alle Ebenen anzeigen
end;

{----------------------------------------------}
procedure TStationsList.SetDatabaseName(sDatabaseName: string);
{----------------------------------------------}
begin
  if (FDatabaseName <> sDatabaseName) then begin
    FDatabaseName := sDatabaseName;
    if (FGerTypen = []) or (C_GerArtDSfG in FGerTypen) then begin
      FillDSfGStationen;
      if (FSichtLevel > dslStation) then FillInstanzen;
      if (FSichtLevel > dslInstanzen) and
         (not (FSichtLevel in [dslLogbuecher]))
      then FillArchive;
      if (FSichtLevel > dslInstanzen) and
         (not (FSichtLevel in [dslArchive, dslKanaele]))
      then FillLogbuecher;
      if ((FSichtLevel > dslLogbuecher) or (FSichtLevel = dslKanaele)) {and
         (FQuellArchive in [szoQuelle, szoArchQuell_LogReg, szoAll]) }
      then FillKanaele;
      if (FSichtLevel > dslInstanzen) and
         (FQuellArchive in [szoQuelle, szoArchQuell_LogReg, szoAll])
      then RearrangeArchive;
      if (FSichtLevel > dslInstanzen) and
         (FQuellArchive in [szoQuelle, szoArchReg_LogQuell, szoAll])
      then RearrangeLogbuecher;
    end;

    if (FGerTypen = []) or (C_GerArtMrg in FGerTypen) then begin
      FillMRGStationen;
    end;

    if (FGerTypen = []) or (C_GerArtGruppe in FGerTypen) then begin
      FillGruppen;
    end;
  end;
end;

{----------------------------------------------}
function TStationsList.GetInstanzList(iStaId: integer): TInstanzList;
{----------------------------------------------}
var
  i : integer;
begin
  i := IndexOf(Format('%.8d', [iStaId]));

  if (i >= 0) then begin
    Result := TInstanzList(FSubList.Items[i]);
  end
  else Result := nil;
end;

{----------------------------------------------}
procedure TStationsList.FillDSfGStationen;
{----------------------------------------------}
begin
  if (FDatabaseName = '') then Exit;

  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    TableName := C_DTB_Station;
    if (not Exists) then Exit;
  finally
    Free;
  end;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    Sql.Add('SELECT ' + C_DTF_Station_StationId + ',');
    Sql.Add(C_DTF_Station_Stationsname);
    Sql.Add('FROM ' + C_DTB_Station);
//    if (SortOrder = ssoId) then
//      Sql.Add('ORDER BY ' + C_DTF_Station_StationId)
//    else if (SortOrder = ssoName) then
    Sql.Add('ORDER BY ' + C_DTF_Station_Stationsname);  // Bei DSfG ist nur Name sinnvoll
    Open;
    while (not Eof) do begin
      Self.AddString(FieldByName(C_DTF_Station_StationId).asInteger,
        FieldByName(C_DTF_Station_Stationsname).asString);
      Next;
    end;
    if (Active) then Close;
  finally
    Free;
  end;
end;

{----------------------------------------------}
procedure TStationsList.FillMRGStationen;
{----------------------------------------------}
begin
  if (FDatabaseName = '') then Exit;

  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    TableName := CDBSta;
    if (not Exists) then Exit;
  finally
    Free;
  end;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    Sql.Add('SELECT ' + C_Sta_MrgId + ',' + C_Sta_StationsName + ',');
    Sql.Add(C_Sta_Kennung);
    Sql.Add('FROM ' + CDBSta);
    Sql.Add('WHERE ' + C_Sta_Aktiv + '= 0');
    if (SortOrder = ssoName) then Sql.Add('ORDER BY ' + C_Sta_StationsName)
    else if (SortOrder = ssoId) then Sql.Add('ORDER BY ' + C_Sta_Kennung);
    Open;
    while (not Eof) do begin
      Self.AddString(FieldByName(C_Sta_MrgId).asInteger,
        FieldByName(C_Sta_StationsName).asString, C_GerArtMrg);
      Next;
    end;
    if (Active) then Close;
  finally
    Free;
  end;
end;

{----------------------------------------------}
procedure TStationsList.FillGruppen;
{----------------------------------------------}
var
  pSl   : TStrings;
  iId   : integer;
  sName : string;
begin
  if (FDatabaseName = '') then Exit;

  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    TableName := C_TbMrgGrp;
    if (not Exists) then Exit;
    TableName := CDBSta;
    if (not Exists) then Exit;
  finally
    Free;
  end;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    Sql.Add('SELECT A.' + C_TfGrpId + ', A.' + C_TfGrpName + ',');
    Sql.Add('B.' + C_TfMrgId + ',' + 'C.' + C_Sta_StationsName + ',');
    Sql.Add('C.' + C_Sta_Kennung);
    Sql.Add('FROM ' + C_TbGrpStamm + ' A,' + C_TbMrgGrp + ' B,' + CDBSta + ' C');
    Sql.Add('WHERE A.' + C_TfGrpId + ' = B.' + C_TfGrpId);
    Sql.Add('AND B.' + C_TfMrgId + ' = C.' + C_Sta_MrgId);
    Sql.Add('ORDER BY A.' + C_TfGrpName + ', A.' + C_TfGrpId + ', C.' +
      C_Sta_StationsName);
    Open;

    iId := -1;    // ID der Gruppe
    sName := '';  // Name der Gruppe
    pSl := TStringList.Create;
    while (not Eof) do begin
      if (iId <> FieldByName(C_TfGrpId).asInteger) then begin
        if (iId >= 0) then begin
          Self.AddInstanzValue(iId, integer(pSl), sName, C_GerArtGruppe);
          pSl := TStringList.Create;
        end;
        iId := FieldByName(C_TfGrpId).asInteger;
        sName := FieldByName(C_TfGrpName).asString;
      end;
      pSl.Add(C_GerArtMrg + Chr(us) + IntToStr(FieldByName(C_TfMrgId).asInteger)
        + Chr(us) + FieldByName(C_Sta_StationsName).asString
        + Chr(us) + FieldByName(C_Sta_Kennung).asString);
      Next;
    end;
    if (iId >= 0) then  // Letzte Gruppe aufnehmen
      Self.AddInstanzValue(iId, integer(pSl), sName, C_GerArtGruppe);
    if (Active) then Close;
  finally
    Free;
  end;
end;

{----------------------------------------------}
procedure TStationsList.FillInstanzen;
{----------------------------------------------}
var
  i, iStaId : integer;
begin
  if (FDatabaseName = '') or (Self.Count = 0) then Exit;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    Sql.Add('SELECT ' + C_DTF_Instanz_StationId + ',' + C_DTF_Instanz_InstanzId + ',');
    Sql.Add(C_DTF_Instanz_Instanztyp + ',' + C_DTF_Instanz_Busadresse + ',' +
      C_DTF_Instanz_Instanzname);
    Sql.Add('FROM ' + C_DTB_Instanz);
    Sql.Add('ORDER BY ' + C_DTF_Instanz_StationId + ',' + C_DTF_Instanz_Busadresse);

    Open;
    while (not Eof) do begin
      try
        iStaId := FieldByName(C_DTF_Instanz_StationId).asInteger;
        i := Self.IndexOf(Format('%.8d', [iStaId]));
        if (i >= 0) then begin
          Self.InstanzList[iStaId].AddString(
            FieldByName(C_DTF_Instanz_InstanzId).asInteger,
            FieldByName(C_DTF_Instanz_Instanztyp).asString[1] +
              FieldByName(C_DTF_Instanz_Busadresse).asString[1] +
              FieldByName(C_DTF_Instanz_Instanzname).asString);
        end;
      except
      // Schrott in Tabelle - ignorieren
      end;
      Next;
    end;

    if (Active) then Close;
  finally
    Free;
  end;
end;

{----------------------------------------------}
procedure TStationsList.FillArchive;
{----------------------------------------------}
var
  i, j, iStaId, iInstId, iArchNr : integer;
  sEADR                          : string;
begin
  if (FDatabaseName = '') or (Self.Count = 0) then Exit;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    Sql.Add('SELECT DISTINCT A.' + C_DTF_Archive_InstanzID + ',');  // 01.09.2003
    Sql.Add('A.' + C_DTF_Archive_ArchivNr + ', A.' + C_DTF_Archive_Name + ',');
    Sql.Add('B.' + C_DTF_AKanaele_EADR);
    Sql.Add('FROM ' + C_DTB_Archive + ' A, ' + C_DTB_AKanaele + ' B');
    Sql.Add('WHERE A.' + C_DTF_Archive_InstanzID +
      ' = B.' + C_DTF_AKanaele_InstanzID);
    Sql.Add('AND A.' + C_DTF_Archive_ArchivNr +
      ' = B.' + C_DTF_AKanaele_ArchivNr);
    Sql.Add('ORDER BY A.' + C_DTF_Archive_InstanzID + ', A.' +
      C_DTF_Archive_ArchivNr);
    Open;

    iStaId := StrToIntDef(Strings[0], -1);
    while (not Eof) do begin
      iInstId := FieldByName(C_DTF_Archive_InstanzID).asInteger;
      i := Self.InstanzList[iStaId].IndexOf(Format('%.8d', [iInstId]));
      if (i < 0) then begin
        iStaId := -1;
        for j := 0 to Count-1 do begin
          iStaId := StrToIntDef(Strings[j], -1);
          i := Self.InstanzList[iStaId].IndexOf(Format('%.8d', [iInstId]));
          if (i < 0) then iStaId := -1 else Break;
        end;
      end;

      if (iStaId >= 0) then
        with Self.InstanzList[iStaId].ArchiveList[iInstId] do begin
          iArchNr := FieldByName(C_DTF_Archive_ArchivNr).asInteger;
          sEADR := Trim(FieldByName(C_DTF_AKanaele_EADR).asString);
          AddInstanzValue(
            iArchNr, iInstId, FieldByName(C_DTF_Archive_Name).asString);
          if (sEADR <> '') then KanaeleList[iArchNr].QuellAdresse := sEADR[1];
        end;
      Next;
    end;
    if (Active) then Close;
  finally
    Free;
  end;
end;

{----------------------------------------------}
procedure TStationsList.RearrangeArchive;
{----------------------------------------------}
var
  i, j, k, l, m            : integer;
  iStaId, iInstId, iArchId : integer;
  pInstList                : TInstanzList;
  pArchList                : TArchiveList;
  pKanList1, pKanList2     : TKanaeleList;
  cBusAdr                  : char;
begin
  for i := 0 to Count-1 do begin  // Schleife über alle Stationen

    iStaId := MyId[i];
    pInstList := InstanzList[iStaId];
    for j := 0 to pInstList.Count-1 do begin  // Schleife über Instanzen der Station

      iInstId := pInstList.MyId[j];
      pArchList := pInstList.ArchiveList[iInstId];
      k := 0;
      while (k < pArchList.Count) do begin  // Schleife über Archive der Instanz

        iArchId := pArchList.MyId[k];
        pKanList1 := pArchList.KanaeleList[iArchId];
        cBusAdr := pKanList1.QuellAdresse;

        if (cBusAdr in ['A'..'_']) and (cBusAdr <> pInstList.InstanzAdresse[j])
        then begin

          for l := 0 to pInstList.Count-1 do begin  // 2. Schleife über Instanzen der Station

            // Archivgruppe von Reg. zur Quelle transferieren
            if (cBusAdr = pInstList.InstanzAdresse[l]) then begin
              pInstList.ArchiveList[pInstList.MyId[l]].AddInstanzValue(
                iArchId, iInstId, pArchList.MyString[k]);
              pKanList2 :=      // Kanalliste der Quellinstanz
                pInstList.ArchiveList[pInstList.MyId[l]].KanaeleList[iArchId];
              if (QuellArchive <> szoAll) then begin
                // Archivgruppe tauschen
                pInstList.ArchiveList[pInstList.MyId[l]].KanaeleList[iArchId] :=
                  pKanList1;
                pArchList.KanaeleList[iArchId] := pKanList2;
                pArchList.Delete(k);
                Dec(k);  // Zähler für Sätze dekrementieren
              end
              else begin
                // Archivgruppe duplizieren
                for m := 0 to pKanList1.Count-1 do begin
                  pKanList2.AddInstanzValue(
                    pKanList1.MyId[m], iInstId, pKanList1.MyString[m]);
                end;
              end;

              Break;
            end;  // Archivgruppe tauschen

          end; // 2. Schleife über Instanzen

        end;

        Inc(k);  // Zähler für Sätze inkrementieren
      end;  // Archive-Schleife

    end;  // Instanz-Schleife

  end;  // Stations-Schleife
end;

{----------------------------------------------}
procedure TStationsList.FillLogbuecher;
{----------------------------------------------}
var
  i, j, iStaId, iInstId : integer;
begin
  if (FDatabaseName = '') or (Self.Count = 0) then Exit;

  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    TableName := C_DTB_Logbuch;
    OpenShared;

    iStaId := StrToIntDef(Strings[0], -1);
    while (not Eof) do begin
      iInstId := FieldByName(C_DTF_Logbuch_InstanzID).asInteger;
      i := Self.InstanzList[iStaId].IndexOf(Format('%.8d', [iInstId]));
      if (i < 0) then begin
        iStaId := -1;
        for j := 0 to Count-1 do begin
          iStaId := StrToIntDef(Strings[j], -1);
          i := Self.InstanzList[iStaId].IndexOf(Format('%.8d', [iInstId]));
          if (i < 0) then iStaId := -1 else Break;
        end;
      end;

      if (iStaId >= 0) and (FieldByName(C_DTF_Logbuch_EADR).asString <> '') then
      begin
        Self.InstanzList[iStaId].LogbuchList[iInstId].AddInstanzValue(
          FieldByName(C_DTF_Logbuch_LogbuchNr).asInteger, iInstId,
          FieldByName(C_DTF_Logbuch_EADR).asString[1] +
          FieldByName(C_DTF_Logbuch_Name).asString);
      end;
      Next;
    end;
    if (Active) then Close;
  finally
    Free;
  end;
end;

{----------------------------------------------}
procedure TStationsList.RearrangeLogbuecher;
{----------------------------------------------}
var
  i, j, k, l        : integer;
  iStaId, iInstId, iLogbId : integer;
  pInstList                : TInstanzList;
  pLogbList                : TLogbuchList;
  cBusAdr                  : char;
begin
  for i := 0 to Count-1 do begin  // Schleife über alle Stationen

    iStaId := MyId[i];
    pInstList := InstanzList[iStaId];
    for j := 0 to pInstList.Count-1 do begin  // Schleife über Instanzen der Station

      iInstId := pInstList.MyId[j];
      pLogbList := pInstList.LogbuchList[iInstId];
      for k := pLogbList.Count-1 downto 0 do begin  // Schleife über Logbücher der Instanz

        iLogbId := pLogbList.MyId[k];
        cBusAdr := pLogbList.QuellAdresse[k];
        if (cBusAdr in ['A'..'_']) and (cBusAdr <> pInstList.InstanzAdresse[j]) then
        begin

          for l := 0 to pInstList.Count-1 do begin  // 2. Schleife über Instanzen der Station

            if (cBusAdr = pInstList.InstanzAdresse[l]) then begin  // Logbücher einfügen
              pInstList.LogbuchList[pInstList.MyId[l]].AddInstanzValue(
                iLogbId, iInstId, pLogbList.MyString[k]);
              if (QuellArchive <> szoAll) then begin
                pLogbList.Delete(k);
              end;
              Break;
            end;  // Logbücher einfügen

          end; // 2. Schleife über Instanzen

        end;

      end;  // Logbücher-Schleife

    end;  // Instanz-Schleife

  end;  // Stations-Schleife

end;

{----------------------------------------------}
procedure TStationsList.FillKanaele;
{----------------------------------------------}
var
  i, j, iStaId, iInstId, iArchId : integer;
begin
  if (FDatabaseName = '') or (Self.Count = 0) then Exit;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    Sql.Add('SELECT ' + C_DTF_AKanaele_InstanzID + ',' + C_DTF_AKanaele_Name + ',');
    Sql.Add(C_DTF_AKanaele_ArchivNr + ',' + C_DTF_AKanaele_KanalNr + ',' +
      C_DTF_AKanaele_EADR);
    Sql.Add('FROM ' + C_DTB_AKanaele);

    if (SortOrder = ssoName) then Sql.Add('ORDER BY ' + C_DTF_AKanaele_InstanzID
      + ',' + C_DTF_AKanaele_ArchivNr + ',' + C_DTF_AKanaele_Name);
    if (SortOrder = ssoId) then Sql.Add('ORDER BY ' + C_DTF_AKanaele_InstanzID
      + ',' + C_DTF_AKanaele_ArchivNr + ',' + C_DTF_AKanaele_KanalNr);

    Open;

    iStaId := StrToIntDef(Strings[0], -1);

    while (not Eof) do begin
      iInstId := FieldByName(C_DTF_AKanaele_InstanzID).asInteger;
      iArchId := FieldByName(C_DTF_AKanaele_ArchivNr).asInteger;
      // Richtige Station finden
      i := Self.InstanzList[iStaId].IndexOf(Format('%.8d', [iInstId]));
      if (i < 0) then begin
        iStaId := -1;
        for j := 0 to Count-1 do begin
          iStaId := StrToIntDef(Strings[j], -1);
          i := Self.InstanzList[iStaId].IndexOf(Format('%.8d', [iInstId]));
          if (i < 0) then iStaId := -1 else Break;
        end;
      end;

      if (iStaId >= 0) then begin
        if (FieldByName(C_DTF_AKanaele_EADR).asString <> '') then
          Self.InstanzList[iStaId].ArchiveList[iInstId].KanaeleList[iArchId].
            QuellAdresse := FieldByName(C_DTF_AKanaele_EADR).asString[1];
        Self.InstanzList[iStaId].ArchiveList[iInstId].KanaeleList[iArchId].
          AddInstanzValue(FieldByName(C_DTF_AKanaele_KanalNr).asInteger, iInstId,
          Format('%.2d', [FieldByName(C_DTF_AKanaele_KanalNr).asInteger])
          + ' ' + FieldByName(C_DTF_AKanaele_Name).asString);
      end;
      Next;
    end;

    if (Active) then Close;

  finally
    Free;
  end;
end;

{------------------------------ TInstanzList ----------------------------------}

{----------------------------------------------}
constructor TInstanzList.Create;
{----------------------------------------------}
begin
  inherited Create;

  FLogbuchList := TList.Create;     // Subliste für Logbücher
  FListClass := TArchiveList;       // FSubList ist Archive-Liste
end;

{----------------------------------------------}
destructor TInstanzList.Destroy;
{----------------------------------------------}
var
  i : integer;
begin
  for i := FLogbuchList.Count-1 downto 0 do TStringIdList(FLogbuchList[i]).Free;
  FLogbuchList.Free;
  FLogbuchList := nil;

  inherited Destroy;
end;

{----------------------------------------------}
procedure TInstanzList.Delete(iIndex: integer);
{----------------------------------------------}
begin
  if (Assigned(FLogbuchList)) then begin
    TStringIdList(FLogbuchList[iIndex]).Free;
    FLogbuchList.Delete(iIndex);
  end;

  inherited Delete(iIndex);
end;

{----------------------------------------------}
procedure TInstanzList.Clear;
{----------------------------------------------}
var
  i : integer;
begin
  if (Assigned(FLogbuchList)) then begin
    for i := FLogbuchList.Count-1 downto 0 do TStringIdList(FLogbuchList[i]).Free;
    FLogbuchList.Clear;
  end;

  inherited Clear;
end;

{----------------------------------------------}
function TInstanzList.Add(const sText: string): integer;
{----------------------------------------------}
var
  p : TLogbuchList;
begin
  Result := inherited Add(sText);

  if (Result >= 0) then begin
    p := TLogbuchList.Create;
    FLogbuchList.Add(p);
  end;
end;

{----------------------------------------------}
function TInstanzList.GetArchiveList(iInstId: integer): TArchiveList;
{----------------------------------------------}
var
  i : integer;
begin
  i := IndexOf(Format('%.8d', [iInstId]));

  if (i >= 0) then begin
    Result := TArchiveList(FSubList.Items[i]);
  end
  else Result := nil;
end;

{----------------------------------------------}
function TInstanzList.GetLogbuchList(iInstId: integer): TLogbuchList;
{----------------------------------------------}
var
  i : integer;
begin
  i := IndexOf(Format('%.8d', [iInstId]));

  if (i >= 0) then begin
    Result := TLogbuchList(FLogbuchList.Items[i]);
  end
  else Result := nil;
end;

{----------------------------------------------}
function TInstanzList.GetInstTyp(iIndex: integer): char;
{----------------------------------------------}
var
  s : string;
begin
  s := PStaRecord(Objects[iIndex])^.GName;
  if (s <> '') then Result := s[1] else Result := C_D_Instanztyp_unbest;
end;

{----------------------------------------------}
function TInstanzList.GetInstName(iIndex: integer): Str40;
{----------------------------------------------}
var
  s : string;
begin
  s := PStaRecord(Objects[iIndex])^.GName;
  if (Length(s) > 2) then Result := Copy(s, 3, Length(s)-2) else Result := '';
end;

{----------------------------------------------}
function TInstanzList.GetInstAdr(iIndex: integer): char;
{----------------------------------------------}
var
  s : string;
begin
  s := PStaRecord(Objects[iIndex])^.GName;
  if (Length(s) > 1) then Result := s[2] else Result := '0';
end;

{------------------------------ TArchiveList ----------------------------------}

{----------------------------------------------}
constructor TArchiveList.Create;
{----------------------------------------------}
begin
  inherited Create;

  FListClass := TKanaeleList;
end;

{----------------------------------------------}
function TArchiveList.GetKanaeleList(iArchId: integer): TKanaeleList;
{----------------------------------------------}
var
  i : integer;
begin
  i := IndexOf(Format('%.8d', [iArchId]));

  if (i >= 0) then begin
    Result := TKanaeleList(FSubList.Items[i]);
  end
  else Result := nil;
end;

{----------------------------------------------}
procedure TArchiveList.SetKanaeleList(iArchId: integer; pKanaele: TKanaeleList);
{----------------------------------------------}
var
  i : integer;
begin
  i := IndexOf(Format('%.8d', [iArchId]));

  if (i >= 0) then begin
//    TObject(FSubList.Items[i]).Free;
    FSubList.Items[i] := pKanaele;
  end;
end;

{------------------------------ TKanaeleList ----------------------------------}

{----------------------------------------------}
constructor TKanaeleList.Create;
{----------------------------------------------}
begin
  inherited Create;

  FQuellAdresse := '0';
end;

{-------------------------------- TLogbuchList --------------------------------}

{----------------------------------------------}
function TLogbuchList.GetQuellAdr(iIndex: integer): char;
{----------------------------------------------}
var
  s : string;
begin
  s := PStaRecord(Objects[iIndex])^.GName;
  if (s <> '') then Result := s[1] else Result := '0';
end;

{----------------------------------------------}
function TLogbuchList.GetLogbuchName(iIndex: integer): Str40;
{----------------------------------------------}
var
  s : string;
begin
  s := PStaRecord(Objects[iIndex])^.GName;
  if (Length(s) > 1) then Result := Copy(s, 2, Length(s)-1) else Result := '';
end;

{-------------------------------- TBuildStaTree -------------------------------}

{----------------------------------------------}
constructor TBuildStaTree.Create(pStaTree: TTreeView; sDatabaseName: string;
  pMSStammdaten: TMSStammdaten= nil);
{----------------------------------------------}
begin
  FTree := pStaTree;
  FDatabaseName := sDatabaseName;
  FGerTypen := [];   // Ggf. Einschränkung der anzuz. Gerätetypen ([]=Alle)
  FInstTypen := [];  // Ggf. Einschränkung der anzuz. Instanztypen ([]=Alle)
  FQuellArchive := szoQuelle;  // Archive werden ihren Datenquellen zugeordnet
  FSortOrder := ssoName;  // Stationen werden nach dem Namen sortiert
  FSichtLevel := dslAll;  // Alle Ebenen anzeigen
  FMSStmCreated := (not Assigned(pMSStammdaten));
  if (FMSStmCreated) then begin
    FMSStammdaten := TMSStammdaten.Create(nil);
    with FMSStammdaten do begin
      DatabaseName := sDatabaseName;
      SysDatabase := DatabaseName;
      Sectors := [msi_Allgemein, msi_Info, msi_Kanal];
    end;
  end
  else FMSStammdaten := pMSStammdaten;
end;

{----------------------------------------------}
destructor TBuildStaTree.Destroy;
{----------------------------------------------}
begin
  if (FMSStmCreated) then FMSStammdaten.Free;

  inherited;
end;

{----------------------------------------------}
procedure TBuildStaTree.BuildTree;
{----------------------------------------------}

  // 08.08.2002
  procedure ClearTree;
  var
    i : integer;
  begin
    with FTree do begin
      for i := 0 to Items.Count-1 do
        if (Assigned(Items[i].Data)) then begin
          // InstId einer Gruppe verweist auf Stringliste  // 16.07.2001
          if (PStaRecord(Items[i].Data)^.GTyp = C_GerArtGruppe) then
          try
            TStrings(PStaRecord(Items[i].Data)^.InstId).Free;
          except
            // dann halt nicht ...
          end;
          Dispose(PStaRecord(Items[i].Data));
          Items[i].Data := nil;
        end;

      Items.Clear;
    end;
  end;

  // 08.08.2002
  procedure DeleteNode(pNode: TTreeNode);
  begin
    with FTree do begin
      if (Assigned(pNode.Data)) then begin
        while (pNode.HasChildren) do DeleteNode(pNode.GetFirstChild);

        // InstId einer Gruppe verweist auf Stringliste  // 16.07.2001
        if (PStaRecord(pNode.Data)^.GTyp = C_GerArtGruppe) then
        try
          TStrings(PStaRecord(pNode.Data)^.InstId).Free;
        except
          // dann halt nicht ...
        end;
        Dispose(PStaRecord(pNode.Data));
      end;

      Items.Delete(pNode);
    end;
  end;

var
  pRootNode       : TTreeNode;
  pStationen      : TStationsList;
  p               : PStaRecord;
begin
  FTree.Items.BeginUpdate;
  try
    ClearTree;

    pStationen := TStationsList.Create;
    with pStationen do
    try
      GerTypen := Self.GerTypen;
      QuellArchive := Self.QuellArchive;
      SortOrder := Self.SortOrder;
      SichtLevel := Self.FSichtLevel;
      DatabaseName := Self.FDatabaseName;

      if (FGerTypen = []) or (C_GerArtDSfG in FGerTypen) then begin
        New(p);
        p^.GTyp := C_GerArtDSfG;
        p^.GName := 'DSfG-Stationen';
        p^.InstId := 0;
        p^.MyId := 1;

        pRootNode := FTree.Items.AddObject(nil, p^.GName, p);
        pRootNode.ImageIndex := C_ImageIndex_Aktiv;
        pRootNode.SelectedIndex := C_ImageIndex_Aktiv;
        if (WriteStationen(pStationen, pRootNode, C_GerArtDSfG) = 0) then
          DeleteNode(pRootNode);
      end;

      if (FGerTypen = []) or (C_GerArtMrg in FGerTypen) then begin
        New(p);
        p^.GTyp := C_GerArtMrg;
        p^.GName := 'MRG-Stationen';
        p^.InstId := 0;
        p^.MyId := 2;
        pRootNode := FTree.Items.AddObject(nil, p^.GName, p);
        pRootNode.ImageIndex := C_ImageIndex_M_Aktiv;
        pRootNode.SelectedIndex := C_ImageIndex_M_Aktiv;
        if (WriteStationen(pStationen, pRootNode, C_GerArtMrg) = 0)
        then DeleteNode(pRootNode)
        else begin
          FillMRGKennungen;
          if (FSichtLevel >= dslKanaele) then WriteKanaele(pRootNode);
        end;
      end;

      if (FGerTypen = []) or (C_GerArtGruppe in FGerTypen) then begin
        New(p);
        p^.GTyp := C_GerArtGruppe;
        p^.GName := 'Gruppen';
        p^.InstId := 0;
        p^.MyId := 3;
        pRootNode := FTree.Items.AddObject(nil, p^.GName, p);
        pRootNode.ImageIndex := C_ImageIndex_G_Aktiv;
        pRootNode.SelectedIndex := C_ImageIndex_G_Aktiv;
        if (WriteStationen(pStationen, pRootNode, C_GerArtGruppe) = 0) then 
          DeleteNode(pRootNode);
      end;
    finally
      Free;
    end;

    if (FTree.Items.Count = 0) then
      with FTree.Items.Add(nil, 'Keine Stationen in den Stammdaten') do begin
        ImageIndex := -1;
        SelectedIndex := -1;
      end;
  finally
    FTree.Items.EndUpdate;
  end;
end;

{ Trägt Stationen eines Gerätetyps ein         }
{ Parameter: Liste mit Stationsdaten,          }
{            übergeordneter Knoten, Geräteart  }
{ Rückgabe: Anzahl der eingetragenen Stationen }
{----------------------------------------------}
function TBuildStaTree.WriteStationen(
  pStationen: TStationsList; pRootNode: TTreeNode; sGTyp: char): integer;
{----------------------------------------------}
var
  i : integer;
  pNode : TTreeNode;
  p : PStaRecord;
begin
  Result := 0;

  for i := 0 to pStationen.Count-1 do begin
    if (PStaRecord(pStationen.Objects[i])^.GTyp <> sGTyp) then Continue;

    New(p);
    p^.GName := PStaRecord(pStationen.Objects[i])^.GName;
    p^.GTyp := PStaRecord(pStationen.Objects[i])^.GTyp;
    p^.InstId := PStaRecord(pStationen.Objects[i])^.InstId;
    p^.MyId := PStaRecord(pStationen.Objects[i])^.MyId;
    pNode := FTree.Items.AddChildObject(pRootNode, pStationen.MyString[i], p);

    if (p^.GTyp = C_GerArtDSfG) then begin
      pNode.ImageIndex := C_ImageIndex_Aktiv;
      pNode.SelectedIndex := C_ImageIndex_Inaktiv;
      WriteInstanzen(pStationen.InstanzList[pStationen.MyId[i]], pNode);
    end
    else if (p^.GTyp = C_GerArtMRG) then begin
      pNode.ImageIndex := C_ImageIndex_M_Aktiv;
      pNode.SelectedIndex := C_ImageIndex_M_Inaktiv;
    end
    else if (p^.GTyp = C_GerArtGruppe) then begin
      pNode.ImageIndex := C_ImageIndex_G_Aktiv;
      pNode.SelectedIndex := C_ImageIndex_G_Inaktiv;
    end;

    Inc(Result);
  end;
end;

{----------------------------------------------}
procedure TBuildStaTree.WriteInstanzen(
  pInstanzen: TInstanzList; pRootNode: TTreeNode);
{----------------------------------------------}
var
  i, j : integer;
  pNode : TTreeNode;
  p : PStaRecord;
begin
  for i := 0 to pInstanzen.Count-1 do begin
    if (FInstTypen <> []) and (not (pInstanzen.InstanzTyp[i] in FInstTypen))
    then Continue;

    New(p);
    p^.GTyp := PStaRecord(pInstanzen.Objects[i])^.GTyp;
    p^.GName := PStaRecord(pInstanzen.Objects[i])^.GName;
    p^.InstId := PStaRecord(pInstanzen.Objects[i])^.InstId;
    p^.MyId := PStaRecord(pInstanzen.Objects[i])^.MyId;
    pNode := FTree.Items.AddChildObject(pRootNode, pInstanzen.InstanzName[i], p);

    case pInstanzen.InstanzTyp[i] of
      C_D_Instanztyp_Umw : j := C_ImageIndex_Umwerter;
      C_D_Instanztyp_Reg : j := C_ImageIndex_Registrierung;
      C_D_Instanztyp_Wieser : j := C_ImageIndex_Wieser;
      C_D_Instanztyp_DFU : j := C_ImageIndex_DFU;
      C_D_Instanztyp_Gas : j := C_ImageIndex_Gasbeschaffenheit;
      C_D_Instanztyp_Strg : j := C_ImageIndex_Steuerung;  // 29.04.2002
      C_D_Instanztyp_Rev : j := C_ImageIndex_REVISION;
      'Q'                : j := C_ImageIndex_KORRGASBESCH;
      C_D_Instanztyp_unbest : j := C_ImageIndex_unbestimmt;
      else j := -1;
    end;

    pNode.ImageIndex := j;
    pNode.SelectedIndex := j;

    WriteArchive(pInstanzen, pInstanzen.ArchiveList[pInstanzen.MyId[i]], pNode);
    WriteLogbuecher(pInstanzen.LogbuchList[pInstanzen.MyId[i]], pNode);
  end;
end;

{----------------------------------------------}
procedure TBuildStaTree.WriteArchive(
  pInstanzen: TInstanzList; pArchive: TArchiveList; pRootNode: TTreeNode);
{----------------------------------------------}
var
  i     : integer;
  pNode : TTreeNode;
  p : PStaRecord;
begin
  for i := 0 to pArchive.Count-1 do begin
    New(p);
    p^.GTyp := PStaRecord(pArchive.Objects[i])^.GTyp;
    p^.GName := PStaRecord(pArchive.Objects[i])^.GName;
    p^.InstId := PStaRecord(pArchive.Objects[i])^.InstId;
    p^.MyId := PStaRecord(pArchive.Objects[i])^.MyId;
    pNode := FTree.Items.AddChildObject(pRootNode, pArchive.MyString[i], p);
    pNode.ImageIndex := C_ImageIndex_Archive;
    pNode.SelectedIndex := C_ImageIndex_Archive;

    if (FSichtLevel >= dslKanaele)
    then WriteKanaele(pArchive.KanaeleList[pArchive.MyId[i]], pNode);
  end;
end;

{----------------------------------------------}
procedure TBuildStaTree.WriteLogbuecher(
  pLogbuecher: TLogbuchList; pRootNode: TTreeNode);
{----------------------------------------------}
var
  i : integer;
  pNode : TTreeNode;
  p : PStaRecord;
begin
  for i := 0 to pLogbuecher.Count-1 do begin
    New(p);
    p^.GTyp := PStaRecord(pLogbuecher.Objects[i])^.GTyp;
    p^.GName := PStaRecord(pLogbuecher.Objects[i])^.GName;
    p^.InstId := PStaRecord(pLogbuecher.Objects[i])^.InstId;
    p^.MyId := PStaRecord(pLogbuecher.Objects[i])^.MyId;
    pNode := FTree.Items.AddChildObject(pRootNode, pLogbuecher.LogbuchName[i], p);
    pNode.ImageIndex := C_ImageIndex_Logbuecher;
    pNode.SelectedIndex := C_ImageIndex_Logbuecher;
  end;
end;

{----------------------------------------------}
procedure TBuildStaTree.WriteKanaele(pKanaele: TKanaeleList; pRootNode: TTreeNode);
{----------------------------------------------}
var
  i : integer;
  pNode : TTreeNode;
  p : PStaRecord;
begin
  for i := 0 to pKanaele.Count-1 do begin
    New(p);
    p^.GTyp := PStaRecord(pKanaele.Objects[i])^.GTyp;
    p^.GName := PStaRecord(pKanaele.Objects[i])^.GName;
    p^.InstId := PStaRecord(pKanaele.Objects[i])^.InstId;
    p^.MyId := PStaRecord(pKanaele.Objects[i])^.MyId;
    pNode := FTree.Items.AddChildObject(pRootNode, pKanaele.MyString[i], p);
    pNode.ImageIndex := C_ImageIndex_Kanaele;
    pNode.SelectedIndex := C_ImageIndex_Kanaele;
  end;
end;

{----------------------------------------------}
procedure TBuildStaTree.FillMRGKennungen;
{----------------------------------------------}
var
  pNode : TTreeNode;
begin
  pNode := FTree.Items.GetFirstNode;

  // RootNode 'MRG-Stationen' finden
  while (Assigned(pNode)) do
    if (PStaRecord(pNode.Data)^.GTyp = C_GerArtMrg)
    then Break
    else pNode := pNode.GetNextSibling;

  // Erste MRG-Station finden oder procedure verlassen
  if (Assigned(pNode)) and (pNode.HasChildren)
  then pNode := pNode.GetFirstChild
  else Exit;

  if (FDatabaseName = '') then Exit;

  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    TableName := CDBSta;
    if (not Exists) then Exit;
  finally
    Free;
  end;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    Sql.Add('SELECT ' + C_Sta_MrgId + ',' + C_Sta_StationsName + ',' +
      C_Sta_Kennung);
    Sql.Add('FROM ' + CDBSta);
    Sql.Add('WHERE ' + C_Sta_Aktiv + '= 0');
    if (SortOrder = ssoName) then Sql.Add('ORDER BY ' + C_Sta_StationsName)
    else if (SortOrder = ssoId) then Sql.Add('ORDER BY ' + C_Sta_Kennung);
    Open;

    // Schleife über alle MRG-Stationen (müßte die gleiche Reihenfolge sein ...)
    while (not Eof) do begin
      if (Assigned(pNode)) and
         (PStaRecord(pNode.Data)^.MyId = FieldByName(C_Sta_MrgId).asInteger)
      then begin
        PStaRecord(pNode.Data)^.GName := FieldByName(C_Sta_Kennung).asString;
        if (SortOrder = ssoID) then
          pNode.Text :=
            '[' + FieldByName(C_Sta_Kennung).asString + '] ' + pNode.Text
        else if (SortOrder = ssoName) then
          pNode.Text :=
            pNode.Text + ' [' + FieldByName(C_Sta_Kennung).asString + ']';
        pNode := pNode.GetNextSibling;
      end
      else begin  // Reihenfolge stimmte doch nicht ...
        pNode := pNode.Parent.GetFirstChild;
        while (Assigned(pNode)) do begin
          if (PStaRecord(pNode.Data)^.MyId =
             FieldByName(C_Sta_MrgId).asInteger) then
          begin
            PStaRecord(pNode.Data)^.GName := FieldByName(C_Sta_Kennung).asString;
            if (SortOrder = ssoID) then
              pNode.Text :=
                '[' + FieldByName(C_Sta_Kennung).asString + '] ' + pNode.Text
            else if (SortOrder = ssoName) then
              pNode.Text :=
                pNode.Text + ' [' + FieldByName(C_Sta_Kennung).asString + ']';
            Break;
          end;
          pNode := pNode.GetNextSibling
        end;
      end;
      Next;
    end;
    if (Active) then Close;
  finally
    Free;
  end;
end;

{----------------------------------------------}
procedure TBuildStaTree.WriteKanaele(pRootNode: TTreeNode);
{----------------------------------------------}
var
  pNode   : TTreeNode;
  i       : integer;
  pStaRec : PStaRecord;
begin
  pNode := pRootNode.GetFirstChild;
  while (Assigned(pNode)) do begin
    with FMSStammdaten do begin
      MrgId := PStaRecord(pNode.Data)^.MyId;
      Read;

      for i := 0 to Kanal.Count-1 do begin
        New(pStaRec);
        pStaRec^.GTyp := C_GerArtMrg;
        pStaRec^.GName := Kanal.Kanal[i].Kanalname.asString;
        pStaRec^.InstId := 0;
        pStaRec^.MyId := Kanal.Kanal[i].MrgKanal.asInteger;
        with FTree.Items.AddChildObject(pNode, pStaRec^.GName, pStaRec) do begin
          ImageIndex := C_ImageIndex_Kanaele;
          SelectedIndex := C_ImageIndex_Kanaele;
        end;
      end;
    end;

    pNode := pRootNode.GetNextChild(pNode);
  end;
end;

{-------------------------- TBuildDSfGStationenList ---------------------------}

{----------------------------------------------}
constructor TBuildDSfGStationenList.Create(
  pDSfGStaList: TStrings; sDatabaseName: string);
{----------------------------------------------}
begin
  inherited Create;

  FDatabaseName := sDatabaseName;
  FDSfGStationen := pDSfGStaList;
end;

{----------------------------------------------}
function TBuildDSfGStationenList.FillDSfGStationen: TQueryExt;
{----------------------------------------------}
begin
  Result := TQueryExt.Create(nil);

  if (FDatabaseName = '') then Exit;

  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    TableName := C_DTB_Station;
    if (not Exists) then Exit;
  finally
    Free;
  end;

  with Result do begin
    DatabaseName := FDatabaseName;
    UniDirectional := True;
    Sql.Add('SELECT ' + C_DTF_Station_StationId + ',');
    Sql.Add(C_DTF_Station_Stationsname);
    Sql.Add('FROM ' + ChangeFileExt(C_DTB_Station, ''));
    Sql.Add('ORDER BY ' + C_DTF_Station_StationId);
    Prepare;
    Open;
  end;
end;

{----------------------------------------------}
function TBuildDSfGStationenList.FillDSfGInstanzen: TQueryExt;
{----------------------------------------------}
begin
  Result := TQueryExt.Create(nil);

  if (FDatabaseName = '') then Exit;

  with Result do begin
    DatabaseName := FDatabaseName;
    UniDirectional := True;
    Sql.Add('SELECT *');
    Sql.Add('FROM ' + ChangeFileExt(C_DTB_Instanz, ''));
    Sql.Add('ORDER BY ' + C_DTF_Instanz_StationId + ',' + C_DTF_Instanz_Busadresse);
    Prepare;
    Open;
  end;
end;

{----------------------------------------------}
procedure TBuildDSfGStationenList.Aktualisieren;
{----------------------------------------------}
var
  pQSta, pQInst   : TQuery;
  iStaId, iIStaId : integer;
  sStaName        : string;
  pInstList       : TList;
  pInstData       : PInstanzData;
begin
  if (FDatabaseName <> '') and (Assigned(FDSfGStationen)) then begin
    FDSfGStationen.Clear;
    pQSta := FillDSfGStationen;
    try
      if (pQSta.Active) then begin

        pQInst := FillDSfGInstanzen;
        try
          if (pQInst.Active) then begin
            iIStaId := -1;

            while (not pQSta.Eof) do begin
              sStaName := pQSta.FieldByName(C_DTF_Station_Stationsname).asString;
              iStaId := pQSta.FieldByName(C_DTF_Station_StationId).asInteger;
              pInstList := TList.Create;

              while ((iIStaId = iStaId) or (iIStaId < 0)) and (not pQInst.Eof)
              do begin
                if (iIStaId < 0) then iIStaId :=
                  pQInst.FieldByName(C_DTF_Instanz_StationId).asInteger;

                if (iIStaId = iStaId) then begin
                  New(pInstData);
                  pInstData^.InstanzId := pQInst.FieldByName(C_DTF_Instanz_InstanzId).asInteger;
                  pInstData^.StationId := iIStaId;
                  pInstData^.Instanzname := pQInst.FieldByName(C_DTF_Instanz_Instanzname).asString;
                  pInstData^.Instanztyp := pQInst.FieldByName(C_DTF_Instanz_Instanztyp).asString;
                  pInstData^.Busadresse := pQInst.FieldByName(C_DTF_Instanz_Busadresse).asString;
                  pInstData^.Hersteller := pQInst.FieldByName(C_DTF_Instanz_Hersteller).asString;
                  pInstData^.Geraetetyp := pQInst.FieldByName(C_DTF_Instanz_Geraetetyp).asString;
                  pInstData^.Stand := pQInst.FieldByName(C_DTF_Instanz_Stand).asInteger;
                  pInstData^.FabrikNr := pQInst.FieldByName(C_DTF_Instanz_FabrikNr).asString;
                  pInstData^.SoftwareVersion := pQInst.FieldByName(C_DTF_Instanz_SoftwareVs).asString;
                  pInstData^.Baujahr := pQInst.FieldByName(C_DTF_Instanz_Baujahr).asInteger;
                  pInstData^.Inbetriebnahme := pQInst.FieldByName(C_DTF_Instanz_Inbetriebnahme).asDateTime;
                  pInstData^.GerTypNr := pQInst.FieldByName(C_DTF_Instanz_GerTypNr).asInteger;
                  pInstList.Add(pInstData);
                end;

                pQInst.Next;
                iIStaId :=
                  pQInst.FieldByName(C_DTF_Instanz_StationId).asInteger;
              end;
              FDSfGStationen.AddObject(sStaName, pInstList);
              pQSta.Next;
            end;

            pQInst.Close;
            pQInst.UnPrepare;
          end;
        finally
          pQInst.Free;
        end;

        pQSta.Close;
        pQSta.UnPrepare;
      end;
    finally
      pQSta.Free;
    end;
  end;
end;

{--------------------------- TBuildMRGStationenList ---------------------------}

{----------------------------------------------}
constructor TBuildMRGStationenList.Create(
  pMRGStaList: TStrings; sDatabaseName: string);
{----------------------------------------------}
begin
  inherited Create;

  FDatabaseName := sDatabaseName;
  FMRGStationen := pMRGStaList;
end;

{----------------------------------------------}
procedure TBuildMrgStationenList.Aktualisieren;
{----------------------------------------------}
begin
  FMRGStationen.Clear;
  if (FDatabaseName = '') then Exit;

  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    TableName := CDBSta;
    if (not Exists) then Exit;
  finally
    Free;
  end;

  with TQueryExt.Create(nil) do
  try
    DatabaseName := FDatabaseName;
    UniDirectional := True;
    Sql.Add('SELECT ' + C_Sta_MrgId + ',' + C_Sta_StationsName + ',');
    Sql.Add(C_Sta_Kennung);
    Sql.Add('FROM ' + ChangeFileExt(CDBSta, ''));
    Sql.Add('WHERE ' + C_Sta_Aktiv + '= 0');
    Sql.Add('ORDER BY ' + C_Sta_StationsName);
    Prepare;
    Open;
    while (not Eof) do begin
      FMRGStationen.AddObject(FieldByName(C_Sta_StationsName).asString + Chr(us)
        + FieldByName(C_Sta_Kennung).asString,
        TObject(FieldByName(C_Sta_MrgId).asInteger));
      Next;
    end;
    Close;
    UnPrepare;
  finally
    Free;
  end;
end;

end.
