{------------------------------------------------------------------------------}
{ Stammdaten-Auswahl-Komponente                                                }
{                                                                              }
{ 04.10.2000  GD  Neu                                                          }
{ 28.03.2001  GD  Stammdatenzugriff auf DLL umgestellt                         }
{ 12.04.2001  GD  Anzuzeigende DEAs nach Zugriffstyp filterbar                 }
{ 26.06.2001  GD  Erweitert um Unterlevel (Archive, Kanäle, ...) und MRG       }
{ 16.07.2001  GD  Erweitert um Gruppen                                         }
{ 28.08.2001  GD  Einschränkung auf definierte Instanztypen                    }
{ 15.10.2001  GD  Erweiterung der Sichtlevel                                   }
{ 19.03.2002  GD  Erweiterung um Steuerung                                     }
{ 25.04.2002  GD  Selectierung mit rechter Maustaste verbessert                }
{ 25.05.2002  GD  Mehrfachzweige vermeiden                                     }
{ 08.08.2002  GD  Typdefinitionen in DSG_Utils ausgelagert                     }
{ 02.10.2003  GD  Typdefinitionen erweitert                                    }
{ 30.09.2004  GD  ImageList veröffentlicht                                     }
{ 26.07.2005  GD  Änderungen beim Freigeben                                    }
{ 07.11.2007  WW  resourcestrings                                              }
{ 13.11.2007  GD  Blendenrechner                                               }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, RMG Messtechnik GmbH 2007               }
{------------------------------------------------------------------------------}
unit DSfGStationen;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls,
  GD_Utils, DSG_Utils, WSysCon, DStaDll, DMomLists, DListen;

type
  TDSfGStationen = class(TTreeView)
    constructor Create(AOwner: TComponent; tRootLevel: TDSfGRootLevel;
      tSichtLevel: TDSfGSichtLevel); reintroduce;
    destructor Destroy; override;
  private
    { Private-Deklarationen }
    FRootLevel         : TDSfGRootLevel;
    FSichtLevel        : TDSfGSichtLevel;
    FDSfGStationBitmap : TBitmap;
    FDSfGStationChange : TTVChangedEvent;
    FStationsId        : integer;
    procedure SetDatabaseName(Value: string);
    procedure WriteTreeView;
  protected
    { Protected-Deklarationen }
    FImageList      : TImageList;
    FInstTypen      : string;
    procedure InitMyComponents(aState: boolean); virtual;
    procedure InitImages(aState: boolean); virtual;
    procedure DblClick; override;
    procedure Change(Node: TTreeNode); override;
    procedure ReadDSfGStationen(pRootNode : TTreeNode); virtual;
    procedure ReadDSfGInstanzen(pRootNode : TTreeNode); virtual;
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;  // 25.04.2002 }
  public
    { Public-Deklarationen }
    procedure Clear; virtual;                    // 26.06.2001
    procedure DeleteNode(pNode: TTreeNode); virtual;  // 26.06.2001
    procedure BuildTree; virtual;
    function InsertNode
      (pNode: TTreeNode; bWithChildren: boolean = False): TTreeNode; virtual; // 26.06.2001
    function InsertGruppenNode(pNode: TTreeNode): TTreeNode; virtual; // 16.07.2001
    function GetDKanaele: TAKanaeleDataList;      // 26.06.2001
    function GetDLogbuecher: TLogbuchDataList;    // 26.06.2001
    function GetDLogbuecherByRegInst: TLogbuchDataList;    // 26.06.2001
    function GetMStationen: TStrings;             // 26.06.2001
    function GetDStationen: TStrings;
    procedure DeleteStation(iStaId: integer); virtual; // 28.03.2001
    procedure DeleteInstanz(iStaId: integer; CInstAdr: char); virtual;  // 28.03.2001
    function GetName(                                             // 26.06.2001
      sGerArt: char; pLevel: TDSfGSichtLevel; iId: integer): string;
    function GetStaNameFromInstanz(iInstId: integer): string;     // 26.06.2001
    function GetKennungFromMrgId(iMrgId: integer): string;        // 26.06.2001
    function GetLogbuchFromInstId(iInstId: integer): string;      // 26.06.2001
    function GetInstanzName(iInstId: integer): string; // 26.06.2001
    function GetArchivName(iInstId, iArchGrpNr: integer): string; // 26.06.2001
    property DatabaseName: string write SetDatabaseName;
    property StationsId: integer read FStationsId write FStationsId;
    property InstanzTypen: string read FInstTypen write FInstTypen;
    property MyImageList: TImageList read FImageList;
  published
    { Published-Deklarationen }
    property OnDSfGStationChange: TTVChangedEvent
      read FDSfGStationChange write FDSfGStationChange;
  end;

procedure Register;

implementation

resourcestring
  S_DataType_MRG = 'MRG-Stationen';
  S_DataType_DSfG = 'DSfG-Stationen';

  S_KeineEintraege = 'Keine Einträge';


{$R DSfGStationen.RES}

{---------------------------- Allgemeine Funktionen ---------------------------}

{----------------------------------------------}
procedure Register;
{----------------------------------------------}
begin
  RegisterComponents('Beispiele', [TDSfGStationen]);
end;

{-------------------------------- TDSfGStationen ------------------------------}

{----------------------------------------------}
constructor TDSfGStationen.Create(
  AOwner: TComponent; tRootLevel: TDSfGRootLevel; tSichtLevel: TDSfGSichtLevel);
{----------------------------------------------}
begin
  inherited Create(AOwner);

  Self.RightClickSelect := True;
  InitMyComponents(True);

  FRootLevel := tRootLevel;
  FSichtLevel := tSichtLevel;

  Self.Width := 200;
  ReadOnly := True;
  FStationsId := -1;  // Default
end;

{----------------------------------------------}
destructor TDSfGStationen.Destroy;
{----------------------------------------------}
var
  i : integer;
begin
  FDSfGStationChange := nil;   // 26.07.2005
  InitMyComponents(False);

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

  inherited;
end;

{----------------------------------------------}
procedure TDSfGStationen.Clear;
{----------------------------------------------}
var
  i : integer;
begin
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

{----------------------------------------------}
procedure TDSfGStationen.DeleteNode(pNode: TTreeNode);
{----------------------------------------------}
begin
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

{ Initialisiert die verwendeten Komponenten    }
{ Parameter: Initialisiern (T), Freigeben (F)  }
{----------------------------------------------}
procedure TDSfGStationen.InitMyComponents(aState: boolean);
{----------------------------------------------}
begin
  if (aState) then begin
    InitImages(True);
  end
  else begin
    InitImages(False);
  end;
end;

{----------------------------------------------}
procedure TDSfGStationen.Change(Node: TTreeNode);
{----------------------------------------------}
begin
  if (not (csDestroying in Self.ComponentState)) then begin // 26.07.2005
    if (Assigned(Selected)) then begin
      if (Assigned(FDSfGStationChange)) and (Selected.Level = 1)
      then FDSfGStationChange(Self, Node)
      else if (Assigned(FDSfGStationChange)) then FDSfGStationChange(Self, nil);
    end;

    inherited;
  end;
end;

{----------------------------------------------}
procedure TDSfGStationen.DblClick;
{----------------------------------------------}
begin
  inherited;
end;

{ Initialisiert die verwendeten Bitmaps        }
{ Parameter: Initialisiern (T), Freigeben (F)  }
{----------------------------------------------}
procedure TDSfGStationen.InitImages(aState: boolean);
{----------------------------------------------}
begin
  if (aState) then begin
    if (not Assigned(FImageList)) then begin
      FImageList := TImageList.Create(Self);
      FImageList.Masked := False;
      FImageList.Height := 16;
      FImageList.Width := 16;
    end;

    Self.Images := FImageList;

    if (not Assigned(FDSfGStationBitmap)) then begin
      FDSfGStationBitmap := TBitmap.Create;
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Aktiv);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Inaktiv);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Umwerter);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Registrierung);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Wieser);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_DFU);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Gasbeschaffenheit);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_unbestimmt);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Ordner);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Parameter);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_MRG2100);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_MRG2200);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_AnwOrdner);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_AnwParameter);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_MRGStation_Aktiv);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_MRGStation_InAktiv);
      FImageList.Add(FDSfGStationBitmap, nil);
      // 19.03.2002
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSFGSTATION_STEUERUNG);
      FImageList.Add(FDSfGStationBitmap, nil);
      // 02.10.2003
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_REVISION);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_KORRGASBESCH);
      FImageList.Add(FDSfGStationBitmap, nil);

      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_Gruppe_Aktiv);
      FImageList.Add(FDSfGStationBitmap, nil);
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_Gruppe_InAktiv);
      FImageList.Add(FDSfGStationBitmap, nil);
      // 13.11.2007
      FDSfGStationBitmap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Blende);
      FImageList.Add(FDSfGStationBitmap, nil);
    end;
  end

  else begin
    if (Assigned(FDSfGStationBitmap)) then begin
      FDSfGStationBitmap.Free;
      FDSfGStationBitmap := nil;
    end;

    if (Assigned(FImageList)) then begin
      FImageList.Free;
      FImageList := nil;
    end;
  end;
end;

{ Setzt den Datenbanknamen für Datenzugriffe   }
{ Parameter: Datenbankname                     }
{----------------------------------------------}
procedure TDSfGStationen.SetDatabaseName(Value: string);
{----------------------------------------------}
begin
  if (ClientStammdaten.DStaDatabaseName <> Value) then begin
    ClientStammdaten.Free;
    ClientStammdaten := TClientStammdaten.Create(Value);

    WriteTreeView;
  end;
end;

{ Schreibt Stationsinfos aus Datenbank in Baum }
{ Parameter: RootNode                          }
{----------------------------------------------}
procedure TDSfGStationen.ReadDSfGStationen(pRootNode : TTreeNode);
{----------------------------------------------}
var
  i     : integer;
  pNode : TTreeNode;
  pSl   : TStringList;
begin
  if (ClientStammdaten.DStaDatabaseName <> '') then begin

    pSl := TStringList.Create;
    try

      for i := 0 to ClientStammdaten.DStationsListe.Count-1 do
        pSl.Add(ClientStammdaten.DStationsListe[i]);
      pSl.Sort;

      for i := 0 to pSl.Count-1 do begin
        if (not Assigned(pRootNode)) then begin
          pRootNode := Self.Items.Add(nil, S_DataType_DSfG);
          pRootNode.ImageIndex := 0;
          pRootNode.SelectedIndex := 0;
        end;

        pNode := Self.Items.AddChild(pRootNode, pSl[i]);
        pNode.ImageIndex := 1;
        pNode.SelectedIndex := 0;
      end;

      if (Assigned(pRootNode)) then pRootNode.Expand(False);

    finally
       pSl.Free;
    end;

  end;
end;

{ Schreibt Instanzinfos aus Datenbank in Baum  }
{ Parameter: RootNode                          }
{----------------------------------------------}
procedure TDSfGStationen.ReadDSfGInstanzen(pRootNode : TTreeNode);
{----------------------------------------------}
var
  i, j      : integer;
  c         : char;
  s         : string;
  pNode     : TTreeNode;
begin
  if (ClientStammdaten.DStaDatabaseName <> '') and (StationsId > -1) then begin

    s := ClientStammdaten.DStationsName(StationsId);
    if (s = '') then raise Exception.Create(
      'StationsId ''' + IntToStr(Self.StationsId) + ''' nicht vorhanden !');

    if (not Assigned(pRootNode)) then begin
      pRootNode :=
        Self.Items.Add(nil, s);
      pRootNode.ImageIndex := 0;
      pRootNode.SelectedIndex := 0;
    end;

    with ClientStammdaten.FillDInstanzListe(s) do begin

      for i := 0 to Count-1 do begin
        c := InstanzTyp[i];
        if (FInstTypen <> '') and (Pos(c, FInstTypen) = 0) then Continue;  // 28.08.2001

        case c of
          C_D_Instanztyp_Umw : j := C_ImageIndex_Umwerter;
          C_D_Instanztyp_Reg : j := C_ImageIndex_Registrierung;
          C_D_Instanztyp_Wieser : j := C_ImageIndex_Wieser;
          C_D_Instanztyp_DFU : j := C_ImageIndex_DFU;
          C_D_Instanztyp_Gas : j := C_ImageIndex_Gasbeschaffenheit;
          C_D_Instanztyp_Strg : j := C_ImageIndex_Steuerung;
          C_D_Instanztyp_Rev : j := C_ImageIndex_REVISION;
          C_D_Instanztyp_KGM : j := C_ImageIndex_KORRGASBESCH;
          C_D_Instanztyp_Blende : j := C_ImageIndex_KORRGASBESCH;
          C_D_Instanztyp_unbest : j := C_ImageIndex_unbestimmt;
          else j := -1;
        end;

        s := InstanzAdresse[i] + ' - ' + InstanzName[i];
        pNode := Self.Items.AddChild(pRootNode, s);
        pNode.ImageIndex := j;
        pNode.SelectedIndex := j;
      end;

    end;

    if (Assigned(pRootNode)) then pRootNode.Expand(False);

  end;
end;

{ Baut Auswahlbaum auf                         }
{----------------------------------------------}
procedure TDSfGStationen.WriteTreeView;
{----------------------------------------------}
var
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    Self.Items.BeginUpdate;
    try

      Self.Clear;

      if (FRootLevel = drlDSfG) then begin  // 26.06.2001
        if (FSichtLevel = dslStation) then ReadDSfGStationen(nil)
        else begin
          ReadDSfGStationen(nil);
        end;
      end
      else if (FRootLevel = drlStation) and (StationsId > -1)
        then ReadDSfGInstanzen(nil);

    finally
      Self.Items.EndUpdate;
    end;

    if (Self.Items.Count = 0) then Items.Add(nil, S_KeineEintraege);
  finally
    Screen.Cursor := oldCursor;
  end;
end;

{ Baut Baum auf                                }
{----------------------------------------------}
procedure TDSfGStationen.BuildTree;
{----------------------------------------------}
begin
  if (ClientStammdaten.DStaDatabaseName <> '') then WriteTreeView;
end;

{ Löscht Station aus Baum                      }
{ Parameter: Stations-ID                       }
{----------------------------------------------}
procedure TDSfGStationen.DeleteStation(iStaId: integer); // 28.03.2001
{----------------------------------------------}
begin
end;

{ Löscht Station aus Baum                      }
{ Parameter: Stations-ID, Instanzadresse       }
{----------------------------------------------}
procedure TDSfGStationen.DeleteInstanz(iStaId: integer; cInstAdr: char); // 28.03.2001
{----------------------------------------------}
var
  i : integer;
begin
  if (FRootLevel = drlStation) and (StationsId > -1) then
    for i := Items.Count-1 downto 0 do
      if (Items[i].Level = 1) and (Copy(Items[i].Text, 1, 4) = cInstAdr + ' - ')
      then begin
        Self.DeleteNode(Items[i]);
        Break;
      end;
end;

{----------------------------------------------}
function TDSfGStationen.InsertNode(
  pNode: TTreeNode; bWithChildren: boolean = False): TTreeNode;  // 26.06.2001
{----------------------------------------------}

  procedure AddChildren(pParent, pTarget: TTreeNode);
  var
    p        : PStaRecord;
    pn1, pn2 : TTreeNode;
  begin
    while pTarget.HasChildren do DeleteNode(pTarget.GetFirstChild); // Schnellversion

    if (pParent.HasChildren) then begin
      pn1 := pParent.GetFirstChild;
      while (Assigned(pn1)) do begin

        p := nil;
        if (Assigned(pn1.Data)) then begin
          New(p);
          p^.GTyp := PStaRecord(pn1.Data)^.GTyp;
          p^.GName := PStaRecord(pn1.Data)^.GName;
          p^.InstId := PStaRecord(pn1.Data)^.InstId;
          p^.MyId := PStaRecord(pn1.Data)^.MyId;
        end;

        pn2 := Items.AddChildObject(pTarget, pn1.Text, p);
        pn2.ImageIndex := pn1.ImageIndex;
        pn2.SelectedIndex := pn1.SelectedIndex;
        AddChildren(pn1, pn2);

        pn1 := pParent.GetNextChild(pn1);
      end;
    end;
  end;

var
  pChildNode, pParentNode : TTreeNode;
  p                       : PStaRecord;
begin
  Result := nil;

  // Gruppen werden gesondert behandelt
  if (PStaRecord(pNode.Data)^.GTyp = C_GerArtGruppe) then begin
    Result := InsertGruppenNode(pNode);
    Exit;
  end;

  Items.BeginUpdate;
  try
    if (pNode.Level > 0)  then
    begin
      pParentNode := InsertNode (pNode.Parent);
      if (Items.Count > 0) then begin
        pChildNode := pParentNode.GetFirstChild;
        while (Assigned(pChildNode)) do begin
          if (pChildNode.Text = pNode.Text) and
            (PStaRecord(pChildNode.Data)^.InstId = PStaRecord(pNode.Data)^.InstId)
            and (PStaRecord(pChildNode.Data)^.MyId = PStaRecord(pNode.Data)^.MyId)
          then begin
            Result := pChildNode;
            if (bWithChildren) then AddChildren(pNode, Result);
            Exit;
          end;
          pChildNode := pParentNode.GetNextChild (pChildNode);
        end;
      end;

      if (Assigned(pNode.Data)) then begin
        New(p);
        p^.GTyp := PStaRecord(pNode.Data)^.GTyp;
        p^.GName := PStaRecord(pNode.Data)^.GName;
        p^.InstId := PStaRecord(pNode.Data)^.InstId;
        p^.MyId := PStaRecord(pNode.Data)^.MyId;
      end
      else p := nil;

      Result := Items.AddChildObject(pParentNode, pNode.Text, p);
      Result.ImageIndex := pNode.ImageIndex;
      Result.SelectedIndex := pNode.SelectedIndex;

      if (bWithChildren) then AddChildren(pNode, Result);
    end

    else begin
      if (Items.Count > 0) then begin
        pParentNode := Items.GetFirstNode;
        while (Assigned(pParentNode)) do begin
          if (PStaRecord(pParentNode.Data)^.InstId = PStaRecord(pNode.Data)^.InstId)
            and (PStaRecord(pParentNode.Data)^.MyId = PStaRecord(pNode.Data)^.MyId)
          then begin
            Result := pParentNode;
            if (bWithChildren) then AddChildren(pNode, Result);
            Exit;
          end;
          pParentNode := pParentNode.GetNextSibling;
        end;
      end;

      if (not Assigned(Result)) then begin
        if (Assigned(pNode.Data)) then begin
          New(p);
          p^.GTyp := PStaRecord(pNode.Data)^.GTyp;
          p^.GName := PStaRecord(pNode.Data)^.GName;
          p^.InstId := PStaRecord(pNode.Data)^.InstId;
          p^.MyId := PStaRecord(pNode.Data)^.MyId;
        end
        else p := nil;

        Result := Self.Items.AddObject(nil, pNode.Text, p);
        Result.ImageIndex := pNode.ImageIndex;
        Result.SelectedIndex := pNode.SelectedIndex;
        if (bWithChildren) then AddChildren(pNode, Result);
      end;
    end;
  finally
    Items.EndUpdate;
  end;
end;

{----------------------------------------------}
function TDSfGStationen.InsertGruppenNode(pNode: TTreeNode): TTreeNode;  // 16.07.2001
{----------------------------------------------}
var
  pChildNode, pParentNode : TTreeNode;
  p                       : PStaRecord;
  i                       : integer;
  pSl, pSlSumme           : TStrings;
begin
  Result := nil;

  Items.BeginUpdate;
  try

    pSlSumme := TStringList.Create;
    try

      // Alle im Knoten enthaltenen MRG-Stationen zusammensammeln
      // Strings haben Struktur: GerArt<us>GerId<us>Name<us>Kennung
      if (pNode.Level = 1) then begin      // Eine Gruppe
        pSl := TStringList(PStaRecord(pNode.Data)^.InstId);
        for i := 0 to pSl.Count-1 do begin
          pSlSumme.Add(pSl[i]);
        end;
      end
      else if (pNode.Level = 0) then begin  // Alle Gruppen
        pChildNode := pNode.GetFirstChild;
        while (Assigned(pChildNode)) do begin
          pSl := TStringList(PStaRecord(pChildNode.Data)^.InstId);
          for i := 0 to pSl.Count-1 do begin
            pSlSumme.Add(pSl[i]);
          end;
          pChildNode := pNode.GetNextChild(pChildNode);
        end;
      end
      else Exit;

      // Temporären Baum für diese Stationen erzeugen
      with TDSfGStationen.Create(Self, drlDSfG, dslNone) do
      try
        Parent := Self.Parent;
        Items.BeginUpdate;

        // Rootnode
        New(p);
        p^.GTyp := C_GerArtMrg;
        p^.GName := S_DataType_MRG;
        p^.InstId := 0;
        p^.MyId := 2;
        pParentNode := Items.AddObject(nil, p^.GName, p);
        pParentNode.ImageIndex := C_ImageIndex_M_Aktiv;
        pParentNode.SelectedIndex := C_ImageIndex_M_Aktiv;

        // Stations-Nodes
        for i := 0 to pSlSumme.Count-1 do begin
          New(p);
          p^.GTyp := GetStringPart(pSlSumme[i], 1)[1];
          p^.GName := GetStringPart(pSlSumme[i], 4);
          p^.InstId := 0;
          p^.MyId := StrToInt(GetStringPart(pSlSumme[i], 2));
          with Items.AddChildObject(
            pParentNode, GetStringPart(pSlSumme[i], 3), p) do
          begin
            ImageIndex := C_ImageIndex_M_Aktiv;
            SelectedIndex := C_ImageIndex_M_Inaktiv;
          end;
        end;

        // MRG-Zweig einfügen
        Result := Self.InsertNode(pParentNode, True);

        Items.EndUpdate;
      finally
        Free;
      end;

    finally
      pSlSumme.Free;
    end;

  finally
    Items.EndUpdate;
  end;
end;

{----------------------------------------------}
function TDSfGStationen.GetDKanaele: TAKanaeleDataList;  // 26.06.2001
{----------------------------------------------}
var
  pKanRec : PAKanaeleData;
  pStaRec : PStaRecord;
  i       : integer;
begin
  Result := TAKanaeleDataList.Create;

  for i := 0 to Items.Count-1 do begin
    pStaRec := PStaRecord(Items[i].Data);
    if (pStaRec^.GTyp = C_GerArtDSfG) and (Items[i].Level = C_Level_Kanaele)
    then begin
      New(pKanRec);
      FillChar(pKanRec^, SizeOf(TAKanaeleData), 0);
      pKanRec^.KanalNr := pStaRec^.MyId;
      pKanRec^.InstanzId := pStaRec^.InstId;
      pKanRec^.ArchivNr := PStaRecord(Items[i].Parent.Data)^.MyId;
      pKanRec^.Name := pStaRec^.GName;
      Result.Add(pKanRec);
    end;
  end;
end;

{----------------------------------------------}
function TDSfGStationen.GetDLogbuecher: TLogbuchDataList;  // 26.06.2001
{----------------------------------------------}
var
  pArcRec : PLogbuchData;
  pStaRec : PStaRecord;
  i       : integer;
begin
  Result := TLogbuchDataList.Create;

  for i := 0 to Items.Count-1 do begin
    pStaRec := PStaRecord(Items[i].Data);
    if (pStaRec^.GTyp = C_GerArtDSfG) and
       (Items[i].Level = C_Level_Logbuecher) and
       (Items[i].ImageIndex = C_ImageIndex_Logbuecher) then
    begin
      New(pArcRec);
      FillChar(pArcRec^, SizeOf(TLogbuchData), 0);
//      pArcRec^.InstanzId := pStaRec^.InstId;  // InstID jetzt quellabhängig !
      pArcRec^.InstanzId := PStaRecord(Items[i].Parent.Data)^.MyId;
      pArcRec^.LogbuchNr := pStaRec^.MyId;
      if (Length(pStaRec^.GName) > 0) then pArcRec^.EAdr := pStaRec^.GName[1];
      if (Length(pStaRec^.GName) > 1) then
        pArcRec^.Name := Copy(pStaRec^.GName, 2, Length(pStaRec^.GName)-1);
      Result.Add(pArcRec);
    end;
  end;
end;

{ Gibt Logbücher zugeordnet zur Reg.Inst. zur. }
{----------------------------------------------}
function TDSfGStationen.GetDLogbuecherByRegInst: TLogbuchDataList;  // 25.04.2001
{----------------------------------------------}
var
  pArcRec : PLogbuchData;
  pStaRec : PStaRecord;
  i       : integer;
begin
  Result := TLogbuchDataList.Create;

  for i := 0 to Items.Count-1 do begin
    pStaRec := PStaRecord(Items[i].Data);
    if (pStaRec^.GTyp = C_GerArtDSfG) and
       (Items[i].Level = C_Level_Logbuecher) and
       (Items[i].ImageIndex = C_ImageIndex_Logbuecher) then
    begin
      New(pArcRec);
      FillChar(pArcRec^, SizeOf(TLogbuchData), 0);
      pArcRec^.InstanzId := pStaRec^.InstId;  // InstID jetzt quellabhängig !
//      pArcRec^.InstanzId := PStaRecord(Items[i].Parent.Data)^.MyId;
      pArcRec^.LogbuchNr := pStaRec^.MyId;
      if (Length(pStaRec^.GName) > 0) then pArcRec^.EAdr := pStaRec^.GName[1];
      if (Length(pStaRec^.GName) > 1) then
        pArcRec^.Name := Copy(pStaRec^.GName, 2, Length(pStaRec^.GName)-1);
      Result.Add(pArcRec);
    end;
  end;
end;

{----------------------------------------------}
function TDSfGStationen.GetMStationen: TStrings;        // 26.06.2001
{----------------------------------------------}
var
  pStaRec : PStaRecord;
  i       : integer;
begin
  Result := TStringList.Create;

  for i := 0 to Items.Count-1 do begin
    pStaRec := PStaRecord(Items[i].Data);

    if (Items[i].Level = C_Level_Stationen) and (pStaRec^.GTyp = C_GerArtMrg)
    then Result.Add(IntToStr(pStaRec^.MyId) + Chr(us) + pStaRec^.GName);
  end;
end;

{----------------------------------------------}
function TDSfGStationen.GetDStationen: TStrings;
{----------------------------------------------}
var
  pStaRec : PStaRecord;
  i       : integer;
begin
  Result := TStringList.Create;

  for i := 0 to Items.Count-1 do begin
    pStaRec := PStaRecord(Items[i].Data);

    if (Items[i].Level = C_Level_Stationen) and (pStaRec^.GTyp = C_GerArtDSfG)
    then Result.Add(IntToStr(pStaRec^.MyId) + Chr(us) + pStaRec^.GName);
  end;
end;

{----------------------------------------------}
function TDSfGStationen.GetName(
  sGerArt: char; pLevel: TDSfGSichtLevel; iId: integer): string;
{----------------------------------------------}
var
  p, pNode : TTreeNode;
begin
  Result := '';

  // Zweig der Geräteart lokalisieren
  pNode := Self.Items.GetFirstNode;
  while (Assigned(pNode)) do begin
    if (PStaRecord(pNode.Data)^.GTyp = sGerArt) then Break;
    pNode := pNode.GetNextSibling;
  end;
  if (not Assigned(pNode)) or (not pNode.HasChildren) then Exit; // Kein Eintrag

  pNode := pNode.GetFirstChild;

  // Gesucht wird ein Stationsname
  if (pLevel = dslStation) then begin
    while (Assigned(pNode)) do begin
      if (PStaRecord(pNode.Data)^.MyId = iId) then begin
        Result := pNode.Text;
        Break;
      end;
      pNode := pNode.GetNextSibling;
    end;
  end

  // Gesucht wird ein Instanzname
  else if (sGerArt = C_GerArtDSfG) and (pLevel = dslInstanzen) then begin
    while (Assigned(pNode)) do begin
      p := pNode.GetFirstChild;
      while (Assigned(p)) do begin
        if (PStaRecord(p.Data)^.MyId = iId) then Break;
        p := pNode.GetNextChild(p);
      end;
      if (Assigned(p)) then begin
        Result := p.Text;
        Break;
      end;
      pNode := pNode.GetNextSibling;
    end;
  end;
end;

{----------------------------------------------}
function TDSfGStationen.GetStaNameFromInstanz(iInstId: integer): string;
{----------------------------------------------}
var
  p, pNode : TTreeNode;
begin
  Result := '';

  // Zweig der Geräteart lokalisieren
  pNode := Self.Items.GetFirstNode;
  while (Assigned(pNode)) do begin
    if (PStaRecord(pNode.Data)^.GTyp = C_GerArtDSfG) then Break;
    pNode := pNode.GetNextSibling;
  end;
  if (not Assigned(pNode)) or (not pNode.HasChildren) then Exit; // Kein Eintrag

  pNode := pNode.GetFirstChild;

  // Gesucht wird die Instanz zur Station
  while (Assigned(pNode)) do begin
    p := pNode.GetFirstChild;
    while (Assigned(p)) do begin
      if (PStaRecord(p.Data)^.MyId = iInstId) then Break;
      p := pNode.GetNextChild(p);
    end;
    if (Assigned(p)) then begin
      Result := p.Parent.Text;
      Break;
    end;
    pNode := pNode.GetNextSibling;
  end;
end;

{----------------------------------------------}
function TDSfGStationen.GetKennungFromMrgId(iMrgId: integer): string;
{----------------------------------------------}
var
  pNode : TTreeNode;
begin
  Result := '';

  // Zweig der Geräteart lokalisieren
  pNode := Self.Items.GetFirstNode;
  while (Assigned(pNode)) do begin
    if (PStaRecord(pNode.Data)^.GTyp = C_GerArtMrg) then Break;
    pNode := pNode.GetNextSibling;
  end;
  if (not Assigned(pNode)) or (not pNode.HasChildren) then Exit; // Kein Eintrag

  pNode := pNode.GetFirstChild;

  // Gesucht wird eine Station zur Id
  while (Assigned(pNode)) do begin
    if (PStaRecord(pNode.Data)^.MyId = iMrgId) then begin
      Result := PStaRecord(pNode.Data)^.GName;
      Break;
    end;
    pNode := pNode.GetNextSibling;
  end;
end;

{----------------------------------------------}
function TDSfGStationen.GetLogbuchFromInstId(iInstId: integer): string;
{----------------------------------------------}
var
  p, pNode : TTreeNode;
begin
  Result := '';

  // Zweig der Geräteart lokalisieren
  pNode := Self.Items.GetFirstNode;
  while (Assigned(pNode)) do begin
    if (PStaRecord(pNode.Data)^.GTyp = C_GerArtDSfG) then Break;
    pNode := pNode.GetNextSibling;
  end;
  if (not Assigned(pNode)) or (not pNode.HasChildren) then Exit; // Kein Eintrag

  pNode := pNode.GetFirstChild;

  // Gesucht wird die Instanz zur Station
  while (Assigned(pNode)) do begin
    p := pNode.GetFirstChild;
    while (Assigned(p)) do begin
      // Instanz gefunden, suche Logbuch
      if (PStaRecord(p.Data)^.MyId = iInstId) then begin
        pNode := p;
        p := pNode.GetFirstChild;
        while (Assigned(p)) do begin
          if (p.ImageIndex = C_ImageIndex_Logbuecher) then begin
            Result := p.Text;
            Break;
          end;
          p := pNode.GetNextChild(p);
        end;
        Exit; // Procedure auf jeden Fall verlassen (Zeiger verschoben)
      end;
      p := pNode.GetNextChild(p);
    end;
    if (Result <> '') then Break;
    pNode := pNode.GetNextSibling;
  end;
end;

{----------------------------------------------}
function TDSfGStationen.GetInstanzName(iInstId: integer): string;
{----------------------------------------------}
var
  p, pNode : TTreeNode;
begin
  Result := '';

  // Zweig der Geräteart lokalisieren
  pNode := Self.Items.GetFirstNode;
  while (Assigned(pNode)) do begin
    if (PStaRecord(pNode.Data)^.GTyp = C_GerArtDSfG) then Break;
    pNode := pNode.GetNextSibling;
  end;
  if (not Assigned(pNode)) or (not pNode.HasChildren) then Exit; // Kein Eintrag

  pNode := pNode.GetFirstChild;

  // Gesucht wird die Instanz zur Station
  while (Assigned(pNode)) do begin
    p := pNode.GetFirstChild;
    while (Assigned(p)) do begin
      // Instanz gefunden
      if (PStaRecord(p.Data)^.MyId = iInstId) then begin
        Result := p.Text;
        Exit;
      end;
      p := pNode.GetNextChild(p);
    end;
    pNode := pNode.GetNextSibling;
  end;
end;

{----------------------------------------------}
function TDSfGStationen.GetArchivName(iInstId, iArchGrpNr: integer): string;
{----------------------------------------------}
var
  p, pNode : TTreeNode;
begin
  Result := '';

  // Zweig der Geräteart lokalisieren
  pNode := Self.Items.GetFirstNode;
  while (Assigned(pNode)) do begin
    if (PStaRecord(pNode.Data)^.GTyp = C_GerArtDSfG) then Break;
    pNode := pNode.GetNextSibling;
  end;
  if (not Assigned(pNode)) or (not pNode.HasChildren) then Exit; // Kein Eintrag

  pNode := pNode.GetFirstChild;

  // Gesucht wird die Instanz zur Station
  while (Assigned(pNode)) do begin
    p := pNode.GetFirstChild;
    while (Assigned(p)) do begin
      // Instanz gefunden, suche Archivgruppe
      if (PStaRecord(p.Data)^.MyId = iInstId) then begin
        pNode := p;
        p := pNode.GetFirstChild;
        while (Assigned(p)) do begin
          // Archivgruppe gefunden
          if (p.ImageIndex = C_ImageIndex_Archive) and
             (PStaRecord(p.Data)^.MyId = iArchGrpNr) then
          begin
            Result := p.Text;
            Break;
          end;
          p := pNode.GetNextChild(p);
        end;
        Exit; // Procedure auf jeden Fall verlassen (Zeiger verschoben)
      end;
      p := pNode.GetNextChild(p);
    end;
    pNode := pNode.GetNextSibling;
  end;
end;

{----------------------------------------------}
procedure TDSfGStationen.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);  // 25.04.2002
{----------------------------------------------}
begin
  if (Items.Count > 0) then Selected := GetNodeAt(X, Y);

  inherited;
end;

end.
