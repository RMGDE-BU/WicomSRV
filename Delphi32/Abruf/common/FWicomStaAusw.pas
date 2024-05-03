{------------------------------------------------------------------------------}
{ Zusammenstellung WICOM-Stationen/-.../-Kanälen                               }
{                                                                              }
{ -> Benötigt DSta.Dll                                                         }
{                                                                              }
{ 24.04.2002  GD  Neu                                                          }
{ 11.03.2002  GD  ASCIIDIR entfernt                                            }
{                                                                              }
{ (C) Karl Wieser GmbH 2002                                                    }
{------------------------------------------------------------------------------}
unit FWicomStaAusw;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, Menus,
  DSfGStationen, PathIni, DStaDll, WSysCon, DMomLists, DListen, GD_Utils,
  DSG_Utils;

type
  TWicomAuswahlIni = class(TProgramIni)
  private
    function GetSortOrder: byte;
    procedure SetSortOrder(iSortOrder: byte);
    function GetZuordnung: byte;
    procedure SetZuordnung(iZuordnung: byte);
  protected
  public
    property SortOrder: byte read GetSortOrder write SetSortOrder;
    property Zuordnung: byte read GetZuordnung write SetZuordnung;
  end;

  TFormWicomStationsAuswahl = class(TForm)
    pnSourceTree: TPanel;
    Splitter: TSplitter;
    pnSinkTree: TPanel;
    pnMoveButtons: TPanel;
    pnButtons: TPanel;
    sbtnAddSingle: TSpeedButton;
    sbtnAddAll: TSpeedButton;
    sbtnRemoveSingle: TSpeedButton;
    sbtnRemoveAll: TSpeedButton;
    PopupMenu: TPopupMenu;
    pmiAddAll: TMenuItem;
    pmiAdd: TMenuItem;
    pmiRemove: TMenuItem;
    pmiRemoveAll: TMenuItem;
    N1: TMenuItem;
    pmiSortierung: TMenuItem;
    pmiZuordnung: TMenuItem;
    pmiSortName: TMenuItem;
    pmiSortIDNummer: TMenuItem;
    pmiZ2LogbuchbeiQuelleArchivkanlebeiRegistrierung: TMenuItem;
    pmiZ3LogbuchbeiRegistrierungArchivkanlebeiQuelle: TMenuItem;
    pmiZ0AlleArchivebeiRegstrierung: TMenuItem;
    pmiZ1AlleArchivebeiQuelle: TMenuItem;
    pmiZ4LogbuchbeiQuelleArchivkanlebeiRegistrierungundQuelle: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbtnAddAllClick(Sender: TObject);
    procedure sbtnAddSingleClick(Sender: TObject);
    procedure sbtnRemoveSingleClick(Sender: TObject);
    procedure sbtnRemoveAllClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure pmiSortTypeClick(Sender: TObject);
    procedure pmiZuordnungsTypeClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FStationsId    : integer;
    FTreeSource    : TDSfGStationen;
    FTreeSink      : TDSfGStationen;
    FInitClientSta : boolean;
    FInitPathSrv   : boolean;
    FInstTypen     : TCharSet;
    FGeraeteTypen  : TCharSet;
    FSortOrder     : byte;
    FZuordnung     : byte;
    FAuswahlIni    : TWicomAuswahlIni;
    procedure TVDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TVDragDrop(Sender, Source: TObject; X, Y: Integer);
  protected
    { Protected-Deklarationen }
    procedure InitComponents(bState: boolean); virtual;
    procedure BuildSourceTree; virtual;
    procedure SetPopupMenuProps; virtual;
    procedure AddSelected; virtual;
    procedure AddAll; virtual;
    procedure RemoveSelected; virtual;
    procedure RemoveAll; virtual;
    property TreeSink: TDSfGStationen read FTreeSink;
    property TreeSource: TDSfGStationen read FTreeSource;
    property StationsId: integer read FStationsId write FStationsId;
    property InstTypen: TCharSet read FInstTypen write FInstTypen;
    property GeraeteTypen: TCharSet read FGeraeteTypen write FGeraeteTypen;
    property SortOrder: byte read FSortOrder write FSortOrder;
    property Zuordnung: byte read FZuordnung write FZuordnung;
  public
    { Public-Deklarationen }
    constructor CreateSelected(
      pOwner: TComponent; cGerArt: char; iStaId: integer);
    procedure SetStationsAuswahl(cGerArt: char; iStationsId: integer);
    procedure Aktualisieren;
    function GetDKanaele: TAKanaeleDataList;
    function GetDLogBuecher: TLogbuchDataList;
    function GetDLogBuecherRegInst: TLogbuchDataList;
    function GetMStationen: TStrings;
    function GetDStationen: TStrings;
  end;

implementation

{$R *.DFM}

const
  C_OneStationConstructor : boolean = False;

  C_Section_AuswahlEinstellungen = 'AUSWAHLEINSTELUNGEN';

  C_Ident_Sortierung             = 'SORTIERUNG';
  C_Ident_Zuordnung              = 'ZUORDNUNG';

{------------------------ TWicomAuswahlIni ---------------------------}

{----------------------------------------------------}
function TWicomAuswahlIni.GetSortOrder: byte;
{----------------------------------------------------}
begin
  Result := ReadInteger(C_Section_AuswahlEinstellungen, C_Ident_Sortierung, 1);
end;

{----------------------------------------------------}
procedure TWicomAuswahlIni.SetSortOrder(iSortOrder: byte);
{----------------------------------------------------}
begin
  WriteInteger(C_Section_AuswahlEinstellungen, C_Ident_Sortierung, iSortOrder);
end;

{----------------------------------------------------}
function TWicomAuswahlIni.GetZuordnung: byte;
{----------------------------------------------------}
begin
  Result := ReadInteger(C_Section_AuswahlEinstellungen, C_Ident_Zuordnung, 1);
end;

{----------------------------------------------------}
procedure TWicomAuswahlIni.SetZuordnung(iZuordnung: byte);
{----------------------------------------------------}
begin
  WriteInteger(C_Section_AuswahlEinstellungen, C_Ident_Zuordnung, iZuordnung);
end;

{------------------------ TFormWicomStationsAuswahl ---------------------------}

{ Alternativer Constructor für Init. mit einer Stat. }
{----------------------------------------------------}
constructor TFormWicomStationsAuswahl.CreateSelected(
  pOwner: TComponent; cGerArt: char; iStaId: integer);
{----------------------------------------------------}
begin
  C_OneStationConstructor := True;
  FStationsId := iStaId;
  FGeraeteTypen  := [cGerArt];

  inherited Create(pOwner);
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.FormCreate(Sender: TObject);
{----------------------------------------------------}
begin
  // INI-Objekt
  FAuswahlIni := TWicomAuswahlIni.Create;

  if (not C_OneStationConstructor) then FStationsId := -1; // Alle Stationen anzeigen
  // anzuz. Instanztypen ([]=Alle)
  FInstTypen := ['D', 'G', 'U', 'R', 'W', 'S', 'P', 'M', 'X'];
  // anzuz. Gerätetypen ([]=Alle)
  if (not C_OneStationConstructor) then
    FGeraeteTypen  := [C_GerArtDSfG, C_GerArtMrg, C_GerArtGruppe];
  FSortOrder := FAuswahlIni.SortOrder; // Sortierung (Id=0, Name=1)
  FZuordnung := FAuswahlIni.Zuordnung; // 0=Registr., 1=Quelle, 2=Logb-Q, 3=Arch-Q

  C_OneStationConstructor := False;

  InitComponents(True);
  BuildSourceTree;
  SetPopupMenuProps;
  FormResize(Self);
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.FormDestroy(Sender: TObject);
{----------------------------------------------------}
begin
  InitComponents(False);
  FAuswahlIni.Free;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.FormResize(Sender: TObject);
{----------------------------------------------------}
begin
  if (not (csDestroying in Self.ComponentState)) then begin
    pnSourceTree.Width := (ClientWidth - (pnMoveButtons.Width div 2)) div 2;
    pnButtons.Top := (pnSinkTree.Height - pnButtons.Height) div 2;
  end;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.InitComponents(bState: boolean);
{----------------------------------------------------}

  {--------------------------------------------------}
  procedure CreateStaTree(var pTree: TDSfGStationen; pParent: TWinControl);
  {--------------------------------------------------}
  begin
    pTree := TDSfGStationen.Create(Self, drlDSfG, dslNone);
    pTree.Parent := pParent;
    pTree.Align := alClient;
    pTree.PopupMenu := PopupMenu;
    pTree.DragMode := dmAutomatic;
    pTree.OnDragOver := TVDragOver;
    pTree.OnDragDrop := TVDragDrop;
  end;

begin
  if (bState) then begin

    // Ggf. Pathserver initialisieren
    if (not Assigned(PathServer)) then begin
      with TProgramIni.Create do
      try
        PathServer := TPathServer.Create(
          WieserIniFile, [WStammDir, WWorkDir, DArchivDir, DManuDir,
            LangzeitDir, ManuDir]);  // Pathlist für Export ...
        PathServer.Check;
      finally
        Free;
      end;
      FInitPathSrv := True;
    end
    else FInitPathSrv := False;

    if (not Assigned(ClientStammdaten)) then begin
      ClientStammdaten := TClientStammdaten.Create(PathServer[WStammDir]);
      FInitClientSta := True;
    end
    else FInitClientSta := False;

    CreateStaTree(FTreeSource, pnSourceTree);
    CreateStaTree(FTreeSink, pnSinkTree);

  end
  else begin
    FTreeSink.Free;
    FTreeSource.Free;
    if (Assigned(ClientStammdaten)) and (FInitClientSta) then
      FreeAndNil(ClientStammdaten);
    if (Assigned(PathServer)) and (FInitPathSrv) then
      FreeAndNil(PathServer);
  end;
end;

{ Auswahlbaum aufbauen                               }
{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.BuildSourceTree;
{-----------------------------------------------------}
var
  i : integer;
begin
  EnableMyControls(Self, False, True, False);
  try
    FTreeSource.Items.BeginUpdate;
    try
      // Baum aufbauen
      ClientStammdaten.FillStaTree(
        FTreeSource, FSortOrder, FZuordnung, FInstTypen, FGeraeteTypen);

      // Falls nur eine Station gewünscht ist, alle anderen löschen
      if (FStationsId >= 0) then
        for i := FTreeSource.Items.Count-1 downto 0 do
          if (FTreeSource.Items[i].Level = C_Level_Stationen) and
             (PStaRecord(FTreeSource.Items[i].Data)^.MyId <> FStationsId)
          then FTreeSource.DeleteNode(FTreeSource.Items[i]);
    finally
      FTreeSource.Items.EndUpdate;
    end;
  finally
    EnableMyControls(Self, True, True, False);
  end;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.PopupMenuPopup(Sender: TObject);
{----------------------------------------------------}
begin
  pmiAdd.Enabled :=
    (TTreeView(PopupMenu.PopupComponent).Parent = pnSourceTree);
  pmiAddAll.Enabled :=
    (TTreeView(PopupMenu.PopupComponent).Parent = pnSourceTree);
  pmiRemove.Enabled :=
    (TTreeView(PopupMenu.PopupComponent).Parent = pnSinkTree);
  pmiRemoveAll.Enabled :=
    (TTreeView(PopupMenu.PopupComponent).Parent = pnSinkTree);
//  pmiTextDef.Enabled := EnableFreeTexts;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.TVDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
{----------------------------------------------------}
begin
  Accept :=
    (Sender is TTreeView) and (Source is TTreeView) and (Sender <> Source);
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.TVDragDrop(Sender, Source: TObject;
  X, Y: Integer);
{----------------------------------------------------}
begin
  if (Sender is TTreeView) and (Source is TTreeView) and (Sender <> Source) then
  begin
    if (TTreeView(Source).Parent = pnSourceTree)
    then AddSelected
    else RemoveSelected;
  end;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.sbtnAddAllClick(Sender: TObject);
{----------------------------------------------------}
begin
  AddAll;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.sbtnAddSingleClick(Sender: TObject);
{----------------------------------------------------}
begin
  AddSelected;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.sbtnRemoveSingleClick(Sender: TObject);
{----------------------------------------------------}
begin
  RemoveSelected;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.sbtnRemoveAllClick(Sender: TObject);
{----------------------------------------------------}
begin
  RemoveAll;
end;

{ Ausgewähltes Item in Auswahl aufnehmen              }
{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.AddSelected;
{----------------------------------------------------}
begin
  if (Assigned(FTreeSource.Selected)) and (Assigned(FTreeSource.Selected.Data))
  then FTreeSink.InsertNode(FTreeSource.Selected, True);
end;

{ Alle Items in Auswahl aufnehmen                    }
{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.AddAll;
{----------------------------------------------------}
var
  pNode : TTreeNode;
begin
  pNode := FTreeSource.Items.GetFirstNode;

  while (Assigned(pNode)) and (Assigned(pNode.Data)) do begin
    if (PStaRecord(pNode.Data)^.GTyp in [C_GerArtMrg, C_GerArtDSfG]) then
      FTreeSink.InsertNode(pNode, True);
    pNode := pNode.GetNextSibling;
  end;
end;

{ Ausgewähltes Item aus Auswahl entfernen            }
{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.RemoveSelected;
{----------------------------------------------------}
begin
  if (Assigned(FTreeSink.Selected)) and (Assigned(FTreeSink.Selected.Data)) then
    FTreeSink.DeleteNode(FTreeSink.Selected);
end;

{ Alle Items aus Auswahl entfernen                   }
{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.RemoveAll;
{----------------------------------------------------}
begin
  FTreeSink.Clear;
end;

{ Gibt ausgewählte DSfG-Kanäle zurück                }
{ Rückgabe: Liste mit Kanalobjekten                  }
{----------------------------------------------------}
function TFormWicomStationsAuswahl.GetDKanaele: TAKanaeleDataList;
{----------------------------------------------------}
begin
  Result := FTreeSink.GetDKanaele;
end;

{ Gibt ausgewählte DSfG-Logbücher zurück             }
{ Rückgabe: Liste mit Logbuchobjekten                }
{----------------------------------------------------}
function TFormWicomStationsAuswahl.GetDLogBuecher: TLogbuchDataList;
{----------------------------------------------------}
begin
  Result := FTreeSink.GetDLogbuecher;
end;

{ Gibt DSfG-Logbücher RegInst. zugeordnet zurück     }
{ Rückgabe: Liste mit Logbuchobjekten                }
{----------------------------------------------------}
function TFormWicomStationsAuswahl.GetDLogBuecherRegInst: TLogbuchDataList;
{----------------------------------------------------}
begin
  Result := FTreeSink.GetDLogbuecherByRegInst;
end;

{ Gibt ausgewählte MRG-Stationen zurück              }
{ Rückgabe: Liste mit MRG-Stationen (ID<us>Name)     }
{----------------------------------------------------}
function TFormWicomStationsAuswahl.GetMStationen: TStrings;
{----------------------------------------------------}
begin
  Result := FTreeSink.GetMStationen;
end;

{ Gibt ausgewählte DSfG-Stationen zurück             }
{ Rückgabe: Liste mit MRG-Stationen (ID<us>Name)     }
{----------------------------------------------------}
function TFormWicomStationsAuswahl.GetDStationen: TStrings;
{----------------------------------------------------}
begin
  Result := FTreeSink.GetDStationen;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.pmiSortTypeClick(Sender: TObject);
{----------------------------------------------------}
var
  p : TMenuItem;
begin
  if (Sender is TMenuItem) then begin
    p := TMenuItem(Sender);
    if (not p.Checked) then begin
      p.Checked := True;
      FAuswahlIni.SortOrder := p.Tag;
      FSortOrder := p.Tag;
      BuildSourceTree;
    end;
  end;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.pmiZuordnungsTypeClick(Sender: TObject);
{----------------------------------------------------}
var
  p : TMenuItem;
begin
  if (Sender is TMenuItem) then begin
    p := TMenuItem(Sender);
    if (not p.Checked) then begin
      p.Checked := True;
      FAuswahlIni.Zuordnung := p.Tag;
      FZuordnung := p.Tag;
      BuildSourceTree;
    end;
  end;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.SetPopupMenuProps;
{----------------------------------------------------}
begin
  case FAuswahlIni.SortOrder of
    0  : pmiSortIDNummer.Checked := True;
    1  : pmiSortName.Checked := True;
  end;
  case FAuswahlIni.Zuordnung of
    0  : pmiZ0AlleArchivebeiRegstrierung.Checked := True;
    1  : pmiZ1AlleArchivebeiQuelle.Checked := True;
    2  : pmiZ2LogbuchbeiQuelleArchivkanlebeiRegistrierung.Checked := True;
    3  : pmiZ3LogbuchbeiRegistrierungArchivkanlebeiQuelle.Checked := True;
    4  : pmiZ4LogbuchbeiQuelleArchivkanlebeiRegistrierungundQuelle.Checked := True;
  end;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.Aktualisieren;
{----------------------------------------------------}
begin
  BuildSourceTree;
end;

{----------------------------------------------------}
procedure TFormWicomStationsAuswahl.SetStationsAuswahl(
  cGerArt: char; iStationsId: integer);
{----------------------------------------------------}
begin
  StationsId := iStationsId;
  FGeraeteTypen := [cGerArt];
  Aktualisieren;
end;

end.
