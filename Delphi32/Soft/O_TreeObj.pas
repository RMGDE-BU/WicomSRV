//------------------------------------------------------------------------------
// Baum-Hierarchie als mehrdimensionale Liste aufbauen und danach in Treeview schreiben
//
// 11.07.2011  WN  Neu
//
// Copyright © RMG Messtechnik GmbH 2011
//------------------------------------------------------------------------------
unit O_TreeObj;

interface

uses
  Contnrs, Classes, ComCtrls, SysUtils;

type

  // ein TreeObject
  TTreeNodeContent = class(TStringList)
    destructor Destroy; override;
  private
    FNodeName   : String;    // Knoten-Name
    FNodeObject : TObject;   // Knoten-Objekt
    FImageIndex : Integer;   // Knoten-Bildindex unmarkiert
    FImageSelect: Integer;   // Knoten-Bildindex markiert
  protected
  public
    procedure Clear; override;
    property NodeName: string read FNodeName write FNodeName;
    property NodeObject: TObject read FNodeObject write FNodeObject;
    property ImageIndex  : Integer read FImageIndex;
    property ImageSelect : Integer read FImageSelect;
  end;

  // ein erweitertes TreeObject
  TTreeContent = class(TTreeNodeContent)
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FNodeList : TStrings;
    function GetHasChildNodes: Boolean;
    function GetChildNodeCount: Integer;
    function GetChildNode(iIndex: Integer): TTreeContent;
    function GetChildNodeByName(const sNodeName: String): TTreeContent;
  protected
  public
    procedure Clear; override;
    function AddNode(const sNodeName: String; p: TObject;
                     iImageIndex, iImageSel: Integer): TTreeContent;
    function DeleteNode(iIndex: Integer): Boolean;
    function InsertContentToTreeview(tv: TTreeView; tnParent: TTreeNode): Boolean;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property ChildNodeCount: Integer read GetChildNodeCount;
    property ChildNode [iIndex: Integer]: TTreeContent read GetChildNode;
    property ChildNodeByName [const sNodeName: string]: TTreeContent read GetChildNodeByName;
  end;

  // ein erweitertes WicalStamm-TreeObject
  TWicalStammTreeContent = class(TTreeContent)
  private
  protected
  public
  end;

implementation

(*****************************  TTreeNodeContent  *****************************)

//------------------------------------------------------------------------------
// Objektfreigabe
//------------------------------------------------------------------------------
destructor TTreeNodeContent.Destroy;
//------------------------------------------------------------------------------
begin
  Clear;
  inherited;
end;

//------------------------------------------------------------------------------
// Liste leeren
//------------------------------------------------------------------------------
procedure TTreeNodeContent.Clear;
//------------------------------------------------------------------------------
begin
  FNodeName   := '';
  FNodeObject := nil;  // nicht freigeben, wird im Treenode noch benötigt !
  inherited;
end;

(*******************************  TTreeContent  *******************************)

//------------------------------------------------------------------------------
constructor TTreeContent.Create;
//------------------------------------------------------------------------------
begin
  inherited;
  FNodeList := TStringList.Create;
end;

//------------------------------------------------------------------------------
// Objektfreigabe
//------------------------------------------------------------------------------
destructor TTreeContent.Destroy;
//------------------------------------------------------------------------------
begin
  Clear;
  FreeAndNil(FNodeList);
  inherited;
end;

//------------------------------------------------------------------------------
// Liste leeren
//------------------------------------------------------------------------------
procedure TTreeContent.Clear;
//------------------------------------------------------------------------------
var
  i : integer;
begin
  if (Assigned(FNodeList)) then
  begin
    for i:=FNodeList.Count-1 downto 0 do
    try
      FNodeList.Objects[i].Free;
    except
      // tue nix ...
    end;
    FNodeList.Clear;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
// Fügt einen Unterknoten zum Node hinzu
// Parameter: Name des Unterknotens
//            Object des Unterknotens
//            Image-Index unmarkiert
//            Image-Index markiert
// Rückgabe: Unterknoten
//------------------------------------------------------------------------------
function TTreeContent.AddNode(const sNodeName: String; p: TObject;
                              iImageIndex, iImageSel: Integer): TTreeContent;
//------------------------------------------------------------------------------
begin
  Result := TTreeContent.Create;
  Result.NodeName     := sNodeName;
  Result.FNodeObject  := p;
  Result.FImageIndex  := iImageIndex;
  Result.FImageSelect := iImageSel;
  FNodeList.AddObject(sNodeName, Result);
end;

//------------------------------------------------------------------------------
// Löscht Knoten
// Parameter: Index des Nodes
// Rückgabe: Erfolg ja / nein
//------------------------------------------------------------------------------
function TTreeContent.DeleteNode(iIndex: Integer): Boolean;
//------------------------------------------------------------------------------
begin
  try
    try
      FNodeList.Objects[iIndex].Free;
    except
    end;
    FNodeList.Delete(iIndex);
    Result := True;
  except
    Result := False;
  end;
end;

//------------------------------------------------------------------------------
// Einfügen des Inhaltes in Treeview
// Parameter: Treeview, Parent-Node
// Rückgabe: Erfolg ja / nein
//------------------------------------------------------------------------------
function TTreeContent.InsertContentToTreeview(tv: TTreeView; tnParent: TTreeNode): Boolean;
//------------------------------------------------------------------------------
var
  i : Integer;
  tn: TTreeNode;
begin
  Result := False;
  if Assigned(tv) then
  try
    tv.Items.BeginUpdate;

    // Knoten dem Parent-Node hinzufügen
    if (Self.NodeName <> '') then
    begin
      tn := tv.Items.AddChildObject(tnParent, Self.NodeName, Self.NodeObject);
      tn.ImageIndex := Self.ImageIndex;
      tn.SelectedIndex := Self.ImageSelect;
    end else
    begin
      tn := nil;
    end;

    // Unterknoten diesem Knoten hinzufügen
    for i:=0 to Self.ChildNodeCount-1 do
      ChildNode[i].InsertContentToTreeview(tv, tn);

    Result := True;
  finally
    tv.Items.EndUpdate;
  end;
end;
//------------------------------------------------------------------------------
// Gibt zurück, ob untergeordnete Knoten existieren
//------------------------------------------------------------------------------
function TTreeContent.GetHasChildNodes: boolean;
//------------------------------------------------------------------------------
begin
  Result := (FNodeList.Count > 0);
end;

//------------------------------------------------------------------------------
// Gibt Anzahl der untergeordneten Knoten zurück
//------------------------------------------------------------------------------
function TTreeContent.GetChildNodeCount: Integer;
//------------------------------------------------------------------------------
begin
  Result := FNodeList.Count;
end;

//------------------------------------------------------------------------------
// Gibt Inhalt eines untergeordneten Knoten zurück
//------------------------------------------------------------------------------
function TTreeContent.GetChildNode(iIndex: Integer): TTreeContent;
//------------------------------------------------------------------------------
begin
  Result := TTreeContent(FNodeList.Objects[iIndex]);
end;

//------------------------------------------------------------------------------
// Gibt Inhalt eines untergeordneten Knoten zurück
// Node wird durch Name adressiert, untergeordnete Nodes werden durchsucht
//------------------------------------------------------------------------------
function TTreeContent.GetChildNodeByName(const sNodeName: String): TTreeContent;
//------------------------------------------------------------------------------
var
  i : Integer;
begin
  Result := nil;
  if (Self.FNodeName = sNodeName) then
  begin
    Result := Self;
  end else
  begin
    for i:=0 to Self.ChildNodeCount-1 do
    begin
      Result := GetChildNode(i).GetChildNodeByName(sNodeName);
      if Assigned(Result) then Break;
    end;
  end;
end;

end.
