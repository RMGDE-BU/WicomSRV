//------------------------------------------------------------------------------
// Kapselung von XML-Funktionen
//
// 01.05.2010  GD  Neu - vereinfachte Veröffentlichung DOM-XML-Schnittstellen
// 01.06.2012  GD  Zusätzliche Funktionen zur Content-Zuweisung
// 22.01.2013  WN  Vorbelegung Result "TGDXMLNodeContent.GetAttributeValue"
// 17.10.2018  WW  TGDXMLDocument mit Property CreateDefaultFileWhenNotExisting
//
// Copyright (C) RMG Messtechnik GmbH 2010, 2018
//------------------------------------------------------------------------------
unit GD_XML;

interface

uses
  SysUtils, Classes, Forms, Variants,
  XMLDoc, XMLDom, XMLIntf,
  O_GDObject, GD_Utils;

const
  C_MyXMLHeader = '<?xml version="1.0" encoding="iso-8859-1"?>';
  C_MyXMLTag    = 'content';

type
  TGDXMLNodeContent = class(TStringList)
    destructor Destroy; override;
  private
    FNodeName  : string;
    FNodeValue : string;
    function GetAttributeCount: integer;
    function GetAttributeName(iIndex: integer): string;
    procedure SetAttributeName(iIndex: integer; const sName: string);
    function GetAttributeValue(iIndex: integer): string;
    procedure SetAttributeValue(iIndex: integer; const sValue: string);
    function GetAttributeValueByName(const sName: string): string;
    procedure SetAttributeValueByName(const sName, sValue: string);
  protected
  public
    procedure Clear; override;
    procedure AssignContent(pContent: TGDXMLNodeContent); virtual; // 01.06.2012
    function LoadFromNode(pNode: IXMLNode): boolean; virtual;
    property NodeName: string read FNodeName write FNodeName;
    property NodeValue: string read FNodeValue write FNodeValue;
    property AttributeCount: integer read GetAttributeCount;
    property AttributeName [iIndex: integer]: string
      read GetAttributeName write SetAttributeName;
    property AttributeValue [iIndex: integer]: string
      read GetAttributeValue write SetAttributeValue;
    property Attribute [const sName: string]: string
      read GetAttributeValueByName write SetAttributeValueByName;
  end;

  TGDXMLContent = class(TGDXMLNodeContent)
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FNodeList : TStrings;
    function GetHasChildNodes: boolean;
    function GetChildNodeCount: integer;
    function GetChildNode(iIndex: integer): TGDXMLContent;
    function GetChildNodeByName(const sNodeName: string): TGDXMLContent;
  protected
  public
    procedure Clear; override;
    procedure AssignContent(pContent: TGDXMLNodeContent); override;
    function LoadFromNode(pNode: IXMLNode): boolean; override;
    function AddNode(const sNodeName: string): TGDXMLContent;
    function AddContent(pContent: TGDXMLContent): boolean;
    function DeleteNode(iIndex: integer): boolean;
    function WriteContentToList(
      const sPrefix: string; pSl: TStrings = nil): string;
    property HasChildNodes: boolean read GetHasChildNodes;
    property ChildNodeCount: integer read GetChildNodeCount;
    property ChildNode [iIndex: integer]: TGDXMLContent read GetChildNode;
    property ChildNodeByName [const sNodeName: string]: TGDXMLContent
      read GetChildNodeByName;
  end;

  TGDXMLDocument = class(TGDObject)
    constructor Create; overload; override; 
    constructor Create(pOwner: TComponent); reintroduce; overload;
    destructor Destroy; override;
  private
    FXMLDocument : IXMLDocument;
    FFileName    : TFileName;
    FCreateDefaultFileWhenNotExisting: boolean;  // 17.10.2018, WW
    procedure SetFileName(const sFileName: TFileName);
    procedure SetActive(bActive: boolean);
    function GetActive: boolean;
  protected
    procedure Normalize;
  public
    function NewFile(const sFileName: TFileName): boolean;
    function LoadFile(const sFileName: TFileName): boolean;
    function SaveFile(const sFileName: TFileName = ''): boolean;
    function LoadString(const sXML: string): boolean;
    function HasChildNodes(pNode: IXMLNode): boolean;

    function AddNode(pNode: IXMLNode; pNodeContent: TGDXMLContent): IXMLNode;
    function WriteNode(pNode: IXMLNode; pNodeContent: TGDXMLContent): IXMLNode;

    function AddNodeElementText(
      pNode: IXMLNode; const sElement, sText: string): IXMLNode;
    function AddNodeElementAttribut(
      pNode: IXMLNode; const sElement, sAttrib, sText: string): IXMLNode;
    function WriteNodeElementText(
      pNode: IXMLNode; const sElement, sText: string): IXMLNode;
    function WriteNodeElementAttribut(
      pNode: IXMLNode; const sElement, sAttrib, sText: string): IXMLNode;

    function AddRootElementText(const sElement, sText: string): IXMLNode;
    function AddRootElementAttribut(
      const sElement, sAttrib, sText: string): IXMLNode;
    function WriteRootElementText(const sElement, sText: string): IXMLNode;
    function WriteRootElementAttribut(
      const sElement, sAttrib, sText: string): IXMLNode;

    function AddListElementText(const sElementList, sText: string): IXMLNode;
    function AddListElementAttribut(
      const sElementList, sAttrib, sText: string): IXMLNode;
    function WriteListElementText(const sElementList, sText: string): IXMLNode;
    function WriteListElementAttribut(
      const sElementList, sAttrib, sText: string): IXMLNode;

    function DeleteChildNode(pNode: IXMLNode; sElement: string): boolean;

    function ReadRootElementText(const sElement: string): string;
    function ReadRootElementAttribut(const sElement, sAttrib: string): string;

    property FileName: TFileName read FFileName write SetFileName;
    property XMLDocument: IXMLDocument read FXMLDocument;
    property Active: Boolean read GetActive write SetActive;
    property LastError;
    property LastStatus;
    property CreateDefaultFileWhenNotExisting: boolean write
      FCreateDefaultFileWhenNotExisting;
  end;

// Gibt zurück, ob der übergebene Node Assigned ist
function NodeAssigned(pNode: IXMLNode): boolean;
// Gibt zurück, ob der übergebene Node ein Textnode ist
function IsTextElement(pNode: IXMLNode): boolean;
// Schreibt einen neuen Node unterhalb von pNode
function AddNode(pNode: IXMLNode; pNodeContent: TGDXMLContent): IXMLNode;
// Schreibt einen Node unterhalb von pNode
function WriteNode(pNode: IXMLNode; pNodeContent: TGDXMLContent): IXMLNode;

implementation

//------------------------------ Allgemeine Funktionen -------------------------

// Gibt zurück, ob der übergebene Node Assigned ist
//---------------------------------------------
function NodeAssigned(pNode: IXMLNode): boolean;
//---------------------------------------------
begin
  try
    Result := (Assigned(pNode)) and (pNode.LocalName <> '');
  except
    on E:Exception do begin
      Result := False;
    end;
  end
end;

// Gibt zurück, ob der übergebene Node ein Textnode ist
//---------------------------------------------
function IsTextElement(pNode: IXMLNode): boolean;
//---------------------------------------------
begin
  try
    Result := (pNode.IsTextElement) and (pNode.NodeValue <> NULL);
  except
    on E:Exception do begin
      Result := False;
    end;
  end
end;

// Schreibt einen neuen Node unterhalb von pNode
//   Ein bisher vorhandener Node wird dabei nicht überschrieben
// Parameter: Parent-Node, Node-Inhalt
// Rückgabe: neuer Node
//---------------------------------------------
function AddNode(pNode: IXMLNode; pNodeContent: TGDXMLContent): IXMLNode;
//---------------------------------------------
var
  i : integer;
begin
  try
    // Node erzeugen
    Result := pNode.AddChild(pNodeContent.NodeName);
    // Ggf. Wert zuweisen
    if (pNodeContent.NodeValue <> '') then
      Result.NodeValue := pNodeContent.NodeValue;
    // Attribute zuweisen
    for i := 0 to pNodeContent.AttributeCount-1 do
      if (pNodeContent.AttributeValue[i] <> '') then
        Result.Attributes[pNodeContent.AttributeName[i]] :=
          pNodeContent.AttributeValue[i];
    // Ggf. untergeordnete Knoten einfügen
    for i := 0 to pNodeContent.ChildNodeCount-1 do
      AddNode(Result, pNodeContent.ChildNode[i]);
  except
    on E:Exception do begin
      Result := nil;
    end;
  end;
end;

// Schreibt einen Node unterhalb von pNode
//   Ein ggf. bisher vorhandener gleichnamiger Node wird dabei überschrieben
// Parameter: Parent-Node, Node-Inhalt
// Rückgabe: neuer Node
//---------------------------------------------
function WriteNode(pNode: IXMLNode; pNodeContent: TGDXMLContent): IXMLNode;
//---------------------------------------------
var
  i : integer;
begin
  try
    // Node erzeugen
    Result := pNode.ChildNodes[pNodeContent.NodeName];
    // Ggf. Wert zuweisen
    if (pNodeContent.NodeValue <> '') then
      Result.NodeValue := pNodeContent.NodeValue;
    // Attribute zuweisen
    for i := 0 to pNodeContent.AttributeCount-1 do
      if (pNodeContent.AttributeValue[i] <> '') then
        Result.Attributes[pNodeContent.AttributeName[i]] :=
          pNodeContent.AttributeValue[i];
    // Ggf. untergeordnete Knoten einfügen
    for i := 0 to pNodeContent.ChildNodeCount-1 do
      AddNode(Result, pNodeContent.ChildNode[i]);
  except
    on E:Exception do begin
      Result := nil;
    end;
  end;
end;

//-------------------------------- TGDXMLNodeContent ---------------------------

// Gibt Objekt frei
//---------------------------------------------
destructor TGDXMLNodeContent.Destroy;
//---------------------------------------------
begin
  Clear;

  inherited;
end;

// Leert Liste
//---------------------------------------------
procedure TGDXMLNodeContent.Clear;
//---------------------------------------------
begin
  inherited;

  FNodeName := '';
  FNodeValue := '';
end;

// Überträgt Inhalt
//---------------------------------------------
procedure TGDXMLNodeContent.AssignContent(pContent: TGDXMLNodeContent); 
//---------------------------------------------
begin
  Self.Assign(pContent);
  Self.NodeName := pContent.NodeName;
  Self.NodeValue := pContent.NodeValue;
end;

// Lädt Listeninhalt aus XML-Node
//---------------------------------------------
function TGDXMLNodeContent.LoadFromNode(pNode: IXMLNode): boolean;
//---------------------------------------------
var
  i : integer;
begin
  Clear;
  if (NodeAssigned(pNode)) then begin
    NodeName := pNode.NodeName;
    if (IsTextElement(pNode)) then NodeValue := pNode.NodeValue;
    for i := 0 to pNode.AttributeNodes.Count-1 do begin
      with pNode.AttributeNodes[i] do begin
        if (NodeValue <> NULL)
        then Self.Add(NodeName + '=' + NodeValue)
        else Self.Add(NodeName + '=');
      end;
    end;
    Result := True;
  end
  else Result := False;
end;

// Gibt Anzahl der Attribute zurück
//---------------------------------------------
function TGDXMLNodeContent.GetAttributeCount: integer;
//---------------------------------------------
begin
  Result := Self.Count;
end;

// Gibt Namen eines Attributs abhängig vom Index zurück
//---------------------------------------------
function TGDXMLNodeContent.GetAttributeName(iIndex: integer): string;
//---------------------------------------------
begin
  Result := Self.Names[iIndex];
end;

// Setzt Namen eines Attributs abhängig vom Index
//---------------------------------------------
procedure TGDXMLNodeContent.SetAttributeName(iIndex: integer; const sName: string);
//---------------------------------------------
var
  s : string;
begin
  s := AttributeValue[iIndex];
  Strings[iIndex] := sName + '=' + s;
end;

// Gibt Wert eines Attributs abhängig vom Index zurück
//---------------------------------------------
function TGDXMLNodeContent.GetAttributeValue(iIndex: integer): string;
//---------------------------------------------
var
  iPos : integer;
  s    : string;
begin
	Result := '';  // 22.01.2013  WN
  s := Self.Strings[iIndex];
  iPos := Pos('=', s);
  if (iPos > 0) and (iPos < Length(s)) then
    Result := Copy(s, iPos+1, Length(s)-iPos);
end;

// Setzt Wert eines Attributs abhängig vom Index
//---------------------------------------------
procedure TGDXMLNodeContent.SetAttributeValue(iIndex: integer; const sValue: string);
//---------------------------------------------
var
  s : string;
begin
  s := AttributeName[iIndex];
  Values[s] := sValue;
end;

// Gibt Wert eines Attributs abhängig vom Namen zurück
//---------------------------------------------
function TGDXMLNodeContent.GetAttributeValueByName(const sName: string): string;
//---------------------------------------------
begin
  Result := Values[sName];
end;

// Setzt Wert eines Attributs abhängig vom Namen
//---------------------------------------------
procedure TGDXMLNodeContent.SetAttributeValueByName(const sName, sValue: string);
//---------------------------------------------
begin
  Values[sName] := sValue;
end;

//---------------------------------- TGDXMLContent -----------------------------

//---------------------------------------------
constructor TGDXMLContent.Create;
//---------------------------------------------
begin
  inherited;

  FNodeList := TStringList.Create;
end;

// Gibt Objekt frei
//---------------------------------------------
destructor TGDXMLContent.Destroy;
//---------------------------------------------
begin
  Clear;
  FreeAndNil(FNodeList);

  inherited;
end;

//---------------------------------------------
procedure TGDXMLContent.Clear;
//---------------------------------------------
var
  i : integer;
begin
  if (Assigned(FNodeList)) then begin
    for i := FNodeList.Count-1 downto 0 do
    try
      FNodeList.Objects[i].Free;
    except
      // do nix ...
    end;
    FNodeList.Clear;
  end;
  inherited;
end;

// Überträgt Inhalt
//---------------------------------------------
procedure TGDXMLContent.AssignContent(pContent: TGDXMLNodeContent);
//---------------------------------------------
var
  i : integer;
  p : TGDXMLContent;
begin
  inherited;
  if (pContent is TGDXMLContent) then begin
    p := TGDXMLContent(pContent);
    for i := 0 to p.ChildNodeCount-1 do
      Self.AddContent(p.GetChildNode(i));
  end;
end;

// Lädt Listeninhalt aus XML-Node
//---------------------------------------------
function TGDXMLContent.LoadFromNode(pNode: IXMLNode): boolean;
//---------------------------------------------
var
  i  : integer;
  pN : IXMLNode;
  pL : TGDXMLContent;
begin
  Result := inherited LoadFromNode(pNode);

  if (Result) then begin
    for i := 0 to pNode.ChildNodes.Count-1 do begin
      pN := pNode.ChildNodes[i];
      pL := TGDXMLContent.Create;
      if (pL.LoadFromNode(pN))
      then FNodeList.AddObject(pL.NodeName, pL)
      else pL.Free;
    end;
  end;
end;

// Fügt Informationen zu einem XML-Node hinzu
// Parameter: Name des Nodes
// Rückgabe: Contentobject des neuen Nodes
//---------------------------------------------
function TGDXMLContent.AddNode(const sNodeName: string): TGDXMLContent;
//---------------------------------------------
begin
  Result := TGDXMLContent.Create;
  Result.NodeName := sNodeName;
  FNodeList.AddObject(sNodeName, Result);
end;

// Fügt externen Contentnode zu einem XML-Node hinzu
// Parameter: Content-Objekt
// Rückgabe: Erfolg ja / nein
//---------------------------------------------
function TGDXMLContent.AddContent(pContent: TGDXMLContent): boolean;
//---------------------------------------------
var
  iNode, iAttrib : integer;
  pNode          : TGDXMLContent;
  sAttribName, sAttribVal : string;
begin
  try
    pNode := AddNode(pContent.NodeName);
    for iAttrib := 0 to pContent.AttributeCount-1 do begin
      sAttribName := pContent.AttributeName[iAttrib];
      sAttribVal := pContent.AttributeValue[iAttrib];
      pNode.Attribute[sAttribName] := sAttribVal;
    end;
    if (pContent.NodeValue <> '') then pNode.NodeValue := pContent.NodeValue
    else for iNode := 0 to pContent.ChildNodeCount-1 do begin
      pNode.AddContent(pContent.GetChildNode(iNode));
    end;

    Result := True;
  except
    on E:Exception do begin
      Result := False;
    end;
  end;
end;

// Löscht Information-Knoten zu einem XML-Node hinzu
// Parameter: Index des Nodes
// Rückgabe: Erfolg ja / nein
//---------------------------------------------
function TGDXMLContent.DeleteNode(iIndex: integer): boolean;
//---------------------------------------------
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

// Schreibt gespeicherten Inhalt in lesbare Form
// Wg. iterativem Aufruf Übergabe von Prefix und Liste
//---------------------------------------------
function TGDXMLContent.WriteContentToList(
  const sPrefix: string; pSl: TStrings = nil): string;
//---------------------------------------------
var
  i : integer;
  p : TStrings;
  s : string;
begin
  if (Assigned(pSl)) then p := pSl else p := TStringList.Create;
  try
    p.BeginUpdate;
    try
      s := '<' + Self.NodeName + '>' + Self.NodeValue;
      if (Self.Count = 0) and (ChildNodeCount = 0) then
        s := s + '</' + Self.NodeName + '>';
      p.Add(sPrefix + s);

      for i := 0 to Self.Count-1 do p.Add(sPrefix + '  ' + Self.Strings[i]);
      for i := 0 to Self.ChildNodeCount-1 do
        ChildNode[i].WriteContentToList(sPrefix + '  ', p);

      if (Self.Count > 0) or (ChildNodeCount > 0) then
        p.Add(sPrefix + '</' + Self.NodeName + '>');

      Result := p.CommaText;
    finally
      p.EndUpdate;
    end;
  finally
    if (not Assigned(pSl)) then p.Free;
  end;
end;

// Gibt zurück, ob untergeordneten Knoten existieren
//---------------------------------------------
function TGDXMLContent.GetHasChildNodes: boolean;
//---------------------------------------------
begin
  Result := (FNodeList.Count > 0);
end;

// Gibt Anzahl der untergeordneten Knoten zurück
//---------------------------------------------
function TGDXMLContent.GetChildNodeCount: integer;
//---------------------------------------------
begin
  Result := FNodeList.Count;
end;

// Gibt Inhalt eines untergeordneten Knoten zurück
//---------------------------------------------
function TGDXMLContent.GetChildNode(iIndex: integer): TGDXMLContent;
//---------------------------------------------
begin
  Result := TGDXMLContent(FNodeList.Objects[iIndex]);
end;

// Gibt Inhalt eines untergeordneten Knoten zurück
// Node wird durch Name adressiert, untergeordnete Nodes werden durchsucht
//---------------------------------------------
function TGDXMLContent.GetChildNodeByName(const sNodeName: string): TGDXMLContent;
//---------------------------------------------
var
  i : integer;
begin
  Result := nil;
  if (Self.FNodeName = sNodeName) then Result := Self
  else for i := 0 to Self.ChildNodeCount-1 do begin
    Result := GetChildNode(i).GetChildNodeByName(sNodeName);
    if (Result <> nil) then Break;
  end;
end;

//---------------------------------- TGDXMLDocument ----------------------------

//---------------------------------------------
constructor TGDXMLDocument.Create;
//---------------------------------------------
var
  pOptions   : TXMLDocOptions;
begin
  inherited Create;

  // Internes XML-Objekt erzeugen
  FXMLDocument := TXMLDocument.Create(nil) as IXMLDocument;
  // Optionen setzen (AutoIndnt, Rest ist Standard)
  pOptions := FXMLDocument.Options;
  Include(pOptions, doNodeAutoIndent);
  Include(pOptions, doNodeAutoCreate);
  Include(pOptions, doAttrNull);
  Include(pOptions, doAutoPrefix);
  Include(pOptions, doNamespaceDecl);
  FXMLDocument.Options:= pOptions;
  // Dateiname ist nicht vorbelegt
  FFileName := '';
  FCreateDefaultFileWhenNotExisting:=true;  // Default: Default-File anlegen; 17.10.2018, WW
end;

//---------------------------------------------
constructor TGDXMLDocument.Create(pOwner: TComponent);
//---------------------------------------------
begin
  Create;
end;

//---------------------------------------------
destructor TGDXMLDocument.Destroy;
//---------------------------------------------
begin
  Active := False;
  FXMLDocument := nil;

  inherited Destroy;
end;

// Dateiname für XML-Dokument setzen
//---------------------------------------------
procedure TGDXMLDocument.SetFileName(const sFileName: TFileName);
//---------------------------------------------
begin
  if (UpperCase(sFileName) <> UpperCase(FFileName)) then begin
    if (FileExists(sFileName)) then begin
      FFileName := sFileName;
      FXMLDocument.FileName := FFileName;
      FXMLDocument.Active := True;
    end
    else NewFile(sFileName);
  end;
end;

// Aktiv-Status für Analyse/Bearbeitung des XML-Objektes setzen
//---------------------------------------------
procedure TGDXMLDocument.SetActive(bActive: boolean);
//---------------------------------------------
begin
  FXMLDocument.Active := bActive;
end;

// Aktiv-Status des XML-Objektes abfragen
//---------------------------------------------
function TGDXMLDocument.GetActive: boolean;
//---------------------------------------------
begin
  Result := FXMLDocument.Active;
end;

// "Normalisiert" die Stringliste durch Einfügen von CR
// ACHTUNG: Obsolet bei XMLDocument.Options:= [..., doNodeAutoIndent]
//---------------------------------------------
procedure TGDXMLDocument.Normalize;
//---------------------------------------------
const
  C_IndentChars = '  ';
var
  sXml, sIndent, s : string;
  i, j, iIndent    : integer;
begin
  sXml := FXMLDocument.XML.Text;
  with TStringList.Create do
  try
    // 1. Durchlauf: Führende Leerzeichen Trimmen
    Text := sXml;
    for i := 1 to Count-1 do Strings[i] := Trim(Strings[i]);
    sXml := Text;

    // 2. Durchlauf: Tag-Abschlüsse beginnen neue Zeile
    Text := sXml;
    for i := 1 to Count-1 do begin
      if (Pos('</', Strings[i]) > 1) then Strings[i] :=
        StringReplace(Strings[i], '</', #13#10'</', [rfReplaceAll]);
    end;
    sXml := Text;

    // 3. Durchlauf: Tag-Beginne beginnen neue Zeile
    Text := sXml;
    for i := 1 to Count-1 do begin
      s := Copy(Strings[i], 2, Length(Strings[i])-1);
      if (Pos('<', s) > 1) then Strings[i] := Strings[i][1] +
        StringReplace(s, '<', #13#10'<', [rfReplaceAll]);
    end;
    sXml := Text;

    // 4. Durchlauf: Einzeilige Tags erzwingen neue Zeile
    Text := sXml;
    for i := 1 to Count-1 do begin
      if (Pos('/>', Strings[i]) < Length(Strings[i])-1) then Strings[i] :=
        StringReplace(Strings[i], '/>', '/>'#13#10, [rfReplaceAll]);
    end;
    sXml := Text;

    // 5. Durchlauf: Leerzeilen eleminieren
    Text := sXml;
    for i := Count-1 downto 1 do begin
      if (Trim(Strings[i]) = '') then Delete(i);
    end;
    sXml := Text;

    // 6. Durchlauf: Einrückungen
    Text := sXml;
    iIndent := 0;
    for i := 1 to Count-1 do begin
      if (Pos('</', Strings[i]) = 1) then begin
        if (iIndent > 0) then Dec(iIndent);
        if (iIndent > 0) then begin
          sIndent := '';
          for j := 1 to iIndent do sIndent := sIndent + C_IndentChars;
          Strings[i] := sIndent + Strings[i];
        end;
      end
      else if (Pos('<', Strings[i]) = 1) then begin
        if (iIndent > 0) then begin
          sIndent := '';
          for j := 1 to iIndent do sIndent := sIndent + C_IndentChars;
          Strings[i] := sIndent + Strings[i];
        end;
        Inc(iIndent);
      end;
      if (Pos('/>', Strings[i]) > 0) then begin
        if (iIndent > 0) then Dec(iIndent);
      end;
    end;
    sXml := Text;
  finally
    Free;
  end;
  FXMLDocument.XML.Text := sXml;
end;

// Erzeugt neues XML-Objekt mit vorgegebener Minimalstruktur
//---------------------------------------------
function TGDXMLDocument.NewFile(const sFileName: TFileName): boolean;
//---------------------------------------------
begin
  try
    Active := False;
    FXMLDocument.XML.Add(C_MyXMLHeader);
    FXMLDocument.XML.Add('<' + C_MyXMLTag + '>');
    FXMLDocument.XML.Add('</' + C_MyXMLTag + '>');

    HandleError('File not existing');       // 17.10.2018, WW
    if (sFileName <> '') AND (FCreateDefaultFileWhenNotExisting) then
      SaveFile(sFileName);
    Active := True;
    Result := Active;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.NewFile: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Lädt XML-Inhalt aus Datei und öffnet zum Bearbeiten
//---------------------------------------------
function TGDXMLDocument.LoadFile(const sFileName: TFileName): boolean;
//---------------------------------------------
begin
  try
    if (FileExists(sFileName)) then begin
      Active := False;
      FXMLDocument.LoadFromFile(sFileName);
      FFileName := sFileName;
      Result := Active;
    end
    else begin
      Result := NewFile(sFileName);
    end;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.LoadFile: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Schreibt XML-Inhalt in Datei
//---------------------------------------------
function TGDXMLDocument.SaveFile(const sFileName: TFileName = ''): boolean;
//---------------------------------------------
begin
  try
    if ((sFileName <> '') or (FFileName <> '')) and
      (FXMLDocument.XML.Text <> '') then
    begin
      if (sFileName <> '') then FFileName := sFileName;
      Application.ProcessMessages;
      Active := True;
      FXMLDocument.FileName := FFileName;
      FXMLDocument.SaveToFile(FFileName);
      Result := True;
    end
    else Result := False;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.SaveFile: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Lädt string in XML-Objekt
//---------------------------------------------
function TGDXMLDocument.LoadString(const sXML: string): boolean;
//---------------------------------------------
var
  pStream : TStream;
begin
  pStream := TStringStream.Create(sXML);
  try
    try
      FXMLDocument.LoadFromStream(pStream);
      Result := True;
    except
      Result := False;
    end;
  finally
    pStream.Free;
  end;
end;

// Gibt zurück, ob der übergebene Node ECHTE Childnodes hat
//   Gibt bei "nur Text" auch FALSE zurück
//---------------------------------------------
function TGDXMLDocument.HasChildNodes(pNode: IXMLNode): boolean;
//---------------------------------------------
begin
  if (NodeAssigned(pNode)) then
  try
    Result := (pNode.HasChildNodes) and (not pNode.IsTextElement);
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.HasChildNodes: ' + E.Message);
      Result := False;
    end;
  end
  else Result := False;
end;

// Schreibt einen neuen Node unterhalb von pNode
//   Ein bisher vorhandener Node wird dabei nicht überschrieben
// Parameter: Parent-Node, Node-Inhalt
// Rückgabe: neuer Node
//---------------------------------------------
function TGDXMLDocument.AddNode(
  pNode: IXMLNode; pNodeContent: TGDXMLContent): IXMLNode;
//---------------------------------------------
begin
  Result := GD_XML.AddNode(pNode, pNodeContent);
end;

// Schreibt einen Node unterhalb von pNode
//   Ein ggf. bisher vorhandener gleichnamiger Node wird dabei überschrieben
// Parameter: Parent-Node, Node-Inhalt
// Rückgabe: neuer Node
//---------------------------------------------
function TGDXMLDocument.WriteNode(
  pNode: IXMLNode; pNodeContent: TGDXMLContent): IXMLNode;
//---------------------------------------------
begin
  Result := GD_XML.WriteNode(pNode, pNodeContent);
end;

// Schreibt neues Element mit Text unterhalb von Node
// Ein bisher vorhandener Text wird dabei nicht überschrieben
//---------------------------------------------
function TGDXMLDocument.AddNodeElementText(
  pNode: IXMLNode; const sElement, sText: string): IXMLNode;
//---------------------------------------------
var
  p : IXMLNode;
  s : string;
begin
  try
    p := pNode.ChildNodes.FindNode(sElement);
    if (not NodeAssigned(p))  // kein Knoten: neu schreiben
    then Result := WriteNodeElementText(pNode, sElement, sText)
    else begin
      while (NodeAssigned(p)) do begin
        if (HasChildNodes(p)) then p := pNode.ChildNodes.FindSibling(p, 1)
        else begin
          s := Trim(UpperCase(p.NodeValue));
          if (s = Trim(UpperCase(sText))) then begin
            Result := p;
            Break; // Gleichen Knoten gefunden
          end
          else p := pNode.ChildNodes.FindSibling(p, 1);
        end;
      end;
      // Wenn gleicher Knoten nicht gefunden wurde: neuen anlegen und ausfüllen
      if (not NodeAssigned(p)) then begin
        Result := pNode.AddChild(sElement);
        Result.NodeValue := sText;
      end;
    end;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.AddNodeElementText (' +
        sElement + '): ' + E.Message);
      Result := nil;
    end;
  end;
end;

// Schreibt neues Element mit Attribut und Text unterhalb von Node
// Ein bisher vorhandener Text wird dabei nicht überschrieben
//---------------------------------------------
function TGDXMLDocument.AddNodeElementAttribut(
  pNode: IXMLNode; const sElement, sAttrib, sText: string): IXMLNode;
//---------------------------------------------
var
  p : IXMLNode;
  s : string;
begin
  try
    p := pNode.ChildNodes.FindNode(sElement);
    if (not NodeAssigned(p))  // kein Knoten: neu schreiben
    then Result := WriteNodeElementAttribut(pNode, sElement, sAttrib, sText)
    else begin
      while (NodeAssigned(p)) do begin
        s := Trim(UpperCase(p.Attributes[sAttrib]));
        if (s = Trim(UpperCase(sText))) then begin
          Result := p;
          Break; // Gleichen Knoten gefunden
        end
        else p := pNode.ChildNodes.FindSibling(p, 1);
      end;
      // Wenn gleicher Knoten nicht gefunden wurde: neuen anlegen und ausfüllen
      if (not NodeAssigned(p)) then begin
        Result := pNode.AddChild(sElement);
        Result.Attributes[sAttrib] := sText;
      end;
    end;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.AddNodeElementAttribut (' +
        sElement + '): ' + E.Message);
      Result := nil;
    end;
  end;
end;

// Schreibt neues Element mit Text unterhalb von Node
// Ein bisher vorhandener Text wird dabei überschrieben
//---------------------------------------------
function TGDXMLDocument.WriteNodeElementText(
  pNode: IXMLNode; const sElement, sText: string): IXMLNode;
//---------------------------------------------
begin
  try
    Result := pNode.ChildNodes[sElement];
    if (HasChildNodes(Result)) then Result := pNode.AddChild(sElement);
    Result.NodeValue := sText;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.WriteNodeElementText (' +
        sElement + '): ' + E.Message);
      Result := nil;
    end;
  end;
end;

// Schreibt neues Element mit Attribut und Text unterhalb von Node
// Ein bisher vorhandener Text wird dabei überschrieben
//---------------------------------------------
function TGDXMLDocument.WriteNodeElementAttribut(
  pNode: IXMLNode; const sElement, sAttrib, sText: string): IXMLNode;
//---------------------------------------------
begin
  try
    Result := pNode.ChildNodes[sElement];
    Result.Attributes[sAttrib] := sText;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.WriteNodeElementAttribut (' +
        sElement + '): ' + E.Message);
      Result := nil;
    end;
  end;
end;

// Schreibt neues Element mit Text in erste Ebene
// Ein bisher vorhandener Text wird dabei nicht überschrieben
//---------------------------------------------
function TGDXMLDocument.AddRootElementText(
  const sElement, sText: string): IXMLNode;
//---------------------------------------------
begin
  Result := AddNodeElementText(FXMLDocument.DocumentElement, sElement, sText);
end;

// Schreibt neues Element mit Attribut und Text in erste Ebene
// Ein bisher vorhandener Text wird dabei nicht überschrieben
//---------------------------------------------
function TGDXMLDocument.AddRootElementAttribut(
  const sElement, sAttrib, sText: string): IXMLNode;
//---------------------------------------------
begin
  Result := AddNodeElementAttribut(
    FXMLDocument.DocumentElement, sElement, sAttrib, sText);
end;

// Schreibt neues Element mit Text in erste Ebene
//---------------------------------------------
function TGDXMLDocument.WriteRootElementText(
  const sElement, sText: string): IXMLNode;
//---------------------------------------------
begin
  Result := WriteNodeElementText(FXMLDocument.DocumentElement, sElement, sText);
end;

// Schreibt neues Element mit Attribut und Text in erste Ebene
//---------------------------------------------
function TGDXMLDocument.WriteRootElementAttribut(
  const sElement, sAttrib, sText: string): IXMLNode;
//---------------------------------------------
begin
  Result := WriteNodeElementAttribut(
    FXMLDocument.DocumentElement, sElement, sAttrib, sText);
end;

// Schreibt neues Element mit Text in durch Liste definierte Ebene
// Ein bisher vorhandener Text wird dabei nicht überschrieben
//---------------------------------------------
function TGDXMLDocument.AddListElementText(
  const sElementList, sText: string): IXMLNode;
//---------------------------------------------
var
  i : integer;
  p : IXMLNode;
begin
  try
    with TStringList.Create do
    try
      CommaText := sElementList;
      p := nil;
      if (Count = 0) then begin
        // mache gar nix;
      end
      else if (Count = 1) then begin
        p := AddRootElementText(Strings[0], sText);
      end
      else begin
        for i := 0 to Count-2 do begin
          if (not NodeAssigned(p))
          then p := FXMLDocument.DocumentElement.ChildNodes[Strings[i]]
          else p := p.ChildNodes[Strings[i]];
        end;
        p := AddNodeElementText(p, Strings[Count-1], sText);
      end;
    finally
      Free;
    end;
    Result := p;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.AddListElementText (' +
        sElementList + '): ' + E.Message);
      Result := nil;
    end;
  end;
end;

// Schreibt neues Element mit Attribut und Text in durch Liste definierte Ebene
// Ein bisher vorhandener Text wird dabei nicht überschrieben
//---------------------------------------------
function TGDXMLDocument.AddListElementAttribut(
  const sElementList, sAttrib, sText: string): IXMLNode;
//---------------------------------------------
var
  i : integer;
  p : IXMLNode;
begin
  try
    with TStringList.Create do
    try
      CommaText := sElementList;
      p := nil;
      if (Count = 0) then begin
        // mache gar nix;
      end
      else if (Count = 1) then begin
        p := AddRootElementAttribut(Strings[0], sAttrib, sText);
      end
      else begin
        for i := 0 to Count-2 do begin
          if (not NodeAssigned(p))
          then p := FXMLDocument.DocumentElement.ChildNodes[Strings[i]]
          else p := p.ChildNodes[Strings[i]];
        end;
        p := AddNodeElementAttribut(p, Strings[Count-1], sAttrib, sText);
      end;
    finally
      Free;
    end;
    Result := p;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.AddListElementAttribut (' +
        sElementList + '): ' + E.Message);
      Result := nil;
    end;
  end;
end;

// Schreibt neues Element mit Text in durch Liste definierte Ebene
// Ein bisher vorhandener Text wird dabei überschrieben
//---------------------------------------------
function TGDXMLDocument.WriteListElementText(
  const sElementList, sText: string): IXMLNode;
//---------------------------------------------
var
  i : integer;
  p : IXMLNode;
begin
  try
    with TStringList.Create do
    try
      CommaText := sElementList;
      p := nil;
      if (Count > 0) then begin
        for i := 0 to Count-1 do begin
          if (not NodeAssigned(p))
          then p := FXMLDocument.DocumentElement.ChildNodes[Strings[i]]
          else p := p.ChildNodes[Strings[i]];
        end;
        if (HasChildNodes(p)) then p := p.ParentNode.AddChild(Strings[Count-1]);
        p.NodeValue := sText;
      end
    finally
      Free;
    end;
    Result := p;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.WriteListElementText (' +
        sElementList + '): ' + E.Message);
      Result := nil;
    end;
  end;
end;

// Schreibt neues Element mit Attribut und Text in durch Liste definierte Ebene
// Ein bisher vorhandener Attributs-Text wird dabei überschrieben
//---------------------------------------------
function TGDXMLDocument.WriteListElementAttribut(
  const sElementList, sAttrib, sText: string): IXMLNode;
//---------------------------------------------
var
  i : integer;
  p : IXMLNode;
begin
  try
    with TStringList.Create do
    try
      CommaText := sElementList;
      p := nil;
      if (Count > 0) then begin
        for i := 0 to Count-1 do begin
          if (not NodeAssigned(p))
          then p := FXMLDocument.DocumentElement.ChildNodes[Strings[i]]
          else p := p.ChildNodes[Strings[i]];
        end;
        p.Attributes[sAttrib] := sText;
      end
    finally
      Free;
    end;
    Result := p;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.WriteListElementAttribut (' +
        sElementList + '): ' + E.Message);
      Result := nil;
    end;
  end;
end;

// Löscht alle Childnodes mit einem übergebenen Namen
//---------------------------------------------
function TGDXMLDocument.DeleteChildNode(
  pNode: IXMLNode; sElement: string): boolean;
//---------------------------------------------
var
  p : IXMLNode;
  i : integer;
begin
  try
    p := pNode.ChildNodes.FindNode(sElement);
    while (NodeAssigned(p)) do begin
      i := pNode.ChildNodes.IndexOf(p);
      if (i >= 0) then pNode.ChildNodes.Delete(i);
      p := pNode.ChildNodes.FindNode(sElement);
    end;
    Result := True;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.DeleteChildNode (' +
        sElement + '): ' + E.Message);
      Result := False;
    end;
  end;
end;

// Liest Element mit Text aus erster Ebene
//---------------------------------------------
function TGDXMLDocument.ReadRootElementText(const sElement: string): string;
//---------------------------------------------
var
  p : IXMLNode;
begin
  try
    p := FXMLDocument.DocumentElement.ChildNodes.FindNode(sElement);
    if (NodeAssigned(p)) then begin
      if (not HasChildNodes(p)) then Result := p.NodeValue
      else begin
        p := FXMLDocument.DocumentElement.ChildNodes.FindSibling(p, 1);
        while (NodeAssigned(p)) do begin
          if (p.NodeName = sElement) and (not HasChildNodes(p)) then begin
            Result := p.NodeValue;
            Break;
          end
          else p := FXMLDocument.DocumentElement.ChildNodes.FindSibling(p, 1);
        end;
      end;
    end;
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.ReadRootElementText (' +
        sElement + '): ' + E.Message);
      Result := '';
    end;
  end;
end;

// Liest Text aus Element mit Attribut aus erster Ebene
//---------------------------------------------
function TGDXMLDocument.ReadRootElementAttribut(
  const sElement, sAttrib: string): string;
//---------------------------------------------
begin
  try
    Result :=
      FXMLDocument.DocumentElement.ChildNodes[sElement].Attributes[sAttrib];
  except
    on E:Exception do begin
      HandleError('TGDXMLDocument.ReadRootElementAttribut (' +
        sElement + '): ' + E.Message);
      Result := '';
    end;
  end;
end;

end.
