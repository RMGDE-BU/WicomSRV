{------------------------------------------------------------------------------}
{ unit DSfGOutl                                                                }
{ 17.04.1996 DA, DSfG-Stationsauswahl                                          }
{ 05.04.1999 GD; 'GetOutlineIndex' ist zum Eintragen der Nodes ungeeignet      }
{                -> durch sortierte Query ersetzt                              }
{                'InsertNode' ist zur Übernahme ganzer Outlines ungeeignet     }
{                -> bei Übernahme ganzer Outlines 'InsertOutline' verwenden    }
{ 28.02.2000 WW  Erweiterung: Archive/Logbücher in einer Liste (dlKanalLogbuch)}
{------------------------------------------------------------------------------}
unit DSfGOutl;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Grids, Outline, DB, DBTables, StdCtrls;

const
  CStation = 'Station.db';
  CInstanz = 'Instanz.db';
  CArchive = 'Archive.db';
  CAKanaele = 'AKanaele.db';
  CLogbuch = 'Logbuch.db';

  CStationID = 'StationID';
  CInstanzID = 'InstanzID';
  CArchivNr = 'ArchivNr';
  CKanalNr = 'KanalNr';
  CLogbuchNr = 'LogbuchNr';
  CStationsname = 'Stationsname';
  CInstanzname = 'Instanzname';
  CArchivname = 'Name';
  CKanalName = 'Name';
  CLogbuchName = 'Name';

type

  TDSfGLevel = (dlNone, dlStation, dlInstanz, dlArchiv, dlKanal, dlLogbuch, dlKanalLogbuch);

  TDSfGOutline = class(TCustomOutline)
  private
    RootIndex: integer;
    RootNode: TOutlineNode;
    FDSfGLevel: TDSfGLevel;
    FDatabaseName: TFilename;
    TblStation: TTable;
    TblInstanz: TTable;
    TblArchive: TTable;
    TblAKanaele: TTable;
    TblLogBuch: TTable;
    FSelectedStation: integer;
    FSelectedInstanz: integer;
    FSelectedArchiv: integer;
    FSelectedKanal: integer;
    FSelectedLogbuch: integer;
    procedure SetDSfGLevel (Value: TDSfGLevel);
    function GetSelected (Index: Integer): integer;
    procedure SetSelected (Index: Integer; Value: integer);
    procedure SetDatabaseName (Value: TFilename);
    function GetSelectedStationsname: string;
    function GetSelectedInstanzname: string;
    function GetSelectedArchivname: string;
    function GetSelectedKanalname: string;
    function GetSelectedLogbuchname: string;
  protected
    procedure Expand (Index: integer); override;
    function GetOutlineIndex (Node: TOutlineNode; const Data: integer; const Text: string): integer;
    procedure FillDlStation;
    procedure FillDlInstanz;
    procedure FillDlArchiv;
    procedure FillDlLogbuch;
    procedure FillDlArchivLogbuch;
    function AddChildSorted (Index: integer; const Text: string): integer;
  public
    ExpandLevel : TDSfGLevel;
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FillList; virtual;
    function InsertNode (Node: TOutlineNode): integer;
    function InsertOutline (anOutline: TCustomOutline): integer;
    procedure Select (IDFeld: array of integer);
    property SelectedStation: integer index 1 read GetSelected write SetSelected;
    property SelectedInstanz: integer index 2 read GetSelected write SetSelected;
    property SelectedArchiv: integer index 3 read GetSelected write SetSelected;
    property SelectedKanal: integer index 4 read GetSelected write SetSelected;
    property SelectedLogbuch: integer index 5 read GetSelected write SetSelected;
    property SelectedStationsname: string read GetSelectedStationsname;
    property SelectedInstanzname: string read GetSelectedInstanzname;
    property SelectedArchivname: string read GetSelectedArchivname;
    property SelectedKanalname: string read GetSelectedKanalname;
    property SelectedLogbuchname: string read GetSelectedLogbuchname;
  published
    property Align;
    property Font;
    property Options;
    property OutlineStyle;
    property OnClick;
    property OnCollapse;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpand;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Databasename: TFilename read FDatabasename write SetDatabasename;
    property DSfGLevel: TDSfGLevel read FDSfGLevel write SetDSfGLevel default dlNone;
  end;

procedure Register;

implementation

{ TDSfGOutline }

{------------------------------------------------------}
procedure TDSfGOutline.SetDSfGLevel (Value: TDSfGLevel);
{------------------------------------------------------}
begin
  if Value <> FDSfGLevel then
  begin
    TblStation.Active := (Value in [dlStation, dlInstanz, dlArchiv,
                                    dlKanal, dlLogbuch, dlKanalLogbuch]);
    TblInstanz.Active := (Value in [dlInstanz, dlArchiv, dlKanal, dlLogbuch, dlKanalLogbuch]);
    TblArchive.Active := (Value in [dlArchiv, dlKanal, dlKanalLogbuch]);
    TblAKanaele.Active := (Value in [dlKanal, dlKanalLogbuch]);
    TblLogbuch.Active := (Value in [dlLogbuch, dlKanalLogbuch]);
    FDSfGLevel := Value;
    FillList;
  end;
end;

{----------------------------------------------------------}
function TDSfGOutline.GetSelected (Index: Integer): integer;
{----------------------------------------------------------}
var
  SelNode: TOutlineNode;
begin
  if SelectedItem > 0 then
  begin
    SelNode := Items [SelectedItem];
    FSelectedStation := -1;
    FSelectedInstanz := -1;
    FSelectedArchiv := -1;
    FSelectedKanal := -1;
    FSelectedLogbuch := -1;
    case SelNode.Level of
      2 : begin
            FSelectedStation := integer (SelNode.Data);
          end;
      3 : begin
            FSelectedInstanz := integer (SelNode.Data);
            FSelectedStation := integer (SelNode.Parent.Data);
          end;
      4 : begin
            if integer (SelNode.Data) > 0 then
              FSelectedArchiv := integer (SelNode.Data)
            else
              FSelectedLogbuch := Abs (integer (SelNode.Data));                  // Logbücher mit negativer Nummerierung
            FSelectedInstanz := integer (SelNode.Parent.Data);
            FSelectedStation := integer (SelNode.Parent.Parent.Data);
          end;
      5 : begin
            FSelectedKanal := integer (SelNode.Data);
            FSelectedArchiv := integer (SelNode.Parent.Data);
            FSelectedInstanz := integer (SelNode.Parent.Parent.Data);
            FSelectedStation := integer (SelNode.Parent.Parent.Parent.Data);
          end;
    end;
  end;
  case Index of
    1 : Result := FSelectedStation;
    2 : Result := FSelectedInstanz;
    3 : Result := FSelectedArchiv;
    4 : Result := FSelectedKanal;
    5 : Result := FSelectedLogbuch;
    else Result := -1;
  end;
end;

{------------------------------------------------------------------}
procedure TDSfGOutline.SetSelected (Index: Integer; Value: integer);
{------------------------------------------------------------------}
begin
  case Index of
    1: Select ([Value]);
    2: Select ([SelectedStation, Value]);
    3: Select ([SelectedStation, SelectedInstanz, Value]);
    4: Select ([SelectedStation, SelectedInstanz, SelectedArchiv, Value]);
    5: Select ([SelectedStation, SelectedInstanz, Value]);
  end;
end;

{--------------------------------------------------------}
procedure TDSfGOutline.SetDatabaseName (Value: TFilename);
{--------------------------------------------------------}
begin
  if Value <> FDatabaseName then
  begin
    FDatabaseName := Value;
    TblStation.DatabaseName := FDatabaseName;
    TblInstanz.DatabaseName := FDatabaseName;
    TblArchive.DatabaseName := FDatabaseName;
    TblAKanaele.DatabaseName := FDatabaseName;
    TblLogbuch.DatabaseName := FDatabaseName;
  end;
end;

{----------------------------------------------------}
function TDSfGOutline.GetSelectedStationsname: string;
{----------------------------------------------------}
begin
  Result := '?';
  if SelectedStation > 0 then
  begin
    if TblStation.FindKey ([FSelectedStation]) then
      Result := TblStation.FieldByName (CStationsname).asString;
  end;
end;

{---------------------------------------------------}
function TDSfGOutline.GetSelectedInstanzname: string;
{---------------------------------------------------}
begin
  Result := '?';
  if SelectedInstanz > 0 then
  begin
    if TblInstanz.FindKey ([FSelectedInstanz]) then
      Result := TblInstanz.FieldByName (CInstanzname).asString;
  end;
end;

{--------------------------------------------------}
function TDSfGOutline.GetSelectedArchivname: string;
{--------------------------------------------------}
begin
  Result := '?';
  if SelectedArchiv > 0 then
  begin
    if TblArchive.FindKey ([FSelectedInstanz, FSelectedArchiv]) then
      Result := TblArchive.FieldByName (CArchivname).asString;
  end;
end;

{-------------------------------------------------}
function TDSfGOutline.GetSelectedKanalname: string;
{-------------------------------------------------}
begin
  Result := '?';
  if SelectedKanal > 0 then
  begin
    if TblAKanaele.FindKey ([FSelectedKanal, FSelectedInstanz,
                             FSelectedArchiv]) then
      Result := TblAKanaele.FieldByName (CKanalname).asString;
  end;
end;

{---------------------------------------------------}
function TDSfGOutline.GetSelectedLogbuchname: string;
{---------------------------------------------------}
begin
  Result := '?';
  if SelectedLogbuch > 0 then
  begin
    if TblLogbuch.FindKey ([FSelectedInstanz, FSelectedLogbuch]) then
      Result := TblLogbuch.FieldByName (CLogbuchname).asString;
  end;
end;

{-----------------------------------------------------------------------------------------------------------}
function TDSfGOutline.GetOutlineIndex (Node: TOutlineNode; const Data: integer; const Text: string): integer;
{-----------------------------------------------------------------------------------------------------------}
var
  Index: integer;
begin
  Index := Node.GetFirstChild;
  while Index > 0 do
  begin
    if integer (Items [Index].Data) = Data then
    begin
      GetOutlineIndex := Index;
      exit;
    end;
    Index := Node.GetNextChild (Index);
  end;
  Index := AddChildSorted (Node.Index, Text);
  Items [Index].Data := Pointer (Data);
  GetOutlineIndex := Index;
end;

{------------------------------}
procedure TDSfGOutline.FillList;
{------------------------------}
begin
  Clear;
  RootIndex := Add (0, 'DSfG-Stationen');
  RootNode := Items [RootIndex];
  case DSfGLevel of
    dlStation:
      begin
        FillDlStation;
      end;
    dlInstanz:
      begin
        FillDlInstanz;
      end;
    dlArchiv:
      begin
        FillDlArchiv
      end;
    dlKanal:
      begin
        FillDlArchiv;
      end;
    dlLogbuch:
      begin
        FillDlLogbuch;
      end;
    dlKanalLogbuch:
      begin
        FillDlArchivLogbuch;
      end;
  end;
end;

{-------------------------------------------------------------}
function TDSfGOutline.InsertNode (Node: TOutlineNode): integer;
{-------------------------------------------------------------}
var
  ParentIndex: integer;
  ParentNode: TOutlineNode;
  ChildIndex: integer;
begin
  if Node.Level > 0  then
  begin
    ParentIndex := InsertNode (Node.Parent);
    if ItemCount > 0 then
    begin
      if ParentIndex = 0 then
        ParentNode := Items [1].Parent
      else
        ParentNode := Items [ParentIndex];
      ChildIndex := ParentNode.GetFirstChild;
      while ChildIndex > 0 do
      begin
        if Items [ChildIndex].Data = Node.Data then
        begin
          Result := ChildIndex;
          exit;
        end;
        ChildIndex := ParentNode.GetNextChild (ChildIndex);
      end;
    end;
    Result := AddChildObject (ParentIndex, Node.Text, Node.Data);
  end
  else
    Result := Node.Index;
end;

{-----------------------------------------------------------------------}
function TDSfGOutline.InsertOutline (anOutline: TCustomOutline): integer;
{-----------------------------------------------------------------------}
var
  i: integer;
begin
  Result:=0;
  for i := 1 to anOutline.ItemCount do
    Result:=AddChildObject(anOutline[i].Parent.Index,
                           anOutline[i].Text,
                           anOutline[i].Data);
end;

{-------------------------------------------------------}
procedure TDSfGOutline.Select (IDFeld: array of integer);
{-------------------------------------------------------}
var
  i: Integer;
  ParentNode: TOutlineNode;
  ChildIndex: integer;
  ChildFound: Boolean;
begin
  if itemCount > 0 then
  begin
    ParentNode := Items [1];
    for i := Low (IDFeld) to High (IdFeld) do
    begin
      ChildFound := False;
      ChildIndex := ParentNode.GetFirstChild;
      while (ChildIndex > 0) and not ChildFound do
      begin
        if (integer (Items [ChildIndex].Data)) = IDFeld [i] then
        begin
          ChildFound := True;
          ParentNode.Expand;
          SelectedItem := ChildIndex;
          ParentNode := Items [ChildIndex];
        end;
        ChildIndex := ParentNode.GetNextChild (ChildIndex);
      end;
      if not ChildFound then
        exit;
    end;
  end;
end;

{-----------------------------------}
procedure TDSfGOutline.FillDlStation;
{-----------------------------------}
var
  q        : TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= TblStation.DatabaseName;
    q.Sql.Add('SELECT Stationsname, StationId');
    q.Sql.Add('FROM "' + TblStation.TableName + '"');
    q.Sql.Add('ORDER BY Stationsname');
    q.Open;
    q.First;
    while not q.Eof do begin
      AddChildObject(Rootnode.Index,
                     q.FieldByName('Stationsname').asString,
                     Pointer(q.FieldByName('StationId').asInteger));
      q.Next;
    end;
  finally
    q.Free;
  end;
end;

{-----------------------------------}
procedure TDSfGOutline.FillDlInstanz;
{-----------------------------------}
var
  Index    : integer;
  q        : TQuery;
  aStaId   : integer;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= TblInstanz.DatabaseName;
    q.Sql.Add('SELECT A.Stationsname, A.StationId, B.Instanzname, B.InstanzID');
    q.Sql.Add('FROM "' + TblStation.TableName + '" A,');
    q.Sql.Add('"' + TblInstanz.TableName + '" B');
    q.Sql.Add('WHERE A.StationId = B.StationId');
    q.Sql.Add('ORDER BY A.Stationsname, A.StationId, B.Instanzname');
    q.Open;
    q.First;
    while not q.Eof do begin
      aStaId:= q.FieldByName('StationId').asInteger;
      Index:= AddChildObject(Rootnode.Index,
                     q.FieldByName('Stationsname').asString,
                     Pointer(q.FieldByName('StationId').asInteger));
      while (not q.Eof) and (q.FieldByName('StationId').asInteger = aStaId)
      do begin
        AddChildObject(Index,
                       q.FieldByName('Instanzname').asString,
                       Pointer(q.FieldByName('InstanzID').asInteger));
        q.Next;
      end;
    end;
  finally
    q.Free;
  end;
end;

{----------------------------------}
procedure TDSfGOutline.FillDlArchiv;
{----------------------------------}
var
  q                   : TQuery;
  StaIndex, InstIndex : integer;
  aStaId, anInstId    : integer;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= TblArchive.DatabaseName;
    q.Sql.Add('SELECT A.Stationsname, A.StationId,');
    q.Sql.Add('B.Instanzname, B.InstanzID,');
    q.Sql.Add('C.Name, C.ArchivNr');
    q.Sql.Add('FROM "' + TblStation.TableName + '" A,');
    q.Sql.Add('"' + TblInstanz.TableName + '" B,');
    q.Sql.Add('"' + TblArchive.TableName + '" C');
    q.Sql.Add('WHERE A.StationId = B.StationId');
    q.Sql.Add('AND B.InstanzID = C.InstanzID');
    q.Sql.Add('ORDER BY A.Stationsname, A.StationId,');
    q.Sql.Add('B.Instanzname, B.InstanzID, C.Name');
    q.Open;
    q.First;
    while not q.Eof do begin
      aStaId:= q.FieldByName('StationId').asInteger;
      StaIndex:= AddChildObject(Rootnode.Index,
                     q.FieldByName('Stationsname').asString,
                     Pointer(q.FieldByName('StationId').asInteger));
      while (not q.Eof) and (q.FieldByName('StationId').asInteger = aStaId)
      do begin
        anInstId:= q.FieldByName('InstanzID').asInteger;
        InstIndex:= AddChildObject(StaIndex,
                       q.FieldByName('Instanzname').asString,
                       Pointer(q.FieldByName('InstanzID').asInteger));
        while (not q.Eof) and (q.FieldByName('InstanzID').asInteger = anInstId)
        do begin
          AddChildObject(InstIndex,
                         q.FieldByName('Name').asString,
                         Pointer(q.FieldByName('ArchivNr').asInteger));
          q.Next;
      end;
      end;
    end;
  finally
    q.Free;
  end;
end;

{-----------------------------------}
procedure TDSfGOutline.FillDlLogbuch;
{-----------------------------------}
var
  q                   : TQuery;
  StaIndex, InstIndex : integer;
  aStaId, anInstId    : integer;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= TblLogbuch.DatabaseName;
    q.Sql.Add('SELECT A.Stationsname, A.StationId,');
    q.Sql.Add('B.Instanzname, B.InstanzID,');
    q.Sql.Add('C.Name, C.LogbuchNr');
    q.Sql.Add('FROM "' + TblStation.TableName + '" A,');
    q.Sql.Add('"' + TblInstanz.TableName + '" B,');
    q.Sql.Add('"' + TblLogbuch.TableName + '" C');
    q.Sql.Add('WHERE A.StationId = B.StationId');
    q.Sql.Add('AND B.InstanzID = C.InstanzID');
    q.Sql.Add('ORDER BY A.Stationsname, A.StationId,');
    q.Sql.Add('B.Instanzname, B.InstanzID, C.Name');
    q.Open;
    q.First;

    while not q.Eof do begin
      aStaId:= q.FieldByName('StationId').asInteger;
      StaIndex:= AddChildObject(Rootnode.Index,
                     q.FieldByName('Stationsname').asString,
                     Pointer(q.FieldByName('StationId').asInteger));
      while (not q.Eof) and (q.FieldByName('StationId').asInteger = aStaId)
      do begin
        anInstId:= q.FieldByName('InstanzID').asInteger;
        InstIndex:= AddChildObject(StaIndex,
                       q.FieldByName('Instanzname').asString,
                       Pointer(q.FieldByName('InstanzID').asInteger));
        while (not q.Eof) and (q.FieldByName('InstanzID').asInteger = anInstId)
        do begin
          AddChildObject(InstIndex,
                         q.FieldByName('Name').asString,
                         Pointer(q.FieldByName('LogbuchNr').asInteger * (-1)));
          q.Next;
      end;
      end;
    end;
  finally
    q.Free;
  end;
end;

{-----------------------------------------}
procedure TDSfGOutline.FillDlArchivLogbuch;
{-----------------------------------------}
{ Archivkanäle und Logbücher in eine Liste füllen }
var
  qa, ql               : TQuery;
  StaIndex, InstIndex : integer;
  aStaId, anInstId    : integer;
begin
  qa:= TQuery.Create(nil);
  try
    qa.DatabaseName:= TblArchive.DatabaseName;
    qa.Sql.Add('SELECT A.Stationsname, A.StationId,');
    qa.Sql.Add('B.Instanzname, B.InstanzID,');
    qa.Sql.Add('C.Name, C.ArchivNr');
    qa.Sql.Add('FROM "' + TblStation.TableName + '" A,');
    qa.Sql.Add('"' + TblInstanz.TableName + '" B,');
    qa.Sql.Add('"' + TblArchive.TableName + '" C');
    qa.Sql.Add('WHERE A.StationId = B.StationId');
    qa.Sql.Add('AND B.InstanzID = C.InstanzID');
    qa.Sql.Add('ORDER BY A.Stationsname, A.StationId,');
    qa.Sql.Add('B.Instanzname, B.InstanzID, C.Name');
    qa.Open;
    qa.First;

    ql:= TQuery.Create(nil);
    try
      ql.DatabaseName:= TblLogbuch.DatabaseName;
      ql.Sql.Add('SELECT A.Stationsname, A.StationId,');
      ql.Sql.Add('B.Instanzname, B.InstanzID,');
      ql.Sql.Add('C.Name, C.LogbuchNr');
      ql.Sql.Add('FROM "' + TblStation.TableName + '" A,');
      ql.Sql.Add('"' + TblInstanz.TableName + '" B,');
      ql.Sql.Add('"' + TblLogbuch.TableName + '" C');
      ql.Sql.Add('WHERE A.StationId = B.StationId');
      ql.Sql.Add('AND B.InstanzID = C.InstanzID');
      ql.Sql.Add('ORDER BY A.Stationsname, A.StationId,');
      ql.Sql.Add('B.Instanzname, B.InstanzID, C.Name');
      ql.Open;
      ql.First;

      while not qa.Eof do begin
        aStaId:= qa.FieldByName('StationId').asInteger;
        StaIndex:= AddChildObject(Rootnode.Index,
                                  qa.FieldByName('Stationsname').asString,
                                  Pointer(qa.FieldByName('StationId').asInteger));
        while (not qa.Eof) and (qa.FieldByName('StationId').asInteger = aStaId) do begin
          anInstId:= qa.FieldByName('InstanzID').asInteger;
          InstIndex:= AddChildObject(StaIndex,
                                     qa.FieldByName('Instanzname').asString,
                                     Pointer(qa.FieldByName('InstanzID').asInteger));
          { zuerst die Archivgruppen... }
          while (not qa.Eof) and (qa.FieldByName('InstanzID').asInteger = anInstId) do begin
            AddChildObject(InstIndex,
                           qa.FieldByName('Name').asString,
                           Pointer(qa.FieldByName('ArchivNr').asInteger));
            qa.Next;
          end;
          { dann die Logbücher... }
          while (not ql.Eof) and (ql.FieldByName('InstanzID').asInteger = anInstId) do begin
            AddChildObject(InstIndex,
                           ql.FieldByName('Name').asString,
                           Pointer(ql.FieldByName('LogbuchNr').asInteger * (-1)));     // negative Nummern für Logbücher
            ql.Next;
          end;
        end;
      end;
    finally
      ql.Free;
    end;
  finally
    qa.Free;
  end;
end;

{---------------------------------------------------------------------------------}
function TDSfGOutline.AddChildSorted (Index: integer; const Text: string): integer;
{---------------------------------------------------------------------------------}
var
  Node: TOutlineNode;
  newIndex: integer;
begin
  Node := Items [Index];
  newIndex := Node.GetFirstChild;
  while newIndex > 0 do
  begin
    if AnsiCompareText (Text, Items [newIndex].Text) < 0 then
      break;
    newIndex := Node.GetNextChild (newIndex);
  end;
  if newIndex > 0 then
    Result := Insert (newIndex, Text)
  else
    Result := AddChild (Index, Text);
end;

{---------------------------------------------}
procedure TDSfGOutline.Expand (Index: integer);
{---------------------------------------------}
var
  IndexArchiv: integer;
  NodeArchiv: TOutlineNode;
//  IndexKanal: integer;
  KanalNr: integer;
begin
  inherited Expand (Index);
  if ((DSfGLevel = dlKanal) OR (DSfGLevel = dlKanalLogbuch)) and (Items [Index].Level = 3) then begin
    TblAKanaele.Open;
    try
      IndexArchiv := Items [Index].GetFirstChild;
      while IndexArchiv > 0 do begin
        NodeArchiv := Items [IndexArchiv];
        TblAKanaele.SetRange ([integer (Items [Index].Data),
                               integer (NodeArchiv.Data)],
                              [integer (Items [Index].Data),
                               integer (NodeArchiv.Data)]);
        TblAKanaele.ApplyRange;
        TblAKanaele.First;
        while not TblAKanaele.EOF do begin
          KanalNr:=TblAKanaele.FieldByName ('KanalNr').asInteger;
      {    IndexKanal := }GetOutlineIndex (NodeArchiv,
                          KanalNr,
                          TblAKanaele.FieldByName ('Name').asString +
                          ' [K' +
                          IntToStr (KanalNr) +
                          ', ' +
                          TblAKanaele.FieldByName ('Kanaltyp').asString +
                          ', ' +
                          TblAKanaele.FieldByName ('Werteart').asString +
                          ']');
          TblAKanaele.Next;
        end;
        IndexArchiv := Items [Index].GetNextChild (IndexArchiv);
      end;
    finally
      TblAKanaele.Close;
    end;
  end;
end;

{---------------------------------------------------}
constructor TDSfGOutline.Create (AOwner: TComponent);
{---------------------------------------------------}
begin
  inherited Create (AOwner);
  ExpandLevel:= dlKanal;  { default-ExpandLevel }
  FDSfGLevel := dlNone;
  TblStation := TTable.Create (Self);
  TblStation.Databasename := FDatabasename;
  TblStation.TableName := CStation;
  TblStation.ReadOnly := True;
  TblInstanz := TTable.Create (Self);
  TblInstanz.DatabaseName := FDatabasename;
  TblInstanz.Tablename := CInstanz;
  TblInstanz.ReadOnly := True;
  TblArchive := TTable.Create (Self);
  TblArchive.Databasename := FDatabasename;
  TblArchive.Tablename := CArchive;
  TblArchive.Readonly := True;
  TblAKanaele := TTable.Create (Self);
  TblAKanaele.Databasename := FDatabaseName;
  TblAKanaele.Tablename := CAKanaele;
  TblAKanaele.Indexname := 'Archiv-K';
  TblAKanaele.ReadOnly := True;
  TblLogbuch := TTable.Create (Self);
  TblLogbuch.Databasename := FDatabasename;
  TblLogbuch.Tablename := CLogbuch;
  TblLogbuch.ReadOnly := True;
end;

{------------------------------}
destructor TDSfGOutline.Destroy;
{------------------------------}
begin
  TblLogbuch.Free;
  TblAKanaele.Free;
  TblArchive.Free;
  TblInstanz.Free;
  TblStation.Free;
  inherited Destroy;
end;

procedure Register;
begin
  RegisterComponents('Wieser', [TDSfGOutline]);
end;

end.
