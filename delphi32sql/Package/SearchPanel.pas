// -----------------------------------------------------------------------------
// Unit: Komponente für die globale Suche nach einem String in
//       String-, Draw- oder DB-Grid / Treeview / ListView / ListBox
//
// 08.02.2011  WN  Neu
// 11.04.2011  WN  Treeview: markieren des gefundenen Eintrags, automat. Scrollen
// 15.01.2014  WW  Ereignis OnSearchEditClick
//
// Copyright © RMG Messtechnik GmbH 2011, 2014
// -----------------------------------------------------------------------------
unit SearchPanel;

interface

uses
  Controls, ComCtrls, DB, ExtCtrls, SysUtils, Classes, StdCtrls, Grids, DBGrids,
  Buttons, Forms, MeldAbgleichListe, GD_Utils;

const
  C_Bmp_Previous = 'LINKS';
  C_Bmp_Next     = 'RECHTS';

type
  // Prozedurtypen
  TCBOnCellClick = procedure (Column: TColumn) of object;
  TCBOnClick = procedure (Sender: TObject) of object;

  TSearchPanel = class(TPanel)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    { Private-Deklarationen }
    FSearchEdit: TEdit;
    FSearchGrid1: TCustomGrid;
    FSearchGrid2: TCustomGrid;
    FSearchGrid1_OnCellClick: TCBOnCellClick;
    FSearchGrid2_OnCellClick: TCBOnCellClick;
    FSearchTree1: TTreeView;
    FSearchTree2: TTreeView;
    FSearchList1: TListView;
    FSearchList2: TListView;
    FSearchBox1: TCustomListBox;
    FSearchBox2: TCustomListBox;
    FSearchGridList: TList;
    FbbtnPrev: TBitBtn;
    FbbtnNext: TBitBtn;
    FAktiv: Boolean;
    FSearchEditClick: TCBOnClick;
    function CompareText(const sSource: String): Boolean;
    procedure ResizePanel(Sender: TObject);
    procedure InitPanel(bState: Boolean);
    procedure ClickPanel(Sender: TObject);  virtual;
    procedure DBGridCellClick(Column: TColumn);
    procedure GridSearchStart(pGrid: TCustomGrid; bPrevious: Boolean);
    procedure SearchStringInSource(bFindPrev: Boolean; bFirst: Boolean);
    procedure SearchStringInSourceStart(Sender: TObject);
    procedure SearchStringInSourcePrevious(Sender: TObject);
    procedure SearchStringInSourceNext(Sender: TObject);
    procedure SucheStringImGrid(pGrid: TCustomGrid; iRowStart, iColStart: Integer;
                                bFindPrev: Boolean; bFirst: Boolean);
    procedure SucheStringImTree(pTree: TTreeView; tnStart: TTreeNode;
                                bFindPrev: Boolean; bFirst: Boolean);
    procedure SucheStringInList(pList: TObject; iStart: Integer; bFindPrev: Boolean);
    procedure SearchEditClick(Sender: TObject);
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
  published
    { Published-Deklarationen }
    property SearchGrid_1: TCustomGrid read FSearchGrid1 write FSearchGrid1;
    property SearchGrid_2: TCustomGrid read FSearchGrid2 write FSearchGrid2;
    property SearchTree_1: TTreeView read FSearchTree1 write FSearchTree1;
    property SearchTree_2: TTreeView read FSearchTree2 write FSearchTree2;
    property SearchList_1: TListView read FSearchList1 write FSearchList1;
    property SearchList_2: TListView read FSearchList2 write FSearchList2;
    property SearchBox_1: TCustomListBox read FSearchBox1 write FSearchBox1;
    property SearchBox_2: TCustomListBox read FSearchBox2 write FSearchBox2;
    property SearchGridList: TList read FSearchGridList write FSearchGridList;
    property SearchGrid1_OnCellClick: TCBOnCellClick
                   read FSearchGrid1_OnCellClick write FSearchGrid1_OnCellClick;
    property SearchGrid2_OnCellClick: TCBOnCellClick
                   read FSearchGrid2_OnCellClick write FSearchGrid2_OnCellClick;
    property Aktiv: Boolean read FAktiv write InitPanel;
    property OnSearchEditClick: TCBOnClick read FSearchEditClick write
      FSearchEditClick;  // 15.01.2014, WW
  end;

const
  SearchPnl : TSearchPanel = nil;  // Man. Alternative zu "TSearchPanel"

procedure Register;

implementation

{$R *.RES}

// -----------------------------------------------------------------------------
procedure Register;
// -----------------------------------------------------------------------------
begin
  RegisterComponents('Wieser', [TSearchPanel]);
end;

// -----------------------------------------------------------------------------
constructor TSearchPanel.Create(AOwner: TComponent);
// -----------------------------------------------------------------------------
begin
  inherited Create(AOwner);

  // Panel-Eigenschaften:
  BevelOuter  := bvLowered;
  Align       := alNone;
  AutoSize    := False;
  TabStop     := True;
  Caption     := '';
  Height      := 32;
  Width       := 227;
  OnResize    := ResizePanel;
  OnClick     := ClickPanel;
  ParentFont  := False;
  Font.Name   := 'MS Sans Serif';
  Font.Size   := 10;
  FAktiv      := True;

  // Edit-Feld für Eingabe des Suchstrings hinzufügen:
  FSearchEdit := TEdit.Create(Self);
  with FSearchEdit do
  begin
    Autosize := True;
    Text     := '';
    TabOrder := 1;
    TabStop  := False;
    OnChange := SearchStringInSourceStart;
    OnEnter  := Self.OnEnter;
    OnClick  := SearchEditClick;  // 15.01.2014, WW
    Parent   := Self;
    Left     := 32;
    Top      := 5;
    Width    := 163;
    Height   := 22;
  end;

  // Buttons für erweiterte Suche:
  FbbtnPrev := TBitBtn.Create(Self);
  with FbbtnPrev do
  begin
    Parent     := Self;
    TabOrder   := 0;
    TabStop    := False;
    NumGlyphs  := 2;
    Glyph.LoadFromResourceName(HInstance, C_Bmp_Previous);
    Caption    := '';
    Visible    := True;
    Left       := 5;
    Top        := 5;
    Width      := 22;
    Height     := 22;
    OnClick    := SearchStringInSourcePrevious;
    OnEnter    := Self.OnEnter;
  end;

  FbbtnNext := TBitBtn.Create(Self);
  with FbbtnNext do
  begin
    Parent     := Self;
    TabOrder   := 2;
    TabStop    := False;
    NumGlyphs  := 2;
    Glyph.LoadFromResourceName(HInstance, C_Bmp_Next);
    Caption    := '';
    Visible    := True;
    Left       := 200;
    Top        := 5;
    Width      := 22;
    Height     := 22;
    OnClick    := SearchStringInSourceNext;
    OnEnter    := Self.OnEnter;
  end;

  // Quellen, in denen String gesucht werden soll:
  FSearchGrid1 := nil;
  FSearchGrid2 := nil;
  FSearchTree1 := nil;
  FSearchTree2 := nil;
  FSearchList1 := nil;
  FSearchList2 := nil;
  FSearchBox1  := nil;
  FSearchBox2  := nil;
  FSearchGridList := nil;  // Liste für DrawGrid (z.B. Meldungen)
  FSearchGrid1_OnCellClick := nil;
  FSearchGrid2_OnCellClick := nil;
  FSearchEditClick := nil;
end;

// -----------------------------------------------------------------------------
destructor TSearchPanel.Destroy;
// -----------------------------------------------------------------------------
begin
  FreeAndNil(FbbtnNext);
  FreeAndNil(FbbtnPrev);
  FreeAndNil(FSearchEdit);

  inherited;
end;

// -----------------------------------------------------------------------------
// prüfen, ob Teilstring (FSearchEdit.Text) in String (sSource) enthalten ist
// Parameter: String, der durchsucht werden soll
// -----------------------------------------------------------------------------
function TSearchPanel.CompareText(const sSource: String): Boolean;
// -----------------------------------------------------------------------------
begin
  if (Pos(AnsiUpperCase(FSearchEdit.Text), AnsiUpperCase(sSource)) > 0) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------
// Breite Edit-Feld entsprechend Panel-Breite anpassen
// -----------------------------------------------------------------------------
procedure TSearchPanel.ResizePanel(Sender: TObject);
// -----------------------------------------------------------------------------
var
  iWidth: Integer;
begin
  // Breite Buttons abziehen
  iWidth := ClientWidth - FbbtnPrev.Width - FbbtnNext.Width - 20;
  if (iWidth > 0) then
  begin
    // Breite Edit-Feld festlegen
    FSearchEdit.Width := iWidth;
    // Position FbbtnNext festlegen
    FbbtnNext.Left := FSearchEdit.Left + FSearchEdit.Width + 5;
  end;
end;

// -----------------------------------------------------------------------------
procedure TSearchPanel.ClickPanel(Sender: TObject);
// -----------------------------------------------------------------------------
begin
  // falls beim OnClick was passieren soll...
  // -> z.B. Zuweisung der aktuellen Search-Objekte
end;

// -----------------------------------------------------------------------------
// Wechsel des Fokus: Datenmenge im DB Grid aktivieren
// -----------------------------------------------------------------------------
procedure TSearchPanel.DBGridCellClick(Column: TColumn);
// -----------------------------------------------------------------------------
begin
  if Assigned(SearchGrid_1) and (SearchGrid_1 is TDBGrid) then
  begin
    with TDBGrid(SearchGrid_1) do
    begin
      Visible := False;
      Visible := True;
      // "Original"-OnClick-Ereignis zuweisen und ausführen
      OnCellClick := FSearchGrid1_OnCellClick;
      if Assigned(OnCellClick) then OnCellClick(Column);
    end;
  end;
  if Assigned(SearchGrid_2) and (SearchGrid_2 is TDBGrid) then
  begin
    with TDBGrid(SearchGrid_2) do
    begin
      Visible := False;
      Visible := True;
      // "Original"-OnClick-Ereignis zuweisen und ausführen
      OnCellClick := FSearchGrid2_OnCellClick;
      if Assigned(OnCellClick) then OnCellClick(Column);
    end;
  end;
end;

// -----------------------------------------------------------------------------
// En- bzw. Disable
// -----------------------------------------------------------------------------
procedure TSearchPanel.InitPanel(bState: Boolean);
// -----------------------------------------------------------------------------
begin
  FAktiv := bState;
  FSearchEdit.Enabled := bState;
  FbbtnPrev.Enabled   := bState;
  FbbtnNext.Enabled   := bState;
end;

// -----------------------------------------------------------------------------
// Ermittlung des Start-Eintrages für Grid-Suche
// Parameter: Grid, in dem gesucht werden soll
//            Suchrichtung
// -----------------------------------------------------------------------------
procedure TSearchPanel.GridSearchStart(pGrid: TCustomGrid; bPrevious: Boolean);
// -----------------------------------------------------------------------------
var
  iRowStart, iColStart: Integer;
begin
  iColStart := -1;
  iRowStart := -1;

  if Assigned(pGrid) then
  begin
    if (pGrid is TStringGrid) then  // Stringgrid
    begin
      with TStringGrid(pGrid) do
      begin
        if bPrevious then  // vorheriger Eintrag
        begin
          if (Col > FixedCols) then
          begin
            iColStart := Col - 1;  // vorherige Spalte
            iRowStart := Row;      // gleiche Zeile
          end else if (Col = FixedCols) then
          begin
            if (Row > FixedRows) then
            begin
              iColStart := ColCount - 1;  // letzte Spalte
              iRowStart := Row - 1;       // vorherige Zeile
            end;
          end;
        end else  // nächster Eintrag
        begin
          if (Col < ColCount - 1) then
          begin
            iColStart := Col + 1;  // nächste Spalte
            iRowStart := Row;      // gleiche Zeile
          end else if (Col = ColCount - 1) then
          begin
            if (Row < (RowCount - 1)) then
            begin
              iColStart := FixedCols;  // erste Spalte
              iRowStart := Row + 1;    // nächste Zeile
            end;
          end;
        end;
      end;
    end else if (pGrid is TDrawGrid) then  // Drawgrid
    begin
      iColStart := 0;
      with TDrawGrid(pGrid) do
      begin
        if bPrevious and (Row > FixedRows) then
          iRowStart := Row - 1    // vorherige Zeile
        else if not bPrevious and (Row < (RowCount - 1)) then
          iRowStart := Row + 1;   // nächste Zeile
      end;
    end else if (pGrid is TDBGrid) then  // DBGrid
    begin
      with TDBGrid(pGrid).DataSource.DataSet do
      begin
        if (bPrevious and (RecNo > 0)) or
           (not bPrevious and (RecNo < RecordCount)) then
        begin
          iColStart := 0;
          iRowStart := 0;
        end;
      end;
    end;
  end;

  if (iColStart <> -1) and (iRowStart <> -1) then
    SucheStringImGrid(pGrid, iRowStart, iColStart, bPrevious, False);
end;

// -----------------------------------------------------------------------------
// globale Suche in allen Objekten nach String
// Parameter: Suchrichtung
//            Suche ab erstem oder markierten Eintrag ?
// -----------------------------------------------------------------------------
procedure TSearchPanel.SearchStringInSource(bFindPrev: Boolean; bFirst: Boolean);
// -----------------------------------------------------------------------------
var
  i: Integer;
  iStart: Integer;
  tn: TTreeNode;
  pTree: TTreeView;
  pGrid: TCustomGrid;
  pList: TListView;
  pBox: TCustomListBox;
begin
  OnClick(Self);
  if Aktiv then
  begin
    for i:=0 to 1 do
    begin
      case i of
        0: begin
          pTree := SearchTree_1;
          pGrid := SearchGrid_1;
          pList := SearchList_1;
          pBox  := SearchBox_1;
        end;
        else begin
          pTree := SearchTree_2;
          pGrid := SearchGrid_2;
          pList := SearchList_2;
          pBox  := SearchBox_2;
        end;
      end;

      // Suche im Grid (String,- Draw- oder DB-Grid)
      if Assigned(pGrid) then
      begin
        if (pGrid is TDBGrid) then
          TDBGrid(pGrid).OnCellClick := DBGridCellClick;

        if bFirst then
        begin
          if (pGrid is TDrawGrid) then
            SucheStringImGrid(pGrid, TDrawGrid(pGrid).FixedRows,
                              TDrawGrid(pGrid).FixedCols, bFindPrev, bFirst)
          else if (pGrid is TDBGrid) then
            SucheStringImGrid(pGrid, 0, 0, bFindPrev, bFirst);
        end else
        begin
          GridSearchStart(pGrid, bFindPrev);
        end;
      end;

      // Suche im TreeView
      if Assigned(pTree) then
      begin
        pTree.HideSelection := False;
        if bFirst then
          tn := pTree.Items.GetFirstNode
        else begin
          tn := pTree.Selected;
          if bFindPrev then
          begin
            if not Assigned(tn) then
              tn := pTree.Items.GetFirstNode
            else
              tn := tn.GetPrev;
          end else
          begin
            if not Assigned(tn) then
              tn := pTree.Items.GetFirstNode
            else
              tn := tn.GetNext;
          end;
        end;
        if Assigned(tn) then
          SucheStringImTree(pTree, tn, bFindPrev, bFirst);
      end;

      // Suche in ListView
      if Assigned(pList) and (pList.Items.Count > 0) then
      begin
        if bFirst then
          iStart := 0
        else if bFindPrev then
          iStart := pList.Selected.Index - 1
        else
          iStart := pList.Selected.Index + 1;
        if (iStart >= 0) and (iStart < pList.Items.Count) then
        begin
          SucheStringInList(pList, iStart, bFindPrev);
        end;
      end;

      // Suche in ListBox
      if Assigned(pBox) and (pBox.Items.Count > 0) then
      begin
        if bFirst then
          iStart := 0
        else if bFindPrev then
          iStart := pBox.ItemIndex - 1
        else
          iStart := pBox.ItemIndex + 1;
        if (iStart >= 0) and (iStart < pBox.Items.Count) then
        begin
          SucheStringInList(pBox, iStart, bFindPrev);
        end;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------
// globale Suche nach String ab erstem Eintrag, Such-Richtung vorwärts
// -----------------------------------------------------------------------------
procedure TSearchPanel.SearchStringInSourceStart(Sender: TObject);
// -----------------------------------------------------------------------------
begin
  SearchStringInSource(False, True);
end;

// -----------------------------------------------------------------------------
// Suche nach String ab markiertem Eintrag, Such-Richtung rückwärts
// -----------------------------------------------------------------------------
procedure TSearchPanel.SearchStringInSourcePrevious(Sender: TObject);
// -----------------------------------------------------------------------------
begin
  SearchStringInSource(True, False);
end;

// -----------------------------------------------------------------------------
// Suche nach String ab markiertem Eintrag, Such-Richtung vorwärts
// -----------------------------------------------------------------------------
procedure TSearchPanel.SearchStringInSourceNext(Sender: TObject);
// -----------------------------------------------------------------------------
begin
  SearchStringInSource(False, False);
end;

// -----------------------------------------------------------------------------
// Suche eines Strings im Grid
// Parameter: Grid, in dem gesucht werden soll
//            Start-Zelle, ab der gesucht werden soll
//            Suchrichtung (vorwärts oder rückwärts)
//            Flag: Suche beginnt beim ersten Eintrag
// -----------------------------------------------------------------------------
procedure TSearchPanel.SucheStringImGrid(pGrid: TCustomGrid;
                                         iRowStart, iColStart: Integer;
                                         bFindPrev: Boolean; bFirst: Boolean);
// -----------------------------------------------------------------------------
var
  iRow, iCol: Integer;
  iRowEnde, iColEnde: Integer;
  bGefunden, bStart: Boolean;
  s: String;
  bm: TBookmark;
begin
  bGefunden := False;
  // Suchen im StringGrid
  if pGrid is TStringGrid then
  begin
    with TStringGrid(pGrid) do
    begin
      // max. mögl. Such-Ende ermitteln
      if bFindPrev then
      begin
        iRowEnde := FixedRows - 1;
        iColEnde := FixedCols - 1;
      end else
      begin
        iRowEnde := RowCount;
        iColEnde := ColCount;
      end;
      iRow := iRowStart;
      iCol := iColStart;
      while (iRow <> iRowEnde) do
      begin
        Application.ProcessMessages;
        if bFindPrev and (iRow <> iRowStart) then  // Spalten rückwärts
        begin
          iCol := ColCount - 1;
        end else if not bFindPrev and (iRow <> iRowStart) then  // Spalten vorwärts
        begin
          iCol := FixedCols;
        end;
        // in Zelle String suchen
        while (iCol <> iColEnde) do
        begin
          s := Cells[iCol, iRow];
          if CompareText(s) then
          begin
            bGefunden := True;
            Col := iCol;
            Row := iRow;
          end;
          if bGefunden then Break;   // gefunden -> Zelle merken und Suche abbrechen
          if bFindPrev then
            Dec(iCol)
          else
            Inc(iCol);
        end;
        if bGefunden then Break;  // gefunden -> Zelle merken und Suche abbrechen
        if bFindPrev then
          Dec(iRow)
        else
          Inc(iRow);
      end;
    end;
  // Suchen im DrawGrid
  end else if (pGrid is TDrawGrid) and Assigned(SearchGridList) then
  begin
    // Meldungen
    if (FSearchGridList is TMeldAbgleichList) then
    begin
      with TDrawGrid(pGrid) do
      begin
        // max. mögl. Such-Ende ermitteln
        if bFindPrev then
          iRowEnde := FixedRows - 1
        else
          iRowEnde := RowCount;
        iRow := iRowStart;
        while (iRow <> iRowEnde) do
        begin
          Application.ProcessMessages;
          // alle Einträge einer Zeile prüfen:
          s := TMeldAbgleichList(FSearchGridList).GetGeraeteArt(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetZeitzone_K(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetZeitzone_G(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetMNrAllg_K(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetMNrAllg_G(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetMNrGeraet_K(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetMNrGeraet_G(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetMText(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetMTyp_K(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetMTyp_G(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetMArt_K(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetMArt_G(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetBemerkung_K(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetBemerkung_G(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetDSfG_Status_K(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetDSfG_Status_G(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetDSfG_CRC_K(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetDSfG_CRC_G(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetPara_Text(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetPara_WertAlt(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetPara_WertNeu(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetStationstext1(iRow - FixedRows) + '|';
          s := s + TMeldAbgleichList(FSearchGridList).GetStationstext2(iRow - FixedRows) + '|';
          if CompareText(s) then
          begin
            Row := iRow;
            Break;
          end;
          if bFindPrev then
            Dec(iRow)
          else
            Inc(iRow);
        end;
      end;
    end;
  // Suchen im DBGrid
  end else if pGrid is TDBGrid then
  try
    TDBGrid(pGrid).DataSource.DataSet.DisableControls;
    // markierten Eintrag merken
    with TDBGrid(pGrid).DataSource.DataSet do
    try
      bm := GetBookmark;  // markierten Eintrag merken
      if bFirst then
        First  // markiere ersten Eintrag
      else begin
        pGrid.DoubleBuffered := True;
      end;

      if bFindPrev then  // suche rückwärts
      begin
        while not Bof do
        begin
          Application.ProcessMessages;
          Prior;
          iCol := 0;
          while (iCol < FieldCount) do  // in Zelle String suchen
          begin
            Application.ProcessMessages;
            s := Fields[iCol].AsString;
            if CompareText(s) then
            begin
              bGefunden := True;  // gefunden -> Suche abbrechen
              Break;
            end else
              Inc(iCol);
          end;
          if bGefunden then Break;
        end;
      end else  // suche vorwärts
      begin
        bStart := bFirst;
        while not Eof do
        begin
          if bStart then
            bStart := False
          else
            Next;
          iCol := 0;
          while (iCol < FieldCount) do  // in Zelle String suchen
          begin
            Application.ProcessMessages;
            s := Fields[iCol].AsString;
            if CompareText(s) then
            begin
              bGefunden := True;  // gefunden -> Suche abbrechen
              Break;
            end else
              Inc(iCol);
          end;
          if bGefunden then Break;
        end;
      end;
      // springe wieder an ursprüngliche Position
      if not bGefunden then GotoBookmark(bm);
    finally
      FreeBookmark(bm);
    end;
  finally
    pGrid.Visible := True;
    TDBGrid(pGrid).DataSource.DataSet.EnableControls;
  end;
end;

// -----------------------------------------------------------------------------
// Suche eines Strings im Treeview (geprüft wird jeder Knoten)
// Parameter: Tree, in dem gesucht werden soll
//            Start-Knoten, ab dem gesucht werden soll
//            Suchrichtung (vorwärts oder rückwärts)
//            Flag: Suche beginnt beim ersten Eintrag
// -----------------------------------------------------------------------------
procedure TSearchPanel.SucheStringImTree(pTree: TTreeView; tnStart: TTreeNode;
                                         bFindPrev: Boolean; bFirst: Boolean);
// -----------------------------------------------------------------------------
var
  tn: TTreeNode;
begin
  // Suchen im Treeview
  if Assigned(pTree) then
  begin
    with pTree do
    try
      // 11.04.2011  WN
      HideSelection := False;  // selektierten Eintrag immer anzeigen
      Items.BeginUpdate;
      tn := tnStart;
      while Assigned(tn) do
      begin
        Application.ProcessMessages;
        if CompareText(tn.Text) then  // String gefunden
        begin
          Selected := tn;
          Selected.MakeVisible;  // springe zum selektierten Eintrag  // 11.04.2011  WN
          Break;
        end else  // nächsten Knoten finden
        begin
          if bFindPrev then
            tn := tn.GetPrev
          else
            tn := tn.GetNext;
        end;
      end;
    finally
      Items.EndUpdate;
      if not bFirst then SetFocus;  // Focus
    end;
  end;
end;

// -----------------------------------------------------------------------------
// Suche eines Strings im ListView / ListBox
// Parameter: Object, in dem gesucht werden soll
//            Start-Position, ab der gesucht werden soll
//            Suchrichtung (vorwärts oder rückwärts)
// -----------------------------------------------------------------------------
procedure TSearchPanel.SucheStringInList(pList: TObject; iStart: Integer;
                                         bFindPrev: Boolean);
// -----------------------------------------------------------------------------
var
  i: Integer;
  pListItem: TListItem;
begin
  // Suchen im Listview
  if Assigned(pList) and (pList is TListView) then
  begin
    with TListView(pList) do
    begin
      i := iStart;
      pListItem := Items.Item[i];
      while Assigned(pListItem) do
      begin
        Application.ProcessMessages;
        if CompareText(pListItem.Caption) or
           CompareText(pListItem.SubItems.CommaText) then  // String gefunden
        begin
          Selected := pListItem;
          Selected.MakeVisible(False);  // springe zum selektierten Eintrag  // 11.04.2011  WN
          Break;
        end else  // nächsten Eintrag finden
        begin
          if bFindPrev then Dec(i)
          else Inc(i);
          pListItem := Items.Item[i];
        end;
      end;
    end;
  // Suchen in Listbox
  end else if Assigned(pList) and (pList is TCustomListBox) then
  begin
    with TCustomListBox(pList) do
    begin
      i := iStart;
      while (i < Items.Count) and (i >= 0) do
      begin
        Application.ProcessMessages;
        if CompareText(Items.Strings[i]) then  // String gefunden
        begin
          ItemIndex := i;
          Break;
        end else  // nächsten Eintrag finden
        begin
          if bFindPrev then Dec(i)
          else Inc(i);
        end;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------
procedure TSearchPanel.SearchEditClick(Sender: TObject);  // 15.01.2014, WW
// -----------------------------------------------------------------------------
begin
  if Assigned (FSearchEditClick) then
    FSearchEditClick (Sender);
  FSearchEdit.SetFocus;
end;

end.
