{------------------------------------------------------------------------------}
{ Darstellungsobjekte für einen DSfG-Bus (von TMyCustomShape abgeleitet)       }
{                                                                              }
{ 20.07.2000  GD    Neu                                                        }
{ 10.10.2000  GD    TDSfGGeraeteShape                                          }
{ 18.10.2000  GD    TDSfGInstanzParameterShape                                 }
{ 28.11.2000  GD    TDSfGMesstreckenShape, TDSfGInstanzParameterShape ausgegl. }
{ 07.06.2004  GD    Erweitert um KGM-Instanzen                                 }
{ 13.11.2007  GD    Erweitert um Blenden-Instanzen                             }
{ 09.07.2010  GD    Erweitert um DFÜ2                                          }
{ 26.01.2016  WW    mit Symbol für erweiterte DSfG-DFÜ und elektron. Gaszähler }
{ 05.03.2024  WW    mit Symbol für erweiterte Odorierung und Gasbegleitstoff-  }
{                   messung; resourcestrings                                   }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, RMG Messtechnik GmbH 2024               }
{------------------------------------------------------------------------------}
unit DSfGShapes;

interface

uses
  Windows, Classes, Controls, Forms, Graphics, Buttons, Grids, MyShapes,
  GD_Utils, ExtCtrls, SysUtils, Dialogs, DSG_Utils, WSysCon;

const
  C_Glyph_MRG2200  = 'BB_MRG2200';
  C_Glyph_Wieser2  = 'BBWIESER2';
  C_Glyph_Trash    = 'BBTRASH';
  C_Glyph_DivBlock = 'BB_DIVBLOCK';
  C_Glyph_Ventil   = 'BB_VENTIL';

  // Allgemeine Farben
  C_Color_Canvas   = clWindow;
  C_Color_BackBone = clBlack;

  // Instanzdarstellung
  C_Shape_Umwerter = stRectangle;
  C_Color_Umwerter = clAqua;
  C_Glyph_Umwerter = 'BBUMWERTER';

  C_Shape_Blende   = stRectangle;
  C_Color_Blende   = clAqua;
  C_Glyph_Blende   = 'BBBLENDENRECHNER';

  C_Shape_Registrierung = stRectangle;
  C_Color_Registrierung = clRed;
  C_Glyph_Registrierung = 'BBREGISTRIERUNG';

  C_Shape_Gasbeschaffenheit = stRectangle;
  C_Color_Gasbeschaffenheit = clYellow;
  C_Glyph_Gasbeschaffenheit = 'BBGASBESCHAFFENHEIT';

  C_Shape_KorrGasbeschaffenheit = stRectangle;
  C_Glyph_KorrGasbeschaffenheit = 'BBKORRGASBESCHAFFENHEIT';

  C_Shape_Revision = stRectangle;
  C_Color_Revision = clGray;
  C_Glyph_Revision = 'BBREVISION';

  C_Shape_Steuerung = stRectangle;
  C_Color_Steuerung = clRed;
  C_Glyph_Steuerung = 'BBSTEUERUNG';

  C_Shape_DFUE = stRectangle;
  C_Color_DFUE = clLime;
  C_Glyph_DFUE = 'BBDFUE';

  C_Shape_Drucker = stRectangle;
  C_Color_Drucker = clAqua;
  C_Glyph_Drucker = 'BBPRINTER';

  C_Shape_Wieser = stRectangle;
  C_Color_Wieser = $000080FF;
  C_Glyph_Wieser = 'BBWIESERW';

  C_Shape_unkown = stRectangle;
  C_Color_unkown = clWhite;
  C_Glyph_unknown = 'BBUNKNOWN';

  C_Shape_DFUE_erweitert = stRectangle;
  C_Color_DFUE_erweitert = clLime;
  C_Glyph_DFUE_erweitert = 'BBDFUE_ERWEITERT';  // 26.01.2016, WW

  C_Shape_Gaszaehler = stRectangle;
  C_Color_Gaszaehler = clFuchsia;
  C_Glyph_Gaszaehler = 'BBGASZAEHLER';  // 04.02.2016, WW

  C_Shape_Odorierung = stRectangle;
  C_Color_Odorierung = clOlive;
  C_Glyph_Odorierung = 'BBODOR';  // 05.03.2024, WW

  C_Shape_Begleit = stRectangle;
  C_Color_Begleit = clYellow;
  C_Glyph_Begleit = 'BBBEGLEIT';  // 05.03.2024, WW

type
  // Ereignis-Procedure fur Anklicken eines Instanz-Shapes
  TDSfGInstBtnClick = procedure (cInstAdr, cInstTyp: char) of Object;
  TBlockClick = procedure (iNumber: integer) of Object;

  TDSfGInstanzShape = class;
  TDSfGGeraeteShape = class;
  TDSfGMesstreckenShape = class;

  // Record zur Verwaltung der graphischen Instanzobjekte
  TInstanzRec = record
    InstanzAdresse : char;
    InstanzTyp     : char;
    InstanzObjekt  : TDSfGInstanzShape;
  end;
  PInstanzRec = ^TInstanzRec;

  TDSfGInstanzShape = class(TMyCustomShape)
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
  private
    FText        : string;
    FIsBitMap    : boolean;
    FIsGrid      : boolean;
    FIsButton    : boolean;
    FBitmap      : TBitmap;
    FSpdBtn      : TSpeedButton;
    FStringGrid  : TStringGrid;
    FSpdBtnClick : TNotifyEvent;
    FInstTyp     : char;
    FInstAdr     : char;
    procedure SpdBtnClick(Sender: TObject);
    procedure InitMyControls(bState: boolean);
    procedure SetIsButton(Value: boolean);
    procedure SetIsBitmap(Value: boolean);
    procedure SetIsGrid(Value: boolean);
    procedure SetText(Value: string);
    procedure SetInstTyp(Value: char);
    procedure DoInstSetting;
  protected
    procedure Paint; override;
    procedure SetColor(Value: TColor); override;
    property StringGrid: TStringGrid read FStringGrid write FStringGrid;
    property Bitmap: TBitmap read FBitmap write FBitmap;
    property SpeedBtn: TSpeedButton read FSpdBtn write FSpdBtn;
  public
    procedure InitValues(pValues: TStrings);
    property InstanzAdresse: char read FInstAdr write FInstAdr;
    property InstanzTyp: char read FInstTyp write SetInstTyp;
//    property Bitmap: TBitmap read FBitmap;
    property Text: string read FText write SetText;
    property IsBitmap: boolean read FIsBitmap write SetIsBitmap;
    property IsButton: boolean read FIsButton write SetIsButton;
    property IsGrid: boolean read FIsGrid write SetIsGrid;
  published
    property Options;
    property OnSpdBtnClick: TNotifyEvent read FSpdBtnClick write FSpdBtnClick;
  end;

  TDSfGBusShape = class(TMyCustomShape)
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
  private
    FText       : string;
    FInstanzList : TStringList;
    FInstBtnClick : TDSfGInstBtnClick;
    procedure SetText(Value: string);
    procedure InstSpdBtnClick(Sender: TObject);
    procedure ResizeInstanzen;
    procedure DoInstSetting(cInstTyp: char; pInstShape: TDSfGInstanzShape);
  protected
    procedure Paint; override;
  public
    procedure Resize; override;
    procedure Clear;
    procedure NewShape(cInstanzAdr, cInstanzTyp: char; doResize: boolean = True);
    procedure DeleteShape(cInstanzAdr: char);
  published
    property Text: string read FText write SetText;
    property OnInstBtnClick: TDSfGInstBtnClick
      read FInstBtnClick write FInstBtnClick;
  end;

  TDSfGGeraeteShape = class(TMyCustomShape)
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
  private
    FText       : string;
    FInstanzList : TStringList;
    FInstBtnClick : TDSfGInstBtnClick;
    FBitmap      : TBitmap;
    FIsBitmap    : boolean;
    function GetFText(iIndex: integer): string;
    procedure SetFText(iIndex: integer; Value: string);
    procedure SetIsBitmap(Value: boolean);
    procedure InstSpdBtnClick(Sender: TObject);
    procedure ResizeInstanzen;
    procedure DoInstSetting(cInstTyp: char; pInstShape: TDSfGInstanzShape);
  protected
    procedure Resize; override;
    procedure Paint; override;
  public
    procedure NewShape(cInstanzAdr, cInstanzTyp: char; doResize: boolean = True);
    procedure DeleteShape(cInstanzAdr: char);
  published
    property Bitmap: TBitmap read FBitmap;
    property IsBitmap: boolean read FIsBitmap write SetIsBitmap;
    property Geraetetyp: string index 1 read GetFText write SetFText;
    property Hersteller: string index 2 read GetFText write SetFText;
    property Fabriknummer: string index 3 read GetFText write SetFText;
    property OnInstBtnClick: TDSfGInstBtnClick
      read FInstBtnClick write FInstBtnClick;
  end;

  TBlockShape = class(TMyCustomShape)
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
  private
    FText        : string;
    FBitmap      : TBitmap;
    procedure InitMyControls(bState: boolean);
    procedure SetText(Value: string);
  protected
    procedure Paint; override;
  public
  published
    property Text: string read FText write SetText;
    property Options;
    property onClick;
  end;

  TDSfGMesstreckenShape = class(TMyCustomShape)
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
  private
    FText       : string;
    FShapeList : TStringList;
    FBlockClick : TBlockClick;
    FStatus       : byte;
    FBitmap      : TBitmap;
    procedure SetText(Value: string);
    procedure BlockClick(Sender: TObject);
    procedure ResizeInstanzen;
    procedure SetStatus(iStatus: byte);
    function GetBlockShape(iIndex: integer): TBlockShape;
  protected
    procedure Paint; override;
  public
    procedure Resize; override;
    procedure Clear;
    property BlockShape[iIndex: integer]: TBlockShape read GetBlockShape;
  published
    property Text: string read FText write SetText;
    property Status: byte read FStatus write SetStatus;
    property OnBlockClick: TBlockClick read FBlockClick write FBlockClick;
  end;

implementation

{$R *.RES}

resourcestring
  S_Geraete_Text_Geraetetyp  = 'Gerätetyp:';
  S_Geraete_Text_Hersteller  = 'Hersteller:';
  S_Geraete_Text_Fabriknummer = 'Fabriknummer:';

  S_DEA = 'DEA';
  S_Value = 'Wert';
  S_DSfGBus = 'DSfG-Bus';
  S_Messtreckendarstellung = 'Meßstreckendarstellung';

  S_InstAdrNotRegistered = 'Instanzadresse %s nicht registriert !';
  S_InstBusAdrInstTyp = 'Instanz an Busadresse ''' + '%s' + ''''#13#10 +
    'Instanztyp ''' + '%s' + '''';


{---------------------------- Allgemeine Funktionen ---------------------------}

{ Leert eine Liste mit TInstanzRec-Objekten          }
{ Parameter: Liste, die geleert werden soll          }
{----------------------------------------------------}
procedure ClearInstanzList(pSl: TStrings);
{----------------------------------------------------}
var
  i : integer;
begin
  if (Assigned(pSl)) then begin
    try
      for i := 0 to pSl.Count-1 do begin
        PInstanzRec(pSl.Objects[i]).InstanzObjekt.Free;
        Dispose(PInstanzRec(pSl.Objects[i]));
      end;
    finally
      pSl.Clear;
    end;
  end;
end;

{------------------------------ TDSfGInstanzShape -----------------------------}

{----------------------------------------------------}
constructor TDSfGInstanzShape.Create(anOwner: TComponent);
{----------------------------------------------------}
begin
  inherited;
  InitMyControls(True);
  FInstTyp := ' ';
  FInstAdr := '0';
  FText := '';
  FIsBitMap := False;
  FIsGrid := False;
  FIsButton := False;
end;

{----------------------------------------------------}
destructor TDSfGInstanzShape.Destroy;
{----------------------------------------------------}
begin
  InitMyControls(False);
  inherited;
end;

{ Erzeugt/Gibt frei interne Controls                 }
{ Parameter: True-Erzeugen, False-Freigeben          }
{----------------------------------------------------}
procedure TDSfGInstanzShape.InitMyControls(bState: boolean);
{----------------------------------------------------}
begin
  if (bState) then begin
    FBitmap := TBitmap.Create;
    FBitmap.Transparent := True;

    FSpdBtn := TSpeedButton.Create(Self);
    FSpdBtn.Parent := Self;
    FSpdBtn.OnClick := SpdBtnClick;
    FSpdBtn.Transparent := True;
    FSpdBtn.Flat := True;
    FSpdBtn.Visible := FIsButton;

    FStringGrid := TStringGrid.Create(Self);
    FStringGrid.Parent := Self;
    FStringGrid.ParentColor := False;
    FStringGrid.RowCount := 2;
    FStringGrid.ColCount := 2;
    FStringGrid.FixedRows := 1;
    FStringGrid.FixedCols := 0;
    FStringGrid.Font.Size := 8;
    FStringGrid.DefaultRowHeight :=
      EstimatedTextHeightOf('DEA', FStringGrid.Font) + 4;
    FStringGrid.Cells[0, 0] := S_DEA;
    FStringGrid.Cells[1, 0] := S_Value;
    FStringGrid.Visible := FIsGrid;
  end
  else begin
    if (Assigned(FBitmap)) then FBitmap.Free;
    if (Assigned(FStringGrid)) then FStringGrid.Free;
  end;
end;

{ Ereignisprocedure für Speedbutton-Click            }
{----------------------------------------------------}
procedure TDSfGInstanzShape.SpdBtnClick(Sender: TObject);
{----------------------------------------------------}
begin
  if (Assigned(FSpdBtnClick)) then FSpdBtnClick(Self);
end;

{----------------------------------------------------}
procedure TDSfGInstanzShape.SetColor(Value: TColor);
{----------------------------------------------------}
begin
  if (Assigned(FStringGrid)) then FStringGrid.Color := Value;
  inherited;
end;

{----------------------------------------------------}
procedure TDSfGInstanzShape.SetIsButton(Value: boolean);
{----------------------------------------------------}
begin
  if (Value <> FIsButton) then begin
    FIsButton := Value;
    FSpdBtn.Visible := Value;
    Invalidate;
  end;
end;

{----------------------------------------------------}
procedure TDSfGInstanzShape.SetIsBitmap(Value: boolean);
{----------------------------------------------------}
begin
  if (Value <> FIsBitMap) then begin
    FIsBitMap := Value;
    Invalidate;
  end;
end;

{----------------------------------------------------}
procedure TDSfGInstanzShape.SetIsGrid(Value: boolean);
{----------------------------------------------------}
begin
  if (Value <> FIsGrid) then begin
    FIsGrid := Value;
    FStringGrid.Visible := Value;
    FStringGrid.Color := Color;
    Invalidate;
  end;
end;

{----------------------------------------------------}
procedure TDSfGInstanzShape.SetText(Value: string);
{----------------------------------------------------}
begin
  if (Value <> FText) then begin
    FText := Value;
    Invalidate;
  end;
end;

{ Stringliste mit Datenelementen zur Anzeige         }
{ DEA und Wert sind jeweils durch <us> getrennt      }
{ Parameter: Liste mit DEA-Wert - Paaren             }
{----------------------------------------------------}
procedure TDSfGInstanzShape.InitValues(pValues: TStrings);
{----------------------------------------------------}
var
  i, j : integer;
  iRow, iTopRow : integer;
begin
  // Aktuelle Zeile merken
  iRow := StringGrid.Row;
  iTopRow := StringGrid.TopRow;

  // Stringgrid leeren
  for i := 1 to FStringGrid.RowCount-1 do FStringGrid.Rows[i].Clear;
  FStringGrid.RowCount := 2;

  // Datenelementadressen und Werte trennen und eintragen
  if (Assigned(pValues)) then begin
    for i := 0 to pValues.Count-1 do begin
      j := Pos(Chr(us), pValues[i]);
      if (j > 1) then begin
        FStringGrid.Cells[0, i+1]:= GetStringPart(pValues[i], 1);
        FStringGrid.Cells[1, i+1]:= GetStringPart(pValues[i], 2);
      end
      else FStringGrid.Cols[0].Add(Copy(pValues[i], 1, j-1));
    end;
  end;

  i := pValues.Count+1;
  if (i < 2) then i := 2;
  FStringGrid.RowCount := i;

  // Aktuelle Zeile wenn möglich wieder selektieren
  if (StringGrid.RowCount >= iTopRow+1) then StringGrid.TopRow := iTopRow;
  if (StringGrid.RowCount >= iRow+1) then StringGrid.Row := iRow;
end;

{----------------------------------------------------}
procedure TDSfGInstanzShape.Paint;
{----------------------------------------------------}
begin
  inherited Paint;

  if (FText <> '') then begin
    Canvas.Font.Height := (Self.Height div 2) - 2;
    Canvas.Font.Color := (not Canvas.Brush.Color);
    Canvas.TextOut(((Self.Width div 2) - Canvas.TextWidth(FText)) div 2,
      ((Self.Height div 2) - Canvas.TextHeight(FText)) div 2, FText);
  end;

  if (IsBitmap) then
  begin
    Canvas.StretchDraw(Rect(
      (Self.Width div 2) + 1, 1, Self.Width -1, (Self.Height div 2) - 2),
      FBitMap);
  end;

  if (IsButton) then begin
    FSpdBtn.Top := 2;
    FSpdBtn.Left := Self.Width div 2 + 1;
    FSpdBtn.Width := (Self.Width div 2) - 4;
    FSpdBtn.Height := (Self.Height div 2) - 4;
  end;

  if (IsGrid) then begin
    FStringGrid.Left := 4;
    FStringGrid.Top := Self.Height div 2;
    FStringGrid.Width := Self.Width - 8;
    FStringGrid.Height := (Self.Height div 2) - 4;

    if (FStringGrid.VisibleRowCount+FStringGrid.FixedRows = FStringGrid.RowCount)
    then FStringGrid.DefaultColWidth := (FStringGrid.Width-6) div 2
    else FStringGrid.DefaultColWidth :=
      (FStringGrid.Width-6-GetSystemMetrics(SM_CYHSCROLL)) div 2;
  end;
end;

{ Passt Instanz-Objekt an Typ an                     }
{----------------------------------------------------}
procedure TDSfGInstanzShape.DoInstSetting;
{----------------------------------------------------}
begin
  case Self.InstanzTyp of
    C_D_Instanztyp_Umw : begin
      Shape := C_Shape_Umwerter;
      Color := C_Color_Umwerter;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Umwerter);
    end;
    C_D_Instanztyp_Blende : begin
      Shape := C_Shape_Blende;
      Color := C_Color_Blende;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Blende);
    end;
    C_D_Instanztyp_Reg : begin
      Shape := C_Shape_Registrierung;
      Color := C_Color_Registrierung;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Registrierung);
    end;
    C_D_Instanztyp_Gas : begin
      Shape := C_Shape_Gasbeschaffenheit;
      Color := C_Color_Gasbeschaffenheit;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Gasbeschaffenheit);
    end;
    C_D_Instanztyp_KGM : begin
      Shape := C_Shape_KorrGasbeschaffenheit;
      Color := C_Color_Gasbeschaffenheit;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_KorrGasbeschaffenheit);
    end;
    C_D_Instanztyp_Rev : begin
      Shape := C_Shape_Revision;
      Color := C_Color_Revision;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Revision);
    end;
    C_D_Instanztyp_Strg : begin
      Shape := C_Shape_Steuerung;
      Color := C_Color_Steuerung;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Steuerung);
    end;
    C_D_Instanztyp_DFU: begin  // 09.07.2010
      Shape := C_Shape_DFUE;
      Color := C_Color_DFUE;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_DFUE);
    end;
    C_D_Instanztyp_DFU2 : begin  // 26.01.2016, WW
      Shape := C_Shape_DFUE_erweitert;
      Color := C_Color_DFUE_erweitert;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_DFUE_erweitert);
    end;
    C_D_Instanztyp_Prot : begin
      Shape := C_Shape_Drucker;
      Color := C_Color_Drucker;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Drucker);
    end;
    C_D_Instanztyp_Wieser : begin
      Shape := C_Shape_Wieser;
      Color := C_Color_Wieser;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Wieser);
    end;
    C_D_Instanztyp_Gaszaehler : begin  // 04.02.2016, WW
      Shape := C_Shape_Gaszaehler;
      Color := C_Color_Gaszaehler;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Gaszaehler);
    end;
    C_D_Instanztyp_Odor : begin  // 05.03.2024, WW
      Shape := C_Shape_Odorierung;
      Color := C_Color_Odorierung;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Odorierung);
    end;
    C_D_Instanztyp_Begleit : begin  // 05.03.2024, WW
      Shape := C_Shape_Begleit;
      Color := C_Color_Begleit;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Begleit);
    end;
    else begin
      Shape := stRectangle;
      Color := C_Color_unkown;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_unknown);
    end
  end;
end;

{ Setzt den Instanz-Typ des Shapes                   }
{ Parameter: Instanztyp                              }
{----------------------------------------------------}
procedure TDSfGInstanzShape.SetInstTyp(Value: char);
{----------------------------------------------------}
begin
  if (Value <> InstanzTyp) then begin
    FInstTyp := Value;
    DoInstSetting;
  end;
end;

{-------------------------------- TDSfGBusShape -------------------------------}

{----------------------------------------------------}
constructor TDSfGBusShape.Create(anOwner: TComponent);
{----------------------------------------------------}
begin
  inherited;
  FInstBtnClick := nil;
  Self.ParentColor := False;
  Color := C_Color_Canvas;
  FText := S_DSfGBus;
  Width := 200;
  Height := 100;
  FInstanzList := TStringlist.Create;
  FInstanzList.Sorted := False; // Sortierung selber implementiert
end;

{----------------------------------------------------}
destructor TDSfGBusShape.Destroy;
{----------------------------------------------------}
begin
  if (Assigned(FInstanzList)) then begin
    ClearInstanzList(FInstanzList);
    FInstanzList.Free;
  end;
  inherited;
end;

{----------------------------------------------------}
procedure TDSfGBusShape.Resize;
{----------------------------------------------------}
begin
  if (Width <= 10) or (Height <= 10) then Exit;

  // Instanzengrößen ermitteln
  ResizeInstanzen;
  inherited Resize;
  Invalidate;
end;

{----------------------------------------------------}
procedure TDSfGBusShape.ResizeInstanzen;
{----------------------------------------------------}
var
  i, x, y : integer;
  iKante  : integer;
  iEinzeilig : byte;
begin
  if (FInstanzList.Count > 0) then begin
    // Einzeilige oder zweizeilige Darstellung
    if (Width div (Height div 2) > FInstanzList.Count)
      then iEinzeilig := 1 else iEinzeilig := 2;
    // Kantenlänge für Instanzen ermitteln
    iKante := Self.Width div
      ((FInstanzList.Count div iEinzeilig) +
        Integer(FInstanzList.Count mod iEinzeilig <> 0));
    Dec(iKante, 2);
    if (iKante > (Self.Height-2) div (2*iEinzeilig)) then
      iKante := (Self.Height-2) div (2*iEinzeilig);
    // Instanzobjekte dimensionieren
    x := 2;
    y := (3*(Self.Height div 4) - (iKante)) div (2*iEinzeilig);
    for i := 0 to FInstanzList.Count-1 do
      with PInstanzRec(FInstanzList.Objects[i])^.InstanzObjekt do begin
        if (iEinzeilig = 2) and ((x+iKante) > Self.Width) then begin
          x := 2;
          y := Self.Height - y - iKante;
        end;
        Width := iKante;
        Height := iKante;
        Left := x;
        Top := y;
        Inc(x, 2+iKante);
      end;
  end;
end;

{----------------------------------------------------}
procedure TDSfGBusShape.Paint;
{----------------------------------------------------}
var
  i, y, x1, y1, x2, y2 : integer;
begin
  // Voreinstellungen
  if (Width div (Height div 2) > FInstanzList.Count)  // Ein- oder zweizeilige Darstellung
    then y := Trunc(Self.Height*7/8) else y := (Self.Height div 2);
  // Verbindungslinien zu Instanzen zeichnen
  Canvas.Pen.Style := psSolid;
  for i := 0 to FInstanzList.Count-1 do begin
    y1 := y;
    y2 := PInstanzRec(FInstanzList.Objects[i])^.InstanzObjekt.Top+
      (PInstanzRec(FInstanzList.Objects[i])^.InstanzObjekt.Height div 2);
    x1 := PInstanzRec(FInstanzList.Objects[i])^.InstanzObjekt.Left +
      (PInstanzRec(FInstanzList.Objects[i])^.InstanzObjekt.Width div 2);
    x2 := x1;
    Canvas.MoveTo(x1, y1);
    Canvas.LineTo(x2, y2);
  end;
  // Bus-Backbone zeichnen
  i := Self.Height div 20;
  if (i < 2) then i := 2;
  y1 := y - i;
  y2 := y + i;
  x1 := 4;
  x2 := Self.Width - x1;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Color := clRed;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(x1, y1, x2, y2);
  // Beschriftung
  Canvas.Font.Height := 2*i - 2;
  Canvas.Font.Color := (not Canvas.Brush.Color);
  Canvas.TextOut(x1+4, y1+1, FText);
end;

{ Erzeugt eine neue Instanz                          }
{ Parameter: Adresse, Typ der neuen Instanz          }
{----------------------------------------------------}
procedure TDSfGBusShape.NewShape(
  cInstanzAdr, cInstanzTyp: char; doResize: boolean = True);
{----------------------------------------------------}
var
  pIR : PInstanzRec;
begin
  if (cInstanzAdr in ['A'..'_']) and (FInstanzList.IndexOf(cInstanzAdr) < 0)
  then begin // Adresse nicht vergeben
    New(pIR);
    pIR^.InstanzAdresse := cInstanzAdr;
    pIR^.InstanzTyp := cInstanzTyp;
    pIR^.InstanzObjekt := TDSfGInstanzShape.Create(Self);
    DoInstSetting(cInstanzTyp, pIR^.InstanzObjekt);
    with pIR^.InstanzObjekt do begin
      Parent := Self;
      Tag := Ord(cInstanzAdr);
      Text := cInstanzAdr;
      Options := [];
      IsButton := True;
      IsBitmap := True;
      OnSpdBtnClick := InstSpdBtnClick;
    end;
    FInstanzList.AddObject(cInstanzAdr, TObject(pIR));
    FInstanzList.CustomSort(StringListStrCompare);
    if (doResize) then Resize;
  end;
end;

{ Löscht ein Instanz-Shape                           }
{ Parameter: Instanzadresse                          }
{----------------------------------------------------}
procedure TDSfGBusShape.DeleteShape(cInstanzAdr: char);
{----------------------------------------------------}
var
  i : integer;
begin
  i := FInstanzList.IndexOf(cInstanzAdr);
  if (i >= 0) then begin
    PInstanzRec(FInstanzList.Objects[i]).InstanzObjekt.Release;
    Dispose(PInstanzRec(FInstanzList.Objects[i]));
    FInstanzList.Delete(i);
    Resize;
  end;
end;

{ Löscht alle Instanz-Shapes                         }
{----------------------------------------------------}
procedure TDSfGBusShape.Clear;
{----------------------------------------------------}
var
  i : integer;
begin
  for i := FInstanzList.Count-1 downto 0 do begin
    PInstanzRec(FInstanzList.Objects[i]).InstanzObjekt.Release;
    Dispose(PInstanzRec(FInstanzList.Objects[i]));
    FInstanzList.Delete(i);
  end;
  Invalidate;
end;

{ Passt Instanz-Objekt an Typ an                     }
{ Parameter: Instanztyp, Instanzobjekt               }
{----------------------------------------------------}
procedure TDSfGBusShape.DoInstSetting(cInstTyp: char; pInstShape: TDSfGInstanzShape);
{----------------------------------------------------}
begin
  case cInstTyp of
    C_D_Instanztyp_Umw : begin
      pInstShape.Shape := C_Shape_Umwerter;
      pInstShape.Color := C_Color_Umwerter;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Umwerter);
    end;
    C_D_Instanztyp_Blende : begin
      pInstShape.Shape := C_Shape_Blende;
      pInstShape.Color := C_Color_Blende;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Blende);
    end;
    C_D_Instanztyp_Reg : begin
      pInstShape.Shape := C_Shape_Registrierung;
      pInstShape.Color := C_Color_Registrierung;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Registrierung);
    end;
    C_D_Instanztyp_Gas : begin
      pInstShape.Shape := C_Shape_Gasbeschaffenheit;
      pInstShape.Color := C_Color_Gasbeschaffenheit;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Gasbeschaffenheit);
    end;
    C_D_Instanztyp_KGM : begin
      pInstShape.Shape := C_Shape_KorrGasbeschaffenheit;
      pInstShape.Color := C_Color_Gasbeschaffenheit;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_KorrGasbeschaffenheit);
    end;
    C_D_Instanztyp_Rev : begin
      pInstShape.Shape := C_Shape_Revision;
      pInstShape.Color := C_Color_Revision;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Revision);
    end;
    C_D_Instanztyp_Strg : begin
      pInstShape.Shape := C_Shape_Steuerung;
      pInstShape.Color := C_Color_Steuerung;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Steuerung);
    end;
    C_D_Instanztyp_DFU: begin // 09.07.2010
      pInstShape.Shape := C_Shape_DFUE;
      pInstShape.Color := C_Color_DFUE;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_DFUE);
    end;
    C_D_Instanztyp_DFU2 : begin  // 26.01.2016, WW
      pInstShape.Shape := C_Shape_DFUE_erweitert;
      pInstShape.Color := C_Color_DFUE_erweitert;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_DFUE_erweitert);
    end;
    C_D_Instanztyp_Prot : begin
      pInstShape.Shape := C_Shape_Drucker;
      pInstShape.Color := C_Color_Drucker;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Drucker);
    end;
    C_D_Instanztyp_Wieser : begin
      pInstShape.Shape := C_Shape_Wieser;
      pInstShape.Color := C_Color_Wieser;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Wieser);
    end;
    C_D_Instanztyp_Gaszaehler : begin  // 04.02.2016, WW
      pInstShape.Shape := C_Shape_Gaszaehler;
      pInstShape.Color := C_Color_Gaszaehler;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Gaszaehler);
    end;
    C_D_Instanztyp_Odor : begin  // 05.03.2024, WW
      pInstShape.Shape := C_Shape_Odorierung;
      pInstShape.Color := C_Color_Odorierung;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Odorierung);
    end;
    C_D_Instanztyp_Begleit : begin  // 05.03.2024, WW
      pInstShape.Shape := C_Shape_Begleit;
      pInstShape.Color := C_Color_Begleit;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Begleit);
    end;
    else begin
      pInstShape.Shape := stRectangle;
      pInstShape.Color := C_Color_unkown;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_unknown);
    end
  end;
end;

{----------------------------------------------------}
procedure TDSfGBusShape.SetText(Value: string);
{----------------------------------------------------}
begin
  if (Value <> FText) then begin
    FText := Value;
    Invalidate;
  end;
end;

{ Ereignisprocedure für Reaktion auf Instanz-Click   }
{----------------------------------------------------}
procedure TDSfGBusShape.InstSpdBtnClick(Sender: TObject);
{----------------------------------------------------}
var
  cAdr, cTyp : char;
  i          : integer;
begin
  if (Sender is TDSfGInstanzShape) and
     (Chr(TDSfGInstanzShape(Sender).Tag) in ['A'..'_'])
  then begin
    cAdr := Chr(TDSfGInstanzShape(Sender).Tag);
    i := FInstanzList.IndexOf(cAdr);
    if (i < 0)
    then begin
      MessageDlg(Format(S_InstAdrNotRegistered, [cAdr]), mtError, [mbOk], 0);
      Exit;
    end
    else cTyp := PInstanzRec(FInstanzList.Objects[i])^.InstanzTyp;

    if (Assigned(FInstBtnClick))
    then FInstBtnClick(cAdr, cTyp)
    else MessageDlg(Format(S_InstBusAdrInstTyp, [cAdr, cTyp]), mtInformation, [mbOk], 0);
  end;
end;

{-------------------------------- TDSfGGeraeteShape -------------------------------}

{----------------------------------------------------}
constructor TDSfGGeraeteShape.Create(anOwner: TComponent);
{----------------------------------------------------}
begin
  inherited;
  FInstBtnClick := nil;

  FIsBitMap := False;
  FBitmap := TBitmap.Create;
  FBitmap.Transparent := False;

  Self.ParentColor := False;
  Color := C_Color_Canvas;
  FText := S_Geraete_Text_Geraetetyp + Chr(us) + S_Geraete_Text_Hersteller +
    Chr(us) + S_Geraete_Text_Fabriknummer;  // 05.03.2024, WW
  Width := 150;
  Height := 100;
  FInstanzList := TStringlist.Create;
  FInstanzList.Sorted := False; // Sortierung selber implementiert
end;

{----------------------------------------------------}
destructor TDSfGGeraeteShape.Destroy;
{----------------------------------------------------}
begin
  if (Assigned(FInstanzList)) then begin
    ClearInstanzList(FInstanzList);
    FInstanzList.Free;
  end;

  if (Assigned(FBitmap)) then FBitmap.Free;

  inherited;
end;

{----------------------------------------------------}
procedure TDSfGGeraeteShape.Resize;
{----------------------------------------------------}
begin
  // Instanzengrößen ermitteln
  ResizeInstanzen;
  inherited Resize;
  Invalidate;
end;

{----------------------------------------------------}
procedure TDSfGGeraeteShape.ResizeInstanzen;
{----------------------------------------------------}
var
  i, x, y : integer;
  iKante  : integer;
  iEinzeilig : byte;
begin
  if (FInstanzList.Count > 0) then begin
    // Einzeilige oder zweizeilige Darstellung
    if (Width div (Height div 2) > FInstanzList.Count)
      then iEinzeilig := 1 else iEinzeilig := 2;
    // Kantenlänge für Instanzen ermitteln
    iKante := Self.Width div
      ((FInstanzList.Count div iEinzeilig) +
        Integer(FInstanzList.Count mod iEinzeilig <> 0));
    Dec(iKante, 2);
    if (iKante > (Self.Height-2) div (2*iEinzeilig)) then
      iKante := (Self.Height-2) div (2*iEinzeilig);
    // Instanzobjekte dimensionieren
    x := 2;
    y := (Self.Height div 2);
    for i := 0 to FInstanzList.Count-1 do
      with PInstanzRec(FInstanzList.Objects[i])^.InstanzObjekt do begin
        if (iEinzeilig = 2) and ((x+iKante) > Self.Width) then begin
          x := 2;
          y := 3*(Self.Height div 4);
        end;
        Width := iKante;
        Height := iKante;
        Left := x;
        Top := y;
        Inc(x, 2+iKante);
      end;
  end;
end;

{----------------------------------------------------}
procedure TDSfGGeraeteShape.Paint;
{----------------------------------------------------}
var
  i, x, y : integer;
  s       : string;
begin
  // Graphik ausgeben
  if (IsBitmap) then
  begin
    canvas.CopyMode := cmSrcCopy;
    Canvas.StretchDraw(Rect(1, 1, Self.Width-2, Self.Height-2), FBitMap);
  end;

  // Text ausgeben
  if (FText <> '') then begin
    Canvas.Font.Height := (Self.Height div 20);
    Canvas.Font.Color := (not Canvas.Brush.Color);
    x := 4;
    y := 4;
    i := 1;
    s := ' ';
    while (Length(s) > 0) do begin
      s := GetStringPart(FText, i);
      Inc(i);
      Canvas.TextOut(x, y, s);
      y := y + 4 + Canvas.TextHeight(s);
    end;
  end;

end;

{ Erzeugt eine neue Instanz                          }
{ Parameter: Adresse, Typ der neuen Instanz          }
{----------------------------------------------------}
procedure TDSfGGeraeteShape.NewShape(cInstanzAdr, cInstanzTyp: char; doResize: boolean = True);
{----------------------------------------------------}
var
  pIR : PInstanzRec;
begin
  if (cInstanzAdr in ['A'..'_']) and (FInstanzList.IndexOf(cInstanzAdr) < 0)
  then begin // Adresse nicht vergeben
    New(pIR);
    pIR^.InstanzAdresse := cInstanzAdr;
    pIR^.InstanzTyp := cInstanzTyp;
    pIR^.InstanzObjekt := TDSfGInstanzShape.Create(Self);
    DoInstSetting(cInstanzTyp, pIR^.InstanzObjekt);
    with pIR^.InstanzObjekt do begin
      Parent := Self;
      Tag := Ord(cInstanzAdr);
      Text := cInstanzAdr;
      IsButton := True;
      IsBitmap := True;
      OnSpdBtnClick := InstSpdBtnClick;
    end;
    FInstanzList.AddObject(cInstanzAdr, TObject(pIR));
    FInstanzList.CustomSort(StringListStrCompare);
    if (doResize) then Resize;
  end;
end;

{ Löscht ein Instanz-Shape                           }
{ Parameter: Instanzadresse                          }
{----------------------------------------------------}
procedure TDSfGGeraeteShape.DeleteShape(cInstanzAdr: char);
{----------------------------------------------------}
var
  i : integer;
begin
  i := FInstanzList.IndexOf(cInstanzAdr);
  if (i >= 0) then begin
    PInstanzRec(FInstanzList.Objects[i]).InstanzObjekt.Release;
    Dispose(PInstanzRec(FInstanzList.Objects[i]));
    FInstanzList.Delete(i);
    Resize;
  end;
end;

{ Passt Instanz-Objekt an Typ an                     }
{ Parameter: Instanztyp, Instanzobjekt               }
{----------------------------------------------------}
procedure TDSfGGeraeteShape.DoInstSetting(cInstTyp: char; pInstShape: TDSfGInstanzShape);
{----------------------------------------------------}
begin
  case cInstTyp of
    C_D_Instanztyp_Umw : begin
      pInstShape.Shape := C_Shape_Umwerter;
      pInstShape.Color := C_Color_Umwerter;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Umwerter);
    end;
    C_D_Instanztyp_Blende : begin
      pInstShape.Shape := C_Shape_Blende;
      pInstShape.Color := C_Color_Blende;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Blende);
    end;
    C_D_Instanztyp_Reg : begin
      pInstShape.Shape := C_Shape_Registrierung;
      pInstShape.Color := C_Color_Registrierung;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Registrierung);
    end;
    C_D_Instanztyp_Gas : begin
      pInstShape.Shape := C_Shape_Gasbeschaffenheit;
      pInstShape.Color := C_Color_Gasbeschaffenheit;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Gasbeschaffenheit);
    end;
    C_D_Instanztyp_KGM : begin
      Shape := C_Shape_KorrGasbeschaffenheit;
      Color := C_Color_Gasbeschaffenheit;
      Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_KorrGasbeschaffenheit);
    end;
    C_D_Instanztyp_Rev : begin
      pInstShape.Shape := C_Shape_Revision;
      pInstShape.Color := C_Color_Revision;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Revision);
    end;
    C_D_Instanztyp_Strg : begin
      pInstShape.Shape := C_Shape_Steuerung;
      pInstShape.Color := C_Color_Steuerung;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Steuerung);
    end;
    C_D_Instanztyp_DFU: begin  // 09.07.2010
      pInstShape.Shape := C_Shape_DFUE;
      pInstShape.Color := C_Color_DFUE;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_DFUE);
    end;
    C_D_Instanztyp_DFU2 : begin  // 26.01.2016, WW
      pInstShape.Shape := C_Shape_DFUE_erweitert;
      pInstShape.Color := C_Color_DFUE_erweitert;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_DFUE_erweitert);
    end;
    C_D_Instanztyp_Prot : begin
      pInstShape.Shape := C_Shape_Drucker;
      pInstShape.Color := C_Color_Drucker;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Drucker);
    end;
    C_D_Instanztyp_Wieser : begin
      pInstShape.Shape := C_Shape_Wieser;
      pInstShape.Color := C_Color_Wieser;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Wieser);
    end;
    C_D_Instanztyp_Gaszaehler : begin  // 04.02.2016, WW
      pInstShape.Shape := C_Shape_Gaszaehler;
      pInstShape.Color := C_Color_Gaszaehler;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Gaszaehler);
    end;
    C_D_Instanztyp_Odor : begin  // 05.03.2024, WW
      pInstShape.Shape := C_Shape_Odorierung;
      pInstShape.Color := C_Color_Odorierung;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Odorierung);
    end;
    C_D_Instanztyp_Begleit : begin  // 05.03.2024, WW
      pInstShape.Shape := C_Shape_Begleit;
      pInstShape.Color := C_Color_Begleit;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Begleit);
    end;
    else begin
      pInstShape.Shape := stRectangle;
      pInstShape.Color := C_Color_unkown;
      pInstShape.Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_unknown);
    end
  end;
end;

{----------------------------------------------------}
function TDSfGGeraeteShape.GetFText(iIndex: integer): string;
{----------------------------------------------------}
var
  l : integer;
begin
  case iIndex of
    1 : l := Length(S_Geraete_Text_Geraetetyp + ' ');
    2 : l := Length(S_Geraete_Text_Hersteller + ' ');
    3 : l := Length(S_Geraete_Text_Fabriknummer + ' ');
    else begin
      Result := '';
      Exit;
    end;
  end;

  Result := GetStringPart(FText, iIndex);
  Result := Copy(Result, l+1, Length(Result)-l);
end;

{----------------------------------------------------}
procedure TDSfGGeraeteShape.SetFText(iIndex: integer; Value: string);
{----------------------------------------------------}
var
  s : string;
begin
  FIsBitmap := False;
  case iIndex of
    1 : begin
          s := GetStringPart(FText, 2) + Chr(us) + GetStringPart(FText, 3);
          s := S_Geraete_Text_Geraetetyp + ' ' + Value + Chr(us) + s;
        end;
    2 : begin
          s := GetStringPart(FText, 1) + Chr(us);
          s := s + S_Geraete_Text_Hersteller + ' ' + Value + Chr(us);
          s := s + GetStringPart(FText, 3);
        end;
    3 : begin
          s := GetStringPart(FText, 1) + Chr(us) + GetStringPart(FText, 2);
          s := s + Chr(us) + S_Geraete_Text_Fabriknummer + ' ' + Value;
        end;
    else Exit;
  end;

  FText := s;

  if (Pos('wieser', LowerCase(Hersteller)) = 0) then begin
    Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Trash);
  end
  else begin
    if (Pos('2200', LowerCase(Geraetetyp)) = 0)
      then Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Wieser2)
      else Bitmap.LoadFromResourceName(HINSTANCE, C_Glyph_MRG2200);
  end;

  IsBitmap := True;
  Invalidate;
end;

{ Ereignisprocedure für Reaktion auf Instanz-Click   }
{----------------------------------------------------}
procedure TDSfGGeraeteShape.InstSpdBtnClick(Sender: TObject);
{----------------------------------------------------}
var
  cAdr, cTyp : char;
  i          : integer;
begin
  if (Assigned(FInstBtnClick)) and (Sender is TDSfGInstanzShape) and
     (Chr(TDSfGInstanzShape(Sender).Tag) in ['A'..'_']) then
  begin
    cAdr := Chr(TDSfGInstanzShape(Sender).Tag);
    i := FInstanzList.IndexOf(cAdr);
    if (i < 0)
    then MessageDlg(Format(S_InstAdrNotRegistered, [cAdr]), mtError, [mbOk], 0)
    else begin
      cTyp := PInstanzRec(FInstanzList.Objects[i])^.InstanzTyp;
      FInstBtnClick(cAdr, cTyp);
    end;
  end;
end;

{----------------------------------------------------}
procedure TDSfGGeraeteShape.SetIsBitmap(Value: boolean);
{----------------------------------------------------}
begin
  if (Value <> FIsBitMap) then begin
    FIsBitMap := Value;
    Invalidate;
  end;
end;

{---------------------------- TDSfGMesstreckenShape ---------------------------}

{----------------------------------------------------}
constructor TDSfGMesstreckenShape.Create(anOwner: TComponent);
{----------------------------------------------------}
begin
  inherited;
  FBlockClick := nil;
  Self.ParentColor := False;
  FBitmap := TBitmap.Create;
  FBitmap.Transparent := True;
  FBitmap.LoadFromResourceName(HINSTANCE, C_Glyph_Ventil);
  Color := C_Color_Canvas;
  FText := S_Messtreckendarstellung;
  Options := [];
  FStatus := 0;  //1,2,3,4
  Width := 200;
  Height := 100;
  FShapeList := TStringlist.Create;
  FShapeList.Sorted := False; // Sortierung selber implementiert
end;

{----------------------------------------------------}
destructor TDSfGMesstreckenShape.Destroy;
{----------------------------------------------------}
var
  i : integer;
begin
  if (Assigned(FShapeList)) then begin
    for i := FShapeList.Count-1 downto 0 do
    try
      TMyCustomShape(FShapeList.Objects[i]).Free;
    except
    end;
    FShapeList.Free;
  end;
  FBitmap.Free;
  
  inherited;
end;

{----------------------------------------------------}
procedure TDSfGMesstreckenShape.Resize;
{----------------------------------------------------}
begin
  if (Width <= 10) or (Height <= 10) then Exit;

  // Instanzengrößen ermitteln
  ResizeInstanzen;
  inherited Resize;
  Invalidate;
end;

{----------------------------------------------------}
procedure TDSfGMesstreckenShape.ResizeInstanzen;
{----------------------------------------------------}
var
  i      : integer;
  iH, iW : integer;
  p      : TMyCustomShape;
begin
  if (FShapeList.Count > 0) then begin
    iH := Self.Height div 5;
    iW := Self.Width div 6;
    for i := 0 to FShapeList.Count-1 do begin
      p := TMyCustomShape(FShapeList.Objects[i]);
      p.Height := iH;
      p.Width := iW;
      case FStatus of
        1 : begin
              p.Top := (Self.Height - p.Height) div 2;
              p.Left := (Self.Width - p.Width) div 2;
            end;
        2 : begin
              p.Top := (Self.Height - p.Height) div 2;
              case i of
                0 : p.Left := p.Width + (p.Width div 2);
                1 : p.Left := Self.Width - 2*p.Width -(p.Width div 2);
              end;
            end;
        3 : begin
              p.Top := (Self.Height - p.Height) div 2;
              case i of
                0,1 : p.Top := p.Height div 2;
                2,3 : p.Top := Self.Height - p.Height -(p.Height div 2);
              end;
              case i of
                0,2 : p.Left := p.Width + (p.Width div 2);
                1,3 : p.Left := Self.Width - 2*p.Width -(p.Width div 2);
              end;
            end;
        4 : begin
              case i of
                0 : p.Top := p.Height div 2;
                1 : p.Top := Self.Height - p.Height -(p.Height div 2);
              end;
              p.Left := (Self.Width - p.Width) div 2;
            end;
      end;
    end;
  end;
end;

{----------------------------------------------------}
procedure TDSfGMesstreckenShape.Paint;
{----------------------------------------------------}
var
  x, y : integer;
begin
  // Verbindungslinien zu Instanzen zeichnen
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := Self.Height div 100;
  if (Canvas.Pen.Width < 2) then Canvas.Pen.Width := 2;

  x := Self.Width div 6;
  y := Self.Height div 5;

  // Strippen zieh'n
  case FStatus of
    1, 2 : begin
             Canvas.MoveTo(1, Self.Height div 2);
             Canvas.LineTo(Self.Width - 1, Self.Height div 2);
           end;
    3, 4 : begin
             Canvas.MoveTo(1, Self.Height div 2);
             Canvas.LineTo(x div 2, Self.Height div 2);
             Canvas.MoveTo(Self.Width - (x div 2), Self.Height div 2);
             Canvas.LineTo(Self.Width - 1, Self.Height div 2);
             Canvas.MoveTo(x div 2, y);
             Canvas.LineTo(Self.Width - (x div 2), y);
             Canvas.MoveTo(x div 2, Self.Height - y);
             Canvas.LineTo(Self.Width - (x div 2), Self.Height - y);
             Canvas.MoveTo(x div 2, y);
             Canvas.LineTo(x div 2, Self.Height - y);
             Canvas.MoveTo(Self.Width - (x div 2), y);
             Canvas.LineTo(Self.Width - (x div 2), Self.Height - y);

             if (FStatus = 4) then begin
               Canvas.MoveTo(Trunc(2*x), Self.Height div 2);
               Canvas.LineTo(Trunc(2*x), Self.Height - y);
               Canvas.MoveTo(Self.Width - Trunc(2*x), Self.Height div 2);
               Canvas.LineTo(Self.Width - Trunc(2*x), y);
               Canvas.MoveTo(Trunc(2*x), Self.Height div 2);
               Canvas.LineTo(Self.Width - Trunc(2*x), Self.Height div 2);
             end;
           end;
  end;

  // Ventile reinhau'n
  case FStatus of
    1, 2 : begin
             Canvas.StretchDraw(Rect(Trunc(0.75*x), Trunc(2.25*y),
               Trunc(1.25*x), Trunc(2.75*y)), FBitMap);
             Canvas.StretchDraw(Rect(Trunc(4.75*x), Trunc(2.25*y),
               Trunc(5.25*x), Trunc(2.75*y)), FBitMap);
           end;
    3, 4 : begin
             Canvas.StretchDraw(Rect(Trunc(0.75*x), Trunc(0.75*y),
               Trunc(1.25*x), Trunc(1.25*y)), FBitMap);
             Canvas.StretchDraw(Rect(Trunc(4.75*x), Trunc(0.75*y),
               Trunc(5.25*x), Trunc(1.25*y)), FBitMap);
             Canvas.StretchDraw(Rect(Trunc(0.75*x), Self.Height - y - (y div 4),
               Trunc(1.25*x), Self.Height - y + (y div 4)), FBitMap);
             Canvas.StretchDraw(Rect(Trunc(4.75*x), Self.Height - y - (y div 4),
               Trunc(5.25*x), Self.Height - y + (y div 4)), FBitMap);
             if (FStatus = 4) then begin
               Canvas.StretchDraw(Rect(Trunc(2.75*x), (Self.Height div 2) - (y div 4),
                 Trunc(3.25*x), (Self.Height div 2) + (y div 4)), FBitMap);
             end;
           end;
  end;

  // Beschriftung
  Canvas.Brush.Color := clBlue;
  Canvas.FrameRect(Rect(0, 0, Width, Height));
  Canvas.Brush.Color := clWindow;
  Canvas.Font.Color := clBlue;
  Canvas.TextOut(1, 1, FText);

end;

{ Setzt den Status > Shapes werden erzeugt           }
{   1-einschienig; 2-einsch./Dauerreihenschaltung;   }
{   3-zweischienig/DRS; 4-zweischienig/Z-Schaltung   }
{----------------------------------------------------}
procedure TDSfGMesstreckenShape.SetStatus(iStatus: byte);
{----------------------------------------------------}
var
  i, j : integer;
  p    : TBlockShape;
begin
  if (iStatus <> FStatus) and (iStatus in [0..4]) then begin
    FStatus := iStatus;

  // Bisherige Shapes löschen
    for i := FShapeList.Count-1 downto 0 do
    try
      TMyCustomShape(FShapeList.Objects[i]).Free;
    except
    end;
    FShapeList.Clear;

  // Neue Shapes erzeugen
    case iStatus of
      1: j := 1;
      2, 4: j := 2;
      3: j := 4;
      else j := 0;
    end;
    for i := 1 to j do begin
      p := TBlockShape.Create(Self);
      p.Parent := Self;
      p.Tag := i;
      p.onClick := BlockClick;
      FShapeList.AddObject(IntToStr(i), p);
    end;

    ResizeInstanzen;
  end;
end;

{ Löscht alle Instanz-Shapes                         }
{----------------------------------------------------}
procedure TDSfGMesstreckenShape.Clear;
{----------------------------------------------------}
var
  i : integer;
begin
  for i := FShapeList.Count-1 downto 0 do
  try
    TMyCustomShape(FShapeList.Objects[i]).Free;
  except
  end;
  FShapeList.Clear;
  Invalidate;
end;


{----------------------------------------------------}
procedure TDSfGMesstreckenShape.SetText(Value: string);
{----------------------------------------------------}
begin
  if (Value <> FText) then begin
    FText := Value;
    Invalidate;
  end;
end;

{ Ereignisprocedure für Reaktion auf Instanz-Click   }
{----------------------------------------------------}
procedure TDSfGMesstreckenShape.BlockClick(Sender: TObject);
{----------------------------------------------------}
var
  i          : integer;
begin
  if (Sender is TBlockShape) and (TBlockShape(Sender).Tag in [1..4]) then begin
    i := TBlockShape(Sender).Tag;
    if (i > 0) and (Assigned(FBlockClick)) then FBlockClick(i);
  end;
end;

{ Gibt Zeiger auf Blockshape zurück                  }
{ Parameter: kennumer des Shape                      }
{ Rückgabe: Zeiger oder nil                          }
{----------------------------------------------------}
function TDSfGMesstreckenShape.GetBlockShape(iIndex: integer): TBlockShape;
{----------------------------------------------------}
var
  i : integer;
begin
  Result := nil;  // Default

  for i := 0 to FShapeList.Count-1 do
    if (FShapeList[i] = IntToStr(iIndex)) then begin
      Result := TBlockShape(FShapeList.Objects[i]);
      Break;
    end;
end;

{--------------------------------- TBlockShape --------------------------------}

{ Überschriebene Methode                             }
{----------------------------------------------------}
constructor TBlockShape.Create(anOwner: TComponent);
{----------------------------------------------------}
begin
  inherited Create(anOwner);

  InitMyControls(True);
  Color := clBlue;
  FText := '';
  Shape := stRectangle;
  Options := [];
end;

{ Überschriebene Methode                             }
{----------------------------------------------------}
destructor TBlockShape.Destroy;
{----------------------------------------------------}
begin
  InitMyControls(False);

  inherited Destroy;
end;

{ Überschriebene Methode                             }
{----------------------------------------------------}
procedure TBlockShape.Paint;
{----------------------------------------------------}
begin
  inherited Paint;

  Canvas.Brush.Color := clBlue;
  Canvas.StretchDraw(Rect(0, 0, Width, Height), FBitMap);
  Canvas.FrameRect(Rect(0, 0, Width, Height));

  if (FText <> '') then begin
    Canvas.Brush.Color := clWindow;
    Canvas.Font.Size := Self.Height div 5;
    while (Canvas.Font.Size > 8) and (Canvas.TextWidth(FText) >= Self.Width) do
      Canvas.Font.Size := Canvas.Font.Size - 1;
    Canvas.Font.Color := clBlue;
    Canvas.TextOut((Self.Width - Canvas.TextWidth(FText)) div 2,
      (Self.Height- Canvas.TextHeight(FText)) div 2, FText);
  end;
end;

{ Erzeugt/Gibt frei interne Controls                 }
{ Parameter: True-Erzeugen, False-Freigeben          }
{----------------------------------------------------}
procedure TBlockShape.InitMyControls(bState: boolean);
{----------------------------------------------------}
begin
  if (bState) then begin
    FBitmap := TBitmap.Create;
    FBitmap.Transparent := False;
    FBitmap.LoadFromResourceName(HINSTANCE, C_Glyph_DivBlock);
  end
  else begin
    if (Assigned(FBitmap)) then FBitmap.Free;
  end;
end;

{----------------------------------------------------}
procedure TBlockShape.SetText(Value: string);
{----------------------------------------------------}
begin
  if (Value <> FText) then begin
    FText := Value;
    Invalidate;
  end;
end;

end.
