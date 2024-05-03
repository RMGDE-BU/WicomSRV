{------------------------------------------------------------------------------}
{ Für Dea-Baum abgeleitete TreeView-Komponente                                 }
{                                                                              }
{ 21.06.2002  GD  Neu                                                          }
{ 08.11.2007  WW  resourcestrings                                              }
{ 09.07.2010  GD  DFÜ2                                                         }
{ 26.01.2016  WW  mit Symbol für erweiterte DSfG-DFÜ und elektron. Gaszähler   }
{ 05.03.2024  WW  mit Symbol für Odorierung und Gasbegleitstoffmessung         }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2002, RMG Messtechnik GmbH 2024               }
{------------------------------------------------------------------------------}
unit DeaTree;

interface

uses
  SysUtils, Classes, Controls, ComCtrls, Graphics, Forms,
  GD_Utils, DSG_Utils, DStaDll, WSysCon, DMomLists;

type
  TDeaTree = class(TTreeView)
    constructor Create(AnOwner: TComponent); override;
    constructor InitDeaView(AnOwner: TComponent; sInstTypen: string); virtual;
    destructor Destroy; override;
  private
    FImageList    : TImageList;
    FInstanzTypen : string;
    FValueMode    : TDeaValueTypes;
    FBuildLevel   : integer;
    procedure InitImages(bState: boolean); virtual;
    procedure SetValueMode(pValueMode: TDeaValueTypes);
  protected
    procedure InitMyComponents(bState: boolean); virtual;
    procedure BuildDeaNode(pNode: TTreeNode; sInstanzTyp: string); virtual;
    procedure ExpandInstDeaNode(pNode: TTreeNode);
    function CanExpand(Node: TTreeNode): Boolean; override;
    function IsExpanded(pNode: TTreeNode): boolean;
  public
    procedure Clear;
    procedure BuildDeaTree(sInstanzTypen: string = ''); virtual;
    property InstanzTypen: string read FInstanzTypen write FInstanzTypen;
    property ValueMode: TDeaValueTypes read FValueMode write SetValueMode;
    property BuildLevel: integer read FBuildLevel write FBuildLevel;
  end;

implementation

// {$R DSfGStationen.RES}

const
  C_BuildLevel = 1;  // Level des Baumes, ab dem Informationen nachgeladen werden

resourcestring
  S_Common = 'Allgemeiner Teil';
  S_Wieser = 'Wieser-Instanz';


{---------------------------------- TDeaTree ----------------------------------}

{----------------------------------------------}
constructor TDeaTree.Create(AnOwner: TComponent);
{----------------------------------------------}
begin
  inherited Create(AnOwner);

  if (AnOwner is TWinControl) then begin
    Parent := TWinControl(AnOwner);
    Align := alClient;
  end;

  FBuildLevel := C_BuildLevel;
  ReadOnly := True;
  FValueMode := [dvtAlle];
  FInstanzTypen := '';
  Self.RightClickSelect := True;
  InitMyComponents(True);
  InitImages(True);
  Self.Images := FImageList;
end;

{----------------------------------------------}
constructor TDeaTree.InitDeaView(AnOwner: TComponent; sInstTypen: string);
{----------------------------------------------}
begin
  inherited Create(AnOwner);

  FBuildLevel := C_BuildLevel;
  FInstanzTypen := sInstTypen;
  Self.RightClickSelect := True;
  InitMyComponents(True);
  InitImages(True);
  Self.Images := FImageList;

  ReadOnly := True;
  FValueMode := [dvtAlle];

  if (AnOwner is TWinControl) then begin
    Parent := TWinControl(AnOwner);
    Align := alClient;
    BuildDeaTree(FInstanzTypen);
  end;
end;

{----------------------------------------------}
destructor TDeaTree.Destroy;
{----------------------------------------------}
begin
  InitMyComponents(False);
  InitImages(False);

  inherited;
end;

{ Wird aufgerufen, bevor Zweig erweitert wird  }
{ Parameter: Aufrufender Zweig                 }
{ Rückgabe: Kann erweitert werden: Ja/Nein     }
{----------------------------------------------}
function TDeaTree.CanExpand(Node: TTreeNode): Boolean;
{----------------------------------------------}
begin
  if (not IsExpanded(Node)) then ExpandInstDeaNode(Node);

  Result := inherited CanExpand(Node);
end;

{ Gibt zurück, ob der Hauptzweig erweitert ist }
{ Parameter: Hauptzweig                        }
{ Rückgabe: Bereits erweitert: Ja/Nein         }
{----------------------------------------------}
function TDeaTree.IsExpanded(pNode: TTreeNode): boolean;
{----------------------------------------------}
var
  p : TTreeNode;
begin
  Result := True;

  if (pNode.Level = FBuildLevel-1) then begin
    p := pNode.GetFirstChild;
    while (Assigned(p)) do begin
      if (p.ImageIndex in [C_ImageIndex_Ordner, C_ImageIndex_Parameter,
        C_ImageIndex_MRG2100, C_ImageIndex_MRG2200]) then
      begin
        Result := p.HasChildren;
        Break;
      end
      else p := pNode.GetNextChild(p);
    end;
  end;
end;

{ Erweitert Knoten um Datenelemente            }
{ Parameter: Hauptzweig, der erweitert wird    }
{----------------------------------------------}
procedure TDeaTree.ExpandInstDeaNode(pNode: TTreeNode);
{----------------------------------------------}

  function GetDea1String(pNode: TTreeNode): string;
  begin
    if (pNode.Text <> '') then begin
      Result := Trim(Copy(pNode.Text, 1, FBuildLevel));
      if (Length(Result) <> FBuildLevel) or (not (Result[1] in ['a'..'z'])) then
      begin
        if (Assigned(pNode.Data))
        then Result := IntToStr(Integer(pNode.Data))[1]
        else Result := ' ';
      end;
    end
    else Result := ' ';
  end;

var
  p : TTreeNode;
  s : string;
begin
  if (pNode.Level = FBuildLevel-1) then begin
    Items.BeginUpdate;
    try
      p := pNode.GetFirstChild;
      while (Assigned(p)) do begin
        if (p.ImageIndex in
          [C_ImageIndex_Ordner, C_ImageIndex_MRG2100, C_ImageIndex_MRG2200]) then
        begin
          if (not p.HasChildren) then begin
            s := GetDea1string(p);
            if (Length(s) = FBuildLevel) and (s[1] in ['1'..'9', 'a'..'z']) then
              BuildDeaNode(p, s);
          end;
        end;
        p := pNode.GetNextChild(p);
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

{ Initialisiert die verwendeten Komponenten    }
{ Parameter: Initialisiern (T), Freigeben (F)  }
{----------------------------------------------}
procedure TDeaTree.InitMyComponents(bState: boolean);
{----------------------------------------------}
begin
  if (bState) then begin
    if (not Assigned(FImageList)) then FImageList := TImageList.Create(Self);
  end
  else begin
    if (Assigned(FImageList)) then FImageList.Free;
  end;
end;

{ Initialisiert die verwendeten Bitmaps        }
{ Parameter: Initialisiern (T), Freigeben (F)  }
{----------------------------------------------}
procedure TDeaTree.InitImages(bState: boolean);
{----------------------------------------------}
var
  pBitmap : TBitmap;
begin
  if (bState) then begin
    pBitmap := TBitmap.Create;
    try
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Aktiv);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Inaktiv);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Umwerter);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Registrierung);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Wieser);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_DFU);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Gasbeschaffenheit);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_unbestimmt);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Ordner);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Parameter);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_MRG2100);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_MRG2200);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_AnwOrdner);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_AnwParameter);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_MRGStation_Aktiv);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_MRGStation_InAktiv);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSFGSTATION_STEUERUNG);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_REVISION);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_KORRGASBESCH);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_Gruppe_Aktiv);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_Gruppe_InAktiv);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Blende);
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_WicalMessstelle_Aktiv);  // 26.01.2016, WW
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_WicalMessstelle_Inaktiv);  // 26.01.2016, WW
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_DFU_erweitert);  // 26.01.2016, WW
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Gaszaehler);  // 04.02.2016, WW
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Odorierung);  // 05.03.2024, WW
      FImageList.Add(pBitMap, nil);
      pBitMap.LoadFromResourceName(HINSTANCE, C_Res_DSfGStation_Begleit);  // 05.03.2024, WW
      FImageList.Add(pBitMap, nil);
    finally
      pBitmap.Free;
    end;
  end
  else begin
  end;
end;

{ Baut Baum mit Deabezeichnungen auf           }
{ Parameter: Instanztypen                      }
{----------------------------------------------}
procedure TDeaTree.BuildDeaTree(sInstanzTypen: string = '');
{----------------------------------------------}

  function GetImageIndex(cInstTyp: char): integer;
  begin
    case cInstTyp of
      C_D_Instanztyp_Umw : Result := C_ImageIndex_Umwerter;
      C_D_Instanztyp_Reg : Result := C_ImageIndex_Registrierung;
      C_D_Instanztyp_Wieser : Result := C_ImageIndex_Wieser;
      C_D_Instanztyp_DFU: Result := C_ImageIndex_DFU;  // 09.07.2010
      C_D_Instanztyp_DFU2 : Result := C_ImageIndex_DFU_erweitert;  // 26.01.2016, WW
      C_D_Instanztyp_Gas : Result := C_ImageIndex_Gasbeschaffenheit;
      C_D_Instanztyp_KGM : Result := C_ImageIndex_KORRGASBESCH;
      C_D_Instanztyp_Blende : Result := C_ImageIndex_Blendenrechner;
      C_D_Instanztyp_Strg : Result := C_ImageIndex_Steuerung;
      C_D_Instanztyp_Gaszaehler : Result := C_ImageIndex_Gaszaehler;  // 04.02.2016, WW
      C_D_Instanztyp_Odor : Result := C_ImageIndex_Odor;  // 05.03.2024, WW
      C_D_Instanztyp_Begleit : Result := C_ImageIndex_Begleit;  // 05.03.2024, WW
      C_D_Instanztyp_unbest : Result := C_ImageIndex_unbestimmt;
      else Result := C_ImageIndex_Aktiv;
    end;
  end;

  function GetDeaCaption(cDea1: char; var cInstTyp: char): string;
  begin
    case cDea1 of
      'a' : begin
              cInstTyp := '0';
              Result := S_Common;
            end;
      'b' : cInstTyp := C_D_Instanztyp_Umw;
      'c' : cInstTyp := C_D_Instanztyp_Reg;
      'd' : cInstTyp := C_D_Instanztyp_Gas;
      'e' : cInstTyp := C_D_Instanztyp_DFU2;  // 09.07.2010
      'f' : cInstTyp := C_D_Instanztyp_Strg;
      'h' : cInstTyp := C_D_Instanztyp_Odor;
      'i' : cInstTyp := C_D_Instanztyp_Blende;
      'q' : cInstTyp := C_D_Instanztyp_KGM;
      'w' : cInstTyp := C_D_Instanztyp_Wieser;
      'm' : cInstTyp := C_D_Instanztyp_Gaszaehler;  // 04.02.2016, WW
      'r' : cInstTyp := C_D_Instanztyp_Begleit;  // 05.03.2024, WW
      else cInstTyp := C_D_Instanztyp_unbest;
    end;
    if (cDea1 <> 'a') then Result := ClientStammdaten.GetInstTypName(cInstTyp);
  end;

const
  C_MRG2100D = 'Wieser';
  C_MRG2100DName = 'MRG 2100 D';  // 08.11.2007, WW
var
  pNode                      : TTreeNode;
  s, sTyp, sDea, sBez, sZugr : string;
  i, j                       : integer;
  c, cITyp                   : char;
  pSl, pNodes                : TStrings;
  oldCursor                  : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Items.BeginUpdate;
  try
    Clear;

    pNodes := TStringList.Create;
    pSl := ClientStammdaten.GetDeaTree(sInstanzTypen, 1);
    try
      pNode := nil;

      j := 0;     // Zeichenanzahl des ersten Deas eines Zweiges
      c := ' ';   // Erstes Zeichen eines Zweiges
     for i := 0 to pSl.Count-1 do begin
        // Struktur: Dea<#9>Bezeichnung<#9>DeaTyp<#9>Zugriff
        sDea := (GetStringPart(pSl[i], 1, #9));  // Datenelementadresse
        if (Length(sDea) > FBuildLevel) then Continue;

        // Ggf. Zugriff überprüfen
        if (not (dvtAlle in FValueMode)) then begin
          sZugr := GetStringPart(pSl[i], 4, #9);
          if ((sZugr = 'L') and (not(dvtLesen in FValueMode))) or
             ((sZugr = 'C') and (not(dvtCode in FValueMode))) or
             ((sZugr = 'E') and (not(dvtEich in FValueMode))) or
             ((sZugr = 'B') and (not(dvtBenSchloss in FValueMode))) or
             ((sZugr = 'S') and (not(dvtSchreiben in FValueMode))) then Continue;
        end;

        sBez := (GetStringPart(pSl[i], 2, #9));  // Bezeichnung
        sTyp := (GetStringPart(pSl[i], 3, #9));  // Variablentyp
        if (sDea <> '') then sBez := sDea + ' - ' + sBez;

        // Bei Wechsel des Zweiges Zeichenanzahl für erstes Element feststellen
        if (not Assigned(pNode)) or (c <> sDea[1]) then begin
          s := GetDeaCaption(sDea[1], cITyp);
          if (pNodes.IndexOf(s) >= 0)
          then begin
            pNode := TTreeNode(pNodes.Objects[pNodes.IndexOf(s)]);
            while (pNode.Level < (Length(sDea))) and (pNode.HasChildren) do
              pNode := pNode.GetFirstChild;
          end
          else begin
            pNode := Items.AddObject(nil, s, TObject(Integer(Ord(cITyp))));
            pNode.ImageIndex := GetImageIndex(cITyp);
            pNode.SelectedIndex := GetImageIndex(cITyp);
            pNodes.AddObject(s, pNode);
          end;
          j := 0;
          c := sDea[1];
        end;

        while (Assigned(pNode)) and (pNode.Level >= (Length(sDea)-j)) do
          pNode := pNode.Parent;
        if (Assigned(pNode))
        then pNode := Items.AddChild(pNode, sBez)
        else pNode := Items.Add(pNode, sBez);

        if (sTyp = '')
        then begin
          if (sDea = 'w') then begin
            if (Pos(C_MRG2100D, sBez) > 0) then begin
              pNode.Text := C_MRG2100DName;
              pNode.Data := TObject(Integer(6));
              pNode.ImageIndex := C_ImageIndex_MRG2100;
              pNode.SelectedIndex := C_ImageIndex_MRG2100;
            end
            else begin
              pNode.Text := S_Wieser;
              pNode.Data := TObject(Integer(7));
              pNode.ImageIndex := C_ImageIndex_MRG2200;
              pNode.SelectedIndex := C_ImageIndex_MRG2200;
            end;
          end
          else begin
            pNode.ImageIndex := C_ImageIndex_Ordner;
            pNode.SelectedIndex := C_ImageIndex_Ordner;
          end;
        end
        else begin
          pNode.ImageIndex := C_ImageIndex_Parameter;
          pNode.SelectedIndex := C_ImageIndex_Parameter;
        end;
      end;

    finally
      pSl.Free;
      pNodes.Free;
    end;
  finally
    Items.EndUpdate;
    Screen.Cursor := oldCursor;
  end;
end;

{ Baut Zweig mit Deabezeichnungen auf           }
{ Parameter: Zweig, Instanztypen               }
{----------------------------------------------}
procedure TDeaTree.BuildDeaNode(pNode: TTreeNode; sInstanzTyp: string);
{----------------------------------------------}

  function GetImageIndex(cInstTyp: char): integer;
  begin
    case cInstTyp of
      C_D_Instanztyp_Umw : Result := C_ImageIndex_Umwerter;
      C_D_Instanztyp_Reg : Result := C_ImageIndex_Registrierung;
      C_D_Instanztyp_Wieser : Result := C_ImageIndex_Wieser;
      C_D_Instanztyp_DFU : Result := C_ImageIndex_DFU;  // 09.07.2010
      C_D_Instanztyp_DFU2 : Result := C_ImageIndex_DFU_erweitert;  // 26.01.2016, WW
      C_D_Instanztyp_Gas : Result := C_ImageIndex_Gasbeschaffenheit;
      C_D_Instanztyp_Strg : Result := C_ImageIndex_Steuerung;
      C_D_Instanztyp_KGM : Result := C_ImageIndex_KORRGASBESCH;
      C_D_Instanztyp_Blende : Result := C_ImageIndex_Blendenrechner;
      C_D_Instanztyp_Gaszaehler : Result := C_ImageIndex_Gaszaehler;  // 04.02.2016, WW
      C_D_Instanztyp_Odor : Result := C_ImageIndex_Odor;  // 05.03.2024, WW
      C_D_Instanztyp_Begleit : Result := C_ImageIndex_Begleit;  // 05.03.2024, WW
      C_D_Instanztyp_unbest : Result := C_ImageIndex_unbestimmt;
      else Result := C_ImageIndex_Aktiv;
    end;
  end;

  function GetDeaCaption(cDea1: char; var cInstTyp: char): string;
  begin
    case cDea1 of
      'a' : begin
              cInstTyp := '0';
              Result := S_Common;
            end;
      'b' : cInstTyp := C_D_Instanztyp_Umw;
      'c' : cInstTyp := C_D_Instanztyp_Reg;
      'd' : cInstTyp := C_D_Instanztyp_Gas;
      'e' : cInstTyp := C_D_Instanztyp_DFU2;  // 09.07.2010
      'f' : cInstTyp := C_D_Instanztyp_Strg;
      'h' : cInstTyp := C_D_Instanztyp_Odor;
      'i' : cInstTyp := C_D_Instanztyp_Blende;
      'q' : cInstTyp := C_D_Instanztyp_KGM;
      'w' : cInstTyp := C_D_Instanztyp_Wieser;
      'm' : cInstTyp := C_D_Instanztyp_Gaszaehler;  // 04.02.2016, WW
      'r' : cInstTyp := C_D_Instanztyp_Begleit;  // 05.03.2024, WW
      else cInstTyp := C_D_Instanztyp_unbest;
    end;
    if (cDea1 <> 'a') then Result := ClientStammdaten.GetInstTypName(cInstTyp);
  end;

var
//  pNode : TTreeNode;
  sTyp, sDea, sBez, sZugr : string;
  i, j                    : integer;
  pSl                     : TStrings;
  oldCursor               : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Items.BeginUpdate;
  try
    pSl := ClientStammdaten.GetDeaTree(sInstanzTyp, FBuildLevel);
    try
      j := 0;     // Zeichenanzahl des ersten Deas eines Zweiges
      for i := 1 to pSl.Count-1 do begin  // 1. Eintrag (0) enthält Knotennamen !
        // Ggf. Zugriff überprüfen
        if (not (dvtAlle in FValueMode)) then begin
          sZugr := GetStringPart(pSl[i], 4, #9);
          if ((sZugr = 'L') and (not(dvtLesen in FValueMode))) or
             ((sZugr = 'C') and (not(dvtCode in FValueMode))) or
             ((sZugr = 'E') and (not(dvtEich in FValueMode))) or
             ((sZugr = 'B') and (not(dvtBenSchloss in FValueMode))) or
             ((sZugr = 'S') and (not(dvtSchreiben in FValueMode))) then Continue;
        end;

        // Struktur: Dea<#9>Bezeichnung<#9>DeaTyp
        sDea := (GetStringPart(pSl[i], 1, #9));  // Datenelementadresse
        sBez := (GetStringPart(pSl[i], 2, #9));  // Bezeichnung
        sTyp := (GetStringPart(pSl[i], 3, #9));  // Variablentyp
        if (sDea <> '') then sBez := sDea + ' - ' + sBez;

        while (Assigned(pNode)) and (pNode.Level >= (Length(sDea)-j)) do
          pNode := pNode.Parent;
        if (Assigned(pNode))
        then pNode := Items.AddChild(pNode, sBez)
        else pNode := Items.Add(pNode, sBez);

        if (sTyp = '')
        then begin
          pNode.ImageIndex := C_ImageIndex_Ordner;
          pNode.SelectedIndex := C_ImageIndex_Ordner;
        end
        else begin
          pNode.ImageIndex := C_ImageIndex_Parameter;
          pNode.SelectedIndex := C_ImageIndex_Parameter;
        end;
      end;

    finally
      pSl.Free;
    end;
  finally
    Items.EndUpdate;
    Screen.Cursor := oldCursor;
  end;
end;

{ Leert den TreeView                           }
{----------------------------------------------}
procedure TDeaTree.Clear;
{----------------------------------------------}
begin
  Self.Items.BeginUpdate;
  try
    Self.Items.Clear;
  finally
    Self.Items.EndUpdate;
  end;
end;

{----------------------------------------------}
procedure TDeaTree.SetValueMode(pValueMode: TDeaValueTypes);
{----------------------------------------------}
begin
  if (pValueMode <> FValueMode) then begin
    FValueMode := pValueMode;
    BuildDEATree(FInstanzTypen);
  end;
end;

end.
