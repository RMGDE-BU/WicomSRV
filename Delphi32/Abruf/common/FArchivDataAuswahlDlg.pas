{------------------------------------------------------------------------------}
{ Liste mit Archivdatenauswahl für WK22                                        }
{                                                                              }
{ 02.10.2003  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003                                          }
{------------------------------------------------------------------------------}
unit FArchivDataAuswahlDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls,
  FStaList, DSfGArchive, DMomLists, WSysCon, DSG_Utils, GD_Utils;

type
  TFormArchivdatenAuswahlDlg = class(TFormStaAuswahlDlg)
    bbtnAktualisieren: TBitBtn;
    Panel1: TPanel;
    bbtnCancel: TBitBtn;
    procedure bbtnAktualisierenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    FDSfGArchivInfo : TDSfGArchivInfo;
  protected
    { Protected-Deklarationen }
    procedure DSfGStationenChange; override;
    procedure SetDSfGArchivInfo(pDSfGArchivInfo: TDSfGArchivInfo); virtual;
    procedure ReduceStaList; virtual;
    procedure Aktualisieren; override;
  public
    { Public-Deklarationen }
    property DSfGArchivInfo: TDSfGArchivInfo
      read FDSfGArchivInfo write SetDSfGArchivInfo;
    property DSfGStationen;
  end;

implementation

{$R *.DFM}

{----------------------------------------------------}
procedure TFormArchivdatenAuswahlDlg.FormCreate(Sender: TObject);
{----------------------------------------------------}
begin
  inherited;

  bbtnAktualisieren.BringToFront;
end;

{ Übergibt Archivinfo-Object für Eingrenzung d. Liste}
{ Parameter: Archivinfo-Object                       }
{----------------------------------------------------}
procedure TFormArchivdatenAuswahlDlg.SetDSfGArchivInfo(
  pDSfGArchivInfo: TDSfGArchivInfo);
{----------------------------------------------------}
begin
  FDSfGArchivInfo := pDSfGArchivInfo;
  
  ReduceStaList;
end;

{ Führt Eingrenzung d. Liste durch                   }
{----------------------------------------------------}
procedure TFormArchivdatenAuswahlDlg.ReduceStaList;
{----------------------------------------------------}

  {------------------------------------------------}
  function HasData(pNode: TTreeNode): boolean;
  {------------------------------------------------}
  var
    p : TTreeNode;
  begin
    Result := False;  // Default
    if (Assigned(pNode)) and (Assigned(pNode.Data)) then begin
      with PStaRecord(pNode.Data)^ do begin
        if (GTyp = C_GerArtDSfG) then begin
          // Prüfen, ob Abrufdaten vorhanden sind
          if (pNode.Level = C_Level_Archive) and
             (pNode.ImageIndex = C_ImageIndex_Archive)
          then Result := (FDSfGArchivInfo.GetIndex(InstId, MyId) > 0)
          else begin
            p := pNode.GetFirstChild;
            while (Assigned(p)) do begin
              Result := HasData(p);
              if (Result) then Exit;
              p := pNode.GetNextChild(p);
            end;
          end;
        end;
      end;
    end;
  end;

resourcestring
  S_NoData = 'Keine Stationen mit Daten';
var
  i : integer;
  p1, p2 : TTreeNode;
  p   : PStaRecord;
begin
  if (DSfGStationen.Items.Count = 0) then Exit;

  DSfGStationen.Items.BeginUpdate;
  try
    // Löschen der Stationen ohne Daten
    p1 := DSfGStationen.Items.GetFirstNode.GetFirstChild;  // DSfG-Stationen
    while (Assigned(p1)) do begin
      p2 := p1;
      p1 := p2.GetNextSibling;
      if (not HasData(p2)) then DSfGStationen.DeleteNode(p2);
    end;

    // Löschen der Archive ohne Daten
    for i := DSfGStationen.Items.Count-1 downto 0 do
      if (not HasData(DSfGStationen.Items[i]))
      then DSfGStationen.DeleteNode(DSfGStationen.Items[i]);

    DSfGStationen.AlphaSort;
  finally
    DSfGStationen.Items.EndUpdate;
  end;

  if (DSfGStationen.Items.Count = 0) then begin
    New(p);
    p^.GTyp:=#0;
    p^.GName:='';
    p^.InstId:=0;
    p^.MyId:=0;
    DSfGStationen.Items.AddObject(nil, S_NoData, p);
  end;
end;

{----------------------------------------------------}
procedure TFormArchivdatenAuswahlDlg.bbtnAktualisierenClick(Sender: TObject);
{----------------------------------------------------}
var
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  EnableMyControls(Self, False, False, False);
  try
    Aktualisieren;
  finally
    EnableMyControls(Self, True, False, False);
    Screen.Cursor := oldCursor;
  end;
end;

{ TreeView füllen                                    }
{----------------------------------------------------}
procedure TFormArchivdatenAuswahlDlg.Aktualisieren;
{----------------------------------------------------}
begin
  inherited;
  DSfGStationen.Items[0].Expand(False);

  if (Assigned(DSfGArchivInfo)) then begin
    DSfGArchivInfo.Aktualisieren;
    ReduceStaList;
  end;
end;

{----------------------------------------------------}
procedure TFormArchivdatenAuswahlDlg.DSfGStationenChange;
{----------------------------------------------------}
begin
  bbtnOk.Enabled := (Assigned(DSfGStationen.Selected)) and
    (DSfGStationen.Selected.Level = C_Level_Archive);
end;

end.
