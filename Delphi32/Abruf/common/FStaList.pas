{------------------------------------------------------------------------------}
{ Liste mit Stationsauswahl für WK22                                           }
{                                                                              }
{ 02.10.2003  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003                                          }
{------------------------------------------------------------------------------}
unit FStaList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FDlgDef, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  GD_Utils, DStaDll, DSfGStationen, DSG_Utils, DMomLists;

type
  TFormStaAuswahlDlg = class(TFormDialogDefault)
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    FDSfGStationen : TDSfGStationen;
    FSortOrder     : byte;
    FQuellArchive  : byte;
    FSichtLevel    : byte;
    FInstTypen     : TCharSet;
    FGerTypen      : TCharSet;
  protected
    { Protected-Deklarationen }
    procedure Aktualisieren; virtual;
    property DSfGStationen: TDSfGStationen read FDSfGStationen;
    procedure OnDSfGStationenChange(Sender: TObject; Node: TTreeNode); virtual;
    procedure DSfGStationenChange; virtual;
  public
    { Public-Deklarationen }
    procedure FillStaTree(iSortOrder: byte = 1; iQuellArchive: byte = 1;
      pInstTypen: TCharSet = []; pGerTypen: TCharSet = [];
      iSichtLevel: byte = 1);
    function GetSelectedRecord: TStaRecord;
  end;

implementation

{$R *.DFM}

{----------------------------------------------------}
procedure TFormStaAuswahlDlg.FormCreate(Sender: TObject);
{----------------------------------------------------}
begin
  inherited;

  FDSfGStationen := TDSfGStationen.Create(Self, drlDSfG, dslNone);
  FDSfGStationen.Parent := pnClient;
  FDSfGStationen.Align := alClient;
  FDSfGStationen.OnChange := OnDSfGStationenChange;
end;

{----------------------------------------------------}
procedure TFormStaAuswahlDlg.FormDestroy(Sender: TObject);
{----------------------------------------------------}
begin
  FDSfGStationen.Free;

  inherited;
end;

{ TreeView füllen                                    }
{ Parameter: TreeView, Sortierung (Id=0, Name=1),    }
{            Archive bei Quellinstanzen (0=Registr., )
{              1=Quelle, 2=Logb-Q, 3=Arch-Q)         }
{            anzuz. Instanztypen ([]=Alle),          }
{            anzuz. Gerätetypen ([]=Alle)            }
{----------------------------------------------------}
procedure TFormStaAuswahlDlg.FillStaTree(iSortOrder: byte = 1;
  iQuellArchive: byte = 1; pInstTypen: TCharSet = [];
  pGerTypen: TCharSet = []; iSichtLevel: byte = 1);
{----------------------------------------------------}
begin
  FSortOrder := iSortOrder;
  FQuellArchive := iQuellArchive;
  FInstTypen := pInstTypen;
  FGerTypen := pGerTypen;
  FSichtLevel := iSichtLevel;
  Aktualisieren;
end;

{ Gibt Record mit Angaben des selekt. Eintrags zurück}
{ Rückgabe: Record mit Angaben oder 0-Record         }
{----------------------------------------------------}
function TFormStaAuswahlDlg.GetSelectedRecord: TStaRecord;
{----------------------------------------------------}
begin
  // Default
  Result.GTyp := #0;
  Result.GName := '';
  Result.InstId := 0;
  Result.MyId := 0;

  if (Assigned(FDSfGStationen.Selected)) and
     (Assigned(FDSfGStationen.Selected.Data)) then
  begin
    with PStaRecord(FDSfGStationen.Selected.Data)^ do begin
      Result.GTyp := GTyp;
      Result.GName := GName;
      Result.InstId := InstId;
      Result.MyId := MyId;
    end;
  end;
end;

{ TreeView füllen                                    }
{----------------------------------------------------}
procedure TFormStaAuswahlDlg.Aktualisieren;
{----------------------------------------------------}
begin
  ClientStammdaten.FillStaTree(FDSfGStationen, FSortOrder, FQuellArchive,
    FInstTypen, FGerTypen, FSichtLevel);
end;

{----------------------------------------------------}
procedure TFormStaAuswahlDlg.OnDSfGStationenChange(
  Sender: TObject; Node: TTreeNode);
{----------------------------------------------------}
begin
  DSfGStationenChange;
end;

{----------------------------------------------------}
procedure TFormStaAuswahlDlg.DSfGStationenChange;
{----------------------------------------------------}
begin
end;

end.
