{------------------------------------------------------------------------------}
{ DSfG - Stationsauswahl und -Anzeige aus WICOM-Stammdaten                     }
{                                                                              }
{ Benötigt DSta.DLL                                                            }
{                                                                              }
{ Hinweis: FDSfGStationsauswahl ist für WISERV ausgerichtet und spezieller     }
{                                                                              }
{ 27.08.2001  GD    Neu                                                        }
{ 03.02.2002  GD    Callback 'TStationChoosen' um freien Parameter erweitert   }
{ 12.05.2004  GD    Erweitert um StationsId                                    }
{ 31.01.2017  WW    Erweiterung für Einlesen über erweit. DFÜ-Instanz (Typ E)  }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2017                                    }
{------------------------------------------------------------------------------}
unit FDStationsAuswahl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls,
  FStationsAuswahl, DStaDll, WSysCon, DSfGShapes, DMomLists, GD_Utils;

type
  TStationChoosen = procedure(iStaId: integer; iState: byte = 0) of object;

  TFormDStationsAuswahl = class(TFormStationsAuswahl)
  private
    { Private-Deklarationen }
    FInstTypen      : TCharSet;
    FStationChoosen : TStationChoosen;
    procedure InitBusShape(bState: boolean);
    procedure DeleteStationsWithoutInstTypen;
    procedure SetInstTypen(pTypen: TCharSet);
  protected
    { Protected-Deklarationen }
    FShape      : TDSfGBusShape;
    FHasDFU     : boolean;
    FStationsId : integer;  // 12.05.2004
    procedure InitComponents(bState: boolean); override;
    procedure InitDSfGStationen(bState: boolean); override;
    procedure ShapeInstBtnClick(cInstAdr, cInstTyp: char);
    procedure DSfGStationChanged(Sender: TObject; Node: TTreeNode); virtual;
    procedure TVOnDblClick(Sender: TObject); virtual;
  public
    { Public-Deklarationen }
    property InstTypen: TCharSet read FInstTypen write SetInstTypen;
    property StationChoosen: TStationChoosen
      read FStationChoosen write FStationChoosen;
    property StationsId: integer read FStationsId;
  end;

implementation

{$R *.DFM}

{ Initialisieren/Freigeben des Stammdatenbaums         }
{ Parameter: T=Initialisieren; F=Freigeben             }
{------------------------------------------------------}
procedure TFormDStationsAuswahl.InitComponents(bState: boolean);
{------------------------------------------------------}
begin
  inherited;

  if (bState) then begin
    FInstTypen := [];        // Alle Typen vorgesehen
    FStationChoosen := nil;  // Kein Callback bei ausgewählter Station
    FHasDFU := False;        // Flag, ob aktuelle Station eine DFÜ-Instanz hat
    FStationsId := -1;       // Aktuell keine Station markiert
    InitBusShape(True);
  end
  else begin
    InitBusShape(False);
  end;
end;

{ Initialisieren/Freigeben des Stammdatenbaums         }
{ Parameter: T=Initialisieren; F=Freigeben             }
{------------------------------------------------------}
procedure TFormDStationsAuswahl.InitDSfGStationen(bState: boolean);
{------------------------------------------------------}
begin
  inherited;

  if (bState) then begin
    StaTree.PopupMenu := PopupMenu;
    StaTree.BuildTree;
    DeleteStationsWithoutInstTypen;
    StaTree.Align := alClient;
    StaTree.OnDSfGStationChange := DSfGStationChanged;
    StaTree.OnDblClick := TVOnDblClick;
  end;
end;

{ Initialisieren der graphischen Busanzeige    }
{ Parameter: Initialisieren (T); freigeben (F) }
{----------------------------------------------}
procedure TFormDStationsAuswahl.InitBusShape(bState: boolean);
{----------------------------------------------}
begin
  if (bState) then begin
    if (not Assigned(FShape)) then begin
      FShape := TDSfGBusShape.Create(Self);
      FShape.Parent := pnClient;
      FShape.Align := alClient;
      FShape.OnInstBtnClick := ShapeInstBtnClick;
    end;
  end
  else begin
    if (Assigned(FShape)) then begin
      FShape.Free;
      FShape := nil;
    end;
  end;
end;

{----------------------------------------------}
procedure TFormDStationsAuswahl.DeleteStationsWithoutInstTypen;
{----------------------------------------------}
var
  i, j    : integer;
  bHasInstTyp : boolean;
begin
  if (FInstTypen = []) then Exit;  // Alle Typen vorgesehen

  for i := StaTree.Items.Count-1 downto 1 do begin

    with ClientStammdaten.FillDInstanzListe(StaTree.Items[i].Text) do
    begin
      bHasInstTyp := False;
      for j := 0 to Count-1 do
        if (InstanzTyp[j] in FInstTypen) then begin
          bHasInstTyp := True;
          Break;
        end;
      if (not bHasInstTyp) then
        StaTree.Items.Delete(StaTree.Items[i]);
    end;

  end;
end;

{ Ereignis-Procedure für Stationswechsel       }
{----------------------------------------------}
procedure TFormDStationsAuswahl.DSfGStationChanged(Sender: TObject; Node: TTreeNode);
{----------------------------------------------}
resourcestring
  C_Msg_NoStaPresent = 'Stationsauswahl ist leer !';
var
  i          : integer;
  cAdr, cTyp : char;
  oldCursor  : TCursor;
  p          : TDSfGStationList;
begin
  FHasDFU := False;     // Flag, ob Station DFÜ hat
  FStationsId := -1;    // Keine Station ausgewählt
  if (Assigned(StaTree)) and (Assigned(Node)) then
    FStationsId := ClientStammdaten.DStationsId(Node.Text);

  if (Assigned(FShape)) then begin

    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try

  // Alle Instanzen löschen
      FShape.Clear;

  // Neue Instanzen eintragen
      if (Assigned(Node)) then begin

        FShape.Text := Node.Text;
        p := TDSfGStationList(ClientStammdaten.FillDInstanzListe(Node.Text));

        if (Assigned(p)) then
          for i := 0 to p.Count-1 do begin
            cAdr := p.InstanzAdresse[i];
            cTyp := p.InstanzTyp[i];
            FShape.NewShape(cAdr, cTyp, False);
            if (cTyp = C_D_Instanztyp_DFU) OR (cTyp = C_D_Instanztyp_DFU2) then  // 31.01.2017, WW
              FHasDFU := True; // Station hat DFÜ
          end;
        FShape.Resize;

      end
      else begin
        if (Assigned(StaTree)) and (StaTree.Items.Count > 0)
          then FShape.Text := StaTree.Items[0].Text
          else FShape.Text := C_Msg_NoStaPresent;
      end;

    finally
      Screen.Cursor := oldCursor;
    end;

  end;
end;

{ Ereignis-Procedure für Doppelclick auf Tree  }
{----------------------------------------------}
procedure TFormDStationsAuswahl.TVOnDblClick(Sender: TObject);
{----------------------------------------------}
begin
  if (Assigned(FStationChoosen)) and (Assigned(StaTree.Selected)) then
    FStationChoosen(ClientStammdaten.DStationsId(StaTree.Selected.Text));
end;

{ Ereignis-Procedure für Click auf Inst-Shape  }
{ Parameter: Instanzadresse, Instanztyp        }
{----------------------------------------------}
procedure TFormDStationsAuswahl.ShapeInstBtnClick(cInstAdr, cInstTyp: char);
{----------------------------------------------}
var
  s : string;
begin
  s := 'Station :  '#13#10'  ' + StaTree.Selected.Text + #13#10#13#10;
  s := s + 'Instanz :  '#13#10;
  s := s + '  Busadresse :  ' + cInstAdr + #13#10;
  s := s + '  Instanztyp :  ' + cInstTyp + ' - ' +
    ClientStammdaten.GetInstTypName(cInstTyp) + #13#10;
  s := s + '  Gerätetyp :  ' + ClientStammdaten.DStationsListe.GetInstanzGeraetetyp(
    StaTree.Selected.Text, cInstAdr) + #13#10;
  s := s + '  Hersteller :  ' + ClientStammdaten.DStationsListe.GetInstanzHersteller(
    StaTree.Selected.Text, cInstAdr) + #13#10;
  s := s + '  Fabriknummer :  ' + ClientStammdaten.DStationsListe.GetInstanzFabriknummer(
    StaTree.Selected.Text, cInstAdr) + #13#10;

  MessageDlg(s, mtInformation, [mbOk], 0);
end;

{ Setzt die notwendigen Instanztypen           }
{ Parameter: Set mit Instanztypen              }
{----------------------------------------------}
procedure TFormDStationsAuswahl.SetInstTypen(pTypen: TCharSet);
{----------------------------------------------}
begin
  FInstTypen := pTypen;
  StaTree.BuildTree;
  DeleteStationsWithoutInstTypen;
end;

end.
