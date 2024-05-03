{------------------------------------------------------------------------------}
{ DSfG - Stationsauswahl und -Anzeige aus WICOM-Stammdaten                     }
{                                                                              }
{ Benötigt DSta.DLL                                                            }
{                                                                              }
{ 11.10.2000  GD    Neu                                                        }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000                                          }
{------------------------------------------------------------------------------}
unit FDSfGStationsAuswahl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GD_Utils, DSfGStationen, ExtCtrls, Buttons, DSfGShapes, ComCtrls, DStaDll,
  FDSfGInstanzAuswahl, DMomLists, FShowDatenelemente, Menus, FPrgBar, ImgList,
  DConnectionCOM, DLL_Calls, WSysCon, DMomIni;

type
  TFormStationsAuswahl = class(TForm)
    pnRight: TPanel;
    sbtnClose: TSpeedButton;
    pnClient: TPanel;
    Splitter: TSplitter;
    sbtnParameterAnzeigen: TSpeedButton;
    PopupMenu: TPopupMenu;
    miConnect: TMenuItem;
    miShowParameters: TMenuItem;
    N1: TMenuItem;
    miDeleteStation: TMenuItem;
    MainMenu: TMainMenu;
    miReadStammdaten: TMenuItem;
    miDeleteStammdaten: TMenuItem;
    miFunctions: TMenuItem;
    miShowParams: TMenuItem;
    sbtnStammdatenEinlesen: TSpeedButton;
    ImageList: TImageList;
    miConnection: TMenuItem;
    N2: TMenuItem;
    sbtnConnect: TSpeedButton;
    sbtnDeleteStammdaten: TSpeedButton;
    sbtnDfuParam: TSpeedButton;
    miDfuParam: TMenuItem;
    miDfuParamP: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbtnCloseClick(Sender: TObject);
    procedure pnClientResize(Sender: TObject);
    procedure sbtnParameterAnzeigenClick(Sender: TObject);
    procedure miConnectClick(Sender: TObject);
    procedure miShowParametersClick(Sender: TObject);
    procedure miDeleteStationClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbtnStammdatenEinlesenClick(Sender: TObject);
    procedure sbtnDfuParamClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FCaptionCtrl : TSetCaptionEvent;
    FStaTree     : TDSfGStationen;
    FShape       : TDSfGBusShape;
    FCanModify   : TProofModify;
    FAbrufStatus : TAbrufStatus;
    FHasDFU      : boolean;
    procedure DSfGStationChanged(Sender: TObject; Node: TTreeNode);
    procedure InitStaAuswahlComponent(aState: boolean);
    procedure InitBusShape(aState: boolean);
    procedure MyOnDblClick(Sender: TObject);
    procedure ShapeInstBtnClick(cInstAdr, cInstTyp: char);
    function PWFreigabe: boolean;
    procedure InitImages;
    procedure EnableControls(bState: boolean);
    procedure StammdatenEinlesen;
  protected
    procedure Notification(
      AComponent: TComponent; Operation: TOperation); override;
  public
    { Public-Deklarationen }
    procedure Aktualisieren;
    property CaptionCtrl: TSetCaptionEvent write FCaptionCtrl;
    property CanModify: TProofModify read FCanModify write FCanModify;
    property AbrufStatus: TAbrufStatus read FAbrufStatus write FAbrufStatus;
  end;

implementation

{$R *.DFM}

const
  C_ImageIndex_ReadSta    = 0;
  C_ImageIndex_ShowParams = 1;
  C_ImageIndex_DeleteSta  = 2;
  C_ImageIndex_Connect    = 3;

{------------------------------------}
procedure TFormStationsAuswahl.FormCreate(Sender: TObject);
{------------------------------------}
begin
  FCaptionCtrl := nil;  // Control für Anzeige
  CanModify := nil;     // Callback für Parametrierungs-berechtigung
  AbrufStatus := nil;   // Callback für Anzeige des Abrufstatus
  InitImages;           // Bilder in SpeedButtons einfügen
  FHasDFU := False;     // Flag, ob Station DFÜ hat

  InitStaAuswahlComponent(True);
  InitBusShape(True);
end;

{----------------------------------------------}
procedure TFormStationsAuswahl.FormDestroy(Sender: TObject);
{----------------------------------------------}
begin
  if (Assigned(FCaptionCtrl)) then FCaptionCtrl('');
  FCaptionCtrl := nil;  // Control für Anzeige

  InitStaAuswahlComponent(False);
  InitBusShape(False);
end;

{----------------------------------------------}
procedure TFormStationsAuswahl.Notification(
  AComponent: TComponent; Operation: TOperation);
{----------------------------------------------}
begin
  if (not (csDestroying in ComponentState)) then begin
    if (Operation = opRemove) then begin
      if (AComponent is TFormStationsDEAs) then begin
        sbtnParameterAnzeigen.Down := False;
        sbtnParameterAnzeigenClick(sbtnParameterAnzeigen);
      end
    end;
  end;

  inherited;
end;

{----------------------------------------------}
procedure TFormStationsAuswahl.FormClose(Sender: TObject;
  var Action: TCloseAction);
{----------------------------------------------}
begin
  Action := caFree;
end;

{----------------------------------------------}
procedure TFormStationsAuswahl.sbtnCloseClick(Sender: TObject);
{----------------------------------------------}
begin
  Close;
end;

{----------------------------------------------}
procedure TFormStationsAuswahl.pnClientResize(Sender: TObject);
{----------------------------------------------}
begin
  if (not (csDestroying in Self.ComponentState)) then begin
    if (Assigned(FCaptionCtrl)) then FCaptionCtrl('[Stationsauswahl]');
    if (Assigned(FStaTree)) then FStaTree.Width := pnClient.Width div 3;
  end;
end;

{ Initialisieren des Stationsauswahl-Baums     }
{ Parameter: Initialisieren (T); freigeben (F) }
{----------------------------------------------}
procedure TFormStationsAuswahl.InitStaAuswahlComponent(aState: boolean);
{----------------------------------------------}
begin
  if (aState) and (not Assigned(FStaTree)) then begin
    FStaTree := TDSfGStationen.Create(Self, drlDSfG, dslStation);
    FStaTree.Parent := pnClient;
    FStaTree.Align := alLeft;
    FStaTree.BuildTree;
    FStaTree.OnDSfGStationChange := Self.DSfGStationChanged;
    FStaTree.OnDblClick := MyOnDblClick;
    FStaTree.Width := pnClient.Width div 2;
    FStaTree.PopupMenu := Self.PopupMenu;
    Splitter.Left := FStaTree.Width + 1;
  end
  else if (Assigned(FStaTree)) then begin
    FStaTree.Free;
    FStaTree := nil;
  end;
end;

{ Initialisieren der graphischen Busanzeige    }
{ Parameter: Initialisieren (T); freigeben (F) }
{----------------------------------------------}
procedure TFormStationsAuswahl.InitBusShape(aState: boolean);
{----------------------------------------------}
begin
  if (aState) and (not Assigned(FShape)) then begin
    FShape := TDSfGBusShape.Create(Self);
    FShape.Parent := pnClient;
    FShape.Align := alClient;
    FShape.OnInstBtnClick := ShapeInstBtnClick;
  end
  else if (Assigned(FShape)) then begin
    FShape.Free;
    FShape := nil;
  end;
end;

{ Ereignis-Procedure für Stationswechsel       }
{----------------------------------------------}
procedure TFormStationsAuswahl.DSfGStationChanged(Sender: TObject; Node: TTreeNode);
{----------------------------------------------}
var
  i          : integer;
  cAdr, cTyp : char;
  oldCursor  : TCursor;
  p          : TDSfGStationList;
begin
  EnableControls(False);
  FHasDFU := False;     // Flag, ob Station DFÜ hat

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
            if (cTyp = C_D_Instanztyp_DFU) then FHasDFU := True; // Station hat DFÜ
          end;
        FShape.Resize;

      end
      else begin
        if (Assigned(FStaTree)) and (FStaTree.Items.Count > 0)
          then FShape.Text := FStaTree.Items[0].Text
          else FShape.Text := 'Stationsauswahl ist leer !';
      end;

    finally
      Screen.Cursor := oldCursor;
    end;

  end;

  EnableControls((Assigned(FStaTree.Selected)) and (FStaTree.Selected.Level > 0));
end;

{ Ereignis-Procedure für Doppelclick auf Baum  }
{----------------------------------------------}
procedure TFormStationsAuswahl.MyOnDblClick(Sender: TObject);
{----------------------------------------------}
var
  oldCursor : TCursor;
begin
  if (Sender is TDSfGStationen) then begin
    if (Assigned(FStaTree.Selected)) and (FStaTree.Selected.Level = 1) then
    begin
      if (MessageDlg('Verbindung zu Station ''' + FStaTree.Selected.Text +
        ''' herstellen ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        oldCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        try

          if (Assigned(AbrufStatus)) then AbrufStatus(2);

          with TFormInstanzAuswahl.Create(Self.Owner) do begin
            CanModify := Self.CanModify;
            AbrufStatus := Self.AbrufStatus;
            Parent := Self.Parent;
            CaptionCtrl := FCaptionCtrl;
            Align := Self.Align;
            StationsId := ClientStammdaten.DStationsId(FStaTree.Selected.Text);
            Show;
          end;
          Self.Hide;

        finally
          Screen.Cursor := oldCursor;
        end;
      end;
    end;
  end;
end;

{ Ereignis-Procedure für Click auf Inst-Shape  }
{ Parameter: Instanzadresse, Instanztyp        }
{----------------------------------------------}
procedure TFormStationsAuswahl.ShapeInstBtnClick(cInstAdr, cInstTyp: char);
{----------------------------------------------}
var
  s : string;
begin
  s := 'Station :  '#13#10'  ' + FStaTree.Selected.Text + #13#10#13#10;
  s := s + 'Instanz :  '#13#10;
  s := s + '  Busadresse :  ' + cInstAdr + #13#10;
  s := s + '  Instanztyp :  ' + cInstTyp + ' - ' +
    ClientStammdaten.GetInstTypName(cInstTyp) + #13#10;
  s := s + '  Gerätetyp :  ' + ClientStammdaten.DStationsListe.GetInstanzGeraetetyp(
    FStaTree.Selected.Text, cInstAdr) + #13#10;
  s := s + '  Hersteller :  ' + ClientStammdaten.DStationsListe.GetInstanzHersteller(
    FStaTree.Selected.Text, cInstAdr) + #13#10;
  s := s + '  Fabriknummer :  ' + ClientStammdaten.DStationsListe.GetInstanzFabriknummer(
    FStaTree.Selected.Text, cInstAdr) + #13#10;

  MessageDlg(s, mtInformation, [mbOk], 0);
end;

{ Aktualisieret Stationsanzeige                }
{----------------------------------------------}
procedure TFormStationsAuswahl.Aktualisieren;
{----------------------------------------------}
begin
  ClientStammdaten.Free;
  ClientStammdaten := TClientStammdaten.Create('WStammDir');
  if (Assigned(FStaTree)) then FStaTree.BuildTree;
end;

{----------------------------------------------}
procedure TFormStationsAuswahl.sbtnParameterAnzeigenClick(Sender: TObject);
{----------------------------------------------}
var
  i         : integer;
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    if (sbtnParameterAnzeigen.Down) then begin
      with TFormStationsDEAs.Create(Self) do begin
        BorderStyle := bsNone;
        CanModify := Self.CanModify;
        Parent := Self.pnClient;
        Align := alClient;
        Stationsname := Self.FStaTree.Selected.Text;
        if (HasInstanzenWithValues) then begin
          Show;
          if (Assigned(FCaptionCtrl)) then
            FCaptionCtrl('[Stationsauswahl] - [Datenelemente von Station: ' +
              FStaTree.Selected.Text + ']');
        end
        else begin
          MessageDlg('Keine Daten für Instanzen für Station ' + StationsName + ' vorhanden',
            mtInformation, [mbOk], 0);
          Release;
        end;
      end;
    end
    else begin
      for i := pnClient.ControlCount-1 downto 0 do
        if (pnClient.Controls[i] is TFormStationsDEAs) then
          if (not(csDestroying in
                  TFormStationsDEAs(pnClient.Controls[i]).ComponentState)) then
            TFormStationsDEAs(pnClient.Controls[i]).Release;
      if (Assigned(FCaptionCtrl)) then FCaptionCtrl('[Stationsauswahl]');
    end;
    FStaTree.Visible := not sbtnParameterAnzeigen.Down;
    FShape.Visible := not sbtnParameterAnzeigen.Down;
    pnRight.Visible := not sbtnParameterAnzeigen.Down;

  finally
    Screen.Cursor := oldCursor;
  end;

end;

{----------------------------------------------}
procedure TFormStationsAuswahl.miConnectClick(Sender: TObject);
{----------------------------------------------}
begin
  MyOnDblClick(FStaTree);
end;

{----------------------------------------------}
procedure TFormStationsAuswahl.miShowParametersClick(Sender: TObject);
{----------------------------------------------}
begin
  sbtnParameterAnzeigen.Down := True;
  sbtnParameterAnzeigenClick(Self);
end;

{ Paßwortfreigabe abfragen                     }
{ Rückgabe: Freigabe Ja/Nein                   }
{----------------------------------------------}
function TFormStationsAuswahl.PWFreigabe: boolean;
{----------------------------------------------}
begin
  Result := ((Assigned(CanModify)) and (CanModify));
end;

{----------------------------------------------}
procedure TFormStationsAuswahl.miDeleteStationClick(Sender: TObject);
{----------------------------------------------}
var
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    if (Assigned(FStaTree.Selected)) and (FStaTree.Selected.Level > 0) and
       (PWFreigabe)
    then
      if (MessageDlg('Alle Daten von Station ' + FStaTree.Selected.Text +
        ' löschen ?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes)
      then begin
        ClientStammdaten.DeleteStaId(
          ClientStammdaten.DStationsId(FStaTree.Selected.Text));
        Aktualisieren;
      end;

  finally
    Screen.Cursor := oldCursor;
  end;
end;

{----------------------------------------------}
procedure TFormStationsAuswahl.FormShow(Sender: TObject);
{----------------------------------------------}
begin
  MergeMenu(MainMenu, True);
end;

{ Bilderliste initialisieren                   }
{----------------------------------------------}
procedure TFormStationsAuswahl.InitImages;
{----------------------------------------------}
var
  pBitmap : TBitmap;
begin
  pBitmap := TBitmap.Create;
  try
    ImageList.GetBitmap(C_ImageIndex_ReadSta, pBitmap);
    sbtnStammdatenEinlesen.Glyph := pBitmap;
    ImageList.GetBitmap(C_ImageIndex_ShowParams, pBitmap);
    sbtnParameterAnzeigen.Glyph := pBitmap;
    ImageList.GetBitmap(C_ImageIndex_DeleteSta, pBitmap);
    sbtnDeleteStammdaten.Glyph := pBitmap;
    ImageList.GetBitmap(C_ImageIndex_Connect, pBitmap);
    sbtnConnect.Glyph := pBitmap;
  finally
    if (Assigned(pBitmap)) then pBitmap.Free;
  end;
end;

{ En-/Disables visuelle Komponenten            }
{----------------------------------------------}
procedure TFormStationsAuswahl.EnableControls(bState: boolean);
{----------------------------------------------}
begin
  sbtnParameterAnzeigen.Enabled := bState;
  sbtnDeleteStammdaten.Enabled := bState;
  sbtnConnect.Enabled := bState;
  sbtnDfuParam.Enabled :=
    ((bState) and (FHasDFU) and (DSfGMomIni.ComDllMode = C_ComMode_WICOM));
  miDfuParam.Enabled := sbtnDfuParam.Enabled;
  miDfuParamP.Enabled := sbtnDfuParam.Enabled;
  miConnect.Enabled := bState;
  miShowParameters.Enabled := bState;
  miDeleteStation.Enabled := bState;
  miDeleteStammdaten.Enabled := bState;
  miConnection.Enabled := bState;
  miShowParams.Enabled := bState;
end;

{ Stammdaten einlesen                          }
{----------------------------------------------}
procedure TFormStationsAuswahl.StammdatenEinlesen;
{----------------------------------------------}
var
  p : TStrings;
  oldCursor : TCursor;
begin
  if (MessageDlg('Stammdaten einer Station einlesen ?'#13#10#13#10 +
    '(Dieser Vorgang kann einige Minuten dauern)', mtConfirmation,
    [mbYes, mbCancel], 0) <> mrYes) then Exit;

  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    with TFormProgressBar.Create(Self, 'Stammdaten einlesen', 0, 100) do
    try
      AutoStep(Self);
      Application.ProcessMessages;

      with TCustomDSfGCOMObject.Create do
      try
        InitConnection(True);
        p := ReadStammdaten;
        try
          Caption := 'Stammdaten in Datenbank eintragen';
          ClientStammdaten.WriteStammdaten(p);
        finally
          p.Free;
        end;

        Aktualisieren;

      finally
        Free;
      end;

    finally
      Release;
    end;

  finally
    Screen.Cursor := oldCursor;
  end;
end;

{----------------------------------------------}
procedure TFormStationsAuswahl.sbtnStammdatenEinlesenClick(
  Sender: TObject);
{----------------------------------------------}
begin
  StammdatenEinlesen;
  MergeMenu(MainMenu, True);
end;

{----------------------------------------------}
procedure TFormStationsAuswahl.sbtnDfuParamClick(Sender: TObject);
{----------------------------------------------}
begin
  DFU_Parametrierung(ClientStammdaten.DStationsId(FStaTree.Selected.Text));
end;

end.
