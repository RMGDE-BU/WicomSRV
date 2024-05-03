{------------------------------------------------------------------------------}
{ Gemeinsames Ausgangsfenster für Momentanwerte aus VCD extrahiert             }
{                                                                              }
{  Diese Zeilen müssen in InitControls abgeleiteter Formulare enthalten sein:  }
{    StartFensterSchliessen;                                                   }
{    EnableMyControls(True);                                                   }
{                                                                              }
{ 12.05.2004  GD  Neu (aus FBuBMain herausgelöst)                              }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2004                                          }
{------------------------------------------------------------------------------}
unit FDMomMainDefault;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  WDialogs, Menus, ActnList, StdActns, ComCtrls, Buttons, ExtCtrls,
  FMainDef, PathIni, BuBConst, BuBDb, T_Zeit, DLL_Calls, WSysCon,
  DBuBDel, StartWin, DStaDll, DpaPanel, WTables, MeldFilterIni,
  FBubDefFenster, FBubDefAnzeige, FBuBFensterIntro, FBuBFensterOverview,
  FBuBFensterSubview, FBuBMeldungenSichten, FBuBAnzeigeSubview,
  FBuBDatenSichten, FBuBFensterLowLevelView, FBuBIntKal, FBuBExtKal,
  FBuBJournal, FBuBDiagramm, DSfGArchive, FBenutzer, PwFreigabe, CardIni, DbAka,
  GD_Utils, BuBComponents;

resourcestring
  S_StartProgram = 'Programm wird gestartet.';
  S_Tabellen = 'Tabellen werden geprüft.';
  S_Tables = 'Datenbank wird initialisiert';
  S_Intro = 'Eingangsbildschirm wird geladen.';
  S_NoData = 'Keine Archivdaten für Archivgruppe %d vorhanden !';
  S_CallData = 'Archivdaten für Archivgruppe %d holen ?';
  S_CallStaData = 'Archivdaten für Station %s holen ?';

type
  TFormMomMainDefault = class(TFormMainWindowDefault)
    miAufruf: TMenuItem;
    miParametrierung: TMenuItem;
    pnAbrufControl: TPanel;
    TimerAbrufStatus: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerAbrufStatusTimer(Sender: TObject); virtual;
  private
    { Private-Deklarationen }
    FOldTag1, FOldTag2 : integer;
    FOldAdr1, FOldAdr2 : char;
    FOldTyp1, FOldTyp2 : char;
    FTerminate         : boolean;
    FDbAka             : TDbAka;
    FDSfGAutoArchiv    : TDSfGArchivInfo;
    FBenutzerObject    : TBenutzerObject;
  protected
    { Protected-Deklarationen }
    function RemoteBuBCallBack(
      iTag: integer; cAdr, cITyp: char): boolean; virtual;
    function InsertMeldungen(iGerId: integer; pParent: TWinControl): TForm;
    function InsertBuBSubWindow(iTag: integer; pParent: TWinControl): TForm;
    function InsertBuBDataForm(
      iTag: integer; pParent: TWinControl): TForm; virtual;
    function InsertBuBDiagramForm(iTag: integer; pParent: TWinControl): TForm;

    procedure AktualisiereBuBFenster; virtual;
    procedure DeleteBuBFenster;
    function CreateBuBFenster(pClass: TComponentClass): TFormBuBDefFenster;
    function ShowBuBFenster(
      pClass: TComponentClass; iTag: integer = 0): TFormBuBDefFenster;

    procedure EnableMyControls(bState: boolean); virtual;
    procedure InitControls(bState: boolean); override;
    procedure Notification(
      AComponent: TComponent; Operation: TOperation); override;
    procedure CheckAbrufStatus; virtual;
    procedure InitAbrufPanel(bState: boolean); virtual;
    property DSfGAutoArchiv: TDSfGArchivInfo read FDSfGAutoArchiv;
    property DbAka: TDbAka read FDbAka;
    property BenutzerObject: TBenutzerObject read FBenutzerObject;
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

{-------------------------------------------------------}
procedure TFormMomMainDefault.FormClose(Sender: TObject; var Action: TCloseAction);
{-------------------------------------------------------}
begin
  FTerminate := True;
  EnableMyControls(False);

  inherited;
end;

{-------------------------------------------------------}
procedure TFormMomMainDefault.Notification(
      AComponent: TComponent; Operation: TOperation);
{-------------------------------------------------------}
begin
  inherited;

  if (not (csDestroying in ComponentState)) then begin
    if (Operation = opRemove) and (AComponent is TFormBuBDefFenster) then begin
      AktualisiereBuBFenster;
      EnableMyControls(True);
    end
    else if (Operation = opRemove) and ((AComponent is TFormBuBDatenSichten) or
            (AComponent is TFormBubIntKalibrierung) or
            (AComponent is TFormBubExtPruefgas) or
            (AComponent is TFormBuBDiagramm)) then
    begin
      EnableMyControls(True);
    end;
  end;
end;

{ En- bzw. disablen der Controls des Formulars          }
{ Parameter: T=Enablen, T=Disablen                      }
{-------------------------------------------------------}
procedure TFormMomMainDefault.EnableMyControls(bState: boolean);
{-------------------------------------------------------}

  procedure EnableThisControl(pControl: TWinControl);
  var
    i : integer;
  begin
    for i := 0 to pControl.ControlCount-1 do
      if (pControl.Controls[i] is TWinControl)
      then EnableThisControl(TWinControl(pControl.Controls[i]))
      else pControl.Controls[i].Enabled := bState;
    pControl.Enabled := bState;
  end;

var
  i : integer;
begin
  for i := pnClient.ControlCount-1 downto 0 do
    if (pnClient.Controls[i] is TFormBuBDefFenster) then
      TFormBuBDefFenster(pnClient.Controls[i]).EnableMyControls(bState);
  for i := 0 to ActionList.ActionCount-1 do
    TAction(ActionList.Actions[i]).Enabled := bState;
  for i := 0 to MainMenu.Items.Count-1 do MainMenu.Items[i].Enabled := bState;
  EnableThisControl(pnTop);
end;

{-------------------------------------------------------}
procedure TFormMomMainDefault.InitControls(bState: boolean);
{-------------------------------------------------------}
var
  s : string;
  i : integer;
begin
  if (bState) then begin
    s := Caption;
    if (Length(s) > 60) then s := Copy(Caption, 1, 60) + ' ...';
    StartFensterZeigen(s);
    StartFenster.Panel1.Caption := S_StartProgram;
  end;
  Application.ProcessMessages;

  inherited;

  if (bState) then begin

    FOldTag1 := 0;
    FOldTag2 := 0;
    FOldAdr1 := '0';
    FOldAdr2 := '0';
    FOldTyp1 := '0';
    FOldTyp2 := '0';
    FTerminate := False;

    VCDIni := TVCDIni.Create;
    VCDIni.BuBWndCall := Self.RemoteBuBCallBack;
    VCDIni.BuBMeldCall := Self.InsertMeldungen;
    VCDIni.BuBArchivCall := Self.InsertBuBDataForm;
    VCDIni.BuBDiagramCall := Self.InsertBuBDiagramForm;
    VCDIni.BuBSubFormCall := Self.InsertBuBSubWindow;
    VCDIni.BubWndMenu := Self.miAufruf;
    VCDIni.BubParamMenu := Self.miParametrierung;

    PathServer := TPathServer.Create(VCDIni.WieserIniFile, [WStammDir, WStammDb,
      DArchivDir, AutoDb, DManuDir, ManuDb, WNetWorkDir, WNetProgDir]);
    PathServer.Check;

    // Manager für Paremeterverwaltung initialisieren
    BuBAbrufManager := TBuBAbrufManager.Create;

    // Passwortgesteuerte Control-Freigabe initialisieren
    FBenutzerObject := TBenutzerObject.Create(PathServer[WStammDb]);
    CtrlFreigabe := TCtrlFreigabe.Create;

    // Tabellen von alten Leichen befreien
    with TTableExt.Create(Self) do
    try
      DatabaseName := PathServer[WStammDb];
      TableName := 'wauftrag';
      if (Exists) then EmptyTable;
      TableName := 'abrfdsfg';
      if (Exists) then EmptyTable;
      TableName := 'admanu';
      if (Exists) then EmptyTable;
      TableName := 'wzustand';
      if (Exists) then EmptyTable;
    finally
      Free;
    end;

    ClientStammdaten := TClientStammdaten.Create(PathServer[WStammDir]);

    // Diverse Datenbankobjekte initialisieren
    StartFenster.Panel1.Caption := S_Tabellen;
    CreateAllTablesFromScript;
    FDbAka := TDbAka.Create(PathServer[WStammDb]);
    TbBuBDeas := TTbBuBDeas.InitBubTable(PathServer[WStammDb]);

    // Diverse Datenbankobjekte initialisieren
    VCDIni.BuBJournal := TBuBJournalTable.Create;
    VCDIni.BuBJournal.SetNewEntry(VCDIni.CurrentUser, S_JrnBem_Start);

    FDSfGAutoArchiv := TDSfGArchivInfo.Create(PathServer[AutoDb]);
    FDSfGAutoArchiv.StammDatabase := ClientStammdaten.DStaDatabaseName;
    FDSfGAutoArchiv.Aktualisieren;

    // Passwortgesteuerte Control-Freigabe initialisieren
//    DpaPanelList.TimeSync := Self.InsertTimeSyncStatus;
    DpaPanelList.EnableMain := Self.EnableMyControls;
    DpaPanelList.DSfGAutoArchiv := Self.FDSfGAutoArchiv;

//    ShowBuBFenster(TFormBuBFensterIntro, C_AbrufGrp_Intro);
//    AktualisiereBuBFenster;
//    EnableMyControls(True);
  end
  else begin
    FTerminate := True;
    DeleteBuBFenster;
    if (Assigned(BuBAbrufManager)) then FreeAndNil(BuBAbrufManager);
    if (Assigned(FDbAka)) then FreeAndNil(FDbAka);
    if (Assigned(FDSfGAutoArchiv)) then FreeAndNil(FDSfGAutoArchiv);
    if (Assigned(TbBuBDeas)) then FreeAndNil(TbBuBDeas);
    if (Assigned(VCDIni)) then begin
      if (Assigned(VCDIni.BuBJournal)) then begin
        VCDIni.BuBJournal.SetNewEntry(VCDIni.CurrentUser, S_JrnBem_End);
        VCDIni.BuBJournal.Free;
      end;
      FreeAndNil(VCDIni);
    end;
    for i := 0 to DpaPanelList.Count-1 do DpaPanelList.Objects[i].Free;
    DpaPanelList.Clear;
    if (Assigned(ClientStammdaten)) then FreeAndNil(ClientStammdaten);
    if (Assigned(PathServer)) then FreeAndNil(PathServer);
    if (Assigned(CtrlFreigabe)) then FreeAndNil(CtrlFreigabe);
    if (Assigned(FBenutzerObject)) then FreeAndNil(FBenutzerObject);
  end;
end;

{ Aktualisiert alle BuBFenster                          }
{-------------------------------------------------------}
procedure TFormMomMainDefault.AktualisiereBuBFenster;
{-------------------------------------------------------}
var
  i : integer;
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Self.Enabled := False;
  try
    for i := pnClient.ControlCount-1 downto 0 do
      if (pnClient.Controls[i] is TFormBuBDefFenster) then
        TFormBuBDefFenster(pnClient.Controls[i]).Aktualisieren;
  finally
    Screen.Cursor := oldCursor;
    Self.Enabled := True;
  end;
end;

{ Zeigt ein BuB-Fenster an (wird ggf. erzeugt)          }
{-------------------------------------------------------}
function TFormMomMainDefault.ShowBuBFenster(
  pClass: TComponentClass; iTag: integer = 0): TFormBuBDefFenster;
{-------------------------------------------------------}
var
  i         : integer;
  oldCursor : TCursor;
  bAktu     : boolean;
begin
  Result := nil;
  bAktu := False;  // Aktualisieren erforderlich

  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Self.Enabled := False;
  try
    if (pClass = nil) then begin
      pClass := TFormBuBDefFenster;
      for i := pnClient.ControlCount-1 downto 0 do
        if (pnClient.Controls[i] is pClass) then begin
          Result := TFormBuBDefFenster(pnClient.Controls[i]);
          Result.Visible := True;
        end
        else pnClient.Controls[i].Visible := False;
      if (not Assigned(Result)) then begin
        pClass := TFormBuBFensterIntro;
        iTag := C_AbrufGrp_Intro;
        bAktu := True;
      end;
    end
    else begin
      for i := pnClient.ControlCount-1 downto 0 do
        if (pnClient.Controls[i] is pClass) and
           ((iTag = 0) or (pnClient.Controls[i].Tag = iTag)) then
        begin
          Result := TFormBuBDefFenster(pnClient.Controls[i]);
          Result.Visible := True;
        end
        else pnClient.Controls[i].Visible := False;
    end;

    if (not Assigned(Result)) then Result := CreateBuBFenster(pClass);
    Result.Tag := iTag;
    if (bAktu) then Result.Aktualisieren;
    BuBAbrufManager.FormProperties.SetFormProperties(Result);
    if ((Result.Tag div 100) in [2..6]) then begin
      FOldTag2 := FOldTag1;
      FOldTag1 := Result.Tag;
    end;
    pnTopRest.Caption := Result.Caption;
  finally
    Screen.Cursor := oldCursor;
    Self.Enabled := True;
  end;
end;

{-------------------------------------------------------}
function TFormMomMainDefault.CreateBuBFenster(
  pClass: TComponentClass): TFormBuBDefFenster;
{-------------------------------------------------------}
begin
  Result := TFormBuBDefFenster(pClass.Create(Self));
  with Result do begin
    Parent := Self.pnClient;
    Align := alClient;
    BorderStyle := bsNone;
    Show;
  end;
end;

{ Löscht alle BuBFenster                                }
{-------------------------------------------------------}
procedure TFormMomMainDefault.DeleteBuBFenster;
{-------------------------------------------------------}
var
  i, iCount : integer;
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Self.Enabled := False;
  try
    iCount := pnClient.ControlCount;  // Anzahl der nicht zu schliessenden Fenster
    for i := pnClient.ControlCount-1 downto 0 do
      if (pnClient.Controls[i] is TFormBuBDefFenster) then begin
        TForm(pnClient.Controls[i]).Release;
        Dec(iCount);
      end;
    while (pnClient.ControlCount > iCount) do Delay(1);
  finally
    Screen.Cursor := oldCursor;
    Self.Enabled := True;
  end;
end;

{ CallBack: Aufruf von BuB-Fenstern                     }
{ Parameter: Übergabeparameter zur Initialisierung      }
{ Rückgabe: Fenster soll sich Releasen                  }
{-------------------------------------------------------}
function TFormMomMainDefault.RemoteBuBCallBack(
  iTag: integer; cAdr, cITyp: char): boolean;
{-------------------------------------------------------}
var
  pClass : TComponentClass;
  pForm  : TForm;
begin
  EnableMyControls(False);
  pClass := nil;
  Result := False;  // Aufrufendes Fenster soll sich nicht freigeben
  case (iTag div 100) of
    1 : aHelpContentsExecute(nil);          // Hilfe
    2 : begin                               // Eingangsbild
          pClass := TFormBuBFensterIntro;
          FOldAdr2 := FOldAdr1;
          FOldTyp2 := FOldTyp1;
          FOldAdr1 := cAdr;
          FOldTyp1 := cITyp;
          Result := True;
        end;
    3 : begin                               // Overview
          pClass := TFormBuBFensterOverview;
          FOldAdr2 := FOldAdr1;
          FOldTyp2 := FOldTyp1;
          FOldAdr1 := cAdr;
          FOldTyp1 := cITyp;
          Result := True;
        end;
    4 : begin                               // Subview
          pClass := TFormBuBFensterSubview;
          FOldAdr2 := FOldAdr1;
          FOldTyp2 := FOldTyp1;
          FOldAdr1 := cAdr;
          FOldTyp1 := cITyp;
          Result := True;
        end;
    5 : begin                               // Low-Level-View
          pClass := TFormBuBLowLevelView;
          FOldAdr2 := FOldAdr1;
          FOldTyp2 := FOldTyp1;
          FOldAdr1 := cAdr;
          FOldTyp1 := cITyp;
          Result := True;
        end;
    10 : begin                               // Funktionen
          case (iTag mod 1000) of
            1 : if (FOldTag2 > 0) and (FOldTag2 <> FOldTag1) then
                  Result := RemoteBuBCallBack(FOldTag2, FOldAdr2, FOldTyp2);
            else ShowMessage('Funktion noch nicht implementiert.');
          end;
        end;
    else ShowMessage('Funktion noch nicht implementiert.');
  end;

  if (Assigned(pClass)) then begin
    pForm := ShowBuBFenster(pClass, iTag);
    if (Result) and (pForm is TFormBuBDefFenster)
    then EnableMyControls(False)
    else EnableMyControls(True);
    if ((iTag div 100) = 4) then TFormBuBFensterSubview(pForm).Gruppe := iTag;
  end
  else EnableMyControls(True);
end;

{------------------------------------------------------}
function TFormMomMainDefault.InsertMeldungen(
  iGerId: integer; pParent: TWinControl): TForm;
{------------------------------------------------------}
begin
  // Meldungsfilter einstellen
  with TMeldFilterIniFile.Create(ChangeFileExt(ParamStr(0), '.INI')) do
  try
    FilterMRG := False;
    FilterDSfG := True;
    FeldQuittiert := True;
    FeldDatumZeit := True;
    FeldMeldung := True;
    FilterAbrufart := 0;
    if (pParent <> pnClient) then begin
      FeldZeitzone := False;
      FeldGeraeteArt := True;
      FeldStation := False;
      FeldOrdNr := False;
      FeldMNrGeraet := False;
      FeldMNrAllg := False;
      FeldDSfGStatus := False;
      FeldCRC := False;
      FeldBemerkung := False;
    end
    else begin
      FeldZeitzone := True;
      FeldGeraeteArt := True;
      FeldStation := True;
      FeldOrdNr := False;
      FeldMNrGeraet := False;
      FeldMNrAllg := False;
      FeldDSfGStatus := True;
      FeldCRC := False;
      FeldBemerkung := True;
    end;
  finally
    Free;
  end;

  // Neues Meldungsfenster initialisieren
  Result := TFormBuBMeldungenSichten.Create(pParent, PathServer[WStammDir],
    ChangeFileExt(ParamStr(0), '.INI'), C_GerArtDSfG, iGerId, False);
  with Result do begin
    Parent := pParent;
    Align := alClient;
    Show;
  end;
end;

{------------------------------------------------------}
function TFormMomMainDefault.InsertBuBSubWindow(
  iTag: integer; pParent: TWinControl): TForm;
{------------------------------------------------------}
begin
  // Neues Unterfenster zum Einbinden initialisieren
  Result := TFormBubAnzeigeSubview.Create(pParent);

  with TFormBubAnzeigeSubview(Result) do begin
    Parent := pParent;
    Align := alClient;
    Gruppe := iTag;
    Aktualisieren;
    BuBAbrufManager.FormProperties.SetFormProperties(Result);
    Show;
  end;
end;

{------------------------------------------------------}
function TFormMomMainDefault.InsertBuBDataForm(
  iTag: integer; pParent: TWinControl): TForm;
{------------------------------------------------------}
var
  iInstId, iAgNr : integer;
  i, iIx, iStaId : integer;
  oldCursor      : TCursor;
  pArchiv        : TArchivList;
begin
  Result := nil;  // Default

  iInstId := iTag div 100;
  iAgNr := iTag mod 100;
  iStaId := -1;

  if (iInstId > 0) and (iAgNr > 0) then begin

    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try

      EnableMyControls(False);

      // Daten vorhanden ?
      FDSfGAutoArchiv.AktualisiereArchiv(iInstId, iAgNr);
      pArchiv := FDSfGAutoArchiv.GetArchiv(iInstId, iAgNr);
      // Wenn nein: Daten holen ?
      if ((not Assigned(pArchiv)) or (pArchiv.DatenVon <= 1)) and
         (DpaPanelList.Count > 0) then
      begin
        if (MessageDlg(
          Format(S_CallData, [iAgNr]), mtInformation, [mbYes, mbNo], 0) = mrYes)
        then begin
          with TQueryExt.Create(Self) do
          try
            DatabaseName := PathServer[WStammDb];
            Sql.Add('SELECT ' + C_DTF_Instanz_StationId);
            Sql.Add('FROM ' + ChangeFileExt(C_DTB_Instanz, ''));
            Sql.Add('WHERE ' + C_DTF_Instanz_InstanzId + ' = ' +
              IntToStr(iInstId));
            Open;
            if (not Eof)
            then iStaId := FieldByName(C_DTF_Instanz_StationId).asInteger
            else raise Exception.Create('unknown instance !');  // darf nicht vorkommen !!!
            Close;
          finally
            Free;
          end;

          iIx := -1;  // Index der zuständigen Verbindung
          for i := 0 to DpaPanelList.Count-1 do
            if (TDpaPanel(DpaPanelList.Objects[i]).StationsId = iStaId) then
            begin
              iIx := i;
              Break;
            end;
          if (iIx = -1) then iIx := iStaId;

          TDpaPanel(DpaPanelList.Objects[iIx]).AutoArchivAbruf(
            iInstId, iAgNr, 0, True);
          Sleep(1000);
          Application.ProcessMessages;
          FDSfGAutoArchiv.AktualisiereArchiv(iInstId, iAgNr);
          pArchiv := FDSfGAutoArchiv.GetArchiv(iInstId, iAgNr);
        end;
      end;

      // Neues Unterfenster zum Einbinden initialisieren
      if (Assigned(pArchiv)) and (pArchiv.DatenVon > 1) then begin

        Result := TFormBuBDatenSichten.Create(Self);
        if (not Assigned(pParent)) then pParent := Self.pnClient;
        with TFormBuBDatenSichten(Result) do begin
          Tag := iTag;
          Parent := pParent;
          Align := alClient;
          ShowBubAbrufDaten(FDSfGAutoArchiv, FDbAka, iInstId, iAgNr);
          Show;
        end;

      end
      else begin
        MessageDlg(Format(S_NoData, [iAgNr]), mtInformation, [mbOk], 0);
        EnableMyControls(True);
      end;

    finally
      Screen.Cursor := oldCursor;
    end;

  end;
end;

{------------------------------------------------------}
function TFormMomMainDefault.InsertBuBDiagramForm(
  iTag: integer; pParent: TWinControl): TForm;
{------------------------------------------------------}
var
  iGrp, iKng : integer;
  oldCursor  : TCursor;
  s          : string;
begin
  Result := nil;  // Default

  iGrp := iTag div 100;
  iKng := iTag mod 100;
  s := BuBAbrufManager.FormProperties.GetPropertyString(
    C_Tk_BuBFormProps_Diagr, iGrp, iKng);

  if (s <> '') then begin

    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try

      EnableMyControls(False);

      Result := TFormBuBDiagramm.Create(Self);
      if (not Assigned(pParent)) then pParent := Self.pnClient;
      with TFormBuBDiagramm(Result) do begin
        Tag := iTag;
        Parent := pParent;
        Align := alClient;
        InsertNewDataChart(FDSfGAutoArchiv, PathServer[AutoDb], s);
        Show;
      end;

    finally
      Screen.Cursor := oldCursor;
    end;

  end;
end;

{ Zeigt den Status der Abrufmodule an                   }
{-------------------------------------------------------}
procedure TFormMomMainDefault.CheckAbrufStatus;
{-------------------------------------------------------}
const
  C_TimeOut = 30000;
var
  i : integer;
  s : string;
begin
  for i := pnAbrufControl.ControlCount-1 downto 0 do
    with TBuBBulbPanel(pnAbrufControl.Controls[i]) do begin
      if (FTerminate) then Break;
      s := DpaPanelList.DpaPanel[Tag].GetAbrufStatus;
      Text[2] := s;
    end;
end;

{-------------------------------------------------------}
procedure TFormMomMainDefault.TimerAbrufStatusTimer(Sender: TObject);
{-------------------------------------------------------}
const
  iCounter : Cardinal = 0;
var
  i : integer;
begin
  try
    TimerAbrufStatus.Enabled := False;
    try
      // Prüfen, ob Abrufmodule aktiv sind
      CheckAbrufStatus;

      // "Halten"-Eintrag für Abrufmodule schreiben
      if (iCounter = 0) or (GetTickCount < iCounter) then iCounter := GetTickCount;
      if (GetTickCount >= iCounter + 30000) then begin
        for i := 0 to DpaPanelList.Count-1 do
          TDPAPanel(DpaPanelList.Objects[i]).WriteStationToAfDSfGAbruf(C_AbrArtMomHalten);
        iCounter := GetTickCount;
      end;
    finally
      TimerAbrufStatus.Enabled := (not FTerminate);
    end;
  except
    on E:Exception do
      StringToFile('TimerAbrufStatusTimer (' + DateTimeToStr(now) + '): ' +
        E.Message, ChangeFileExt(ParamStr(0), '.ERR'), False);
  end;
end;

{ Initialisieren/Freigeben des Abrufpanels              }
{-------------------------------------------------------}
procedure TFormMomMainDefault.InitAbrufPanel(bState: boolean);
{-------------------------------------------------------}
var
  i, iLeft : integer;
begin
  if (bState) then begin
    iLeft := 1;
    for i := 0 to DpaPanelList.Count-1 do begin
      // Abrufkontrolle
      with TBuBBulbPanel.Create(pnAbrufControl) do begin
        Parent := Self.pnAbrufControl;
        Left := iLeft;
        Width := 35;
        Top := 1;
        Height := Self.pnAbrufControl.Height-2;
        Tag := StrToInt(DpaPanelList[i]);
        Text[1] := 'COM ' + IntToStr(Tag);
        Text[2] := 'clYellow';
      end;
      iLeft := iLeft + 36;
    end;
    pnAbrufControl.Width := iLeft;
  end
  else begin
    for i := pnAbrufControl.ControlCount-1 downto 0 do
      pnAbrufControl.Controls[i].Free;
  end;
end;

end.
