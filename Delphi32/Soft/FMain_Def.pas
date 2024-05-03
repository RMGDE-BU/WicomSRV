// -----------------------------------------------------------------------------
// Unit: Fenster für die allgemeine Oberfläche aller Hauptmodule 
//
// 20.01.2010  NW  Vs. 1.0  Neu
// 26.07.2013  GD  Vs. 1.1  Änderung in Help-File-Aufruf
// 09.02.2017  WW           Designänderungen am Image-Panel: neues RMG-Logo ohne
//                          Text, obere und untere Linie entfallen, Hintergrund-
//                          farbe weiß statt grau
// 17.05.2021  WW           neues RMG-Logo CI 2020
// 14.11.2022  WW           WriteLogFileApplication mit Log-Type
//
// Copyright © RMG Messtechnik GmbH 2010, 2022
// -----------------------------------------------------------------------------
unit FMain_Def;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ActnList, XPStyleActnCtrls, ActnMan, WDialogs, LogFile,
  ExtCtrls, ActnCtrls, ActnMenus, ComCtrls, ToolWin,
  Startwin, IniMain_Def, Menus, StdStyleActnCtrls, Buttons,
  StdCtrls, StdActns, ShellApi, jpeg;

type
  TFMainDesign = class(TForm)
    pnBtn: TPanel;
    pnTop: TPanel;
    pnImg_Right: TPanel;
    Img_Right: TImage;
    pnImg_Center: TPanel;
    Img_Center: TImage;
    pnImg_Bottom: TPanel;
    Img_Bottom: TImage;
    pnImg: TPanel;
    DialogInfo: TDialogInfo;
    ImageList: TImageList;
    bvTrenner: TBevel;
    mMain: TMainMenu;
    miProgramm: TMenuItem;
    miHilfe: TMenuItem;
    miBeenden: TMenuItem;
    miInfo: TMenuItem;
    ActionList: TActionList;
    aHelpContents: THelpContents;
    aHelpInfo: TAction;
    aClose: TAction;
    miInhalt: TMenuItem;
    miTrennerHelp: TMenuItem;
    pnImg_Top: TPanel;
    Img_Top: TImage;
    StatusBar: TStatusBar;
    pnBottom: TPanel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure aHelpInfoExecute(Sender: TObject);
    procedure aHelpContentsExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure InitImages;
  protected
    { Protected-Deklarationen }
    procedure CopyImages(aImageList: TImageList);
    procedure DisplayHint(Sender: TObject); virtual;
    procedure InitComponents(bState: boolean); virtual;
    procedure InitButtons;
    procedure WriteLogFileApplication(s, sExt: String; bHeader: Boolean = True;
      ALogType: TLogType = lt_Info);
  public
    { Public-Deklarationen }
  end;

var
  FMainDesign: TFMainDesign;

const
  C_LogExt_Ereignis = '.log';
  C_LogExt_Error    = '_ERR.log';
  C_LogExt_Debug    = '_DEBUG.log';

implementation

{$R *.dfm}

resourcestring
  S_NotAvailable = 'Online-Hilfe nicht verfügbar !';
  S_Empty = '';

(******************************************************************************)
(*                               Programm-Start                               *)
(******************************************************************************)

// -----------------------------------------------------------------------------
procedure TFMainDesign.FormCreate(Sender: TObject);
// -----------------------------------------------------------------------------
begin
  InitComponents(True);
end;

// -----------------------------------------------------------------------------
procedure TFMainDesign.FormDestroy(Sender: TObject);
// -----------------------------------------------------------------------------
begin
  InitComponents(False);
end;

// -----------------------------------------------------------------------------
// Startfenster 1 Sekunde anzeigen, bis MainForm sichtbar wird
// -----------------------------------------------------------------------------
procedure TFMainDesign.FormShow(Sender: TObject);
// -----------------------------------------------------------------------------
begin
  StartFensterSchliessen;
end;

(******************************************************************************)
(*                                 Funktionen                                 *)
(******************************************************************************)

// -----------------------------------------------------------------------------
// alle Bilder der ImageList in 2. ImageListe kopieren
// -----------------------------------------------------------------------------
procedure TFMainDesign.CopyImages(aImageList: TImageList);
// -----------------------------------------------------------------------------
var
  i: Integer;
begin
  aImageList.Clear;
  for i:=0 to ImageList.Count-1 do
  begin
    aImageList.AddImage(ImageList, i);
  end;
end;

// -----------------------------------------------------------------------------
procedure TFMainDesign.DisplayHint(Sender: TObject);
// -----------------------------------------------------------------------------
begin
  StatusBar.SimplePanel := True;
  StatusBar.SimpleText  := GetLongHint(Application.Hint);
end;

// -----------------------------------------------------------------------------
// Initialisieren bzw. Freigeben der Komponenten
// Parameter: T=Initialisieren, T=Freigeben
// -----------------------------------------------------------------------------
procedure TFMainDesign.InitComponents(bState: boolean);
// -----------------------------------------------------------------------------
begin
  if (bState) then begin
//    Application.OnHint := DisplayHint;

    if (Application.HelpFile = '') then begin
       if (FileExists(ChangeFileExt(ParamStr(0), '.HLP'))) then
         Application.HelpFile := ChangeFileExt(ParamStr(0), '.HLP')
       else if (FileExists(ChangeFileExt(ParamStr(0), '.PDF'))) then
         Application.HelpFile := ChangeFileExt(ParamStr(0), '.PDF');
    end;
    InitImages;
  end
  else begin
  end;
end;

// -----------------------------------------------------------------------------
// Image-Eigenschaften aus der Wieser.ini lesen und Bilder anpassen                   
// -----------------------------------------------------------------------------
procedure TFMainDesign.InitImages;
// -----------------------------------------------------------------------------
var
  pImgData, pImgData_Center, pImgData_Right, pImgData_Top, pImgData_Bottom: TImgData;
  i, iHeightTop : Integer;
  pPanel : TPanel;
  pImage : TImage;
begin
  // INI-Datei wird gelesen, Informationen abspeichern
  with TGUIIni.Create(false, false) do
  try
    pnImg.Color        := PnImg_Color;
    pnImg_Bottom.Color := pnImg.Color;
    pnImg_Center.Color := pnImg.Color;
    pnImg_Right.Color  := pnImg.Color;
    pnImg_Top.Color    := pnImg.Color;

    // Panel-Align = alNone
    pnImg_Right.Align  := alNone;
    pnImg_Center.Align := alNone;

    pImgData_Center := ImgData_Center;  // Bild mittig neben Buttons (Img_Center)
    pImgData_Right := ImgData_Right;    // Bild rechts oben (Img_Right)
    pImgData_Top := ImgData_Top;        // obere Trennlinie (Img_Top)
    pImgData_Bottom := ImgData_Bottom;  // untere Trennlinie (Img_Bottom)
  finally
    Free;
  end;

  // wenn mind. 1 Bild definiert wurde -> es also eine Definition gibt
  if FileExists(pImgData_Center.sPath) or FileExists(pImgData_Right.sPath) or
     FileExists(pImgData_Top.sPath) or FileExists(pImgData_Bottom.sPath) then
  begin
    // Anzeige der Bilder
    for i := 1 to 4 do
    begin
      case i of
        1: begin  // Anzeige Img_Center: Bild mittig neben Buttons
          pPanel := pnImg_Center;
          pImage := Img_Center;
          pImgData := pImgData_Center;
        end;
        2: begin  // Anzeige Img_Right: Bild rechts
          pPanel := pnImg_Right;
          pImage := Img_Right;
          pImgData := pImgData_Right;
        end;
        3: begin  // Anzeige Img_Top: Bild als untere Trennlinie
          pPanel := pnImg_Top;
          pImage := Img_Top;
          pImgData := pImgData_Top;
        end;
        else begin  // Anzeige Img_Bottom: Bild als untere Trennlinie
          pPanel := pnImg_Bottom;
          pImage := Img_Bottom;
          pImgData := pImgData_Bottom;
        end;
      end;

      with pImgData do
      begin
        pImage.Align := alNone;
        if FileExists(sPath) then
        begin
          // Bild laden
          pPanel.Visible := True;
          pImage.Picture.LoadFromFile(sPath);
          // Bildeigenschaften
          pImage.Stretch      := bStretch;
          pImage.Transparent  := bTransparent;
          pImage.Proportional := bProportional;
          // Bildgröße
          if (iWidth <> -1) then pImage.Width := iWidth;
          if (iHeight <> -1) then pImage.Height := iHeight;
          pPanel.Width  := pImage.Width;
          pPanel.Height := pImage.Height;
        end else
        begin
          pPanel.Visible := False;
          pPanel.Height  := 0;
          pPanel.Width   := 0;
        end;
        pImage.Align := alClient;
      end;
    end;
  end;

  // Panel-Größe der übergeordneten Panel
  if pnImg_Right.Height > pnImg_Center.Height then
    iHeightTop := 2 + pnImg_Right.Height
  else
    iHeightTop := 2 + pnImg_Center.Height;
  if pnImg_Top.Visible then
    iHeightTop := iHeightTop + pnImg_Top.Height;
  if pnImg_Bottom.Visible then
    iHeightTop := iHeightTop + pnImg_Bottom.Height;
  // 28.11.2011  WN
  if (iHeightTop = 0) then iHeightTop := 70;
  pnTop.Height := iHeightTop;
  // Panel-Align der übergeordneten Panel
  pnImg_Right.Align  := alRight;
  pnImg_Center.Align := alClient;
end;

{ alle Buttons in pnBtn anpassen                                           }
{ -> Caption mit Zeilenumbruch, Höhe abh. von Höhe pnButtons                   }
{------------------------------------------------------------------------------}
procedure TFMainDesign.InitButtons;
{------------------------------------------------------------------------------}
var
  i, j, iWidth : Integer;
  pSpBtn : TSpeedButton;
  pPanel: TPanel;
begin
  iWidth := 2;
  for i:=0 to pnBtn.ControlCount-1 do
  begin
    if pnBtn.Controls[i] is TPanel then
    begin
      pPanel := TPanel(pnBtn.Controls[i]);
      for j:=0 to pPanel.ControlCount-1 do
      begin
        if pPanel.Controls[j] is TSpeedButton then
        begin
          pPanel.Width := Round(pPanel.Height * 1.5);   // Verhältnis Btn 1:1,5 (Höhe : Breite)
          pSpBtn := TSpeedButton(pPanel.Controls[j]);
          pSpBtn.ShowHint := True;  // aktivierter Hint
          pSpBtn.Height := pPanel.Height - 4;
          pSpBtn.Width  := pPanel.Width - 4;
          pSpBtn.Top  := 2;
          pSpBtn.Left := 2;
          pSpBtn.Flat := True;
          pSpBtn.Spacing := Round(pPanel.Height / 10);  // Abstand Text - Icon
          pSpBtn.Caption := StringReplace(pSpBtn.Caption, ' ', #13#10, []);  // max. 2 Zeilen Text
//          pSpBtn.Caption := StringReplace(pSpBtn.Caption, ' ', #13#10, [rfReplaceAll]);
        end;
      end;
      iWidth := iWidth + pPanel.Width;
    end;
  end;
  pnBtn.Width := iWidth;
end;

// -----------------------------------------------------------------------------
// Eintrag in programmbezogenes Logfile schreiben
// Parameter: der zu schreibende Text
//            Extension des Logfiles oder kompletter Filename
//            Log-Type
// -----------------------------------------------------------------------------
procedure TFMainDesign.WriteLogFileApplication(s, sExt: String;
  bHeader: Boolean = True; ALogType: TLogType = lt_Info);
// -----------------------------------------------------------------------------
var
  sFileName, sPfadName: String;
begin
  // Prüfung, ob in sExt ein Filename angegeben wurde
  sFileName := ExtractFileName(sExt);
  sPfadName := ExtractFileDir(sExt);
  if (sPfadName = '') then
  begin
    // wenn Ereignis-Logfile geschrieben werden soll
    sFileName := ExpandFileName(Application.ExeName);
    sPfadName := ExtractFileDir(sFileName);
    sFileName := ChangeFileExt (ExtractFileName (sFileName), sExt);
  end;
  sPfadName := sPfadName + '\';
  with TCustomLogFile.Create (sPfadName, sFileName, False) do
  try
    Write (s, bHeader, ALogType);  // String in Log-File schreiben (mit Header ?)
                                   // mit LogType; 14.11.2022, WW
  finally
    Free;
  end;
  Application.ProcessMessages;
end;

(******************************************************************************)
(*                                  Aktionen                                  *)
(******************************************************************************)

// -----------------------------------------------------------------------------
// Programm beenden
// -----------------------------------------------------------------------------
procedure TFMainDesign.aCloseExecute(Sender: TObject);
// -----------------------------------------------------------------------------
begin
  Close;
end;

// -----------------------------------------------------------------------------
// Info-Dialog anzeigen
// -----------------------------------------------------------------------------
procedure TFMainDesign.aHelpInfoExecute(Sender: TObject);
// -----------------------------------------------------------------------------
begin
  DialogInfo.Execute;
end;

// -----------------------------------------------------------------------------
// Hilfe anzeigen
// -----------------------------------------------------------------------------
procedure TFMainDesign.aHelpContentsExecute(Sender: TObject);
// -----------------------------------------------------------------------------
begin
  if (FileExists(Application.HelpFile)) then begin   // 26.07.2013
    if (UpperCase(ExtractFileExt(ParamStr(0))) = '.HLP')
    then Application.HelpContext(1)
    else ShellExecute(Application.Handle, 'open', pChar(Application.HelpFile),
      nil, nil, SW_ShowNormal);
  end
  else MessageDlg(S_NotAvailable, mtInformation, [mbOk], 0);
end;

end.
