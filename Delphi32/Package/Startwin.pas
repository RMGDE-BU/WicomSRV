{------------------------------------------------------------------------------}
{ Startfenster für Wieser-Aplikationen                                         }
{                                                                              }
{ 02.08.2004  GD  Position des Labels geändert                                 }
{ 06.11.2007  WW; RMG Corporate Design                                         }
{ 23.04.2010  WN; Honeywell Corporate Design                                   }
{ 25.05.2011  GD; IFEDEF für Kompatibilität mit MrgDialog                      }
{ 30.12.2011  WW; Neue E-Mailadresse software.messtechnik@honeywell.com; Inter-}
{                 netadresse wieder www.rmg.com                                }
{ 15.09.2014  WN; Neue Adresse Standort Zorneding                              }
{ 03.02.2017  WW  mit neuem RMG-Logo (nur 1 Logo, kein Logo-Wechsel mehr);     }
{                 Neue E-Mailadresse software@rmg.com; Hintergundbild aktual.  }
{ 10.04.2017  WW  mit neuem RMG-Software-Bild                                  }
{ 11.07.2018  WW  statisches Version-Label eliminiert; DEFAULT_CHARSET statt   }
{                 ANSI_CHARSET                                                 }
{ 17.05.2021  WW  Neues RMG-Software-Bild und -Logo (CI 2020)                  }
{ 01.09.2022  WW  Neue Adresse und Telefonnummer Standort Grasbrunn            }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 1995, RMG Messtechnik GmbH 2022               }
{------------------------------------------------------------------------------}
unit Startwin;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, ShellAPI,
  Forms, Dialogs, StdCtrls, ExtCtrls, jpeg, WGraphics, IniMain_Def, IniFiles;
                                                                                 
type
  TSendMail = procedure (var sBetreff: String; var sText: String) of object;

  TStartFenster = class(TForm)
    imgeRMG: TImage;
    pnHPS: TPanel;
    bvHPS: TBevel;
    imgeWico22: TImage;
    imgeRMGInvers: TImage;
    TimerLiveImageStart: TTimer;
    pnMessage: TPanel;
    lMessage: TLabel;
    pnTop: TPanel;
    ImgTop: TImage;
    pnProgramm: TPanel;
    lProgrammName: TLabel;
    lRMG: TLabel;
    lStr: TLabel;
    lStadt: TLabel;
    lTel: TLabel;
    lTelNr: TLabel;
    lInternet: TLabel;
    lHttp: TLabel;
    lMail: TLabel;
    LEMail: TLabel;
    Panel1: TPanel;
    lVersion: TLabel;
    lExeDatum: TLabel;
    lCopyRight: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerLiveImageStartTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lHttpClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure LEMailClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FRectRMG  : TRect;
    FRectImgRMG  : TRect;
    FRectRMGInvers : TRect;
    FRectImgRMGInvers  : TRect;
    FRectWico22 : TRect;
    FRectImgWico22 : TRect;
    FBmRMG    : TBitmap;
    FBmRMGInvers   : TBitmap;
    FBmWico22   : TBitmap;
    FRunDir     : byte;
    FFirstShow  : boolean;
    FStopLive   : boolean;
    FLiveFlag   : byte;
    FStartBlend : SmallInt;
    FStarted    : boolean;
    FUpDownChange : boolean;
    FProgName   : String;
    FVersion    : String;
    FExeDate    : String;
    FCopyRight  : String;
    FBevelOn    : Boolean;
    FTimerLiveImg : Boolean;
    FOnImageClick : TNotifyEvent;
    FSendMail     : TSendMail;
    procedure InitBitmaps(bState: boolean);
    procedure BuildLiveImage;
    procedure LoadImagesFromIni;
    procedure SetProgName(s: String);
    procedure SetVersion(s: String);
    procedure SetExeDate(s: String);
    procedure SetCopyRight(s: String);
    procedure SetBevel(b: Boolean);
    procedure SetTimerLiveImg(b: Boolean);
    function GetHeightGesamt: Integer;
  protected
    procedure ShowLizenzInfos(bVisible: Boolean);
  public
    { Public-Deklarationen }
    property ProgName: String read FProgName write SetProgName;
    property Version: String read FVersion write SetVersion;
    property ExeDate: String read FExeDate write SetExeDate;
    property CopyRight: String read FCopyRight write SetCopyRight;
    property BevelOn: Boolean read FBevelOn write SetBevel;
    property TimerLiveImg: Boolean read FTimerLiveImg write SetTimerLiveImg;
    property OnImageClick : TNotifyEvent write FOnImageClick;
    property SendMail: TSendMail read FSendMail write FSendMail;
    property GesamtHeight: Integer read GetHeightGesamt;
  end;

procedure StartFensterZeigen(Beschriftung: string;
                             bEnableTimer: boolean = True);
procedure StartFensterSchliessen;

var
  StartFenster: TStartFenster;

implementation

{$R *.DFM}

resourcestring
  S_Version = 'Version';


{--------------------------- Allgemeine Funktionen ----------------------------}

{--------------------------------------------}
procedure StartFensterZeigen(Beschriftung: string; bEnableTimer: boolean = True);
{--------------------------------------------}
begin
  StartFenster := TStartFenster.Create(nil);
  StartFenster.TimerLiveImg := false; // bEnableTimer; Logo-Wechsel deaktiviert; 03.02.2017, WW
  StartFenster.lProgrammName.Caption := Beschriftung;
  StartFenster.Show;
  Application.ProcessMessages;
end;

{--------------------------------------------}
procedure StartFensterSchliessen;
{--------------------------------------------}
begin
  if (Assigned(StartFenster)) then StartFenster.Close;
end;

{------------------------------- TStartFenster --------------------------------}

{--------------------------------------------}
procedure TStartFenster.FormCreate(Sender: TObject);
{--------------------------------------------}
var
  i: Integer;
  Component: TComponent;  // 01.04.2010  WN
begin
  FProgName  := '';
  FVersion   := '';
  FExeDate   := '';
  FCopyRight := '';
  FBevelOn      := True;
  FTimerLiveImg := False;
  FOnImageClick := nil;
  FSendMail     := nil;

  ShowLizenzInfos(False);

  Screen.Cursor := crHourglass;
  lHttp.Color := C_ColorRMG;
  LEMail.Font.Color:=C_ColorRMG;

  FFirstShow := True;
  FLiveFlag := 1;
  FStartBlend := 0;
  FStarted := True;
  FUpDownChange := False;
  TimerLiveImg  := False; // True;  Logo-Wechsel deaktiviert; 03.02.2017, WW

  // 01.04.2010  WN
  for i := 0 to ComponentCount-1 do  // Schriftfarbe aller Labels Schwarz
  begin
    Component := Components[i];
    if (Component is TLabel) then
    begin
      TLabel(Component).Font.Color := clBlack;
    end;
  end;

  // RMG-Schriftfarbe für Links
  lHttp.Font.Color:=C_ColorRMG;   // 01.04.2010  WN
  LEMail.Font.Color:=C_ColorRMG;  // 01.04.2010  WN

  LoadImagesFromIni;  // 03.02.2017, WW
end;

{--------------------------------------------}
procedure TStartFenster.FormDestroy(Sender: TObject);
{--------------------------------------------}
begin
  InitBitmaps(False);
  Screen.Cursor := crDefault;  // aus FormClose verlagert; 03.02.2017, WW
end;

{--------------------------------------------}
procedure TStartFenster.FormClose(Sender: TObject; var Action: TCloseAction);
{--------------------------------------------}
begin
  Action := caFree;
  StartFenster := nil;
end;

{--------------------------------------------}
procedure TStartFenster.InitBitmaps(bState: boolean);
{--------------------------------------------}
var
  iFormat  : word;
  iData    : THandle;
  pPalette : HPalette;
  Cursor_Save: TCursor;

begin
  if (bState) then begin
    Cursor_Save:=Screen.Cursor;
    Screen.Cursor:=crHourGlass;      
    try
      FBmRMG := TBitmap.Create;
      imgeRMG.Picture.SaveToClipboardFormat(iFormat, iData, pPalette);
      FBmRMG.LoadFromClipboardFormat(iFormat, iData, pPalette);
      FRectRMG := Rect(0, 0, FBmRMG.Width, FBmRMG.Height);
      FRectImgRMG :=
        CalculateFittingRect(imgeRMG.ClientRect, FRectRMG, 0);
      imgeRMG.Picture := nil;

      FBmRMGInvers := TBitmap.Create;
      imgeRMGInvers.Picture.SaveToClipboardFormat(iFormat, iData, pPalette);
      FBmRMGInvers.LoadFromClipboardFormat(iFormat, iData, pPalette);
      FRectRMGInvers := Rect(0, 0, FBmRMGInvers.Width, FBmRMGInvers.Height);
      FRectImgRMGInvers :=
        CalculateFittingRect(imgeRMG.ClientRect, FRectRMGInvers, 0);

      FBmWico22 := TBitmap.Create;
      imgeWico22.Picture.SaveToClipboardFormat(iFormat, iData, pPalette);
      FBmWico22.LoadFromClipboardFormat(iFormat, iData, pPalette);
      FRectWico22 := Rect(0, 0, FBmWico22.Width, FBmWico22.Height);
      FRectImgWico22 :=
        CalculateFittingRect(imgeRMG.ClientRect, FRectWico22, 0);
      imgeWico22.Picture := nil;

      FRunDir := 1;
      FStopLive := False;
    finally
      Screen.Cursor:=Cursor_Save;
    end;
  end
  else begin
    FreeAndNil(FBmRMG);
    FreeAndNil(FBmRMGInvers);
    FreeAndNil(FBmWico22);
  end;
end;

{--------------------------------------------}
procedure TStartFenster.BuildLiveImage;
{--------------------------------------------}
const
  C_Step = 5;
var
  FUpRectBm, FDownRectBm : TRect;
  FUpRectIe, FDownRectIe : TRect;
  FUpBitmap, FDownBitmap : TBitmap;
  iBlend    : SmallInt;
begin
  if (ModalResult = mrNone) and (not FStopLive) and
    (not (csDestroying in Self.ComponentState)) and
    (Assigned(FBmRMG)) and (Assigned(FBmRMGInvers)) and (Assigned(FBmWico22))
  then begin
    if (FStopLive) then Exit;
    if (FRunDir = 1) and ((FStartBlend+C_Step) >= 250) then begin
      FRunDir := 0;
      Inc(FLiveFlag);
    end
    else if (FRunDir <> 1) and ((FStartBlend-C_Step) <= 0) then begin
      FRunDir := 1;
      Inc(FLiveFlag);
    end;
    if (FLiveFlag >= 4) then begin
      FLiveFlag := 1;
      FUpDownChange := (not FUpDownChange);
    end;
    if (FRunDir = 1) then Inc(FStartBlend, C_Step) else Dec(FStartBlend, C_Step);
    case FLiveFlag of
      1 : begin
            FDownRectBm := FRectRMG;
            FUpRectBm := FRectRMGInvers;
            FDownRectIe := FRectImgRMG;
            FUpRectIe := FRectImgRMGInvers;
            FDownBitmap := FBmRMG;
            FUpBitmap := FBmRMGInvers;
          end;
      2 : begin
            FDownRectBm := FRectWico22;
            FUpRectBm := FRectRMGInvers;
            FDownRectIe := FRectImgWico22;
            FUpRectIe := FRectImgRMGInvers;
            FDownBitmap := FBmWico22;
            FUpBitmap := FBmRMGInvers;
          end;
      3 : begin
            FDownRectBm := FRectWico22;
            FUpRectBm := FRectRMG;
            FDownRectIe := FRectImgWico22;
            FUpRectIe := FRectImgRMG;
            FDownBitmap := FBmWico22;
            FUpBitmap := FBmRMG;
          end;
      else begin
            FDownRectBm := FRectRMG;
            FUpRectBm := FRectRMGInvers;
            FDownRectIe := FRectImgRMG;
            FUpRectIe := FRectImgRMGInvers;
            FDownBitmap := FBmRMG;
            FUpBitmap := FBmRMGInvers;
          end;
    end;

    if (FStopLive) then Exit;
    imgeRMG.Canvas.FillRect(imgeRMG.ClientRect);
    if (FStopLive) then Exit;

    if (FUpDownChange) then
    iBlend := 250-FStartBlend
    else iBlend := FStartBlend;

    DeviceAlphaBlend(imgeRMG.Canvas.Handle, FDownBitmap.Canvas.Handle,
      FDownRectIe, FDownRectBm, 250-iBlend);
    if (FStopLive) then Exit;

    DeviceAlphaBlend(imgeRMG.Canvas.Handle, FUpBitmap.Canvas.Handle,
      FUpRectIe, FUpRectBm, iBlend);
    if (FStopLive) then Exit;

    if (FStopLive) then Exit;
    imgeRMG.Repaint;
    if (FStopLive) then Exit;

    if (FStopLive) then Exit;
    Application.ProcessMessages;
    if (FStopLive) then Exit;
  end;
end;

{--------------------------------------------}
procedure TStartFenster.TimerLiveImageStartTimer(Sender: TObject);
{--------------------------------------------}
begin
  TimerLiveImg := False;
  try
    if (FStarted) then begin
      FStarted := False;
      InitBitmaps(True);
    end;
    BuildLiveImage;
  finally
    TimerLiveImg := not FStopLive;
  end;
end;

{ 23.04.2010  WN                             }
{--------------------------------------------}
procedure TStartFenster.LoadImagesFromIni;
{--------------------------------------------}
var
  s : String;
begin
  // INI-Datei wird gelesen, Informationen abspeichern
{$IFDEF ALTLOKAL}    // 25.05.2011
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + '..\wieser.ini') do
  try
    s := ReadString(C_Section_Dialog, C_Ident_Logo1, '');
    if FileExists(s) then
      imgeRMG.Picture.LoadFromFile(s);

(*  Logo invers nicht mehr verwendet; 03.02.2017, WW
    s := ReadString(C_Section_Dialog, C_Ident_Logo2, '');
    if FileExists(s) then
      imgeRMG.Picture.LoadFromFile(s); *)
  finally
    Free;
  end;
{$ELSE}
  with TGUIIni.Create(false, false) do
  try
    // Logo RMG
    s := ImgDialog_Logo1;
    if FileExists(s) then
      imgeRMG.Picture.LoadFromFile(s);

(*  Logo invers nicht mehr verwendet; 03.02.2017, WW
    // Logo RMGInvers
    s := ImgDialog_Logo2;
    if FileExists(s) then
      imgeRMGInvers.Picture.LoadFromFile(ImgDialog_Logo2);*)
  finally
    Free;
  end;
{$ENDIF}
end;

{ 23.04.2010  WN                                    }
{---------------------------------------------------}
procedure TStartFenster.FormClick(Sender: TObject);
{---------------------------------------------------}
begin
  ModalResult := mrOk;
  if (Assigned (FOnImageClick)) then (FOnImageClick(Sender));
end;

{ 23.04.2010  WN                                 }
{------------------------------------------------}
procedure TStartFenster.FormShow(Sender: TObject);
{------------------------------------------------}
begin
  Height := GesamtHeight;
end;

{ 23.04.2010  WN                                 }
{ im Startdialog Lizenzinfos sichtbar Ja/Nein    }
{------------------------------------------------}
procedure TStartFenster.ShowLizenzInfos(bVisible: Boolean);
{------------------------------------------------}
begin
  // Sichtbarkeit Label
  lCopyRight.Visible := bVisible;
  lExeDatum.Visible  := bVisible;
  lVersion.Visible   := bVisible;

  if bVisible then
  begin
    pnProgramm.Height := 120;
    imgeRMG.Parent := pnProgramm;
    imgeRMG.Top := 13;  // 10.04.2017, WW
  end else
  begin
    pnProgramm.Height := 60;
    imgeRMG.Parent := pnHPS;
    imgeRMG.Top := 14;
  end;
  imgeRMGInvers.Parent := imgeRMG.Parent;
  imgeWico22.Parent := imgeRMG.Parent;

  pnMessage.Visible := not bVisible;
end;

{ 23.04.2010  WN                                 }
{------------------------------------------------}
procedure TStartFenster.SetProgName(s: String);
{------------------------------------------------}
begin
  FProgName := s;
  lProgrammName.Caption := FProgName;
end;

{ 23.04.2010  WN                                 }
{------------------------------------------------}
procedure TStartFenster.SetVersion(s: String);
{------------------------------------------------}
begin
  ShowLizenzInfos(True);
  FVersion := s;
  lVersion.Caption := S_Version + ' ' + FVersion;  // 11.07.2018, WW
end;

{ 23.04.2010  WN                                 }
{------------------------------------------------}
procedure TStartFenster.SetExeDate(s: String);
{------------------------------------------------}
begin
  ShowLizenzInfos(True);
  FExeDate := s;
  lExeDatum.Caption := FExeDate;
end;

{ 23.04.2010  WN                                 }
{------------------------------------------------}
procedure TStartFenster.SetCopyRight(s: String);
{------------------------------------------------}
begin
  ShowLizenzInfos(True);
  FCopyRight := s;
  lCopyRight.Caption := FCopyRight;
end;

{ 23.04.2010  WN                                 }
{------------------------------------------------}
procedure TStartFenster.SetBevel(b: Boolean);
{------------------------------------------------}
begin
  FBevelOn := b;
  if FBevelOn then
  begin
    Panel1.BevelInner := bvRaised;
    Panel1.BevelOuter := bvLowered;
  end else
  begin
    Panel1.BevelInner := bvNone;
    Panel1.BevelOuter := bvNone;
  end;
end;

{ 23.04.2010  WN                                 }
{------------------------------------------------}
procedure TStartFenster.SetTimerLiveImg(b: Boolean);
{------------------------------------------------}
begin
  FTimerLiveImg := False;  // b;  Logo-Wechsel deaktiviert; 03.02.2017, WW
  TimerLiveImageStart.Enabled := FTimerLiveImg;
end;

{ 23.04.2010  WN                                 }
{------------------------------------------------}
procedure TStartFenster.lHttpClick(Sender: TObject);
{------------------------------------------------}
begin
  ShellExecute(Application.Handle, 'open', pChar(lHttp.Caption),
               nil, nil, SW_ShowNormal);
end;

{ 23.04.2010  WN                                 }
{------------------------------------------------}
procedure TStartFenster.LEMailClick(Sender: TObject);
{------------------------------------------------}
var
  sBetreff, sText: String;
begin
  if (Assigned (FSendMail)) then (FSendMail(sBetreff, sText));

  ShellExecute(Application.Handle, 'open',
  pChar('mailto:' + lEmail.Caption + '?subject=' + sBetreff + '&body=' + sText),
  nil, nil, sw_ShowNormal);
end;

{ 23.04.2010  WN                                 }
{------------------------------------------------}
function TStartFenster.GetHeightGesamt: Integer;
{------------------------------------------------}
var
  i: Integer;
begin
  Result := 0;
  for i:=0 to Panel1.ControlCount-1 do
  begin
    if Panel1.Controls[i] is TPanel then
    begin
      if TPanel(Panel1.Controls[i]).Visible then
        Result := Result + TPanel(Panel1.Controls[i]).Height;
    end;
  end;
end;

end.
