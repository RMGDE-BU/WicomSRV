// -----------------------------------------------------------------------------
// Unit: Wieser.INI-Einträge für die globalen Anzeige-Eigenschaften
//
// 25.03.2010  NW  Neu
// 25.05.2011  GD  Kompatibilität mit MrgDialog
// 09.02.2017  WW  Designänderungen am Image-Panel: Hintergrundfarbe weiß statt
//                 grau; 'Logo 2' für Info-Dialog nicht mehr verwendet
//
// Copyright © RMG Messtechnik GmbH 2010, 2017
// -----------------------------------------------------------------------------
unit IniMain_Def;

interface

uses
  IniFiles, PathIni, SysUtils, Graphics;

const
  // Eigenschaften der Bilder im Start-/Info-Dialog  // 25.05.2011
  C_Section_Dialog    = 'DIALOG';
  C_Ident_Logo1       = 'Logo1';     // Pfad und Dateiname des 1. Logos
//  C_Ident_Logo2       = 'Logo2';     // Pfad und Dateiname des 2. Logos
  
type
  // Record für Eigenschaften eines Bildes
  TImgData = record
    sPath  : String;
    iWidth : Integer;
    iHeight: Integer;
    bStretch     : Boolean;
    bTransparent : Boolean;
    bProportional: Boolean;
  end;

  // Objekt für die globalen GUI-Eigenschaften in der Wieser.ini
  TGUIIni = class (TProgramIni)
  private
    function Get_ImgData(sSection: String): TImgData;
    function Get_ImgDataCenter: TImgData;  // Bild mitte (links neben Buttons)
    function Get_ImgDataRight: TImgData;   // Bild rechts
    function Get_ImgDataTop: TImgData;     // Bild für obere Trennlinie
    function Get_ImgDataBottom: TImgData;  // Bild für untere Trennlinie
    function Get_PnImgColor: TColor;
    function Get_ImgDialog_Logo1: String;
//    function Get_ImgDialog_Logo2: String;
  protected
  public
    // Eigenschaften der Images für MainDef
    property ImgData_Center: TImgData read Get_ImgDataCenter;
    property ImgData_Right: TImgData read Get_ImgDataRight;
    property ImgData_Top: TImgData read Get_ImgDataTop;
    property ImgData_Bottom: TImgData read Get_ImgDataBottom;
    property PnImg_Color: TColor read Get_PnImgColor;
    property ImgDialog_Logo1: String read Get_ImgDialog_Logo1;
//    property ImgDialog_Logo2: String read Get_ImgDialog_Logo2;
  end;

implementation

const
  // Eigenschaften der Bilder in der Button-Leiste
  C_Section_ImgCenter = 'IMG_CENTER';
  C_Section_ImgRight  = 'IMG_RIGHT';
  C_Section_ImgTop    = 'IMG_TOP';
  C_Section_ImgBottom = 'IMG_BOTTOM';

  C_Ident_ImgPath     = 'Path';      // Pfad und Dateiname des Bildes
  C_Ident_ImgWidth    = 'Width';     // Bildbreite in Pixel
  C_Ident_ImgHeight   = 'Height';    // Bildhöhe in Pixel
  C_Ident_ImgStretch  = 'Stretch';   // Bildgröße anpassen JA/NEIN
  C_Ident_ImgTrans    = 'Trans';     // Bildtransparenz JA/NEIN
  C_Ident_ImgProp     = 'Prop';      // Bildproportionen beibehalten JA/NEIN

  // Eigenschaften der Image-Panel
  C_Section_ImgPanel  = 'IMG_PANEL';
  C_Ident_PnColor     = 'PnColor';    // Farbe der Image-Panel
  C_Default_Color     = 'clWhite';  // 09.02.2017, WW


// -----------------------------------------------------------------------------
// Bild-Eigenschaften für ein Image laden
// -----------------------------------------------------------------------------
function TGUIIni.Get_ImgData(sSection: String): TImgData;
// -----------------------------------------------------------------------------
var
  pIniFile : TIniFile;
  pImgData : TImgData;
begin
  // Zugriff auf Wieser.INI
  pIniFile := TIniFile.Create(WieserIniFile);
  try
    { Bild-Eigenschaften werden aus der Ini gelesen:
      keine Pfadangabe: Bild wird nicht dargestellt
      keine Höhen- oder Breitenangabe: automat. Setzen der Höhe oder Breite
      -> abhängig von Stretch - und Proportional-Eigenschaften                 }
    with pImgData do
    begin
      // Default: kein Pfad vorgeben und somit kein Bild anzeigen
      sPath  := pIniFile.ReadString(sSection, C_Ident_ImgPath, '');
      if not FileExists(sPath) then sPath := '';  // Pfad existiert nicht
      // Default: keine Breite bzw. Höhe
      //          -> automat. entsprechend der weiteren Eigenschaften ermittelt
      iWidth := pIniFile.ReadInteger(sSection, C_Ident_ImgWidth, -1);
      iHeight:= pIniFile.ReadInteger(sSection, C_Ident_ImgHeight, -1);
      // Default: automatische Größenanpassung mit möglicher Verzerrung
      bStretch:= pIniFile.ReadBool(sSection, C_Ident_ImgStretch, False);  // Default false; 09.02.2017, WW
      // Default: keine Transparenz
      bTransparent := pIniFile.ReadBool(sSection, C_Ident_ImgTrans, False);
      // Default: automatische Verkleinerung ohne Verzerrung
      bProportional:= pIniFile.ReadBool(sSection, C_Ident_ImgProp, False);  // Default false; 09.02.2017, WW
    end;
    Result := pImgData;
  finally
    pIniFile.Free;
  end;
end;

// -----------------------------------------------------------------------------
// Bild-Eigenschaften für Image Center
// -----------------------------------------------------------------------------
function TGUIIni.Get_ImgDataCenter: TImgData;
// -----------------------------------------------------------------------------
begin
  Result := Get_ImgData(C_Section_ImgCenter);
end;

// -----------------------------------------------------------------------------
// Bild-Eigenschaften für Image Right
// -----------------------------------------------------------------------------
function TGUIIni.Get_ImgDataRight: TImgData;
// -----------------------------------------------------------------------------
begin
  Result := Get_ImgData(C_Section_ImgRight);
end;

// -----------------------------------------------------------------------------
// Bild-Eigenschaften für Image Top
// -----------------------------------------------------------------------------
function TGUIIni.Get_ImgDataTop: TImgData;
// -----------------------------------------------------------------------------
begin
  Result := Get_ImgData(C_Section_ImgTop);
end;

// -----------------------------------------------------------------------------
// Bild-Eigenschaften für Image Bottom
// -----------------------------------------------------------------------------
function TGUIIni.Get_ImgDataBottom: TImgData;
// -----------------------------------------------------------------------------
begin
  Result := Get_ImgData(C_Section_ImgBottom);
end;

// -----------------------------------------------------------------------------
// Farbe der Image-Panel ermitteln
// -----------------------------------------------------------------------------
function TGUIIni.Get_PnImgColor: TColor;
// -----------------------------------------------------------------------------
var
  pIniFile : TIniFile;
  sColor   : String;
begin
  // Zugriff auf Wieser.INI
  pIniFile := TIniFile.Create(WieserIniFile);
  try
    sColor := pIniFile.ReadString(C_Section_ImgPanel, C_Ident_PnColor,
                                  C_Default_Color);
    try
      Result := StringToColor(sColor);
    except  // Farbe gibts nicht
      Result := StringToColor(C_Default_Color);
    end;
  finally
    pIniFile.Free;
  end;
end;

// -----------------------------------------------------------------------------
// Pfad des Logos 1 für Start- und InfoDialog ermitteln
// -----------------------------------------------------------------------------
function TGUIIni.Get_ImgDialog_Logo1: String;
// -----------------------------------------------------------------------------
var
  pIniFile : TIniFile;
begin
  // Zugriff auf Wieser.INI
  pIniFile := TIniFile.Create(WieserIniFile);
  try
    Result := pIniFile.ReadString(C_Section_Dialog, C_Ident_Logo1, '');
  finally
    pIniFile.Free;
  end;
end;
(*
// -----------------------------------------------------------------------------
// Pfad des Logos 2 für Start- und InfoDialog ermitteln
// -----------------------------------------------------------------------------
function TGUIIni.Get_ImgDialog_Logo2: String;
// -----------------------------------------------------------------------------
var
  pIniFile : TIniFile;
begin
  // Zugriff auf Wieser.INI
  pIniFile := TIniFile.Create(WieserIniFile);
  try
    Result := pIniFile.ReadString(C_Section_Dialog, C_Ident_Logo2, '');
  finally
    pIniFile.Free;
  end;
end;
*)
end.
