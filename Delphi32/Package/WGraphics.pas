{------------------------------------------------------------------------------}
{  Unit: Wieser Graphics Konstanten und Typen                                  }
{                                                                              }
{  03.02.2006  WW  Neu                                                         }
{  06.02.2006  GD  Diverse Utility-Funktionen hier untergebracht               }
{  23.06.2009  GD  Resource-Leck beseitigt                                     }
{  05.10.2011  GD  IconToBitmap                                                }
{  03.02.2017  WW  neue RMG-Farben CI 2016                                     }
{  17.05.2021  WW  neue RMG-Farben CI 2020                                     }
{  10.03.2022  WW  Fenster auf einem Monitor anzeigen (Set_FormMonitor)        }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2006, 2022                                }
{------------------------------------------------------------------------------}
unit WGraphics;

interface

uses
  Windows, SysUtils, Classes, Forms, ExtCtrls, Graphics, TypInfo, Types,
  Clipbrd;

const
//  C_ColorWieser = $000080FF;  // Wieser-Orange, nicht mehr verwendet
//  C_ColorRMG_Messtechnik = $00FFA54A;  // RMG-Messtechnik-Hellblau, nicht mehr verwendet

  { RMG Corporate Design, bis 10/2016 }
//  C_ColorRMG = $007E4100;    // RMG-Dunkelblau (Pantone 295 C)
//  C_ColorRMG_Gruen1 = $0038AF3F;  // oberer Balken
//  C_ColorRMG_Gruen2 = $006CD072;  // unterer Balken

  { RMG Corporate Design, ab 10/2016 }
//  C_ColorRMG = $00C28700;    // RMG-hellblau (Pantone 7689)
//  C_ColorRMG_Gruen1 = $006CC99F;  // oberer Balken, lindgrün
//  C_ColorRMG_Gruen2 = $006CC99F;  // unterer Balken, lindgrün

  { RMG Corporate Design, ab 05/2021 }
  C_ColorRMG = $008F4206;    // RMG Deep Blue
  C_ColorRMG_Gruen1 = $00AF6634;  // oberer Balken, RMG Lake Blue
  C_ColorRMG_Gruen2 = $00AF6634;  // unterer Balken, RMG Lake Blue


function GetNormalizedScreenFactor: double;
function ConvertBoundsToScreenSize(pBoundsRect: TRect): TRect;
function ConvertBoundsToDefSize(pBoundsRect: TRect): TRect;
procedure DrawTransparentBitmap(DC : HDC; hBmp : HBITMAP;
  xStart : integer; yStart : integer; cTransparentColor : COLORREF);
procedure DrawStretchedBitmap(iTarget, iSource: HDC; pT, pS: TPoint);
procedure DeviceAlphaBlend(
  iTarget, iSource: HDC; pRT, pRS: TRect; iAlphaBlend: byte);
function PictureToBitmap(pPicture: TPicture): TBitmap;
function IconToBitmap(pIcon: TIcon): TBitMap;

function CalculateFittingRect(
  pRTarget, pRSource: TRect; iBorderWidth: integer): TRect;
function CalculateRectList(pRTarget, pRSub: TRect; iRectCount: integer): TList;

procedure Set_FormMonitor (aForm: TForm; aMonitorId: integer; bCenter: boolean);

implementation

{----------------------------- Allgemeine Funktionen --------------------------}

{ Rechnet auf 600x800 normalisierten faktor aus  }
{------------------------------------------------}
function GetNormalizedScreenFactor: double;
{------------------------------------------------}
var
  fFactorX, fFactorY : double;
begin
  if (Screen.Width > Screen.Height) then begin
    fFactorX := Screen.Width / 800;
    fFactorY := Screen.Height / 600;
  end
  else begin
    fFactorX := Screen.Width / 600;
    fFactorY := Screen.Height / 800;
  end;

  Result := 2 / (fFactorX + fFactorY);
end;

{ Rechnet auf 600x800 normierte Darstellung auf  }
{ Screen-Größe um                                }
{------------------------------------------------}
function ConvertBoundsToScreenSize(pBoundsRect: TRect): TRect;
{------------------------------------------------}
var
  fFactorX, fFactorY : double;
begin
  if (Screen.Width > Screen.Height) then begin
    fFactorX := Screen.Width / 800;
    fFactorY := Screen.Height / 600;
  end
  else begin
    fFactorX := Screen.Width / 800;
    fFactorY := Screen.Height / 600;
  end;

  Result := Rect(Round(pBoundsRect.Left * fFactorX),
    Round(pBoundsRect.Top * fFactorY),
    Round(pBoundsRect.Right * fFactorX),
    Round(pBoundsRect.Bottom * fFactorY));
end;

{ Rechnet Screen-Darstellung auf auf 600x800     }
{ normierte Darstellung um                       }
{------------------------------------------------}
function ConvertBoundsToDefSize(pBoundsRect: TRect): TRect;
{------------------------------------------------}
var
  fFactorX, fFactorY : double;
begin
  if (Screen.Width > Screen.Height) then begin
    fFactorX := 800 / Screen.Width;
    fFactorY := 600 / Screen.Height;
  end
  else begin
    fFactorX := 800 / Screen.Width;
    fFactorY := 600 / Screen.Height;
  end;

  Result := Rect(Round(pBoundsRect.Left * fFactorX),
    Round(pBoundsRect.Top * fFactorY),
    Round(pBoundsRect.Right * fFactorX),
    Round(pBoundsRect.Bottom * fFactorY));
end;

{------------------------------------------------}
function CalculateFittingRect(
  pRTarget, pRSource: TRect; iBorderWidth: integer): TRect;
{------------------------------------------------}
var
  fFactor, fYFactor : double;
  iHeight, iWidth   : integer;
begin
  fFactor := (pRSource.Right - pRSource.Left) /
     (pRTarget.Right - pRTarget.Left - 2*iBorderWidth);
  fYFactor := (pRSource.Bottom - pRSource.Top) /
     (pRTarget.Bottom - pRTarget.Top - 2*iBorderWidth);
  if (fYFactor > fFactor) then fFactor := fYFactor;

  iWidth := Round((pRSource.Right - pRSource.Left) / fFactor);
  iHeight := Round((pRSource.Bottom - pRSource.Top) / fFactor);
  Result.Left := pRTarget.Left + iBorderWidth +
    (((pRTarget.Right - pRTarget.Left - 2*iBorderWidth)  - iWidth) div 2);
  Result.Top := pRTarget.Top + iBorderWidth +
    (((pRTarget.Bottom - pRTarget.Top - 2*iBorderWidth)  - iHeight) div 2);
  Result.Right := Result.Left + iWidth;
  Result.Bottom := Result.Top + iHeight;
end;

{------------------------------------------------}
function CalculateRectList(pRTarget, pRSub: TRect; iRectCount: integer): TList;
{------------------------------------------------}
var
  x, y, iActCount, iXCount, iYCount                  : integer;
  iTargetWidth, iTargetHeight, iSubWidth, iSubHeight : integer;
  fXRel, fYRel                                       : double;
  p                                                  : ^TRect;
begin
  Result := TList.Create;

  iTargetWidth := (pRTarget.Right-pRTarget.Left);
  iTargetHeight := (pRTarget.Bottom-pRTarget.Top);
  iSubWidth := (pRSub.Right-pRSub.Left);
  iSubHeight := (pRSub.Bottom-pRSub.Top);
  fXRel := (iTargetWidth / iSubWidth);
  fYRel := (iTargetHeight / iSubHeight);
  iYCount := Round(Sqrt(iRectCount * (fYRel/fXRel)));

  iActCount := iRectCount;
  for y := 0 to iYCount-1 do begin
    iXCount := iActCount div (iYCount-y);
    if (iActCount <= 0) then Break;
    for x := 0 to iXCount-1 do begin
      if (iActCount <= 0) then Break;
      New(p);
      p^.Left := pRTarget.Left + x * (iTargetWidth div iXCount);
      p^.Right := p^.Left + (iTargetWidth div iXCount);
      p^.Top := pRTarget.Top + y * (iTargetHeight div iYCount);
      p^.Bottom := p^.Top + (iTargetHeight div iYCount);
      Result.Add(p);
      Dec(iActCount);
    end;
  end;
end;

{------------------------------------------------}
procedure DeviceAlphaBlend(
  iTarget, iSource: HDC; pRT, pRS: TRect; iAlphaBlend: byte);
{------------------------------------------------}
var
  pBF: TBlendfunction;
begin
  pBF.BlendOp := AC_SRC_OVER;
  pBF.BlendFlags := 0; // AC_SRC_NO_ALPHA;//
  pBF.SourceConstantAlpha := iAlphaBlend;
  pBF.AlphaFormat := 0;
  Windows.AlphaBlend(
    iTarget, pRT.Left, pRT.Top, pRT.Right - pRT.Left, pRT.Bottom - pRT.Top,
    iSource, pRS.Left, pRS.Top, pRS.Right - pRS.Left, pRS.Bottom - pRS.Top,
    pBF);
end;

{ Paßt Bitmap in Größe an. Seitenverhältnis      }
{ bleibt erhalten                                }
{------------------------------------------------}
procedure DrawStretchedBitmap(iTarget, iSource: HDC; pT, pS: TPoint);
{------------------------------------------------}
var
  c, d       : Single;
  l, t, w, h : INTEGER;
begin
  c := pS.X / pT.X;
  d := pS.Y / pT.Y;
  IF d > c THEN c := d;
  w := Round(pS.X / c);
  h := Round(pS.Y / c);
  l := (pT.X  - w)  DIV 2;
  t := (pT.Y - h) DIV 2;
  Stretchblt(iTarget, l, t, w, h, iSource, 0, 0, pS.X, pS.Y, srcCopy);
end;

{------------------------------------------------}
function PictureToBitmap(pPicture: TPicture): TBitmap;
{------------------------------------------------}
var
  iFormat  : word;
  iData    : THandle;
  pPalette : HPalette;
begin
  Result := TBitmap.Create;
  try
    pPicture.SaveToClipboardFormat(iFormat, iData, pPalette);
    Clipboard.SetAsHandle(iFormat, iData); // 23.06.2009
    Result.LoadFromClipboardFormat(iFormat, iData, pPalette);
  except
    FreeAndNil(Result);
  end;
end;

{ Wandelt ein Icon in ein Bitmap um              }
{------------------------------------------------}
function IconToBitmap(pIcon: TIcon): TBitMap;
{------------------------------------------------}
begin
  Result := TBitmap.Create;
  Result.Width:= pIcon.Width;
  Result.Height:= pIcon.Height;
  Result.Canvas.Draw(0, 0, pIcon);
end;

{------------------------------------------------}
procedure DrawTransparentBitmap(DC : HDC; hBmp : HBITMAP;
  xStart : integer; yStart : integer; cTransparentColor : COLORREF);
{------------------------------------------------}
var
  bm : BITMAP;
  bmAndObject, bmAndMem : HBITMAP;
  bmObjectOld, bmMemOld : HBITMAP;
  hdcMem, hdcObject, hdcTemp : HDC;
  ptSize : TPOINT;
begin
  hdcTemp := CreateCompatibleDC(dc);

  // Create some DCs to hold temporary data.
  hdcObject := CreateCompatibleDC(dc);
  hdcMem := CreateCompatibleDC(dc);
  try

    SelectObject(hdcTemp, hBmp); // Select the bitmap

    GetObject(hBmp, sizeof(BITMAP), @bm);
    ptSize.x := bm.bmWidth; // Get width of bitmap
    ptSize.y := bm.bmHeight; // Get height of bitmap
    DPtoLP(hdcTemp, ptSize, 1); // Convert from device to logical points

    // Create a bitmap for each DC. DCs are required for a number of
    // GDI functions.

    // Monochrome DC
    bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
    bmAndMem := CreateCompatibleBitmap(dc, ptSize.x, ptSize.y);

    // Each DC must select a bitmap object to store pixel data.
    bmObjectOld := SelectObject(hdcObject, bmAndObject);
    bmMemOld := SelectObject(hdcMem, bmAndMem);
    try

      SetBkColor(hdcTemp, cTransparentColor);
      BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCCOPY);
      BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, dc, xStart, yStart, SRCCOPY);
      BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);
      BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCPAINT);
      BitBlt(dc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0, SRCCOPY);
    finally
      // Delete the memory bitmaps.
      DeleteObject(SelectObject(hdcObject, bmObjectOld));
      DeleteObject(SelectObject(hdcMem, bmMemOld));
    end;
  finally
    // Delete the memory DCs.
    DeleteDC(hdcMem);
    DeleteDC(hdcObject);
    DeleteDC(hdcTemp);
  end;
end;

// Fenster auf einem Monitor anzeigen
// Übergaben: Fenster
//            Monitor-Id (0 = Primär-Monitor)
//            Flag 'Fenster zentriert anzeigen' ja/nein
// -----------------------------------------------------------------------------
procedure Set_FormMonitor (aForm: TForm; aMonitorId: integer; bCenter: boolean);
// -----------------------------------------------------------------------------
var
  pMonitor: TMonitor;
  MonId: integer;

begin
  if (aMonitorId < 0) OR (aMonitorId >= Screen.MonitorCount) then
    MonId:=0  // Default: Primär-Monitor
  else
    MonId:=aMonitorId;

  if Assigned(aForm) then begin
    pMonitor := Screen.Monitors[MonId];
    if bCenter then begin  // Form zentrieren
      aForm.Left := pMonitor.Left + pMonitor.Width DIV 2 - (aForm.Width DIV 2);
      aForm.Top := pMonitor.Top + pMonitor.Height DIV 2 - (aForm.Height DIV 2);
    end
    else begin  // Left, Top der Form wie designed
      aForm.Left := aForm.Left + pMonitor.Left - aForm.Monitor.Left;
      aForm.Top := aForm.Top + pMonitor.Top - aForm.Monitor.Top;
    end
  end;
end;

end.
