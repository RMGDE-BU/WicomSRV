unit FGDPrintPreview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, ExtCtrls,
  Buttons, StdCtrls, Forms, Dialogs, Printers,
  GD_Print, MrgDlgDatabookPrint;

type
  TMyOnPaint = procedure of object;

  TMyPrintCanvas = class(TCustomControl)
  private
    FMyOnPaint : TMyOnPaint;
  public
    { Public-Deklarationen }
    procedure Paint; override;
    property Canvas;
    property MyOnPaint: TMyOnPaint write FMyOnPaint;
  end;

  TFormGDPrintPreview = class(TForm)
    ScrollBox: TScrollBox;
    pnTop: TPanel;
    pnPageAct: TPanel;
    pnPageMax: TPanel;
    sbtnBack: TSpeedButton;
    sbtnFwd: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    sbtnPrinterSettings: TSpeedButton;
    sbtnPrint: TSpeedButton;
    pnActPrinter: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbtnBackClick(Sender: TObject);
    procedure sbtnFwdClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbtnPrinterSettingsClick(Sender: TObject);
    procedure sbtnPrintClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FPO : TMrgDlgDatabookPrint;
    FPrintCanvas : TMyPrintCanvas;
    procedure PaintCanvas;
  public
    { Public-Deklarationen }
    procedure Initialize;
    property PO: TMrgDlgDatabookPrint write FPO;
  end;

implementation

{$R *.dfm}

procedure TMyPrintCanvas.Paint;
begin
  inherited;

  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(ClientRect);
  if (Assigned(FMyOnPaint)) then FMyOnPaint;
end;


procedure TFormGDPrintPreview.FormCreate(Sender: TObject);
begin
  Self.Scaled := True;
  FPO := nil;
  FPrintCanvas := TMyPrintCanvas.Create(ScrollBox);
  FPrintCanvas.Parent := ScrollBox;
  FPrintCanvas.Left := 0;
  FPrintCanvas.Top := 0;
  FPrintCanvas.Width := Round(21*Self.PixelsPerInch/2.54);
  FPrintCanvas.Height := Round(29.7*Self.PixelsPerInch/2.54);
  FPrintCanvas.MyOnPaint := Self.PaintCanvas;

  with Printer do begin
    if (PrinterIndex >= 0)
    then pnActPrinter.Hint := pnActPrinter.Caption
    else pnActPrinter.Caption := '';
    pnActPrinter.Caption := Printers[PrinterIndex];
  end;
end;

procedure TFormGDPrintPreview.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPrintCanvas);
end;

procedure TFormGDPrintPreview.Initialize;
begin
  if (Assigned(FPO)) then begin
    FPO.Preview(FPrintCanvas.Canvas);
    pnPageAct.Caption := '1';
    FPO.GoToPage(999999);
    pnPageMax.Caption := IntToStr(FPO.ActPage);
    FPO.GoToPage(1);
    sbtnFwd.Enabled := (FPO.ActPage < StrToInt(pnPageMax.Caption));
    sbtnBack.Enabled := (FPO.ActPage > 1);
  end;
end;

procedure TFormGDPrintPreview.PaintCanvas;
begin
  if (Assigned(FPO)) then FPO.Preview(FPrintCanvas.Canvas);
end;

procedure TFormGDPrintPreview.sbtnBackClick(Sender: TObject);
begin
  sbtnBack.Enabled := False;
  try
    if (FPO.ActPage > 1) then begin
      FPO.GoToPage(FPO.ActPage - 1);
      pnPageAct.Caption := IntToStr(FPO.ActPage);
    end;
  finally
    sbtnFwd.Enabled := (FPO.ActPage < StrToInt(pnPageMax.Caption));
    sbtnBack.Enabled := (FPO.ActPage > 1);
  end;
end;

procedure TFormGDPrintPreview.sbtnFwdClick(Sender: TObject);
begin
  sbtnFwd.Enabled := False;
  try
    if (FPO.ActPage < StrToInt(pnPageMax.Caption)) then begin
      FPO.GoToPage(FPO.ActPage + 1);
      pnPageAct.Caption := IntToStr(FPO.ActPage);
    end;
  finally
    sbtnFwd.Enabled := (FPO.ActPage < StrToInt(pnPageMax.Caption));
    sbtnBack.Enabled := (FPO.ActPage > 1);
  end;
end;

procedure TFormGDPrintPreview.FormResize(Sender: TObject);
begin
  if (not (csDestroying in Self.ComponentState)) and (Assigned(FPrintCanvas))
  then begin
    if (ScrollBox.Width > FPrintCanvas.Width)
    then FPrintCanvas.Left := (ScrollBox.Width - FPrintCanvas.Width) div 2
    else FPrintCanvas.Left := 20;
    if (ScrollBox.Height > FPrintCanvas.Height)
    then FPrintCanvas.Top := (ScrollBox.Height - FPrintCanvas.Height) div 2
    else FPrintCanvas.Top := 20;
  end;
end;

procedure TFormGDPrintPreview.sbtnPrinterSettingsClick(Sender: TObject);
begin
  with TPrinterSetupDialog.Create(Self) do
  try
    if (Execute) then begin
      with Printer do begin
        if (PrinterIndex >= 0)
        then pnActPrinter.Hint := pnActPrinter.Caption
        else pnActPrinter.Caption := '';
        pnActPrinter.Caption := Printers[PrinterIndex];
      end;
    end
  finally
    Free;
  end;
end;

procedure TFormGDPrintPreview.sbtnPrintClick(Sender: TObject);
begin
  if (Assigned(FPO)) then begin
    FPrintCanvas.MyOnPaint := nil;
    FPO.Print;
  end;
  ModalResult := mrOk;
end;

end.
