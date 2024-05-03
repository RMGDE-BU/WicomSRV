unit QRDefault;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Qrctrls, QuickRpt;

type
  TFormQuickReportDefault = class(TForm)
    QuickRep1: TQuickRep;
    QRBand1: TQRBand;
    qrlTitle: TQRLabel;
    qrsysDate: TQRSysData;
    qrsysTime: TQRSysData;
    QRSysData3: TQRSysData;
    qrimgeLogo: TQRImage;
    qrlSubTitle: TQRLabel;
  private
    { Private-Deklarationen }
    FLogo : TBitmap;
    procedure SetLogo(Value: TBitmap);
    procedure SetTitle(iIndex: integer; sTitle: string);
  public
    { Public-Deklarationen }
    property Logo: TBitmap read FLogo write SetLogo;
    property Title[iIndex: integer]: string write SetTitle;
  end;

implementation

{$R *.DFM}

{------------------------------------}
procedure TFormQuickReportDefault.SetLogo(Value: TBitmap);
{------------------------------------}
begin
  FLogo := Value;
  if (Assigned(FLogo)) then qrimgeLogo.Picture.Bitmap.Handle := FLogo.Handle;
end;

{------------------------------------}
procedure TFormQuickReportDefault.SetTitle(iIndex: integer; sTitle: string);
{------------------------------------}
begin
  case iIndex of
    1 : qrlTitle.Caption := sTitle;
    2 : qrlSubTitle.Caption := sTitle;
  end;
end;

end.
