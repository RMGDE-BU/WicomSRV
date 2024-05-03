{------------------------------------------------------------------------------}
{ Drucken von Charts                                                           }
{                                                                              }
{ 03.02.2003  GD  Neu                                                          }
{ 09.03.2005  GD  Anpassung für TChart 7.04                                    }                                                          
{ 11.07.2018  WW  DEFAULT_CHARSET statt ANSI_CHARSET                           }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003, RMG Messtechnik GmbH 2018               }
{------------------------------------------------------------------------------}
unit FPrintDiagramme;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Qrctrls, QuickRpt, ExtCtrls, TeeProcs, TeEngine, Chart, DBChart, QrTee,
  Series,
  Printers, PathIni;

type
  TFormPrintDiagramm = class(TForm)
    QuickReport: TQuickRep;
    QRBandPageHeader: TQRBand;
    qrlZeitbereich: TQRLabel;
    QRSysPage: TQRSysData;
    qrimgeWieser: TQRImage;
    QRSysDate: TQRSysData;
    QRSysTime: TQRSysData;
    qrlStationsName: TQRLabel;
    qrlDatenTyp: TQRLabel;
    qrbdDummy: TQRBand;
    QRBand1: TQRBand;
    QRDBChart1: TQRDBChart;
    QRChart: TQRChart;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  protected
    procedure SetCaptions(pSl: TStrings); virtual;
  public
    { Public-Deklarationen }
    procedure PrintChart(pChart: TChart);
    property Captions: TStrings write SetCaptions;
  end;

implementation

{$R *.DFM}

{------------------------------------------------------}
procedure TFormPrintDiagramm.FormCreate(Sender: TObject);
{------------------------------------------------------}
begin
  if (Assigned(PathServer)) and (FileExists(PathServer.LogoName)) then
    qrimgeWieser.Picture.LoadFromFile(PathServer.LogoName);
end;

{------------------------------------------------------}
procedure TFormPrintDiagramm.SetCaptions(pSl: TStrings);
{------------------------------------------------------}
var
  i : byte;
begin
  for i := 0 to pSl.Count-1 do
    case i of
      0 : qrlDatenTyp.Caption := pSl[i];
      1 : qrlStationsName.Caption := pSl[i];
      2 : qrlZeitbereich.Caption := pSl[i];
    end;
end;

{------------------------------------------------------}
procedure TFormPrintDiagramm.PrintChart(pChart: TChart);
{------------------------------------------------------}
var
  i : integer;
begin
  QRChart.Chart.SeriesList.Clear;
  QRChart.Chart.Assign(pChart);

  for i := pChart.SeriesList.Count-1 downto 0 do begin
//    QRChart.Chart.SeriesList.Add(pChart.SeriesList[i]);  // 09.03.2005
    pChart.SeriesList[i].ParentChart := QRChart.Chart;
  end;

  { es soll auf dem Drucker gedruckt werden, der im Programm eingestellt ist
    (z.B. über TPrinterSetupDialog): }
  QuickReport.PrinterSettings.PrinterIndex:=Printer.PrinterIndex;  // 28.02.2006 WW
  QuickReport.Print;

  for i := QRChart.Chart.SeriesList.Count-1 downto 0 do begin
    QRChart.Chart.SeriesList[i].ParentChart := pChart;
    if (QRChart.Chart.SeriesList.Count > i) then
      QRChart.Chart.SeriesList.Delete(i);
  end;
end;

end.
