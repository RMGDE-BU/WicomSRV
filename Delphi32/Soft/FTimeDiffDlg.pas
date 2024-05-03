{------------------------------------------------------------------------------}
{ Zeitdifferenz-Dialog                                                        }
{                                                                              }
{ 22.09.2000  GD    Neu                                                        }
{ 05.12.2000  GD    Vs 1.01                                                    }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000                                          }
{------------------------------------------------------------------------------}
unit FTimeDiffDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ComCtrls, Buttons, ExtCtrls;

type
  TFormTimeDiffDlg = class(TForm)
    pnRight: TPanel;
    sbtnCancel: TSpeedButton;
    Panel1: TPanel;
    sbtnOk: TSpeedButton;
    dtpVon: TDateTimePicker;
    speVon: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    dtpBis: TDateTimePicker;
    speBis: TSpinEdit;
    Label4: TLabel;
    sbtnIgnore: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure sbtnOkClick(Sender: TObject);
    procedure sbtnCancelClick(Sender: TObject);
    procedure sbtnIgnoreClick(Sender: TObject);
  private
    { Private-Deklarationen }
    function GetVonZeit: TDateTime;
    function GetBisZeit: TDateTime;
  public
    { Public-Deklarationen }
    property VonZeit: TDateTime read GetVonZeit;
    property BisZeit: TDateTime read GetBisZeit;
  end;

implementation

{$R *.DFM}

{------------------------------------}
procedure TFormTimeDiffDlg.FormCreate(Sender: TObject);
{------------------------------------}
begin
  dtpVon.DateTime := Trunc(Now) - 1;
  speVon.Value := StrToInt(FormatDateTime('hh', Now));
  dtpBis.DateTime := Trunc(Now);
  speBis.Value := StrToInt(FormatDateTime('hh', Now));
end;

{------------------------------------}
function TFormTimeDiffDlg.GetVonZeit: TDateTime;
{------------------------------------}
begin
  Result := Trunc(dtpVon.DateTime) + EncodeTime(speVon.Value, 0, 0, 0);
end;

{------------------------------------}
function TFormTimeDiffDlg.GetBisZeit: TDateTime;
{------------------------------------}
begin
  Result := Trunc(dtpBis.DateTime) + EncodeTime(speBis.Value, 0, 0, 0);
end;

{------------------------------------}
procedure TFormTimeDiffDlg.sbtnOkClick(Sender: TObject);
{------------------------------------}
begin
  ModalResult := mrOk;
end;

{------------------------------------}
procedure TFormTimeDiffDlg.sbtnCancelClick(Sender: TObject);
{------------------------------------}
begin
  ModalResult := mrCancel;
end;

{------------------------------------}
procedure TFormTimeDiffDlg.sbtnIgnoreClick(Sender: TObject);
{------------------------------------}
begin
  ModalResult := mrIgnore;
end;

end.
