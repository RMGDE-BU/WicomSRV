{------------------------------------------------------------------------------}
{ Allgemeiner Datum-Zeit-Auswahl-Dialog                                        }
{                                                                              }
{ 19.12.2001  GD  Neu                                                          }
{ 01.08.2003  GD  Fehlermeldung 'ShowCheckBox' -> umgangen                     }
{ 24.03.2009  GD  TTimePicker wieder eingeführt                                }
{ 27.01.2011  GD  Möglichkeit zur Vergrößerung (Höhe der Eingabezeilen)        }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, RMG Messtechnik GmbH 2011               }
{------------------------------------------------------------------------------}
unit FDateTimeDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, ExtCtrls, Spin;

type
  TFormDateTimeDlg = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    dtpDate: TDateTimePicker;
    dtpTime: TDateTimePicker;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure SetDateTime(dtValue: TDateTime);
    function GetDateTime: TDateTime;
    procedure SetScale(iScale: byte); // 27.01.2011
  public
    { Public-Deklarationen }
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property MyScale: byte write SetScale;
  end;

implementation

{$R *.DFM}

{------------------------------------------------------}
procedure TFormDateTimeDlg.FormCreate(Sender: TObject);
{------------------------------------------------------}
begin
  SetDateTime(Now);
end;

{------------------------------------------------------}
procedure TFormDateTimeDlg.SetDateTime(dtValue: TDateTime);
{------------------------------------------------------}
begin
  dtpDate.DateTime := dtValue;
  dtpTime.DateTime := dtValue;
end;

{------------------------------------------------------}
function TFormDateTimeDlg.GetDateTime: TDateTime;
{------------------------------------------------------}
begin
  Result := Trunc(dtpDate.DateTime) + Frac(dtpTime.DateTime);
end;

{ Prozentuale Höhenänderung (von Faktor 1 bis 2 in %  }
{------------------------------------------------------}
procedure TFormDateTimeDlg.SetScale(iScale: byte);
{------------------------------------------------------}
var
  iHeight : integer;
begin
  if (iScale in [1..100]) then begin
    iHeight := Round(dtpDate.Height * (1 + (iScale/100)));
    if (iHeight > dtpDate.Height) then begin
      dtpDate.Top := Round(dtpDate.Top * (1 - (iScale/100)));
      dtpTime.Top := dtpTime.Top; // + Round((iHeight - dtpDate.Height) / 2);
      dtpDate.Height := iHeight;
      dtpTime.Height := iHeight;
      dtpDate.Font.Size := Round(dtpDate.Font.Size * (1 + (iScale/100)));
      dtpTime.Font.Size := Round(dtpTime.Font.Size * (1 + (iScale/100)));
    end;
  end;
end;

end.
