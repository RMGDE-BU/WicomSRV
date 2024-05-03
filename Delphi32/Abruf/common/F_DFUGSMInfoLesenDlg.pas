{******************************************************************************}
{* Unit: Dialog 'GSM-Information lesen' über DSfG-DFÜ-Parameter               *}
{* 10.04.2012  WW                                                             *}
{******************************************************************************}
unit F_DFUGSMInfoLesenDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, F_COMPanelDlg, StdCtrls, Buttons, ExtCtrls, ComCtrls, F_COMDlg;

type
  TCBDFUInstGetDevParamProc = function (PNr: string; var PString: string): boolean of object;

  TFormDFUGSMInfoDlg = class(TFormCOMPanelDlg)
    Label5: TLabel;
    lNetz: TLabel;
    Label3: TLabel;
    pbSQ: TProgressBar;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label6: TLabel;
    TimerLesen: TTimer;
    lSQ: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TimerLesenTimer(Sender: TObject);
  private
    { Private-Deklarationen }
//    FMrgDlgCommunication: TMrgDlgCommunication;
    FGetDevParam: TCBDFUInstGetDevParamProc;
    procedure Start_Lesen (Sender: TObject);
  public
    { Public-Deklarationen }
    property CBGetDevParam: TCBDFUInstGetDevParamProc read FGetDevParam write FGetDevParam;
  end;

implementation

{$R *.dfm}

resourcestring
  SGSMInfoLesen = 'GSM-Information lesen...';
  SOKGSMInfoLesen = 'GSM-Information wurde erfolgreich gelesen';
  SKeineInformationVerfuegbar = 'Keine Information verfügbar';


{--------------------------------------------------------}
procedure TFormDFUGSMInfoDlg.FormActivate(Sender: TObject);
{--------------------------------------------------------}
var
  Cursor_Save: TCursor;

begin
  Cursor_Save:=Screen.Cursor;
  Screen.Cursor:=crHourGlass;
  try
    Start_Lesen (Sender);  // Lesen der GSM-Information starten
  finally
    inherited;
    Screen.Cursor:=Cursor_Save;
  end;
end;

{---------------------------------------------------------}
procedure TFormDFUGSMInfoDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
{---------------------------------------------------------}
begin
  Screen.Cursor:=crHourGlass;  // Cursor für Beenden
  bbtnBeenden.Enabled:=false;
end;

{--------------------------------------------------------}
procedure TFormDFUGSMInfoDlg.Start_Lesen (Sender: TObject);
{--------------------------------------------------------}
{ zyklisches Lesen der GSM-Information starten }
begin
  Status:=SGSMInfoLesen;
  pStatus.Font.Color:=clWindowText;

  TimerLesen.Enabled:=true;
  TimerLesenTimer (Sender);
end;

{-----------------------------------------------------------}
procedure TFormDFUGSMInfoDlg.TimerLesenTimer(Sender: TObject);
{-----------------------------------------------------------}
var
  sParaWert: string;
  iSQ: integer;

begin
  if Assigned (CBGetDevParam) then begin
    TimerLesen.Enabled:=false;
    try
      Status:=SGSMInfoLesen;
      pStatus.Font.Color:=clWindowText;

      { Parameter "GSM-Operator" lesen: }
      if CBGetDevParam ('039', sParaWert) then begin
        if sParaWert = '?' then
          lNetz.Caption:=SKeineInformationVerfuegbar
        else
          lNetz.Caption:=sParaWert
      end
      else begin
        lNetz.Caption:='';
        Status:=SFehlerSendReceive;
        pStatus.Font.Color:=clRed;
        exit;
      end;

      { Parameter "GSM-Pegel" lesen: }
      if CBGetDevParam ('038', sParaWert) then begin
        lSQ.Caption:=sParaWert;
        iSQ:=StrToIntDef (sParaWert, -1);
        if (iSQ >= pbSQ.Min) AND (iSQ <= pbSQ.Max) then
          pbSQ.Position:=iSQ
        else
          pbSQ.Position:=0;
      end
      else begin
        lSQ.Caption:='';
        Status:=SFehlerSendReceive;
        pStatus.Font.Color:=clRed;
        exit;
      end;

      Status:=SOKGSMInfoLesen;
      pStatus.Font.Color:=clGreen;
    finally
      TimerLesen.Enabled:=true;
    end;
  end;
end;

end.
