{------------------------------------------------------------------------------}
{ 09.04.1999 GD; Progressbar-Dialog-Fenster                                    }
{                                                                              }
{ 29.03.2001  GD  Ergänzung um 'AutoStep'                                      }
{ 16.07.2001  GD  Ergänzung um Abruch-Funktion                                 }
{ 02.10.2001  GD/WW  FTimer-Prüfung in FormDestroy berichtigt,                 }
{                    globale Var FormProgressBar eliminiert (lokal anlegen !)  }
{ 26.10.2007  WW  resourcestrings                                              }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 1999, 2001                                    }
{------------------------------------------------------------------------------}
unit FPrgBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons;

type
  TFormProgressBar = class(TForm)
    ProgressBar: TProgressBar;
    bbtnCancel: TBitBtn;
    procedure FormDestroy(Sender: TObject);
    procedure bbtnCancelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bbtnCancelClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FTimer    : integer;
    procedure SetCancel(bState: boolean);  // 16.07.2001
  public
    { Public-Deklarationen }
    constructor Create(anOwner: TWinControl; aCaption: string;
                       Min, Max: integer); reintroduce;
    procedure SetMin(Value: integer);
    procedure SetMax(Value: integer);
    procedure Step(Value: integer = 1);
    procedure AutoStep(Sender: TObject = nil);     // 29.03.2002
    property CancelMode: boolean write SetCancel;  // 16.07.2001
  end;

implementation

{$R *.DFM}

resourcestring
  S_Cancel = 'Abbrechen ?';


{----------------------------------------------------}
procedure TimerProc(pHandle: HWND; iMsg, iId: UINT; iElapsedTime: DWORD); stdcall;
{----------------------------------------------------}
const
  pForm: TFormProgressBar = nil;
begin
  if (iElapsedTime = 0)
  then pForm := TFormProgressBar(iId)
  else if (Assigned(pForm)) then pForm.AutoStep;
end;

{----------------------------------------------------}
constructor TFormProgressBar.Create
                  (anOwner: TWinControl; aCaption: string; Min, Max: integer);
{----------------------------------------------------}
begin
  inherited Create(anOwner);
  Caption:= aCaption;
  ProgressBar.Min:= Min;
  if Max <= Min then Max:= Min + 1;
  ProgressBar.Max:= Max;
  ProgressBar.Position:= Min;
  FTimer := -1;
  CancelMode := False;
  Show;
end;

{----------------------------------------------------}
procedure TFormProgressBar.FormDestroy(Sender: TObject);
{----------------------------------------------------}
begin
  if (FTimer > 0) then begin
    KillTimer(0, FTimer);
    Application.HandleMessage;
  end;
end;

{----------------------------------------------------}
procedure TFormProgressBar.SetMin(Value: integer);
{----------------------------------------------------}
begin
  ProgressBar.Min:= Value;
end;

{----------------------------------------------------}
procedure TFormProgressBar.SetMax(Value: integer);
{----------------------------------------------------}
begin
  ProgressBar.Max:= Value;
end;

{----------------------------------------------------}
procedure TFormProgressBar.Step(Value: integer = 1);
{----------------------------------------------------}
begin
  ProgressBar.StepBy(Value);
  Application.ProcessMessages;
end;

{ Automatischer Durchlauf des Balkens bis Ende       }
{----------------------------------------------------}
procedure TFormProgressBar.AutoStep(Sender: TObject = nil);
{----------------------------------------------------}
begin
  if (Assigned(Sender)) then begin
    TimerProc(0, 0, UINT(Self), 0);
    FTimer := SetTimer(0, 0, 100, @TimerProc);
  end
  else begin
    if (ProgressBar.Position = ProgressBar.Max)
    then ProgressBar.Position := ProgressBar.Min
    else Step;
    Application.ProcessMessages;
  end;
end;

{ Abbrucschalter anzeigen                            }
{ Parameter: T=Ja, F=Nein                            }
{----------------------------------------------------}
procedure TFormProgressBar.SetCancel(bState: boolean);  // 16.07.2001
{----------------------------------------------------}
begin
  if (bState)
  then begin
    ModalResult := mrNone;
    ClientHeight := bbtnCancel.Top + bbtnCancel.Height + 4;
    bbtnCancel.Enabled := True;
    bbtnCancel.SetFocus;
  end
  else ClientHeight := ProgressBar.Height;
  Invalidate;
  Application.ProcessMessages;
end;

{----------------------------------------------------}
procedure TFormProgressBar.bbtnCancelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{----------------------------------------------------}
begin
  if (bbtnCancel.Enabled) and (Button = mbLeft) and
     (MessageDlg(S_Cancel, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    ModalResult := mrCancel;
    bbtnCancel.Enabled := False;
  end;
end;

{----------------------------------------------------}
procedure TFormProgressBar.bbtnCancelClick(Sender: TObject);
{----------------------------------------------------}
begin
  if (bbtnCancel.Enabled) and
     (MessageDlg(S_Cancel, mtConfirmation, [mbYes, mbNo], 0) = mrYes)
  then bbtnCancel.Enabled := False
  else ModalResult := mrNone;
end;

end.
