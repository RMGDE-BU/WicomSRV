{------------------------------------------------------------------------------}
{ TimeOut-Dialog                                                               }
{                                                                              }
{ 12.10.2005  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2005                                          }
{------------------------------------------------------------------------------}
unit FTimeOutDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FDlgDef, StdCtrls, Buttons, ExtCtrls;

type
  TFormTimeoutDialog = class(TFormDialogDefault)
    lMessage: TLabel;
    lTimeOut: TLabel;
    TimerTimeOut: TTimer;
    procedure TimerTimeOutTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
    FTimeOut        : Cardinal;
    FTimeOutEnd     : Cardinal;
    FTimeOutEnabled : boolean;
    FAnyKeyRestartsTimeOut : boolean;
    procedure SetTimeOut(iTimeOut: Cardinal);
    procedure SetMessageText(sText: string);
    procedure SetAnyKeyRestartsTimeOut(bState: boolean);
  public
    { Public-Deklarationen }
    property TimeOut: Cardinal read FTimeOut write SetTimeOut;
    property MessageText: string write SetMessageText;
    property AnyKeyRestartsTimeOut: boolean write SetAnyKeyRestartsTimeOut;
  end;

implementation

{$R *.dfm}

{-------------------------------------------------------}
procedure TFormTimeoutDialog.FormCreate(Sender: TObject);
{-------------------------------------------------------}
begin
  inherited;

  FAnyKeyRestartsTimeOut := False;
  KeyPreview := True;
end;

{-------------------------------------------------------}
procedure TFormTimeoutDialog.SetTimeOut(iTimeOut: Cardinal);
{-------------------------------------------------------}
begin
  FTimeOut := iTimeOut;
  FTimeOutEnd := GetTickCount + FTimeOut;
  FTimeOutEnabled := True;
  TimerTimeOut.Enabled := True;
end;

{-------------------------------------------------------}
procedure TFormTimeoutDialog.SetMessageText(sText: string);
{-------------------------------------------------------}
begin
  lMessage.Caption := sText;
end;

{-------------------------------------------------------}
procedure TFormTimeoutDialog.SetAnyKeyRestartsTimeOut(bState: boolean);
{-------------------------------------------------------}
begin
  FAnyKeyRestartsTimeOut := bState;
end;

{-------------------------------------------------------}
procedure TFormTimeoutDialog.TimerTimeOutTimer(Sender: TObject);
{-------------------------------------------------------}
var
  iResult : integer;
begin
  TimerTimeOut.Enabled := False;
  try
    if (not FTimeOutEnabled) then Exit;
    iResult := (FTimeOutEnd - GetTickCount) div 1000;
    lTimeOut.Caption := IntToStr(iResult) + ' sec.';
    if (not FTimeOutEnabled) then Exit;
    if (iResult <= 0) then begin
      FTimeOutEnabled := False;
      ModalResult := mrCancel;
    end;
  finally
    TimerTimeOut.Enabled := FTimeOutEnabled;
  end;
end;

{-------------------------------------------------------}
procedure TFormTimeoutDialog.FormKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
{-------------------------------------------------------}
begin
  inherited;

  if (FAnyKeyRestartsTimeOut) then begin
    ModalResult := mrOk;
    FTimeOutEnabled := False;
  end;
end;

end.
