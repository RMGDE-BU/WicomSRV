unit FShowWMSState;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FComBasis, ImgList, WDialogs, Menus, ExtCtrls, Buttons, ComCtrls,
  Taskbar_utils;

type
  TFormShowWMSState = class(TFormComBasis)
    procedure TimerTimer(Sender: TObject);
  private
    { Private-Deklarationen }
    FMySystem : byte;
  protected
    { Protected-Deklarationen }
    procedure InitComponents(bState: boolean); override;
  public
    { Public-Deklarationen }
  end;

var
  FormShowWMSState: TFormShowWMSState;

const
  C_ImageIndex_1None     = 2;
  C_ImageIndex_1Activ    = 3;
  C_ImageIndex_1Inactiv  = 4;
  C_ImageIndex_2None     = 5;
  C_ImageIndex_2Activ    = 6;
  C_ImageIndex_2Inactiv  = 7;

implementation

{$R *.dfm}

{--------------------------------------------}
function MyFileExists(const sMask: string): boolean;
{--------------------------------------------}
var
  pSR   : TSearchRec;
begin
  try
    Result := False;
    if (FindFirst(sMask, 0, pSR) = 0) then begin
      FindClose(pSR);
      Result := True;
    end;
  except
    Result := False;
  end;
end;

//---------------------------------------------
procedure TFormShowWMSState.InitComponents(bState: boolean);
//---------------------------------------------
begin
  inherited;

  if (bState) then begin
    FMySystem := 0;
    if (ParamCount = 1) then FMySystem := StrToIntDef(ParamStr(1), 0);
    if (not (FMySystem in [1, 2])) then Application.Terminate;
  end
  else begin
  end;
end;

procedure TFormShowWMSState.TimerTimer(Sender: TObject);
var
  pIcon : TIcon;
  iIIx  : byte;
begin
  inherited;

  Timer.Enabled := False;
  try
    Timer.Interval := 10000;

    if (MyFileExists(ExtractFilePath(ParamStr(0)) + 'SFF_*_2.SFF')) then
      iIIx := FMySystem*3
    else if (MyFileExists(ExtractFilePath(ParamStr(0)) + 'SFF_*_*.SFF')) then
      iIIx := (FMySystem*3) + 1
    else iIIx := (FMySystem*3) - 1;

    pIcon := TIcon.Create;
    try
      ImageList.GetIcon(iIIx, pIcon);
      TaskBarModifyIcon(
        0, pIcon.Handle, Self.Handle, '');
    finally
      pIcon.Free;
    end;
  finally
    Timer.Enabled := not FCanClose;
  end;
end;

end.
