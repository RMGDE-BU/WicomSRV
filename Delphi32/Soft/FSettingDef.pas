{------------------------------------------------------------------------------}
{ Default-Einstelldialog                                                       }
{                                                                              }
{ 24.01.2003  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003                                          }
{------------------------------------------------------------------------------}
unit FSettingDef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls,
  FDlgDef;

type
  TFormSettingsDefault = class(TFormDialogDefault)
    pnBottomBtnCancel: TPanel;
    bbtnCancel: TBitBtn;
    PageControl: TPageControl;
    procedure CtrlModified(Sender: TObject); virtual;
    procedure FormCreate(Sender: TObject); virtual;
    procedure bbtnOkClick(Sender: TObject); virtual;
    procedure bbtnCancelClick(Sender: TObject); virtual;
  private
    { Private-Deklarationen }
    FModified : boolean;
  protected
    { Protected-Deklarationen }
    procedure LoadSettings; virtual; abstract;
    procedure SaveSettings; virtual; abstract;
    property Modified: boolean read FModified write FModified;
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

{-------------------------------------------------------}
procedure TFormSettingsDefault.FormCreate(Sender: TObject);
{-------------------------------------------------------}
begin
  inherited;

  LoadSettings;
  FModified := False;
end;

{ Ereignis-Procedure für Änderung der Controls          }
{-------------------------------------------------------}
procedure TFormSettingsDefault.CtrlModified(Sender: TObject);
{-------------------------------------------------------}
begin
  FModified := True;
end;

{-------------------------------------------------------}
procedure TFormSettingsDefault.bbtnOkClick(Sender: TObject);
{-------------------------------------------------------}
begin
  inherited;
  SaveSettings;
end;

{-------------------------------------------------------}
procedure TFormSettingsDefault.bbtnCancelClick(Sender: TObject);
{-------------------------------------------------------}
resourcestring
  S_SaveConfirmation = 'Einstellungen wurden geändert !'#13#10 +
    'Sollen diese vor dem Beenden gespeichert werden ?';
begin
  if (Modified) and (MessageDlg(S_SaveConfirmation, mtConfirmation,
      [mbYes, mbCancel], 0) = mrYes)
  then begin
    SaveSettings;
    ModalResult := mrOk;
  end;
end;

end.
