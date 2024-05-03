inherited FormSettingsDefault: TFormSettingsDefault
  Left = 215
  Top = 116
  Caption = 'Einstellungen'
  ClientWidth = 684
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnClient: TPanel
    Width = 684
    object PageControl: TPageControl
      Left = 5
      Top = 5
      Width = 674
      Height = 519
      Align = alClient
      TabOrder = 0
    end
  end
  inherited pnBottom: TPanel
    Width = 684
    inherited pnBottomBtnOk: TPanel
      Left = 60
      inherited bbtnOk: TBitBtn
        OnClick = bbtnOkClick
      end
    end
    object pnBottomBtnCancel: TPanel
      Left = 492
      Top = 4
      Width = 100
      Height = 33
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object bbtnCancel: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Caption = '&Abbrechen'
        TabOrder = 0
        OnClick = bbtnCancelClick
        Kind = bkCancel
        Spacing = -1
      end
    end
  end
end
