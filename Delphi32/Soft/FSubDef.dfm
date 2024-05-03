object FormSubWindowDefault: TFormSubWindowDefault
  Left = 128
  Top = 81
  Width = 870
  Height = 640
  Caption = 'Unterfenster'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnClient: TPanel
    Left = 0
    Top = 0
    Width = 742
    Height = 613
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
  end
  object pnRight: TPanel
    Left = 742
    Top = 0
    Width = 120
    Height = 613
    Align = alRight
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object pnRight1: TPanel
      Left = 5
      Top = 5
      Width = 110
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 0
    end
    object pnRightRest: TPanel
      Left = 5
      Top = 38
      Width = 110
      Height = 570
      Align = alClient
      TabOrder = 1
    end
  end
end
