object FormComObjectControl: TFormComObjectControl
  Left = 239
  Top = 219
  Width = 379
  Height = 228
  Caption = 'Verf'#252'gbare COM-Server'
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
    Width = 371
    Height = 153
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object ScrollBox: TScrollBox
      Left = 5
      Top = 5
      Width = 361
      Height = 143
      Align = alClient
      BorderStyle = bsNone
      Color = clHighlightText
      ParentColor = False
      TabOrder = 0
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 153
    Width = 371
    Height = 41
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    OnResize = pnBottomResize
    object bbtnOk: TBitBtn
      Left = 128
      Top = 8
      Width = 92
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      Kind = bkOK
      Spacing = -1
    end
  end
end
