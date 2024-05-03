object FormAssistentDefault: TFormAssistentDefault
  Left = 318
  Top = 519
  BorderStyle = bsDialog
  Caption = 'Assistent'
  ClientHeight = 182
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnClient: TPanel
    Left = 0
    Top = 0
    Width = 449
    Height = 182
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object bbtnGoOn: TBitBtn
      Left = 136
      Top = 144
      Width = 121
      Height = 25
      Caption = 'Weiter'
      TabOrder = 0
      Kind = bkOK
      Spacing = -1
    end
    object bbtnCancel: TBitBtn
      Left = 312
      Top = 144
      Width = 121
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
    object Panel1: TPanel
      Left = 132
      Top = 16
      Width = 305
      Height = 105
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 4
      Caption = 'Panel1'
      TabOrder = 2
      object Memo: TMemo
        Left = 5
        Top = 5
        Width = 295
        Height = 95
        Align = alClient
        BorderStyle = bsNone
        Color = clBtnFace
        TabOrder = 0
      end
    end
    object Panel2: TPanel
      Left = 40
      Top = 48
      Width = 57
      Height = 57
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 4
      TabOrder = 3
      object Image: TImage
        Left = 5
        Top = 5
        Width = 47
        Height = 47
        Align = alClient
        Stretch = True
      end
    end
  end
end
