object FormDialogDefault: TFormDialogDefault
  Left = 243
  Top = 145
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 570
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnClient: TPanel
    Left = 0
    Top = 0
    Width = 719
    Height = 529
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
  end
  object pnBottom: TPanel
    Left = 0
    Top = 529
    Width = 719
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object pnBottomBtnOk: TPanel
      Left = 300
      Top = 4
      Width = 100
      Height = 33
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object bbtnOk: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Caption = '&OK'
        TabOrder = 0
        Kind = bkOK
        Spacing = -1
      end
    end
  end
end
