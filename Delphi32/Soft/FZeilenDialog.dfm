object FormZeilenDialog: TFormZeilenDialog
  Left = 287
  Top = 248
  BorderStyle = bsDialog
  Caption = 'FormZeilenDialog'
  ClientHeight = 414
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnClient: TPanel
    Left = 0
    Top = 0
    Width = 410
    Height = 373
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
  end
  object pnBottom: TPanel
    Left = 0
    Top = 373
    Width = 410
    Height = 41
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object bbtnOk: TBitBtn
      Left = 32
      Top = 8
      Width = 92
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      Kind = bkOK
      Spacing = -1
    end
    object bbtnCancel: TBitBtn
      Left = 280
      Top = 8
      Width = 92
      Height = 25
      Caption = '&Abbrechen'
      TabOrder = 1
      Kind = bkCancel
      Spacing = -1
    end
  end
end
