object FormSimpleListDlg: TFormSimpleListDlg
  Left = 370
  Top = 263
  BorderStyle = bsDialog
  Caption = 'Auswahl'
  ClientHeight = 429
  ClientWidth = 365
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
    Width = 365
    Height = 388
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object ListBox: TListBox
      Left = 5
      Top = 5
      Width = 355
      Height = 378
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 388
    Width = 365
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object bbtnOk: TBitBtn
      Left = 16
      Top = 10
      Width = 102
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      Kind = bkOK
      Spacing = -1
    end
    object bbtnCancel: TBitBtn
      Left = 248
      Top = 10
      Width = 102
      Height = 25
      Caption = '&Abbrechen'
      TabOrder = 1
      Kind = bkCancel
      Spacing = -1
    end
  end
end
