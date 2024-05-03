object FormHilfeTexte: TFormHilfeTexte
  Left = 339
  Top = 201
  BorderStyle = bsDialog
  Caption = 'Hilfe'
  ClientHeight = 371
  ClientWidth = 396
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
  object pnBottom: TPanel
    Left = 0
    Top = 330
    Width = 396
    Height = 41
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object bbtnOk: TBitBtn
      Left = 16
      Top = 8
      Width = 145
      Height = 25
      Hint = 'Schlie'#223'en|Hinweisfenster schlie'#223'en'
      Caption = '&OK'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = bbtnOkClick
      Kind = bkOK
      Spacing = -1
    end
    object bbtnsave: TBitBtn
      Left = 232
      Top = 8
      Width = 145
      Height = 25
      Hint = 'Speichern|Inhalt des Memos in Datenbank speichern'
      Caption = '&Speichern'
      Enabled = False
      ModalResult = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = bbtnsaveClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333FFFFFFFFFFFFF33000077777770033377777777777773F000007888888
        00037F3337F3FF37F37F00000780088800037F3337F77F37F37F000007800888
        00037F3337F77FF7F37F00000788888800037F3337777777337F000000000000
        00037F3FFFFFFFFFFF7F00000000000000037F77777777777F7F000FFFFFFFFF
        00037F7F333333337F7F000FFFFFFFFF00037F7F333333337F7F000FFFFFFFFF
        00037F7F333333337F7F000FFFFFFFFF00037F7F333333337F7F000FFFFFFFFF
        00037F7F333333337F7F000FFFFFFFFF07037F7F33333333777F000FFFFFFFFF
        0003737FFFFFFFFF7F7330099999999900333777777777777733}
      NumGlyphs = 2
      Spacing = -1
    end
  end
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 396
    Height = 41
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Thema:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 1
  end
  object pnClient: TPanel
    Left = 0
    Top = 41
    Width = 396
    Height = 289
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
    object memoHelpText: TMemo
      Left = 5
      Top = 5
      Width = 386
      Height = 279
      Align = alClient
      BorderStyle = bsNone
      Lines.Strings = (
        '')
      TabOrder = 0
      OnChange = memoHelpTextChange
    end
  end
end
