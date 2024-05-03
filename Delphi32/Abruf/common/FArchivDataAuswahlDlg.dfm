inherited FormArchivdatenAuswahlDlg: TFormArchivdatenAuswahlDlg
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnClient: TPanel
    object bbtnAktualisieren: TBitBtn
      Left = 304
      Top = 5
      Width = 106
      Height = 25
      Hint = 'Archivinformationen aktualisieren'
      Caption = '&Aktualisieren'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = bbtnAktualisierenClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333FFFFFFFFFFFFF3344444444444443337777777777777F334CCCCCCCCCC
        C43337777777777777F33444881B188444333777F3737337773333308881FF70
        33333337F3373337F3333330888BF770333333373F33F337333333330881F703
        3333333373F73F7333333333308B703333333333373F77333333333333080333
        3333333333777FF333333333301F103333333333377777FF3333333301B1F103
        333333337737777FF3333330881BFB7033333337F3737F77F333333088881F70
        333333F7F3337777FFF334448888888444333777FFFFFFF777F334CCCCCCCCCC
        C43337777777777777F334444444444444333777777777777733}
      NumGlyphs = 2
      Spacing = -1
    end
  end
  inherited pnBottom: TPanel
    inherited pnBottomBtnOk: TPanel
      Left = 28
    end
    object Panel1: TPanel
      Left = 276
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
        Kind = bkCancel
        Spacing = -1
      end
    end
  end
end
