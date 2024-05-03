inherited FormMemoData: TFormMemoData
  Left = 505
  Top = 210
  Width = 695
  Height = 578
  Caption = 'Memodaten'
  OldCreateOrder = True
  Position = poDesigned
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnClient: TPanel
    Width = 577
    Height = 551
    object reMemoText: TRichEdit
      Left = 5
      Top = 5
      Width = 567
      Height = 541
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      PlainText = True
      ScrollBars = ssBoth
      TabOrder = 0
      WantReturns = False
      WordWrap = False
    end
  end
  inherited pnRight: TPanel
    Left = 577
    Width = 110
    Height = 551
    inherited pnRight1: TPanel
      Top = 38
      Width = 100
      Height = 15
      BevelInner = bvNone
      BorderWidth = 0
    end
    inherited pnRightRest: TPanel
      Top = 185
      Width = 100
      Height = 361
    end
    object pnClose: TPanel
      Left = 5
      Top = 5
      Width = 100
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 2
      object bbtnClose: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Hint = 'Fenster schlie'#223'en|Dieses Fenster schlie'#223'en'
        Caption = '&Schlie'#223'en'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = bbtnCloseClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          03333377777777777F333301111111110333337F333333337F33330111111111
          0333337F333333337F333301111111110333337F333333337F33330111111111
          0333337F333333337F333301111111110333337F333333337F33330111111111
          0333337F3333333F7F333301111111B10333337F333333737F33330111111111
          0333337F333333337F333301111111110333337F33FFFFF37F3333011EEEEE11
          0333337F377777F37F3333011EEEEE110333337F37FFF7F37F3333011EEEEE11
          0333337F377777337F333301111111110333337F333333337F33330111111111
          0333337FFFFFFFFF7F3333000000000003333377777777777333}
        NumGlyphs = 2
        Spacing = -1
      end
    end
    object pnRight3: TPanel
      Left = 5
      Top = 53
      Width = 100
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 3
      object bbtnPrint: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Hint = 'Drucken|Aktuell angezeigte Daten drucken'
        Caption = '&Drucken'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = bbtnPrintClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
          00033FFFFFFFFFFFFFFF0888888888888880777777777777777F088888888888
          8880777777777777777F0000000000000000FFFFFFFFFFFFFFFF0F8F8F8F8F8F
          8F80777777777777777F08F8F8F8F8F8F9F0777777777777777F0F8F8F8F8F8F
          8F807777777777777F7F0000000000000000777777777777777F3330FFFFFFFF
          03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
          03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
          33333337F3FF7F3733333330F08F0F0333333337F7737F7333333330FFFF0033
          33333337FFFF7733333333300000033333333337777773333333}
        NumGlyphs = 2
        Spacing = -1
      end
    end
    object pnAsciiExport: TPanel
      Left = 5
      Top = 86
      Width = 100
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 4
      object bbtnAsciiExport: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Hint = 'ASCII-Export|Aktuell angezeigte Daten in ASCII exportieren'
        Caption = '&ASCII'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = bbtnAsciiExportClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333303
          333333333333337FF3333333333333903333333333333377FF33333333333399
          03333FFFFFFFFF777FF3000000999999903377777777777777FF0FFFF0999999
          99037F3337777777777F0FFFF099999999907F3FF777777777770F00F0999999
          99037F773777777777730FFFF099999990337F3FF777777777330F00FFFFF099
          03337F773333377773330FFFFFFFF09033337F3FF3FFF77733330F00F0000003
          33337F773777777333330FFFF0FF033333337F3FF7F3733333330F08F0F03333
          33337F7737F7333333330FFFF003333333337FFFF77333333333000000333333
          3333777777333333333333333333333333333333333333333333}
        NumGlyphs = 2
        Spacing = -1
      end
    end
    object pnRight5: TPanel
      Left = 5
      Top = 119
      Width = 100
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 5
      object bbtnExcel: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Hint = 'Excel|Daten in Excel exportieren'
        Caption = '&Excel'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = bbtnExcelClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          0400000000000001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          33333333333333333333300000000000333337777777777733333077777EEEE0
          733337FFFFF88887F333330777EEEE077733337FFF88887FFF3333307EEEE077
          77733337F88887FFFFF33333EEEE003333333333888877333333333EEEE07703
          333333388887FF73333333EEEE07777033333388887FFFF733333EEEE0307777
          033338888737FFFF7333EEEE033307777033888873337FFFF733333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          3333333333333333333333333333333333333333333333333333}
        NumGlyphs = 2
        Spacing = -1
      end
    end
    object pnRight6: TPanel
      Left = 5
      Top = 152
      Width = 100
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 6
      Visible = False
      object bbtnDelete: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Hint = 'L'#246'schen|Daten aus Archiv l'#246'schen'
        Caption = '&L'#246'schen'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = bbtnDeleteClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          333333333333333333FF33333333333330003333333333333777333333333333
          300033FFFFFF3333377739999993333333333777777F3333333F399999933333
          3300377777733333337733333333333333003333333333333377333333333333
          3333333333333333333F333333333333330033333F33333333773333C3333333
          330033337F3333333377333CC3333333333333F77FFFFFFF3FF33CCCCCCCCCC3
          993337777777777F77F33CCCCCCCCCC399333777777777737733333CC3333333
          333333377F33333333FF3333C333333330003333733333333777333333333333
          3000333333333333377733333333333333333333333333333333}
        NumGlyphs = 2
        Spacing = -1
      end
    end
  end
end
