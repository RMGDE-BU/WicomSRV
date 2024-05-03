object FormDiagramm: TFormDiagramm
  Left = 333
  Top = 251
  Width = 637
  Height = 614
  Caption = 'Diagramm'
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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnRight: TPanel
    Left = 519
    Top = 0
    Width = 110
    Height = 587
    Align = alRight
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object pnRightRest: TPanel
      Left = 5
      Top = 544
      Width = 100
      Height = 38
      Align = alClient
      TabOrder = 3
    end
    object pnRight1: TPanel
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
        Hint = 'Schlie'#223'en|Fenster schlie'#223'en'
        Caption = 'S&chlie'#223'en'
        ModalResult = 1
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
    object pnRight2: TPanel
      Left = 5
      Top = 502
      Width = 100
      Height = 42
      Align = alTop
      BorderWidth = 2
      Caption = 'pnRight2'
      TabOrder = 0
      object chlbSeriesList: TCheckListBox
        Left = 3
        Top = 3
        Width = 94
        Height = 36
        Align = alClient
        BorderStyle = bsNone
        Flat = False
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object pnRight3: TPanel
      Left = 5
      Top = 170
      Width = 100
      Height = 200
      Align = alTop
      TabOrder = 1
      object Label6: TLabel
        Left = 4
        Top = 2
        Width = 35
        Height = 13
        Caption = 'Aktuell:'
      end
      object gbMinMax: TGroupBox
        Left = 1
        Top = 36
        Width = 98
        Height = 160
        TabOrder = 1
        object Label1: TLabel
          Left = 8
          Top = 16
          Width = 30
          Height = 13
          Caption = 'X-Min:'
        end
        object Label2: TLabel
          Left = 8
          Top = 52
          Width = 33
          Height = 13
          Caption = 'X-Max:'
        end
        object Label3: TLabel
          Left = 8
          Top = 88
          Width = 30
          Height = 13
          Caption = 'Y-Min:'
        end
        object Label4: TLabel
          Left = 8
          Top = 124
          Width = 33
          Height = 13
          Caption = 'Y-Max:'
        end
        object eXMin: TEdit
          Left = 0
          Top = 28
          Width = 98
          Height = 21
          TabOrder = 0
          OnDblClick = eXChange
          OnExit = EditExit
          OnKeyDown = EditKeyDown
        end
        object eXMax: TEdit
          Left = 0
          Top = 64
          Width = 98
          Height = 21
          TabOrder = 1
          OnDblClick = eXChange
          OnExit = EditExit
          OnKeyDown = EditKeyDown
        end
        object eYMin: TEdit
          Left = 0
          Top = 100
          Width = 98
          Height = 21
          TabOrder = 2
          OnExit = EditExit
          OnKeyDown = EditKeyDown
        end
        object eYMax: TEdit
          Left = 0
          Top = 136
          Width = 98
          Height = 21
          TabOrder = 3
          OnExit = EditExit
          OnKeyDown = EditKeyDown
        end
      end
      object chbMinMax: TCheckBox
        Left = 8
        Top = 36
        Width = 65
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Min/Max'
        TabOrder = 0
        OnClick = chbMinMaxClick
      end
      object cbAktSeries: TComboBox
        Left = 2
        Top = 14
        Width = 96
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnChange = cbAktSeriesChange
      end
    end
    object pnRight4: TPanel
      Left = 5
      Top = 370
      Width = 100
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 4
      object bbtnAktualisieren: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Hint = 
          'Anzeige aktualisieren|Anzeige wird mit aktuellen Einstellungen a' +
          'ktualisiert'
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
    object pnPrint: TPanel
      Left = 5
      Top = 403
      Width = 100
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 5
      object bbtnPrint: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Hint = 'Diagramme drucken|angezeigte Diagramme drucken'
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
    object pnCopyToClipboard: TPanel
      Left = 5
      Top = 469
      Width = 100
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 6
      object bbtnCopyToClipboard: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Hint = 'In Zwischenablage kopieren|Diagramm in Zwischenablage kopieren'
        Caption = '&Kopieren'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = bbtnCopyToClipboardClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003333330B7FFF
          FFB0333333777F3333773333330B7FFFFFB0333333777F3333773333330B7FFF
          FFB0333333777F3333773333330B7FFFFFB03FFFFF777FFFFF77000000000077
          007077777777777777770FFFFFFFF00077B07F33333337FFFF770FFFFFFFF000
          7BB07F3FF3FFF77FF7770F00F000F00090077F77377737777F770FFFFFFFF039
          99337F3FFFF3F7F777FF0F0000F0F09999937F7777373777777F0FFFFFFFF999
          99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
          99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
          93337FFFF7737777733300000033333333337777773333333333}
        NumGlyphs = 2
        Spacing = -1
      end
    end
    object pnSave: TPanel
      Left = 5
      Top = 436
      Width = 100
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 7
      object bbtnSave: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Hint = 'In Datei speichern|Oberstes Diagramm in Bitmapdatei speichern'
        Caption = '&Speichern'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = bbtnSaveClick
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
    object pnRight5: TPanel
      Left = 5
      Top = 38
      Width = 100
      Height = 132
      Align = alTop
      AutoSize = True
      TabOrder = 8
      object gbAnzeige: TGroupBox
        Left = 1
        Top = 1
        Width = 98
        Height = 130
        Align = alClient
        Caption = 'Anzeige'
        TabOrder = 0
        object Label5: TLabel
          Left = 4
          Top = 84
          Width = 64
          Height = 13
          Caption = 'Diagrammtyp:'
        end
        object Bevel1: TBevel
          Left = 2
          Top = 14
          Width = 93
          Height = 66
        end
        object Bevel2: TBevel
          Left = 2
          Top = 82
          Width = 93
          Height = 42
        end
        object Bevel3: TBevel
          Left = 6
          Top = 46
          Width = 84
          Height = 10
          Shape = bsBottomLine
        end
        object rbtnKombiGraphik: TRadioButton
          Tag = 1
          Left = 4
          Top = 36
          Width = 83
          Height = 17
          Caption = 'Kombigraphik'
          TabOrder = 0
          OnClick = rgAufteilungClick
        end
        object rbtnEinzelGraphik: TRadioButton
          Left = 4
          Top = 16
          Width = 83
          Height = 17
          Caption = 'Einzelgraphik'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnClick = rgAufteilungClick
        end
        object chbSameAxis: TCheckBox
          Left = 4
          Top = 60
          Width = 86
          Height = 17
          Caption = 'Achsen gleich'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = chbSameAxisClick
        end
        object cbSeriesStyle: TComboBox
          Left = 4
          Top = 98
          Width = 90
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
          OnChange = cbSeriesStyleChange
          Items.Strings = (
            'Linien'
            'Balken'
            'Striche'
            'Punkte')
        end
      end
    end
  end
  object pnClient: TPanel
    Left = 0
    Top = 0
    Width = 519
    Height = 587
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'BMP'
    Filter = 'Bitmap-Dateien|*.BMP|Alle Dateien|*.*'
    Left = 128
    Top = 40
  end
  object PopupMenuCharts: TPopupMenu
    Left = 160
    Top = 40
    object pmiCopyToClipboard: TMenuItem
      Caption = '&Kopieren'
      OnClick = pmiCopyToClipboardClick
    end
    object pmiPrint: TMenuItem
      Caption = '&Drucken'
      OnClick = pmiPrintClick
    end
    object pmiSave: TMenuItem
      Caption = '&Speichern'
      OnClick = pmiSaveClick
    end
  end
end
