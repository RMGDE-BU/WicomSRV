object FormListenDialog: TFormListenDialog
  Left = 363
  Top = 195
  ActiveControl = lbSource
  BorderStyle = bsDialog
  Caption = 'Listenauswahl'
  ClientHeight = 328
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnBottom: TPanel
    Left = 0
    Top = 287
    Width = 446
    Height = 41
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object bbtnOk: TBitBtn
      Left = 32
      Top = 8
      Width = 92
      Height = 25
      Caption = '&OK'
      Enabled = False
      TabOrder = 0
      Kind = bkOK
      Spacing = -1
    end
    object bbtnCancel: TBitBtn
      Left = 320
      Top = 8
      Width = 92
      Height = 25
      Caption = '&Abbrechen'
      TabOrder = 1
      Kind = bkCancel
      Spacing = -1
    end
  end
  object pnClient: TPanel
    Left = 0
    Top = 0
    Width = 446
    Height = 287
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object Splitter: TSplitter
      Left = 201
      Top = 5
      Width = 3
      Height = 277
      Cursor = crHSplit
    end
    object lbSource: TListBox
      Left = 5
      Top = 5
      Width = 196
      Height = 277
      Align = alLeft
      BorderStyle = bsNone
      DragMode = dmAutomatic
      ItemHeight = 13
      TabOrder = 0
      OnDragDrop = ListboxDragDrop
      OnDragOver = ListboxDragOver
      OnKeyDown = lbSourceKeyDown
    end
    object pnSplitter: TPanel
      Left = 204
      Top = 5
      Width = 41
      Height = 277
      Align = alLeft
      TabOrder = 2
      object pnButtons: TPanel
        Left = 10
        Top = 64
        Width = 23
        Height = 142
        BevelOuter = bvNone
        TabOrder = 0
        object sbtnAllIn: TSpeedButton
          Left = 0
          Top = 0
          Width = 23
          Height = 22
          Hint = 'Alle wählen'
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            33333FF3333333333333447333333333333377FFF33333333333744473333333
            333337773FF3333333333444447333333333373F773FF3333333334444447333
            33333373F3773FF3333333744444447333333337F333773FF333333444444444
            733333373F3333773FF333334444444444733FFF7FFFFFFF77FF999999999999
            999977777777777733773333CCCCCCCCCC3333337333333F7733333CCCCCCCCC
            33333337F3333F773333333CCCCCCC3333333337333F7733333333CCCCCC3333
            333333733F77333333333CCCCC333333333337FF7733333333333CCC33333333
            33333777333333333333CC333333333333337733333333333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = sbtnAllInClick
        end
        object sbtnIn: TSpeedButton
          Left = 0
          Top = 40
          Width = 23
          Height = 22
          Hint = 'Wählen'
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333FF3333333333333003333
            3333333333773FF3333333333309003333333333337F773FF333333333099900
            33333FFFFF7F33773FF30000000999990033777777733333773F099999999999
            99007FFFFFFF33333F7700000009999900337777777F333F7733333333099900
            33333333337F3F77333333333309003333333333337F77333333333333003333
            3333333333773333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = sbtnInClick
        end
        object sbtnOut: TSpeedButton
          Left = 0
          Top = 80
          Width = 23
          Height = 22
          Hint = 'Abwählen'
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333FF3333333333333003333333333333F77F33333333333009033
            333333333F7737F333333333009990333333333F773337FFFFFF330099999000
            00003F773333377777770099999999999990773FF33333FFFFF7330099999000
            000033773FF33777777733330099903333333333773FF7F33333333333009033
            33333333337737F3333333333333003333333333333377333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = sbtnOutClick
        end
        object sbtnAllOut: TSpeedButton
          Left = 0
          Top = 120
          Width = 23
          Height = 22
          Hint = 'Alle abwählen'
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            333333333333333333FF3333333333333744333333333333F773333333333337
            44473333333333F777F3333333333744444333333333F7733733333333374444
            4433333333F77333733333333744444447333333F7733337F333333744444444
            433333F77333333733333744444444443333377FFFFFFF7FFFFF999999999999
            9999733777777777777333CCCCCCCCCC33333773FF333373F3333333CCCCCCCC
            C333333773FF3337F333333333CCCCCCC33333333773FF373F3333333333CCCC
            CC333333333773FF73F33333333333CCCCC3333333333773F7F3333333333333
            CCC333333333333777FF33333333333333CC3333333333333773}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = sbtnAllOutClick
        end
      end
    end
    object lbSink: TListBox
      Left = 245
      Top = 5
      Width = 196
      Height = 277
      Align = alClient
      BorderStyle = bsNone
      DragMode = dmAutomatic
      ItemHeight = 13
      TabOrder = 1
      OnDragDrop = ListboxDragDrop
      OnDragOver = ListboxDragOver
      OnKeyDown = lbSinkKeyDown
    end
  end
end
