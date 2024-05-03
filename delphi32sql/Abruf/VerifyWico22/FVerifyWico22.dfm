inherited FormVerifyWico22: TFormVerifyWico22
  Left = 264
  Top = 502
  Width = 549
  Height = 340
  Caption = #220'berwachung von WICO22'
  OldCreateOrder = True
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TStatusBar
    Top = 294
    Width = 541
  end
  inherited pnTop: TPanel
    Width = 541
    inherited pnTop1: TPanel
      Width = 68
      BevelOuter = bvNone
      object sbtnReboot: TSpeedButton
        Left = 36
        Top = 4
        Width = 23
        Height = 23
        Action = aReboot
        Flat = True
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
          33333337777FF377FF3333993370739993333377FF373F377FF3399993000339
          993337777F777F3377F3393999707333993337F77737333337FF993399933333
          399377F3777FF333377F993339903333399377F33737FF33377F993333707333
          399377F333377FF3377F993333101933399377F333777FFF377F993333000993
          399377FF3377737FF7733993330009993933373FF3777377F7F3399933000399
          99333773FF777F777733339993707339933333773FF7FFF77333333999999999
          3333333777333777333333333999993333333333377777333333}
        NumGlyphs = 2
        ParentShowHint = False
        ShowHint = True
      end
    end
    inherited pnTopRest: TPanel
      Left = 129
      Width = 135
      BevelOuter = bvNone
      Font.Color = clNavy
      Font.Height = -13
      Font.Style = [fsBold]
      ParentFont = False
    end
    object pnStatus: TPanel
      Left = 264
      Top = 5
      Width = 272
      Height = 31
      Align = alRight
      Alignment = taRightJustify
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 4
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object pnSystemStates: TPanel
      Left = 73
      Top = 5
      Width = 56
      Height = 31
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      object shMyState: TShape
        Left = 8
        Top = 5
        Width = 20
        Height = 20
        Brush.Color = clBtnFace
        Shape = stCircle
      end
      object shOtherState: TShape
        Left = 32
        Top = 5
        Width = 20
        Height = 20
        Brush.Color = clBtnFace
        Shape = stCircle
      end
    end
  end
  object pnClient: TPanel [2]
    Left = 0
    Top = 41
    Width = 541
    Height = 253
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = 'pnClient'
    TabOrder = 2
    object PageControl: TPageControl
      Left = 1
      Top = 1
      Width = 539
      Height = 251
      Align = alClient
      MultiLine = True
      TabOrder = 0
      TabPosition = tpLeft
      OnChange = PageControlChange
    end
  end
  inherited Timer: TTimer
    Left = 8
    Top = 48
  end
  inherited PopupMenu: TPopupMenu
    Left = 40
    Top = 48
  end
  inherited DialogInfo: TDialogInfo
    Version = '1.01'
    Left = 72
    Top = 48
  end
  inherited ImageList: TImageList
    Left = 104
    Top = 48
  end
  object ActionList: TActionList
    Left = 136
    Top = 49
    object aReboot: TAction
      Caption = 'Rechner booten'
      OnExecute = aRebootExecute
    end
    object aShowJounal: TAction
      Caption = 'Journal'
      Hint = 'Journal anzeigen|Journal dieser Instanz anzeigen'
    end
  end
end
