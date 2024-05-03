object FormGDPrintPreview: TFormGDPrintPreview
  Left = 254
  Top = 354
  Width = 816
  Height = 1000
  Caption = 'Druckvorschau'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox: TScrollBox
    Left = 0
    Top = 41
    Width = 808
    Height = 932
    Align = alClient
    TabOrder = 0
  end
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 808
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object sbtnBack: TSpeedButton
      Left = 176
      Top = 8
      Width = 23
      Height = 22
      Hint = 'Zur'#252'ck'
      Caption = '<'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = sbtnBackClick
    end
    object sbtnFwd: TSpeedButton
      Left = 208
      Top = 8
      Width = 23
      Height = 22
      Hint = 'Vor'
      Caption = '>'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = sbtnFwdClick
    end
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 24
      Height = 13
      Caption = 'Seite'
    end
    object Label2: TLabel
      Left = 96
      Top = 16
      Width = 18
      Height = 13
      Caption = 'von'
    end
    object sbtnPrinterSettings: TSpeedButton
      Left = 416
      Top = 8
      Width = 97
      Height = 22
      Hint = 'Druckereinstellungen'
      Caption = 'Einstellungen'
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
        0003377777777777777308888888888888807F33333333333337088888888888
        88807FFFFFFFFFFFFFF7000000000000000077777777777777770F8F8F8F8F8F
        8F807F333333333333F708F8F8F8F8F8F9F07F333333333337370F8F8F8F8F8F
        8F807FFFFFFFFFFFFFF7000000000000000077777777777777773330FFFFFFFF
        03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
        03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
        33333337F3F37F3733333330F08F0F0333333337F7337F7333333330FFFF0033
        33333337FFFF7733333333300000033333333337777773333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      Spacing = -1
      OnClick = sbtnPrinterSettingsClick
    end
    object sbtnPrint: TSpeedButton
      Left = 520
      Top = 8
      Width = 97
      Height = 22
      Hint = 'Drucken'
      Caption = 'Drucken'
      Flat = True
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
      ParentShowHint = False
      ShowHint = False
      Spacing = -1
      OnClick = sbtnPrintClick
    end
    object pnPageAct: TPanel
      Left = 48
      Top = 8
      Width = 41
      Height = 21
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnPageMax: TPanel
      Left = 120
      Top = 8
      Width = 41
      Height = 21
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 1
    end
    object pnActPrinter: TPanel
      Left = 240
      Top = 8
      Width = 169
      Height = 21
      Alignment = taLeftJustify
      BevelInner = bvLowered
      BevelOuter = bvNone
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
  end
end
