object FormMainWindowDefault: TFormMainWindowDefault
  Left = 71
  Top = 252
  Width = 870
  Height = 659
  HelpContext = 1
  Caption = 'Hauptfenster'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 41
    Align = alTop
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object pnTopRest: TPanel
      Left = 36
      Top = 5
      Width = 821
      Height = 31
      Align = alClient
      TabOrder = 0
    end
    object pnTop1: TPanel
      Left = 5
      Top = 5
      Width = 31
      Height = 31
      Align = alLeft
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 1
      object sbtnClose: TSpeedButton
        Left = 4
        Top = 4
        Width = 23
        Height = 23
        Action = aClose
        Flat = True
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
        ParentShowHint = False
        ShowHint = True
      end
    end
  end
  object pnClient: TPanel
    Left = 0
    Top = 41
    Width = 862
    Height = 553
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 594
    Width = 862
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object ActionList: TActionList
    Left = 112
    Top = 9
    object aHelpContents: THelpContents
      Category = 'Hilfe'
      Caption = '&Inhalt'
      Hint = 'Online-Hilfe|Inhaltsverzeichnis der Online-Hilfe anzeigen'
      OnExecute = aHelpContentsExecute
    end
    object aHelpInfo: TAction
      Category = 'Hilfe'
      Caption = 'Inf&o'
      Hint = 'Programmversion|Programmversion anzeigen'
      OnExecute = aHelpInfoExecute
    end
    object aClose: TAction
      Category = 'Funktionen'
      Caption = '&Beenden'
      Hint = 'Programm beenden'
      OnExecute = aCloseExecute
    end
  end
  object MainMenu: TMainMenu
    Left = 144
    Top = 9
    object miProgram: TMenuItem
      Caption = '&Programm'
      object miClose: TMenuItem
        Action = aClose
      end
    end
    object miHelp: TMenuItem
      Caption = '&Hilfe'
      object miContent: TMenuItem
        Action = aHelpContents
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miInfo: TMenuItem
        Action = aHelpInfo
      end
    end
  end
  object DialogInfo: TDialogInfo
    Lizenz32 = False
    ReadResource = False
    Left = 176
    Top = 9
  end
end
