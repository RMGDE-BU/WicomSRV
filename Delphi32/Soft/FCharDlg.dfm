object FormCharDialog: TFormCharDialog
  Left = 412
  Top = 266
  BorderStyle = bsDialog
  Caption = 'Trennzeichen einstellen'
  ClientHeight = 282
  ClientWidth = 234
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnBottom: TPanel
    Left = 0
    Top = 241
    Width = 234
    Height = 41
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object bbtnOk: TBitBtn
      Left = 8
      Top = 8
      Width = 105
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      OnClick = bbtnOkClick
      Kind = bkOK
      Spacing = -1
    end
    object bbtnCancel: TBitBtn
      Left = 120
      Top = 8
      Width = 105
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
    Width = 234
    Height = 241
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object rgrpSeparatorChar: TRadioGroup
      Left = 16
      Top = 8
      Width = 205
      Height = 161
      ItemIndex = 0
      Items.Strings = (
        'Tabstop'
        'Semikolon'
        'Komma'
        'Leerzeichen'
        'Andere:')
      TabOrder = 0
      TabStop = True
    end
    object eAnyChar: TEdit
      Left = 104
      Top = 136
      Width = 41
      Height = 21
      MaxLength = 1
      TabOrder = 1
    end
    object gboxFilename: TGroupBox
      Left = 16
      Top = 176
      Width = 205
      Height = 49
      Caption = 'Dateiname'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object eFileName: TEdit
        Left = 8
        Top = 20
        Width = 169
        Height = 21
        TabOrder = 0
      end
      object btnFileDlg: TButton
        Left = 177
        Top = 20
        Width = 21
        Height = 21
        Hint = 'Durchsuchen|Augabedatei einstellen'
        Caption = '...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btnFileDlgClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    Left = 184
    Top = 40
  end
end
