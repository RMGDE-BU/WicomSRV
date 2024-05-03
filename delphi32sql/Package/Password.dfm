object FormPasswordDlg: TFormPasswordDlg
  Left = 240
  Top = 117
  BorderStyle = bsDialog
  Caption = 'Anmeldung'
  ClientHeight = 153
  ClientWidth = 230
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnClient: TPanel
    Left = 0
    Top = 0
    Width = 230
    Height = 112
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object Label4: TLabel
      Left = 24
      Top = 16
      Width = 48
      Height = 13
      Caption = 'Anwender'
    end
    object Label2: TLabel
      Left = 24
      Top = 56
      Width = 43
      Height = 13
      Caption = 'Passwort'
    end
    object cbUsernames: TComboBox
      Left = 32
      Top = 32
      Width = 170
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'Keine (Gast)'
        'Normal (Sichtmodus)'
        'Erweitert (Parametriermodus)'
        'Administrator (Systemkonfig.)')
    end
    object ePassword: TEdit
      Left = 32
      Top = 72
      Width = 170
      Height = 21
      MaxLength = 20
      PasswordChar = '*'
      TabOrder = 1
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 112
    Width = 230
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object pnBottomBtnOk: TPanel
      Left = 4
      Top = 4
      Width = 100
      Height = 33
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object bbtnOk: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        Caption = '&OK'
        TabOrder = 0
        OnClick = bbtnOkClick
        Kind = bkOK
        Spacing = -1
      end
    end
    object Panel1: TPanel
      Left = 126
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
