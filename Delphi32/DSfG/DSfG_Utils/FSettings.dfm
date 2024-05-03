object FormEinstellungen: TFormEinstellungen
  Left = 357
  Top = 343
  BorderStyle = bsDialog
  Caption = 'Einstellungen'
  ClientHeight = 221
  ClientWidth = 268
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 268
    Height = 180
    ActivePage = tsheetEinstellungen
    Align = alClient
    TabOrder = 0
    object tsheetEinstellungen: TTabSheet
      Caption = '&Einstellungen'
      ImageIndex = 1
      object pnEinstellungen: TPanel
        Left = 0
        Top = 0
        Width = 260
        Height = 152
        Align = alClient
        BevelInner = bvLowered
        BevelOuter = bvNone
        BorderWidth = 4
        TabOrder = 0
        object Label1: TLabel
          Left = 16
          Top = 52
          Width = 43
          Height = 13
          Caption = 'Baudrate'
        end
        object Label2: TLabel
          Left = 16
          Top = 84
          Width = 52
          Height = 13
          Caption = 'Timeout [s]'
        end
        object Label5: TLabel
          Left = 16
          Top = 20
          Width = 55
          Height = 13
          Caption = 'Busadresse'
        end
        object Label3: TLabel
          Left = 16
          Top = 120
          Width = 55
          Height = 13
          Caption = 'COM (DPA)'
        end
        object cbBaudRate: TComboBox
          Left = 80
          Top = 48
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          Items.Strings = (
            '9600'
            '19200'
            '38400'
            '57600'
            '115200')
        end
        object speTimeOut: TSpinEdit
          Left = 80
          Top = 80
          Width = 81
          Height = 22
          MaxValue = 60
          MinValue = 5
          TabOrder = 1
          Value = 10
        end
        object eCardAddress: TEdit
          Left = 80
          Top = 16
          Width = 41
          Height = 21
          TabOrder = 2
        end
        object seCom: TSpinEdit
          Left = 80
          Top = 112
          Width = 49
          Height = 22
          MaxValue = 32
          MinValue = 1
          TabOrder = 3
          Value = 1
        end
      end
    end
  end
  object pnButtons: TPanel
    Left = 0
    Top = 180
    Width = 268
    Height = 41
    Align = alBottom
    BevelInner = bvLowered
    BorderWidth = 4
    TabOrder = 1
    object bbtnOk: TBitBtn
      Left = 16
      Top = 8
      Width = 105
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      OnClick = bbtnOkClick
      Kind = bkOK
    end
    object bbtnCancel: TBitBtn
      Left = 144
      Top = 8
      Width = 105
      Height = 25
      Caption = '&Abbrechen'
      TabOrder = 1
      Kind = bkCancel
    end
  end
end
