object FormGSMModemInitDlg: TFormGSMModemInitDlg
  Left = 634
  Top = 403
  Width = 344
  Height = 250
  Caption = 'GSM-Modeminitialisierung'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnClient: TPanel
    Left = 0
    Top = 0
    Width = 336
    Height = 175
    Align = alClient
    TabOrder = 0
    object lInfo: TLabel
      Left = 16
      Top = 144
      Width = 276
      Height = 13
      Caption = 'Bitte Verbindung mit serieller Modemschnittstelle herstellen.'
    end
    object gbModem: TGroupBox
      Left = 16
      Top = 16
      Width = 305
      Height = 113
      Caption = 'Modem'
      TabOrder = 0
      object lReset: TLabel
        Left = 16
        Top = 20
        Width = 113
        Height = 13
        Caption = 'Werkseinstellung laden:'
      end
      object lInit: TLabel
        Left = 16
        Top = 52
        Width = 63
        Height = 13
        Caption = 'Initialisierung:'
      end
      object lPIN: TLabel
        Left = 16
        Top = 84
        Width = 21
        Height = 13
        Caption = 'PIN:'
      end
      object eReset: TEdit
        Left = 144
        Top = 16
        Width = 145
        Height = 21
        TabOrder = 0
        Text = 'at&f'
      end
      object eInit: TEdit
        Left = 144
        Top = 48
        Width = 145
        Height = 21
        TabOrder = 1
        Text = 'ate0'
      end
      object ePIN: TEdit
        Left = 144
        Top = 80
        Width = 65
        Height = 21
        MaxLength = 8
        TabOrder = 2
      end
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 175
    Width = 336
    Height = 41
    Align = alBottom
    TabOrder = 1
    object bbtnAbbruch: TBitBtn
      Left = 192
      Top = 8
      Width = 102
      Height = 25
      TabOrder = 1
      Kind = bkCancel
      Spacing = -1
    end
    object bbtnOK: TBitBtn
      Left = 40
      Top = 8
      Width = 102
      Height = 25
      TabOrder = 0
      Kind = bkOK
      Spacing = -1
    end
  end
end
