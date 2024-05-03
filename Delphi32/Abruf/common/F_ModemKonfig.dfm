object FormModemKonfig: TFormModemKonfig
  Left = 245
  Top = 263
  BorderStyle = bsDialog
  Caption = 'Modem konfigurieren'
  ClientHeight = 251
  ClientWidth = 551
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
    Width = 551
    Height = 210
    Align = alClient
    TabOrder = 0
    object Label5: TLabel
      Left = 16
      Top = 48
      Width = 52
      Height = 13
      Caption = 'Modem&typ:'
      FocusControl = cbModemTyp
    end
    object Label3: TLabel
      Left = 16
      Top = 112
      Width = 72
      Height = 13
      Caption = 'Max. &Baudrate:'
      FocusControl = cbMaxBaudrate
    end
    object Label4: TLabel
      Left = 16
      Top = 80
      Width = 63
      Height = 13
      Caption = '&Initialisierung:'
      FocusControl = eInit
    end
    object Label15: TLabel
      Left = 16
      Top = 16
      Width = 97
      Height = 13
      Caption = 'S&erielle Schnittstelle:'
      FocusControl = cbCOM
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowFrame
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object sbtnCheckCOMs: TSpeedButton
      Left = 216
      Top = 16
      Width = 23
      Height = 22
      Hint = 'Vorhandene serielle Schnittstellen aktualisieren'
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
      ParentShowHint = False
      ShowHint = True
      OnClick = sbtnCheckCOMsClick
    end
    object lPIN: TLabel
      Left = 280
      Top = 115
      Width = 21
      Height = 13
      Caption = '&PIN:'
      FocusControl = ePIN
    end
    object cbModemTyp: TComboBox
      Left = 128
      Top = 48
      Width = 409
      Height = 21
      Style = csDropDownList
      DropDownCount = 40
      ItemHeight = 13
      TabOrder = 1
      OnChange = cbModemTypChange
    end
    object cbMaxBaudrate: TComboBox
      Left = 128
      Top = 112
      Width = 121
      Height = 21
      Style = csDropDownList
      DropDownCount = 10
      ItemHeight = 13
      TabOrder = 3
      OnChange = cbMaxBaudrateChange
      Items.Strings = (
        '2400'
        '4800'
        '9600'
        '19200'
        '38400'
        '57600'
        '115200')
    end
    object eInit: TEdit
      Left = 128
      Top = 80
      Width = 409
      Height = 21
      TabOrder = 2
      OnChange = eInitChange
    end
    object rgrpWahlverfahren: TRadioGroup
      Left = 16
      Top = 144
      Width = 521
      Height = 49
      Caption = 'Wahlverfahren'
      Columns = 2
      Items.Strings = (
        'Tonwahl'
        'Pulswahl')
      TabOrder = 5
      OnClick = rgrpWahlverfahrenClick
    end
    object cbCOM: TComboBox
      Left = 128
      Top = 16
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbCOMChange
    end
    object ePIN: TEdit
      Left = 312
      Top = 112
      Width = 73
      Height = 21
      MaxLength = 8
      TabOrder = 4
      OnChange = ePINChange
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 210
    Width = 551
    Height = 41
    Align = alBottom
    TabOrder = 1
    object bbtnAbbruch: TBitBtn
      Left = 312
      Top = 8
      Width = 99
      Height = 25
      Cancel = True
      Caption = '&Abbrechen'
      ModalResult = 2
      TabOrder = 1
      OnClick = bbtnAbbruchClick
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333000033338833333333333333333F333333333333
        0000333911833333983333333388F333333F3333000033391118333911833333
        38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
        911118111118333338F3338F833338F3000033333911111111833333338F3338
        3333F8330000333333911111183333333338F333333F83330000333333311111
        8333333333338F3333383333000033333339111183333333333338F333833333
        00003333339111118333333333333833338F3333000033333911181118333333
        33338333338F333300003333911183911183333333383338F338F33300003333
        9118333911183333338F33838F338F33000033333913333391113333338FF833
        38F338F300003333333333333919333333388333338FFF830000333333333333
        3333333333333333333888330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
    object bbtnSpeichern: TBitBtn
      Left = 136
      Top = 8
      Width = 99
      Height = 25
      Caption = '&Speichern'
      TabOrder = 0
      OnClick = bbtnSpeichernClick
      Kind = bkOK
    end
  end
end
