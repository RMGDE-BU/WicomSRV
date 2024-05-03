inherited FormTelegrIec101: TFormTelegrIec101
  Left = 288
  Top = 187
  Caption = 'Telegrammanalyse IEC 870-5-101'
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 16
    Top = 16
    Width = 177
    Height = 225
    Caption = 'Telegramm-Empfang'
    TabOrder = 0
    object lStart: TLabel
      Left = 8
      Top = 50
      Width = 59
      Height = 13
      Caption = 'Startzeichen'
    end
    object lLaenge: TLabel
      Left = 8
      Top = 74
      Width = 30
      Height = 13
      Caption = 'L'#228'nge'
    end
    object lSteuerfeld: TLabel
      Left = 8
      Top = 98
      Width = 48
      Height = 13
      Caption = 'Steuerfeld'
    end
    object lAdressfeld: TLabel
      Left = 8
      Top = 122
      Width = 49
      Height = 13
      Caption = 'Adressfeld'
    end
    object lTypkennung: TLabel
      Left = 8
      Top = 146
      Width = 60
      Height = 13
      Caption = 'Typkennung'
    end
    object lChecksum: TLabel
      Left = 8
      Top = 170
      Width = 64
      Height = 13
      Caption = 'Checksumme'
    end
    object lEnde: TLabel
      Left = 8
      Top = 194
      Width = 62
      Height = 13
      Caption = 'Endezeichen'
    end
    object lEinzel: TLabel
      Left = 8
      Top = 26
      Width = 65
      Height = 13
      Caption = 'Einzelzeichen'
    end
    object EStart1: TEdit
      Tag = 102
      Left = 96
      Top = 48
      Width = 33
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object ELaenge1: TEdit
      Tag = 104
      Left = 96
      Top = 72
      Width = 33
      Height = 21
      ReadOnly = True
      TabOrder = 3
    end
    object ESteuerfeld: TEdit
      Tag = 106
      Left = 96
      Top = 96
      Width = 73
      Height = 21
      ReadOnly = True
      TabOrder = 5
    end
    object EAdressfeldLow: TEdit
      Tag = 107
      Left = 96
      Top = 120
      Width = 33
      Height = 21
      ReadOnly = True
      TabOrder = 6
    end
    object ETypkennung: TEdit
      Tag = 109
      Left = 96
      Top = 144
      Width = 73
      Height = 21
      ReadOnly = True
      TabOrder = 8
    end
    object EChecksum: TEdit
      Tag = 110
      Left = 96
      Top = 168
      Width = 73
      Height = 21
      ReadOnly = True
      TabOrder = 9
    end
    object EEnde: TEdit
      Tag = 111
      Left = 96
      Top = 192
      Width = 73
      Height = 21
      ReadOnly = True
      TabOrder = 10
    end
    object EAdressfeldHigh: TEdit
      Tag = 108
      Left = 136
      Top = 120
      Width = 33
      Height = 21
      ReadOnly = True
      TabOrder = 7
    end
    object ELaenge2: TEdit
      Tag = 105
      Left = 136
      Top = 72
      Width = 33
      Height = 21
      ReadOnly = True
      TabOrder = 4
    end
    object EStart2: TEdit
      Tag = 103
      Left = 136
      Top = 48
      Width = 33
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
    object eEinzel: TEdit
      Tag = 101
      Left = 96
      Top = 24
      Width = 33
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 208
    Top = 16
    Width = 201
    Height = 225
    Caption = 'Status'
    TabOrder = 1
    object Label8: TLabel
      Left = 8
      Top = 26
      Width = 68
      Height = 13
      Caption = 'Formatpr'#252'fung'
    end
    object lFktCode_dez: TLabel
      Left = 8
      Top = 98
      Width = 70
      Height = 13
      Caption = 'Funktionscode'
    end
    object lLinienNr_dez: TLabel
      Left = 8
      Top = 122
      Width = 65
      Height = 13
      Caption = 'Liniennummer'
    end
    object lTypkennung_dez: TLabel
      Left = 8
      Top = 146
      Width = 60
      Height = 13
      Caption = 'Typkennung'
    end
    object lTimeout: TLabel
      Left = 8
      Top = 194
      Width = 38
      Height = 13
      Caption = 'Timeout'
    end
    object lDFC: TLabel
      Left = 136
      Top = 98
      Width = 21
      Height = 13
      Caption = 'DFC'
    end
    object EFormatPruefung: TEdit
      Tag = 201
      Left = 88
      Top = 24
      Width = 105
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object EFktCode_dez: TEdit
      Tag = 202
      Left = 88
      Top = 96
      Width = 25
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object ELinienNr_dez: TEdit
      Tag = 204
      Left = 88
      Top = 120
      Width = 105
      Height = 21
      ReadOnly = True
      TabOrder = 3
    end
    object ETypkennung_dez: TEdit
      Tag = 205
      Left = 88
      Top = 144
      Width = 105
      Height = 21
      ReadOnly = True
      TabOrder = 4
    end
    object eTimeout: TEdit
      Tag = 206
      Left = 88
      Top = 192
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 5
    end
    object eDFC: TEdit
      Tag = 203
      Left = 168
      Top = 96
      Width = 25
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
  end
  object GroupBox3: TGroupBox
    Left = 16
    Top = 252
    Width = 393
    Height = 45
    Caption = 'Sendetelegrammpuffer'
    TabOrder = 2
    object lAnzSekSendTelegr: TLabel
      Left = 128
      Top = 18
      Width = 46
      Height = 13
      Caption = 'Sekund'#228'r'
    end
    object lAnzPrmSendTelegr: TLabel
      Left = 8
      Top = 18
      Width = 29
      Height = 13
      Caption = 'Prim'#228'r'
    end
    object eAnzSekSendTelegr: TEdit
      Tag = 302
      Left = 184
      Top = 16
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object eAnzPrmSendTelegr: TEdit
      Tag = 301
      Left = 48
      Top = 16
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
  end
end
