inherited FormTelegrIec104: TFormTelegrIec104
  Width = 433
  Height = 431
  Caption = 'Telegrammanalyse IEC 870-5-104'
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 16
    Top = 16
    Width = 177
    Height = 201
    Caption = 'Telegramm-Empfang'
    TabOrder = 0
    object lStart: TLabel
      Left = 8
      Top = 26
      Width = 59
      Height = 13
      Caption = 'Startzeichen'
    end
    object lLaenge: TLabel
      Left = 8
      Top = 50
      Width = 30
      Height = 13
      Caption = 'L'#228'nge'
    end
    object lSteuerfeldOkt1: TLabel
      Left = 8
      Top = 74
      Width = 84
      Height = 13
      Caption = 'Steuerfeldoktett 1'
    end
    object lTypkennung: TLabel
      Left = 8
      Top = 170
      Width = 60
      Height = 13
      Caption = 'Typkennung'
    end
    object lSteuerfeldOkt2: TLabel
      Left = 8
      Top = 98
      Width = 84
      Height = 13
      Caption = 'Steuerfeldoktett 2'
    end
    object lSteuerfeldOkt3: TLabel
      Left = 8
      Top = 122
      Width = 84
      Height = 13
      Caption = 'Steuerfeldoktett 3'
    end
    object lSteuerfeldOkt4: TLabel
      Left = 8
      Top = 146
      Width = 84
      Height = 13
      Caption = 'Steuerfeldoktett 4'
    end
    object EStart: TEdit
      Tag = 101
      Left = 104
      Top = 24
      Width = 65
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object ELaenge: TEdit
      Tag = 102
      Left = 104
      Top = 48
      Width = 65
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object ESteuerfeld1: TEdit
      Tag = 103
      Left = 104
      Top = 72
      Width = 65
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
    object ETypkennung: TEdit
      Tag = 107
      Left = 104
      Top = 168
      Width = 65
      Height = 21
      ReadOnly = True
      TabOrder = 6
    end
    object ESteuerfeld2: TEdit
      Tag = 104
      Left = 104
      Top = 96
      Width = 65
      Height = 21
      ReadOnly = True
      TabOrder = 3
    end
    object ESteuerfeld3: TEdit
      Tag = 105
      Left = 104
      Top = 120
      Width = 65
      Height = 21
      ReadOnly = True
      TabOrder = 4
    end
    object ESteuerfeld4: TEdit
      Tag = 106
      Left = 104
      Top = 144
      Width = 65
      Height = 21
      ReadOnly = True
      TabOrder = 5
    end
  end
  object GroupBox2: TGroupBox
    Left = 208
    Top = 16
    Width = 201
    Height = 201
    Caption = 'Status'
    TabOrder = 1
    object Label8: TLabel
      Left = 8
      Top = 26
      Width = 68
      Height = 13
      Caption = 'Formatpr'#252'fung'
    end
    object lFunktion_txt: TLabel
      Left = 8
      Top = 122
      Width = 41
      Height = 13
      Caption = 'Funktion'
    end
    object lTypkennung_dez: TLabel
      Left = 8
      Top = 170
      Width = 60
      Height = 13
      Caption = 'Typkennung'
    end
    object lSendFolgeNr_dez: TLabel
      Left = 8
      Top = 74
      Width = 90
      Height = 13
      Caption = 'Sendefolgenr. N(S)'
    end
    object lEmpfFolgeNr_dez: TLabel
      Left = 8
      Top = 98
      Width = 86
      Height = 13
      Caption = 'Empf.folgenr. R(S)'
    end
    object lDatenuebertragung: TLabel
      Left = 8
      Top = 50
      Width = 86
      Height = 13
      Caption = 'Daten'#252'bertragung'
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
    object EFunktion_txt: TEdit
      Tag = 205
      Left = 112
      Top = 120
      Width = 81
      Height = 21
      ReadOnly = True
      TabOrder = 4
    end
    object ETypkennung_dez: TEdit
      Tag = 206
      Left = 112
      Top = 168
      Width = 81
      Height = 21
      ReadOnly = True
      TabOrder = 5
    end
    object eSendFolgeNr_dez: TEdit
      Tag = 203
      Left = 112
      Top = 72
      Width = 81
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
    object eEmpfFolgeNr_dez: TEdit
      Tag = 204
      Left = 112
      Top = 96
      Width = 81
      Height = 21
      ReadOnly = True
      TabOrder = 3
    end
    object eDatenuebertragung: TEdit
      Tag = 202
      Left = 112
      Top = 48
      Width = 81
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
  end
  object GroupBox3: TGroupBox
    Left = 16
    Top = 228
    Width = 393
    Height = 45
    Caption = 'Sendetelegrammpuffer'
    TabOrder = 2
    object lAnzSendTelegr: TLabel
      Left = 8
      Top = 18
      Width = 93
      Height = 13
      Caption = 'Anzahl Telegramme'
    end
    object lAnzUnquittTelegr: TLabel
      Left = 192
      Top = 18
      Width = 81
      Height = 13
      Caption = 'Anzahl unquittiert'
    end
    object eAnzSendTelegr: TEdit
      Tag = 301
      Left = 120
      Top = 16
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object eAnzUnquittTelegr: TEdit
      Tag = 302
      Left = 304
      Top = 16
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
  end
  object GroupBox4: TGroupBox
    Left = 16
    Top = 284
    Width = 177
    Height = 97
    Caption = #220'berwachung Zeit/Telegrammzahl'
    TabOrder = 3
    object lTimeout_t1: TLabel
      Left = 8
      Top = 22
      Width = 9
      Height = 13
      Caption = 't1'
    end
    object lTimeout_t2: TLabel
      Left = 8
      Top = 46
      Width = 9
      Height = 13
      Caption = 't2'
    end
    object lTimeout_t3: TLabel
      Left = 8
      Top = 70
      Width = 9
      Height = 13
      Caption = 't3'
    end
    object lTelegrZaehler_k: TLabel
      Left = 100
      Top = 22
      Width = 6
      Height = 13
      Caption = 'k'
    end
    object lTelegrZaehler_w: TLabel
      Left = 100
      Top = 46
      Width = 8
      Height = 13
      Caption = 'w'
    end
    object eTimeout_t1: TEdit
      Tag = 401
      Left = 32
      Top = 20
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object eTimeout_t2: TEdit
      Tag = 402
      Left = 32
      Top = 44
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object eTimeout_t3: TEdit
      Tag = 403
      Left = 32
      Top = 68
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
    object eTelegrZaehler_k: TEdit
      Tag = 404
      Left = 120
      Top = 20
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 3
    end
    object eTelegrZaehler_w: TEdit
      Tag = 405
      Left = 120
      Top = 44
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 4
    end
  end
  object GroupBox5: TGroupBox
    Left = 208
    Top = 284
    Width = 201
    Height = 97
    Caption = 'Telegrammz'#228'hler'
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 22
      Width = 105
      Height = 13
      Caption = 'Sendefolgez'#228'hler V(S)'
    end
    object Label2: TLabel
      Left = 8
      Top = 46
      Width = 122
      Height = 13
      Caption = 'Empfangsfolgez'#228'hler V(R)'
    end
    object lAckZS: TLabel
      Left = 8
      Top = 70
      Width = 19
      Height = 13
      Caption = 'Ack'
    end
    object eSendFolgeZS: TEdit
      Tag = 501
      Left = 144
      Top = 20
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object eEmpfFolgeZS: TEdit
      Tag = 502
      Left = 144
      Top = 44
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object eAckZS: TEdit
      Tag = 503
      Left = 144
      Top = 68
      Width = 49
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
  end
end
