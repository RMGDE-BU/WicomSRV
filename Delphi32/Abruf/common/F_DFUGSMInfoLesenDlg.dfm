inherited FormDFUGSMInfoDlg: TFormDFUGSMInfoDlg
  Top = 436
  Caption = 'GSM-Information lesen'
  ClientHeight = 243
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnClient: TPanel
    Height = 202
    object Label5: TLabel [1]
      Left = 16
      Top = 88
      Width = 25
      Height = 13
      Caption = 'Netz:'
    end
    object lNetz: TLabel [2]
      Left = 96
      Top = 88
      Width = 233
      Height = 13
      AutoSize = False
    end
    object Label3: TLabel [3]
      Left = 16
      Top = 152
      Width = 66
      Height = 13
      Caption = 'Signalqualit'#228't:'
    end
    object Label12: TLabel [4]
      Left = 158
      Top = 136
      Width = 40
      Height = 13
      Caption = 'schlecht'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label13: TLabel [5]
      Left = 216
      Top = 136
      Width = 31
      Height = 13
      Caption = 'normal'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label14: TLabel [6]
      Left = 264
      Top = 136
      Width = 38
      Height = 13
      Caption = 'sehr gut'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel [7]
      Left = 150
      Top = 170
      Width = 6
      Height = 13
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel [8]
      Left = 200
      Top = 170
      Width = 12
      Height = 13
      Caption = '11'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel [9]
      Left = 256
      Top = 170
      Width = 12
      Height = 13
      Caption = '23'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel [10]
      Left = 296
      Top = 170
      Width = 12
      Height = 13
      Caption = '31'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lSQ: TLabel [11]
      Left = 96
      Top = 152
      Width = 53
      Height = 13
      AutoSize = False
    end
    object pbSQ: TProgressBar
      Left = 152
      Top = 151
      Width = 150
      Height = 17
      Max = 31
      Smooth = True
      Step = 1
      TabOrder = 1
    end
  end
  inherited pnBottom: TPanel
    Top = 202
  end
  object TimerLesen: TTimer
    Enabled = False
    OnTimer = TimerLesenTimer
    Left = 328
    Top = 76
  end
end
