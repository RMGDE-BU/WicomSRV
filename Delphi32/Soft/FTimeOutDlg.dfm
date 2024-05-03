inherited FormTimeoutDialog: TFormTimeoutDialog
  Left = 460
  Top = 413
  Caption = 'Totzeit aktiviert'
  ClientHeight = 236
  ClientWidth = 360
  OldCreateOrder = True
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnClient: TPanel
    Width = 360
    Height = 195
    object lMessage: TLabel
      Left = 24
      Top = 24
      Width = 305
      Height = 113
      Alignment = taCenter
      AutoSize = False
      Caption = 'Totzeit l'#228'uft'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object lTimeOut: TLabel
      Left = 152
      Top = 160
      Width = 42
      Height = 16
      Caption = '0 sec.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  inherited pnBottom: TPanel
    Top = 195
    Width = 360
    inherited pnBottomBtnOk: TPanel
      Left = 132
    end
  end
  object TimerTimeOut: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimeOutTimer
    Left = 8
    Top = 8
  end
end
