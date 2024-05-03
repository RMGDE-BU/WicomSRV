inherited FormPortIec104: TFormPortIec104
  Caption = 'TCP/IP-Monitor IEC 870-5-104'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pTop: TPanel
    inherited pt0: TPanel
      object lTimeout_t0: TLabel
        Left = 8
        Top = 8
        Width = 9
        Height = 13
        Caption = 't0'
        Visible = False
      end
      object eTimeout_t0: TEdit
        Tag = 103
        Left = 32
        Top = 3
        Width = 49
        Height = 21
        ReadOnly = True
        TabOrder = 0
        Visible = False
      end
    end
  end
  object TimerTimeout_t1: TTimer
    Enabled = False
    OnTimer = TimerTimeout_t1Timer
    Left = 96
    Top = 48
  end
  object TimerTimeout_t2: TTimer
    Enabled = False
    OnTimer = TimerTimeout_t2Timer
    Left = 136
    Top = 48
  end
  object TimerTimeout_t3: TTimer
    Enabled = False
    OnTimer = TimerTimeout_t3Timer
    Left = 176
    Top = 48
  end
  object TimerTimeout_t0: TTimer
    Enabled = False
    OnTimer = TimerTimeout_t0Timer
    Left = 56
    Top = 48
  end
  object TimerRedundanzVerbUnterbrechung: TTimer
    Enabled = False
    OnTimer = TimerRedundanzVerbUnterbrechungTimer
    Left = 16
    Top = 88
  end
end
