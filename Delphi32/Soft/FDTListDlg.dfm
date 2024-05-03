inherited FormDTListenDialog: TFormDTListenDialog
  Caption = 'Auswahl'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnClient: TPanel
    Height = 246
    inherited Splitter: TSplitter
      Height = 236
    end
    inherited lbSource: TListBox
      Height = 236
    end
    inherited pnSplitter: TPanel
      Height = 236
    end
    inherited lbSink: TListBox
      Height = 236
    end
  end
  object pnDT: TPanel
    Left = 0
    Top = 246
    Width = 446
    Height = 41
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 21
      Height = 13
      Caption = 'von:'
    end
    object Label2: TLabel
      Left = 236
      Top = 16
      Width = 16
      Height = 13
      Caption = 'bis:'
    end
    object dtpVonDatum: TDateTimePicker
      Left = 36
      Top = 12
      Width = 89
      Height = 21
      Date = 39659.287262708330000000
      Time = 39659.287262708330000000
      TabOrder = 0
    end
    object dtpVonZeit: TDateTimePicker
      Left = 128
      Top = 12
      Width = 81
      Height = 21
      Date = 39659.287592870370000000
      Time = 39659.287592870370000000
      Kind = dtkTime
      TabOrder = 1
    end
    object dtpBisDatum: TDateTimePicker
      Left = 260
      Top = 12
      Width = 89
      Height = 21
      Date = 39659.287262708330000000
      Time = 39659.287262708330000000
      TabOrder = 2
    end
    object dtpBisZeit: TDateTimePicker
      Left = 352
      Top = 12
      Width = 81
      Height = 21
      Date = 39659.287592870370000000
      Time = 39659.287592870370000000
      Kind = dtkTime
      TabOrder = 3
    end
  end
end
