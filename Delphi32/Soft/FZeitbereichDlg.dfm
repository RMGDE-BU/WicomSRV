inherited FormZeitbereichDialog: TFormZeitbereichDialog
  Left = 355
  Top = 197
  Caption = 'Zeitbereich ausw'#228'hlen'
  ClientHeight = 157
  ClientWidth = 251
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnClient: TPanel
    Width = 251
    Height = 116
    object Label1: TLabel
      Left = 16
      Top = 32
      Width = 22
      Height = 13
      Caption = 'Von:'
    end
    object Label2: TLabel
      Left = 16
      Top = 72
      Width = 17
      Height = 13
      Caption = 'Bis:'
    end
    object dtpDatumVon: TDateTimePicker
      Left = 56
      Top = 28
      Width = 89
      Height = 21
      Date = 39185.147588587960000000
      Time = 39185.147588587960000000
      TabOrder = 0
    end
    object dtpZeitVon: TDateTimePicker
      Left = 152
      Top = 28
      Width = 81
      Height = 21
      Date = 39185.147692280100000000
      Time = 39185.147692280100000000
      Kind = dtkTime
      TabOrder = 1
    end
    object dtpDatumBis: TDateTimePicker
      Left = 56
      Top = 68
      Width = 89
      Height = 21
      Date = 39185.147588587960000000
      Time = 39185.147588587960000000
      TabOrder = 2
    end
    object dtpZeitBis: TDateTimePicker
      Left = 152
      Top = 68
      Width = 81
      Height = 21
      Date = 39185.147692280100000000
      Time = 39185.147692280100000000
      Kind = dtkTime
      TabOrder = 3
    end
  end
  inherited pnBottom: TPanel
    Top = 116
    Width = 251
    inherited pnBottomBtnOk: TPanel
      Left = 12
    end
    object pnBottomBtnCancel: TPanel
      Left = 132
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
