object FormDateTimeDlg: TFormDateTimeDlg
  Left = 337
  Top = 193
  BorderStyle = bsDialog
  Caption = 'Datum und Zeit ausw'#228'hlen'
  ClientHeight = 139
  ClientWidth = 273
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
  object pnClient: TPanel
    Left = 0
    Top = 0
    Width = 273
    Height = 98
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object dtpDate: TDateTimePicker
      Left = 32
      Top = 24
      Width = 209
      Height = 21
      Date = 37244.419331979200000000
      Time = 37244.419331979200000000
      DateFormat = dfLong
      TabOrder = 0
    end
    object dtpTime: TDateTimePicker
      Left = 152
      Top = 56
      Width = 90
      Height = 21
      Date = 39896.637989479170000000
      Time = 39896.637989479170000000
      DateFormat = dfLong
      Kind = dtkTime
      TabOrder = 1
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 98
    Width = 273
    Height = 41
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object bbtnOk: TBitBtn
      Left = 32
      Top = 8
      Width = 92
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      Kind = bkOK
      Spacing = -1
    end
    object bbtnCancel: TBitBtn
      Left = 152
      Top = 8
      Width = 92
      Height = 25
      Caption = '&Abbrechen'
      TabOrder = 1
      Kind = bkCancel
      Spacing = -1
    end
  end
end
