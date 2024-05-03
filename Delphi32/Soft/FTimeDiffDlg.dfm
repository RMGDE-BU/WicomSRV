object FormTimeDiffDlg: TFormTimeDiffDlg
  Left = 360
  Top = 271
  BorderStyle = bsDialog
  Caption = 'Zeitbereich auswählen'
  ClientHeight = 118
  ClientWidth = 347
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
  object pnRight: TPanel
    Left = 306
    Top = 0
    Width = 41
    Height = 118
    Align = alRight
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object sbtnCancel: TSpeedButton
      Left = 9
      Top = 88
      Width = 23
      Height = 22
      Hint = 'Abbrechen'
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333911733333
        973333333377F333333F3339111733391173333337F37F333F77333911117391
        1117333337F337F3F7333333911117111117333337F3337F7333333339111111
        11733333337F33373333333333911111173333333337F333333F333333311111
        7333333333337F33333733333339111173333333333337F33373333333911111
        7333333333333733337F3333391117111733333333337333337F333391117391
        1173333333373337F33733339117333911173333337F33737F33333339133333
        91113333337FF73337F33333333333333919333333377333337F333333333333
        3333333333333333333733333333333333333333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbtnCancelClick
    end
    object sbtnOk: TSpeedButton
      Left = 9
      Top = 8
      Width = 23
      Height = 22
      Hint = 'OK'
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333333333333333333333333333333333F3333333333333343333333
        3333333377F33333333333342433333333333337337F33333333334222433333
        3333337333337F3333333422A2243333333337F37F3337F3333332223A224433
        333337F337F337F333333A2A33A22243333337F7337F337F333333A3333A2224
        333333733337F337333333333333A2224333333333337F33F333333333333A22
        24333333333337F37F333333333333A2224333333333337F37F3333333333333
        2223333333333333F37F333333333333A2A33333333333337F73333333333333
        3A33333333333333373333333333333333333333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbtnOkClick
    end
    object sbtnIgnore: TSpeedButton
      Left = 9
      Top = 32
      Width = 23
      Height = 22
      Hint = 'Alles|Alle Daten abrufen'
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        5555555555FFFFF555555555544C4C5555555555F777775FF5555554C444C444
        5555555775FF55775F55554C4334444445555575577F55557FF554C4C334C4C4
        335557F5577FF55577F554CCC3334444335557555777F555775FCCCCC333CCC4
        C4457F55F777F555557F4CC33333CCC444C57F577777F5F5557FC4333333C3C4
        CCC57F777777F7FF557F4CC33333333C4C457F577777777F557FCCC33CC4333C
        C4C575F7755F777FF5755CCCCC3333334C5557F5FF777777F7F554C333333333
        CC55575777777777F755553333CC3C33C555557777557577755555533CC4C4CC
        5555555775FFFF77555555555C4CCC5555555555577777555555}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbtnIgnoreClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 306
    Height = 118
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object Label1: TLabel
      Left = 272
      Top = 36
      Width = 17
      Height = 13
      Caption = 'Uhr'
    end
    object Label2: TLabel
      Left = 16
      Top = 16
      Width = 18
      Height = 13
      Caption = 'von'
    end
    object Label3: TLabel
      Left = 16
      Top = 64
      Width = 13
      Height = 13
      Caption = 'bis'
    end
    object Label4: TLabel
      Left = 272
      Top = 84
      Width = 17
      Height = 13
      Caption = 'Uhr'
    end
    object dtpVon: TDateTimePicker
      Left = 16
      Top = 32
      Width = 193
      Height = 21
      CalAlignment = dtaLeft
      Date = 36846.7276343866
      Time = 36846.7276343866
      DateFormat = dfLong
      DateMode = dmComboBox
      Kind = dtkDate
      ParseInput = False
      TabOrder = 0
    end
    object speVon: TSpinEdit
      Left = 216
      Top = 32
      Width = 49
      Height = 22
      MaxValue = 23
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object dtpBis: TDateTimePicker
      Left = 16
      Top = 80
      Width = 193
      Height = 21
      CalAlignment = dtaLeft
      Date = 36846.7276343866
      Time = 36846.7276343866
      DateFormat = dfLong
      DateMode = dmComboBox
      Kind = dtkDate
      ParseInput = False
      TabOrder = 2
    end
    object speBis: TSpinEdit
      Left = 216
      Top = 80
      Width = 49
      Height = 22
      MaxValue = 23
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
  end
end
