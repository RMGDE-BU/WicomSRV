inherited FormAbrufAuswahl: TFormAbrufAuswahl
  Left = 374
  Top = 276
  Height = 527
  Caption = 'Definition von Abruf-Auftr'#228'gen'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited Splitter: TSplitter
    Left = 0
    Height = 500
  end
  inherited pnSourceTree: TPanel
    Left = 3
    Width = 142
    Height = 500
  end
  inherited pnSinkTree: TPanel
    Left = 145
    Width = 292
    Height = 500
    inherited pnMoveButtons: TPanel
      Height = 490
    end
  end
  object pnRight: TPanel [3]
    Left = 437
    Top = 0
    Width = 150
    Height = 500
    Align = alRight
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
    object gbDatenTypen: TGroupBox
      Left = 5
      Top = 320
      Width = 140
      Height = 121
      Align = alTop
      Caption = 'Datentypen'
      TabOrder = 0
      object chbMesswerte: TCheckBox
        Left = 16
        Top = 20
        Width = 97
        Height = 17
        Caption = 'Me'#223'werte'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chbMeldungen: TCheckBox
        Left = 16
        Top = 44
        Width = 97
        Height = 17
        Caption = 'Meldungen'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object chbParameter: TCheckBox
        Left = 16
        Top = 68
        Width = 97
        Height = 17
        Caption = 'Parameter'
        TabOrder = 2
      end
      object chbPruefsaetze: TCheckBox
        Left = 16
        Top = 92
        Width = 97
        Height = 17
        Caption = 'Pr'#252'fungss'#228'tze'
        TabOrder = 3
      end
    end
    object rgrpAbrufArt: TRadioGroup
      Left = 5
      Top = 62
      Width = 140
      Height = 65
      Align = alTop
      Caption = 'Abrufart (Art der Zieldaten)'
      ItemIndex = 0
      Items.Strings = (
        'manuell'
        'automatisch')
      TabOrder = 1
      OnClick = rgrpAbrufArtClick
    end
    object gbZeitBereich: TGroupBox
      Left = 5
      Top = 127
      Width = 140
      Height = 193
      Align = alTop
      Caption = 'Zeitbereich'
      TabOrder = 2
      object Bevel1: TBevel
        Left = 6
        Top = 96
        Width = 124
        Height = 10
        Shape = bsBottomLine
      end
      object dtpDatumVon: TDateTimePicker
        Left = 32
        Top = 48
        Width = 86
        Height = 21
        Date = 37371.581780115710000000
        Time = 37371.581780115710000000
        TabOrder = 0
      end
      object dtpZeitVon: TDateTimePicker
        Left = 32
        Top = 72
        Width = 86
        Height = 21
        Date = 37371.582002997700000000
        Time = 37371.582002997700000000
        Kind = dtkTime
        TabOrder = 1
      end
      object chbDateTimeVon: TCheckBox
        Left = 16
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Abruf ab'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = chbDateTimeClick
      end
      object chbDateTimeBis: TCheckBox
        Left = 16
        Top = 112
        Width = 97
        Height = 17
        Caption = 'Abruf bis'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = chbDateTimeClick
      end
      object dtpDatumBis: TDateTimePicker
        Left = 32
        Top = 136
        Width = 86
        Height = 21
        Date = 37371.581780115710000000
        Time = 37371.581780115710000000
        TabOrder = 4
      end
      object dtpZeitBis: TDateTimePicker
        Left = 32
        Top = 160
        Width = 86
        Height = 21
        Date = 37371.582002997700000000
        Time = 37371.582002997700000000
        Kind = dtkTime
        TabOrder = 5
      end
    end
    object pnRightRest: TPanel
      Left = 5
      Top = 441
      Width = 140
      Height = 54
      Align = alClient
      TabOrder = 3
    end
    object pnRight1: TPanel
      Left = 5
      Top = 5
      Width = 140
      Height = 57
      Align = alTop
      BevelInner = bvLowered
      BorderWidth = 2
      TabOrder = 4
      object bbtnOk: TBitBtn
        Left = 4
        Top = 4
        Width = 132
        Height = 25
        Caption = '&OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        Glyph.Data = {
          DE010000424DDE01000000000000760000002800000024000000120000000100
          0400000000006801000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333330000333333333333333333333333F33333333333
          00003333344333333333333333388F3333333333000033334224333333333333
          338338F3333333330000333422224333333333333833338F3333333300003342
          222224333333333383333338F3333333000034222A22224333333338F338F333
          8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
          33333338F83338F338F33333000033A33333A222433333338333338F338F3333
          0000333333333A222433333333333338F338F33300003333333333A222433333
          333333338F338F33000033333333333A222433333333333338F338F300003333
          33333333A222433333333333338F338F00003333333333333A22433333333333
          3338F38F000033333333333333A223333333333333338F830000333333333333
          333A333333333333333338330000333333333333333333333333333333333333
          0000}
        NumGlyphs = 2
        Spacing = -1
      end
      object bbtnCancel: TBitBtn
        Left = 4
        Top = 28
        Width = 132
        Height = 25
        Caption = '&Abbrechen'
        TabOrder = 1
        Kind = bkCancel
        Spacing = -1
      end
    end
  end
end
