inherited FormTabVerifyTakeOver: TFormTabVerifyTakeOver
  Left = 174
  Top = 188
  Height = 740
  Caption = 'Funktions'#252'bernahme'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnButtons: TPanel
    Height = 713
    object sbtnChangeToSecondary: TSpeedButton
      Left = 8
      Top = 8
      Width = 169
      Height = 89
      Hint = 'Systemstatus umschalten|Das Sekund'#228'rsystem aktiv schalten'
      Caption = 'Umschalten'
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333FF3FFFF3FFF33FF003000030003
        300077377773777F377703330033370337037FFF77F3377FF77F700007333300
        0003777777333377777F303003333330370337F77F333337377F303073333333
        070337F77F333333777F3700733333333003377773333333377F330033333333
        30033377F3333333377F33073333333333033377333333333373333333333333
        33333333FF3333333FF3333973333333793333377FF3333377F3333999333339
        993333377733333777F33339933333339933333773FF333377F3333939733379
        39333337377FFF77373333333399999333333333337777733333}
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbtnChangeToSecondaryClick
    end
  end
  inherited pnClient: TPanel
    Height = 713
    object Splitter1: TSplitter [0]
      Left = 3
      Top = 473
      Width = 753
      Height = 3
      Cursor = crVSplit
      Align = alTop
      AutoSnap = False
      MinSize = 50
    end
    inherited pnModules: TPanel
      Top = 476
      Height = 234
      inherited gbExternalModules: TGroupBox
        Height = 232
        inherited lvExternalModules: TListView
          Height = 215
          Columns = <
            item
              Caption = 'Beschreibung'
            end
            item
              Caption = 'Dateiname'
            end
            item
              Caption = 'Pfad/Servicename'
            end
            item
              Caption = 'Typ'
            end
            item
              Caption = 'Aktueller Status'
            end
            item
              Caption = 'Status im Sekund'#228'rmodus'
            end
            item
              Caption = 'Status im Prim'#228'rmodus'
            end>
        end
      end
    end
    object pnSystem: TPanel
      Left = 3
      Top = 3
      Width = 753
      Height = 158
      Align = alTop
      TabOrder = 1
      object gbSystem: TGroupBox
        Left = 1
        Top = 1
        Width = 751
        Height = 156
        Align = alClient
        Caption = 'Status'
        TabOrder = 0
        object Label1: TLabel
          Left = 16
          Top = 24
          Width = 72
          Height = 13
          Caption = 'Eigener Status:'
        end
        object Label2: TLabel
          Left = 24
          Top = 56
          Width = 100
          Height = 13
          Caption = 'Letzte Signalisierung:'
        end
        object Label3: TLabel
          Left = 24
          Top = 88
          Width = 108
          Height = 13
          Caption = 'Pfad f'#252'r Signalisierung:'
        end
        object Label4: TLabel
          Left = 368
          Top = 24
          Width = 82
          Height = 13
          Caption = 'Entfernter Status:'
        end
        object Label5: TLabel
          Left = 376
          Top = 56
          Width = 100
          Height = 13
          Caption = 'Letzte Signalisierung:'
        end
        object Label6: TLabel
          Left = 376
          Top = 88
          Width = 108
          Height = 13
          Caption = 'Pfad f'#252'r Signalisierung:'
        end
        object Label7: TLabel
          Left = 24
          Top = 120
          Width = 79
          Height = 13
          Caption = 'Systemkennung:'
        end
        object Label8: TLabel
          Left = 376
          Top = 120
          Width = 79
          Height = 13
          Caption = 'Systemkennung:'
        end
        object eOwnState: TEdit
          Left = 152
          Top = 20
          Width = 121
          Height = 21
          Color = clInfoBk
          ReadOnly = True
          TabOrder = 0
        end
        object eOwnLastDate: TEdit
          Left = 152
          Top = 52
          Width = 153
          Height = 21
          Color = clInfoBk
          ReadOnly = True
          TabOrder = 1
        end
        object eOwnPath: TEdit
          Left = 152
          Top = 84
          Width = 193
          Height = 21
          Color = clInfoBk
          ReadOnly = True
          TabOrder = 2
        end
        object eOtherState: TEdit
          Left = 504
          Top = 20
          Width = 121
          Height = 21
          Color = clInfoBk
          ReadOnly = True
          TabOrder = 3
        end
        object eOtherLastDate: TEdit
          Left = 504
          Top = 52
          Width = 153
          Height = 21
          Color = clInfoBk
          ReadOnly = True
          TabOrder = 4
        end
        object eOtherPath: TEdit
          Left = 504
          Top = 84
          Width = 193
          Height = 21
          Color = clInfoBk
          ReadOnly = True
          TabOrder = 5
        end
        object eOwnSignatur: TEdit
          Left = 152
          Top = 116
          Width = 153
          Height = 21
          Color = clInfoBk
          ReadOnly = True
          TabOrder = 6
        end
        object eOtherSignatur: TEdit
          Left = 504
          Top = 116
          Width = 153
          Height = 21
          Color = clInfoBk
          ReadOnly = True
          TabOrder = 7
        end
      end
    end
    object pnJournal: TPanel
      Left = 3
      Top = 161
      Width = 753
      Height = 312
      Align = alTop
      TabOrder = 2
      object gbJournal: TGroupBox
        Left = 1
        Top = 1
        Width = 751
        Height = 310
        Align = alClient
        Caption = 'Journal'
        TabOrder = 0
        object lvJournal: TListView
          Left = 2
          Top = 15
          Width = 747
          Height = 293
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Datum/Zeit'
            end
            item
              Caption = 'Alter Status'
            end
            item
              Caption = 'Neuer Status'
            end
            item
              Caption = 'Entfernter Status'
            end
            item
              Caption = 'Beschreibung'
            end>
          TabOrder = 0
          ViewStyle = vsReport
          OnResize = lvExternalModulesResize
        end
      end
    end
  end
end
