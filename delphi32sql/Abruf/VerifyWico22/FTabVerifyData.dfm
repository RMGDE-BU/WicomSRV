inherited FormTabVerifyData: TFormTabVerifyData
  Caption = 'Datenpr'#252'fung'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnButtons: TPanel
    object sbtnCheckBde: TSpeedButton
      Left = 8
      Top = 8
      Width = 169
      Height = 89
      Hint = 'Funktion der BDE pr'#252'fen'
      Caption = 'BDE Pr'#252'fen'
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
      OnClick = sbtnCheckBdeClick
    end
    object sbtnStartMerger: TSpeedButton
      Left = 8
      Top = 104
      Width = 169
      Height = 89
      Hint = 
        'Abrufdaten zusammenf'#252'hren|Die Abrufdaten des Sekund'#228'rsystems duc' +
        'rh neue Daten des Prim'#228'rsystems ersetzen'
      Caption = 'Abrufdaten zusammenf'#252'hren'
      Enabled = False
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
      OnClick = sbtnStartMergerClick
    end
    object sbtnStartTransfer: TSpeedButton
      Left = 8
      Top = 200
      Width = 169
      Height = 89
      Hint = 
        'Datenbanken '#252'berschreiben|Datenbank des Sekund'#228'rsystems mit der ' +
        'des Prim'#228'rsystems '#252'berschreiben'
      Caption = 'Datenbank '#252'bertragen'
      Enabled = False
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
      OnClick = sbtnStartTransferClick
    end
  end
  inherited pnClient: TPanel
    object Splitter1: TSplitter [0]
      Left = 3
      Top = 377
      Width = 753
      Height = 3
      Cursor = crVSplit
      Align = alTop
      AutoSnap = False
      MinSize = 50
    end
    inherited pnModules: TPanel
      Top = 380
      Height = 246
      inherited gbExternalModules: TGroupBox
        Height = 244
        inherited lvExternalModules: TListView
          Height = 227
          Columns = <
            item
              Caption = 'Beschreibung'
            end
            item
              Caption = 'Dateiname'
            end
            item
              Caption = 'Letzte Durchf'#252'hrung'
            end
            item
              Caption = 'Ergebnis'
            end
            item
              Caption = 'Bemerkung'
            end>
        end
      end
    end
    object pnSystem: TPanel
      Left = 3
      Top = 3
      Width = 753
      Height = 54
      Align = alTop
      TabOrder = 1
      object gbSystem: TGroupBox
        Left = 1
        Top = 1
        Width = 751
        Height = 52
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
        object eOwnState: TEdit
          Left = 104
          Top = 20
          Width = 121
          Height = 21
          Color = clInfoBk
          ReadOnly = True
          TabOrder = 0
        end
      end
    end
    object pnJournal: TPanel
      Left = 3
      Top = 57
      Width = 753
      Height = 320
      Align = alTop
      TabOrder = 2
      object gbJournal: TGroupBox
        Left = 1
        Top = 1
        Width = 751
        Height = 318
        Align = alClient
        Caption = 'Journal'
        TabOrder = 0
        object lvJournal: TListView
          Left = 2
          Top = 15
          Width = 747
          Height = 301
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Datum/Zeit'
            end
            item
              Caption = 'Datei/Aktion'
            end
            item
              Caption = 'Start'
            end
            item
              Caption = 'Ende'
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
