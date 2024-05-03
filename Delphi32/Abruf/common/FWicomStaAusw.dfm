object FormWicomStationsAuswahl: TFormWicomStationsAuswahl
  Left = 239
  Top = 198
  Width = 595
  Height = 481
  Caption = 'WICOM - Stationen und Kan'#228'le ausw'#228'hlen'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 377
    Top = 0
    Height = 447
    AutoSnap = False
    Beveled = True
  end
  object pnSourceTree: TPanel
    Left = 0
    Top = 0
    Width = 377
    Height = 447
    Align = alLeft
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
  end
  object pnSinkTree: TPanel
    Left = 380
    Top = 0
    Width = 207
    Height = 447
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object pnMoveButtons: TPanel
      Left = 5
      Top = 5
      Width = 40
      Height = 437
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object pnButtons: TPanel
        Left = 0
        Top = 152
        Width = 40
        Height = 161
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object sbtnAddSingle: TSpeedButton
          Left = 8
          Top = 44
          Width = 23
          Height = 22
          Hint = 'Ausw'#228'hlen|Selektierte Station ausw'#228'hlen'
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333FF3333333333333003333
            3333333333773FF3333333333309003333333333337F773FF333333333099900
            33333FFFFF7F33773FF30000000999990033777777733333773F099999999999
            99007FFFFFFF33333F7700000009999900337777777F333F7733333333099900
            33333333337F3F77333333333309003333333333337F77333333333333003333
            3333333333773333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = sbtnAddSingleClick
        end
        object sbtnAddAll: TSpeedButton
          Left = 8
          Top = 12
          Width = 23
          Height = 22
          Hint = 'Alle ausw'#228'hlen|Alle Stationen ausw'#228'hlen'
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            33333FF3333333333333447333333333333377FFF33333333333744473333333
            333337773FF3333333333444447333333333373F773FF3333333334444447333
            33333373F3773FF3333333744444447333333337F333773FF333333444444444
            733333373F3333773FF333334444444444733FFF7FFFFFFF77FF999999999999
            999977777777777733773333CCCCCCCCCC3333337333333F7733333CCCCCCCCC
            33333337F3333F773333333CCCCCCC3333333337333F7733333333CCCCCC3333
            333333733F77333333333CCCCC333333333337FF7733333333333CCC33333333
            33333777333333333333CC333333333333337733333333333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = sbtnAddAllClick
        end
        object sbtnRemoveSingle: TSpeedButton
          Left = 8
          Top = 96
          Width = 23
          Height = 22
          Hint = 'Entfernen|Selektierte Station aus Auswahl entfernen'
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333FF3333333333333003333333333333F77F33333333333009033
            333333333F7737F333333333009990333333333F773337FFFFFF330099999000
            00003F773333377777770099999999999990773FF33333FFFFF7330099999000
            000033773FF33777777733330099903333333333773FF7F33333333333009033
            33333333337737F3333333333333003333333333333377333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = sbtnRemoveSingleClick
        end
        object sbtnRemoveAll: TSpeedButton
          Left = 8
          Top = 128
          Width = 23
          Height = 22
          Hint = 'Alle entfernen|Alle Stationen aus Auswahl entfernen'
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            333333333333333333FF3333333333333744333333333333F773333333333337
            44473333333333F777F3333333333744444333333333F7733733333333374444
            4433333333F77333733333333744444447333333F7733337F333333744444444
            433333F77333333733333744444444443333377FFFFFFF7FFFFF999999999999
            9999733777777777777333CCCCCCCCCC33333773FF333373F3333333CCCCCCCC
            C333333773FF3337F333333333CCCCCCC33333333773FF373F3333333333CCCC
            CC333333333773FF73F33333333333CCCCC3333333333773F7F3333333333333
            CCC333333333333777FF33333333333333CC3333333333333773}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = sbtnRemoveAllClick
        end
      end
    end
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 8
    Top = 81
    object pmiAddAll: TMenuItem
      Caption = 'Alle h&inzuf'#252'gen'
      OnClick = sbtnAddAllClick
    end
    object pmiAdd: TMenuItem
      Caption = '&Hinzuf'#252'gen'
      OnClick = sbtnAddSingleClick
    end
    object pmiRemove: TMenuItem
      Caption = '&Entfernen'
      OnClick = sbtnRemoveSingleClick
    end
    object pmiRemoveAll: TMenuItem
      Caption = 'Alle e&ntfernen'
      OnClick = sbtnRemoveAllClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmiSortierung: TMenuItem
      Caption = '&Sortierung'
      object pmiSortName: TMenuItem
        Tag = 1
        Caption = '&Name'
        Checked = True
        RadioItem = True
        OnClick = pmiSortTypeClick
      end
      object pmiSortIDNummer: TMenuItem
        Caption = 'I&D/Nummer'
        RadioItem = True
        OnClick = pmiSortTypeClick
      end
    end
    object pmiZuordnung: TMenuItem
      Caption = '&Zuordnung'
      object pmiZ2LogbuchbeiQuelleArchivkanlebeiRegistrierung: TMenuItem
        Tag = 2
        Caption = 'Logbuch bei Quelle/Archivkan'#228'le bei Registrierung'
        RadioItem = True
        OnClick = pmiZuordnungsTypeClick
      end
      object pmiZ3LogbuchbeiRegistrierungArchivkanlebeiQuelle: TMenuItem
        Tag = 3
        Caption = 'Logbuch bei Registrierung/Archivkan'#228'le bei Quelle'
        RadioItem = True
        OnClick = pmiZuordnungsTypeClick
      end
      object pmiZ0AlleArchivebeiRegstrierung: TMenuItem
        Caption = 'Alle Archive bei Registrierung'
        RadioItem = True
        OnClick = pmiZuordnungsTypeClick
      end
      object pmiZ1AlleArchivebeiQuelle: TMenuItem
        Tag = 1
        Caption = 'Alle Archive bei Quelle'
        Checked = True
        RadioItem = True
        OnClick = pmiZuordnungsTypeClick
      end
      object pmiZ4LogbuchbeiQuelleArchivkanlebeiRegistrierungundQuelle: TMenuItem
        Tag = 4
        Caption = 'Logbuch bei Quelle/Archivkan'#228'le bei Reg. und Quelle'
        RadioItem = True
        OnClick = pmiZuordnungsTypeClick
      end
    end
  end
end
