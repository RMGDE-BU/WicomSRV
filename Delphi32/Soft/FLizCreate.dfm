object FormCreateLizenzFile: TFormCreateLizenzFile
  Left = 515
  Top = 406
  BorderStyle = bsDialog
  Caption = 'Lizenzanforderung'
  ClientHeight = 345
  ClientWidth = 269
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
  object Label1: TLabel
    Left = 8
    Top = 32
    Width = 250
    Height = 25
    AutoSize = False
    Caption = 'Ihr Abrufsystem muss f'#252'r diesen PC lizensiert werden.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 250
    Height = 57
    AutoSize = False
    Caption = 
      'Wenn auf Ihrem Rechner MS Outlook aktiv ist, k'#246'nnen Sie die Lize' +
      'nzinformationen mit dem Schalter "Lizenz online anfordern" direk' +
      't an die RMG Messtechnik GmbH senden.'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 8
    Top = 152
    Width = 250
    Height = 65
    AutoSize = False
    Caption = 
      'Alternativ k'#246'nnen Sie eine Datei zur Lizenz- anforderung auf Ihr' +
      'em Rechner speichern und per mail an "software@rmg.com" schicken' +
      '.'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 48
    Top = 8
    Width = 164
    Height = 13
    Caption = 'WICO22 - Lizenzanforderung'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 248
    Width = 250
    Height = 65
    AutoSize = False
    Caption = 
      'Falls Sie sich telefonisch lizensieren lassen wollen, k'#246'nnen Sie' +
      ' Sich an den Software-Service der RMG Messtechnik GmbH wenden, e' +
      'inen Freischaltcode anfordern und diesen selber eingeben.'
    WordWrap = True
  end
  object bbtnOnlineLiz: TBitBtn
    Tag = 1
    Left = 40
    Top = 112
    Width = 193
    Height = 25
    Caption = 'Lizenz online anfordern'
    TabOrder = 0
    OnClick = bbtnOnlineLizClick
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333333333333333333FFFFFFFFFFFFFFF000000000000
      000077777777777777770FFFFFFFFFFFFFF07F3333FFF33333370FFFF777FFFF
      FFF07F333777333333370FFFFFFFFFFFFFF07F3333FFFFFF33370FFFF777777F
      FFF07F33377777733FF70FFFFFFFFFFF99907F3FFF33333377770F777FFFFFFF
      9CA07F77733333337F370FFFFFFFFFFF9A907FFFFFFFFFFF7FF7000000000000
      0000777777777777777733333333333333333333333333333333333333333333
      3333333333333333333333333333333333333333333333333333333333333333
      3333333333333333333333333333333333333333333333333333}
    NumGlyphs = 2
    Spacing = -1
  end
  object bbtnFileLiz: TBitBtn
    Tag = 2
    Left = 40
    Top = 208
    Width = 193
    Height = 25
    Caption = 'Lizenzinformationen speichern'
    TabOrder = 1
    OnClick = bbtnFileLizClick
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
      7700333333337777777733333333008088003333333377F73377333333330088
      88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
      000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
      FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
      99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
      99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
      99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
      93337FFFF7737777733300000033333333337777773333333333}
    NumGlyphs = 2
    Spacing = -1
  end
  object btnEncode: TButton
    Left = 212
    Top = 2
    Width = 57
    Height = 25
    Caption = 'Encode'
    TabOrder = 2
    OnClick = btnEncodeClick
  end
  object bbtnTelLiz: TBitBtn
    Tag = 2
    Left = 40
    Top = 304
    Width = 193
    Height = 25
    Caption = 'Freischaltcode selber eingeben'
    TabOrder = 3
    OnClick = bbtnTelLizClick
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003B3000000000
      003B37F77777777777F73BB09111111110BB3777F3F3F3F3F777311098080808
      10B33777F7373737377313309999999910337F373F3F3F3F3733133309808089
      03337F3373737373733313333099999033337FFFF7FFFFF7FFFFB011B0000000
      BBBB7777777777777777B01110BBBBB0BBBB77F37777777777773011108BB333
      333337F337377F3FFFF33099111BB3010033373F33777F77773F331999100101
      11033373FFF77773337F33300099991999033337773FFFF33373333BB7100199
      113333377377773FF7F333BB333BB7011B33337733377F7777FF3BB3333BB333
      3BB3377333377F33377FBB33333BB33333BB7733333773333377}
    NumGlyphs = 2
    Spacing = -1
  end
  object OpenDialog: TOpenDialog
    Left = 8
  end
end
