object FormMainIec32: TFormMainIec32
  Left = 342
  Top = 229
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'IEC-Kopplung'
  ClientHeight = 233
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 460
    Height = 41
    Align = alTop
    TabOrder = 0
    object bbtnBeenden: TBitBtn
      Left = 8
      Top = 8
      Width = 57
      Height = 25
      Hint = 'Schlie'#223'en'
      ModalResult = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = bbtnBeendenClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
        03333377777777777F333301111111110333337F333333337F33330111111111
        0333337F333333337F333301111111110333337F333333337F33330111111111
        0333337F333333337F333301111111110333337F333333337F33330111111111
        0333337F3333333F7F333301111111B10333337F333333737F33330111111111
        0333337F333333337F333301111111110333337F33FFFFF37F3333011EEEEE11
        0333337F377777F37F3333011EEEEE110333337F37FFF7F37F3333011EEEEE11
        0333337F377777337F333301111111110333337F333333337F33330111111111
        0333337FFFFFFFFF7F3333000000000003333377777777777333}
      NumGlyphs = 2
    end
    object bbtnV24Monitor: TBitBtn
      Left = 72
      Top = 8
      Width = 57
      Height = 25
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = bbtnV24MonitorClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
        003337777777777777F330FFFFFFFFFFF033373F3F3F3F3F3733330F0F0F0F0F
        03333F7F737373737FFF0000FFFFFFF0000377773FFFFFF7777F0FF800000008
        FF037F3F77777773FF7F0F9FFFFFFFF000037F7333333337777F0FFFFFFFFFFF
        FF0373FFFFFFFFFFFF7330000000000000333777777777777733333000000000
        3333333777777777F3333330FFFFFFF033333337F3FFFFF7F3333330F00000F0
        33333337F77777F7F3333330F0AAE0F033333337F7F337F7F3333330F0DAD0F0
        33333337F7FFF7F7F3333330F00000F033333337F7777737F3333330FFFFFFF0
        33333337FFFFFFF7F33333300000000033333337777777773333}
      NumGlyphs = 2
    end
    object bbtnTelegrammanalyse: TBitBtn
      Left = 136
      Top = 8
      Width = 57
      Height = 25
      Hint = 'Telegrammanalyse'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = bbtnTelegrammanalyseClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555775777777
        57705557757777775FF7555555555555000755555555555F777F555555555550
        87075555555555F7577F5555555555088805555555555F755F75555555555033
        805555555555F755F75555555555033B05555555555F755F75555555555033B0
        5555555555F755F75555555555033B05555555555F755F75555555555033B055
        55555555F755F75555555555033B05555555555F755F75555555555033B05555
        555555F75FF75555555555030B05555555555F7F7F75555555555000B0555555
        5555F777F7555555555501900555555555557777755555555555099055555555
        5555777755555555555550055555555555555775555555555555}
      NumGlyphs = 2
    end
    object bbtnInfo: TBitBtn
      Left = 392
      Top = 8
      Width = 57
      Height = 25
      Hint = 'Programminformation anzeigen'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = bbtnInfoClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFF3333333333F797F3333333333F737373FF333333BFB999BFB
        33333337737773773F3333BFBF797FBFB33333733337333373F33BFBFBFBFBFB
        FB3337F33333F33337F33FBFBFB9BFBFBF3337333337F333373FFBFBFBF97BFB
        FBF37F333337FF33337FBFBFBFB99FBFBFB37F3333377FF3337FFBFBFBFB99FB
        FBF37F33333377FF337FBFBF77BF799FBFB37F333FF3377F337FFBFB99FB799B
        FBF373F377F3377F33733FBF997F799FBF3337F377FFF77337F33BFBF99999FB
        FB33373F37777733373333BFBF999FBFB3333373FF77733F7333333BFBFBFBFB
        3333333773FFFF77333333333FBFBF3333333333377777333333}
      NumGlyphs = 2
    end
    object bbtnINIKonfiguration: TBitBtn
      Left = 200
      Top = 8
      Width = 57
      Height = 25
      Hint = 'INI-Konfiguration'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = bbtnINIKonfigurationClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555550FF0559
        1950555FF75F7557F7F757000FF055591903557775F75557F77570FFFF055559
        1933575FF57F5557F7FF0F00FF05555919337F775F7F5557F7F700550F055559
        193577557F7F55F7577F07550F0555999995755575755F7FFF7F5570F0755011
        11155557F755F777777555000755033305555577755F75F77F55555555503335
        0555555FF5F75F757F5555005503335505555577FF75F7557F55505050333555
        05555757F75F75557F5505000333555505557F777FF755557F55000000355557
        07557777777F55557F5555000005555707555577777FF5557F55553000075557
        0755557F7777FFF5755555335000005555555577577777555555}
      NumGlyphs = 2
    end
    object bbtnDatenReset: TBitBtn
      Left = 264
      Top = 8
      Width = 57
      Height = 25
      Hint = 'Zeitbereiche '#252'bertragener Daten zur'#252'cksetzen'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = bbtnDatenResetClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
        3333333777333777FF3333993333339993333377FF3333377FF3399993333339
        993337777FF3333377F3393999333333993337F777FF333337FF993399933333
        399377F3777FF333377F993339993333399377F33777FF33377F993333999333
        399377F333777FF3377F993333399933399377F3333777FF377F993333339993
        399377FF3333777FF7733993333339993933373FF3333777F7F3399933333399
        99333773FF3333777733339993333339933333773FFFFFF77333333999999999
        3333333777333777333333333999993333333333377777333333}
      NumGlyphs = 2
    end
    object bbtnCustomConfig: TBitBtn
      Left = 328
      Top = 8
      Width = 57
      Height = 25
      Hint = 'XML Konfiguration'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = bbtnCustomConfigClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333300000000
        0000333377777777777733330FFFFFFFFFF033337F3FFF3F3FF733330F000F0F
        00F033337F777373773733330FFFFFFFFFF033337F3FF3FF3FF733330F00F00F
        00F033337F773773773733330FFFFFFFFFF033337FF3333FF3F7333300FFFF00
        F0F03333773FF377F7373330FB00F0F0FFF0333733773737F3F7330FB0BF0FB0
        F0F0337337337337373730FBFBF0FB0FFFF037F333373373333730BFBF0FB0FF
        FFF037F3337337333FF700FBFBFB0FFF000077F333337FF37777E0BFBFB000FF
        0FF077FF3337773F7F37EE0BFB0BFB0F0F03777FF3733F737F73EEE0BFBF00FF
        00337777FFFF77FF7733EEEE0000000003337777777777777333}
      NumGlyphs = 2
    end
  end
  object pnClient: TPanel
    Left = 0
    Top = 41
    Width = 460
    Height = 156
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -32
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lPort: TLabel
      Left = 8
      Top = 8
      Width = 65
      Height = 16
      AutoSize = False
      Caption = 'Port: 999999'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object lNAD: TLabel
      Left = 80
      Top = 8
      Width = 233
      Height = 16
      AutoSize = False
      Caption = 'NAD:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object lNADZeit: TLabel
      Left = 312
      Top = 8
      Width = 129
      Height = 16
      AutoSize = False
      Caption = 'am:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object lNorm: TLabel
      Left = 80
      Top = 48
      Width = 281
      Height = 37
      Alignment = taCenter
      AutoSize = False
      Caption = 'IEC'
    end
    object lFunktion: TLabel
      Left = 160
      Top = 88
      Width = 129
      Height = 20
      Alignment = taCenter
      AutoSize = False
      Caption = 'Funktion'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 197
    Width = 460
    Height = 36
    Align = alBottom
    TabOrder = 2
  end
  object DialogInfo: TDialogInfo
    ProgrammName = 'IEC-Kopplung'
    Lizenz32 = True
    ReadResource = True
    Left = 408
    Top = 161
  end
  object ServerSocketWMsg: TServerSocket
    Active = False
    Port = 0
    ServerType = stNonBlocking
    OnClientRead = ServerSocketWMsgClientRead
    Left = 408
    Top = 64
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 376
    Top = 64
  end
  object PopupMenu: TPopupMenu
    AutoHotkeys = maManual
    Left = 408
    Top = 128
    object pmMenueSchliessen: TMenuItem
      Caption = 'Men'#252' schlie'#223'en'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mAnzeigen: TMenuItem
      Caption = 'Anzeigen'
      Default = True
      OnClick = mAnzeigenClick
    end
    object mInfo: TMenuItem
      Caption = 'Info'
      OnClick = mInfoClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mBeenden: TMenuItem
      Caption = 'Beenden'
      OnClick = mBeendenClick
    end
  end
  object LongTermTimer: TTimer
    Enabled = False
    Interval = 300000
    OnTimer = LongTermTimerTimer
    Left = 376
    Top = 97
  end
  object ServerSocketRemoteMonitor: TServerSocket
    Active = False
    Port = 0
    ServerType = stNonBlocking
    OnClientConnect = ServerSocketRemoteMonitorClientConnect
    OnClientDisconnect = ServerSocketRemoteMonitorClientDisconnect
    OnClientError = ServerSocketRemoteMonitorClientError
    Left = 408
    Top = 96
  end
end
