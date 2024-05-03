object FormGPRSServerMain: TFormGPRSServerMain
  Left = 123
  Top = 225
  Width = 1105
  Height = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 1097
    Height = 664
    ActivePage = tsheetKomm
    Align = alClient
    TabOrder = 0
    object tsheetKomm: TTabSheet
      Caption = 'Kommunikation'
      object pLeft: TPanel
        Left = 0
        Top = 0
        Width = 888
        Height = 606
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pLeft'
        TabOrder = 0
        object Splitter1: TSplitter
          Left = 0
          Top = 477
          Width = 888
          Height = 4
          Cursor = crVSplit
          Align = alBottom
        end
        object Splitter2: TSplitter
          Left = 0
          Top = 241
          Width = 888
          Height = 4
          Cursor = crVSplit
          Align = alTop
        end
        object pFehler: TPanel
          Left = 0
          Top = 481
          Width = 888
          Height = 125
          Align = alBottom
          Caption = 'pIPFehler'
          TabOrder = 2
          object memoError: TMemo
            Left = 1
            Top = 22
            Width = 886
            Height = 102
            Align = alClient
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
          end
          object pErrors: TPanel
            Left = 1
            Top = 1
            Width = 886
            Height = 21
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = ' IP-Fehler:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
          end
        end
        object pKommunikation: TPanel
          Left = 0
          Top = 245
          Width = 888
          Height = 232
          Align = alClient
          Caption = 'pIPKomm'
          TabOrder = 1
          object pReceive: TPanel
            Left = 1
            Top = 1
            Width = 886
            Height = 25
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = ' IP-Kommunikation:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
          end
          object memoReceive: TMemo
            Left = 1
            Top = 26
            Width = 886
            Height = 205
            Align = alClient
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 1
          end
        end
        object pVerbindungen: TPanel
          Left = 0
          Top = 0
          Width = 888
          Height = 241
          Align = alTop
          Caption = 'pIPVerb'
          TabOrder = 0
          object Panel2: TPanel
            Left = 1
            Top = 1
            Width = 886
            Height = 25
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = ' IP-Verbindungen:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            object pVerbAktiv: TPanel
              Left = 120
              Top = 2
              Width = 113
              Height = 20
              BevelOuter = bvLowered
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGreen
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 0
            end
            object pVerbInaktiv: TPanel
              Left = 240
              Top = 2
              Width = 113
              Height = 20
              BevelOuter = bvLowered
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clRed
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 1
            end
          end
          object lvVerbindungen: TListView
            Left = 1
            Top = 26
            Width = 886
            Height = 214
            Align = alClient
            BorderWidth = 1
            Columns = <
              item
                Caption = 'Aktiv'
                Width = 40
              end
              item
                Caption = 'IP-Adresse'
                Width = 100
              end
              item
                Alignment = taRightJustify
                Caption = 'Port'
                Width = 46
              end
              item
                Caption = 'Ger'#228'tetyp'
                Width = 70
              end
              item
                Caption = 'Kennung'
                Width = 120
              end
              item
                Alignment = taRightJustify
                Caption = 'Telegr. empf.'
                Width = 80
              end
              item
                Alignment = taRightJustify
                Caption = 'Bl'#246'cke empf.'
                Width = 80
              end
              item
                Alignment = taRightJustify
                Caption = 'Bytes empf.'
                Width = 70
              end
              item
                Alignment = taRightJustify
                Caption = 'Ge'#246'ffnet'
                Width = 60
              end
              item
                Alignment = taRightJustify
                Caption = 'Geschlossen'
                Width = 80
              end
              item
                Caption = 'Letzte Aktion am'
                Width = 114
              end>
            ColumnClick = False
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            SortType = stData
            StateImages = VerbImageList
            TabOrder = 1
            ViewStyle = vsReport
          end
        end
      end
      object pRight: TPanel
        Left = 888
        Top = 0
        Width = 201
        Height = 606
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object pStatistik: TPanel
          Left = 0
          Top = 0
          Width = 201
          Height = 25
          Align = alTop
          Alignment = taLeftJustify
          Caption = ' Gesamt-Statistik:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
        end
        object Panel6: TPanel
          Left = 0
          Top = 25
          Width = 201
          Height = 581
          Align = alClient
          TabOrder = 1
          object lSrvLaufzeit: TLabel
            Left = 8
            Top = 8
            Width = 74
            Height = 13
            Caption = 'Server-Laufzeit:'
          end
          object lAnzVerbGeoeffnet: TLabel
            Left = 8
            Top = 48
            Width = 160
            Height = 13
            Caption = 'Anzahl Ereignisse '#39'Verb. ge'#246'ffnet'#39':'
          end
          object lAnzVerbGeschlossen: TLabel
            Left = 8
            Top = 88
            Width = 180
            Height = 13
            Caption = 'Anzahl Ereignisse '#39'Verb. geschlossen'#39':'
          end
          object lAnzRecTelegramme: TLabel
            Left = 8
            Top = 168
            Width = 152
            Height = 13
            Caption = 'Anzahl Telegramme empfangen:'
          end
          object lAnzRecBytes: TLabel
            Left = 8
            Top = 248
            Width = 120
            Height = 13
            Caption = 'Anzahl Bytes empfangen:'
          end
          object lAnzIPFehler: TLabel
            Left = 8
            Top = 288
            Width = 80
            Height = 13
            Caption = 'Anzahl IP-Fehler:'
          end
          object lAnzReadEvents: TLabel
            Left = 8
            Top = 128
            Width = 163
            Height = 13
            Caption = 'Anzahl Ereignisse '#39'Datenempfang'#39':'
          end
          object lAnzRecBloecke: TLabel
            Left = 8
            Top = 208
            Width = 127
            Height = 13
            Caption = 'Anzahl Bl'#246'cke empfangen:'
          end
          object eSrvLaufzeit: TEdit
            Left = 8
            Top = 24
            Width = 169
            Height = 21
            TabStop = False
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object eAnzVerbGeoeffnet: TEdit
            Left = 8
            Top = 64
            Width = 169
            Height = 21
            TabStop = False
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
          object eAnzVerbGeschlossen: TEdit
            Left = 8
            Top = 104
            Width = 169
            Height = 21
            TabStop = False
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 2
          end
          object eAnzRecTelegramme: TEdit
            Left = 8
            Top = 184
            Width = 169
            Height = 21
            TabStop = False
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 3
          end
          object eAnzRecBytes: TEdit
            Left = 8
            Top = 264
            Width = 169
            Height = 21
            TabStop = False
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 4
          end
          object eAnzIPFehler: TEdit
            Left = 8
            Top = 304
            Width = 169
            Height = 21
            TabStop = False
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 5
          end
          object eAnzReadEvents: TEdit
            Left = 7
            Top = 144
            Width = 169
            Height = 21
            TabStop = False
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 6
          end
          object eAnzRecBloecke: TEdit
            Left = 7
            Top = 224
            Width = 169
            Height = 21
            TabStop = False
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 7
          end
        end
      end
      object pPort: TPanel
        Left = 0
        Top = 606
        Width = 1089
        Height = 30
        Align = alBottom
        Alignment = taLeftJustify
        Caption = ' Server nicht aktiv, Port geschlossen'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
    end
  end
  object ServerSocket: TServerSocket
    Active = False
    Port = 0
    ServerType = stNonBlocking
    OnClientConnect = ServerSocketClientConnect
    OnClientDisconnect = ServerSocketClientDisconnect
    OnClientRead = ServerSocketClientRead
    OnClientError = ServerSocketClientError
    Left = 128
    Top = 88
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 176
    Top = 88
  end
  object VerbImageList: TImageList
    Left = 84
    Top = 88
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000800000008000000080000000800000008000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000080
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000080000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000080
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000080000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000008000000080
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000008000000080
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000008000000080
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000008000000080
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000080
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000080000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000080
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000080000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000800000008000000080000000800000008000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FC1FFC1F00000000
      F007F00700000000E003E00300000000C001C00100000000C001C00100000000
      8000800000000000800080000000000080008000000000008000800000000000
      C001C00100000000C001C00100000000E003E00300000000F007F00700000000
      FC1FFC1F00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
