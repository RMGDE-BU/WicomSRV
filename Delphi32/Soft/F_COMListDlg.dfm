inherited FormCOMListDlg: TFormCOMListDlg
  ClientHeight = 325
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnClient: TPanel
    Height = 284
    object lDaten: TLabel [1]
      Left = 16
      Top = 80
      Width = 32
      Height = 13
      Caption = 'Daten:'
    end
    object lbDaten: TListBox
      Left = 16
      Top = 96
      Width = 337
      Height = 169
      ItemHeight = 13
      PopupMenu = pmDaten
      TabOrder = 1
      TabWidth = 12
    end
  end
  inherited pnBottom: TPanel
    Top = 284
    inherited bbtnBeenden: TBitBtn
      ModalResult = 2
    end
  end
  object pmDaten: TPopupMenu
    Left = 312
    Top = 112
    object miClipboard: TMenuItem
      Caption = 'In Zwischenablage kopieren'
      OnClick = miClipboardClick
    end
  end
end
