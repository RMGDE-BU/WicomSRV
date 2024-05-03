object FormAuswahlDlg: TFormAuswahlDlg
  Left = 460
  Top = 469
  BorderStyle = bsDialog
  Caption = 'Auswahl'
  ClientHeight = 126
  ClientWidth = 415
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnBottom: TPanel
    Left = 0
    Top = 85
    Width = 415
    Height = 41
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object bbtnOk: TBitBtn
      Left = 14
      Top = 8
      Width = 147
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      Kind = bkOK
      Spacing = -1
    end
    object bbtnCancel: TBitBtn
      Left = 248
      Top = 8
      Width = 153
      Height = 25
      Caption = '&Abbrechen'
      TabOrder = 1
      Kind = bkCancel
      Spacing = -1
    end
  end
  object pnClient: TPanel
    Left = 0
    Top = 0
    Width = 415
    Height = 85
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object lAuswahl: TLabel
      Left = 16
      Top = 24
      Width = 43
      Height = 13
      Caption = 'Auswahl:'
    end
    object cbAuswahl: TComboBox
      Left = 16
      Top = 40
      Width = 385
      Height = 21
      Style = csDropDownList
      DropDownCount = 16
      ItemHeight = 13
      TabOrder = 0
    end
  end
end
