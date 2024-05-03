inherited FormCOMPanelDlg: TFormCOMPanelDlg
  ClientHeight = 124
  ClientWidth = 368
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnClient: TPanel
    Width = 368
    Height = 83
    object lStatus: TLabel
      Left = 16
      Top = 16
      Width = 33
      Height = 13
      Caption = 'Status:'
    end
    object pStatus: TPanel
      Left = 16
      Top = 32
      Width = 337
      Height = 33
      BevelOuter = bvLowered
      TabOrder = 0
    end
  end
  inherited pnBottom: TPanel
    Top = 83
    Width = 368
  end
end
