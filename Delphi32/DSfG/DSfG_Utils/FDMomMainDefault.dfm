inherited FormMomMainDefault: TFormMomMainDefault
  Caption = 'Default-Haupfenster für DSfG-Momentanwertdarstellungen'
  FormStyle = fsMDIForm
  OldCreateOrder = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnTop: TPanel
    inherited pnTopRest: TPanel
      Left = 221
      Width = 636
      Font.Color = clNavy
      Font.Height = -24
      Font.Style = [fsBold]
      ParentFont = False
    end
    object pnAbrufControl: TPanel
      Left = 36
      Top = 5
      Width = 185
      Height = 31
      Align = alLeft
      TabOrder = 2
    end
  end
  inherited MainMenu: TMainMenu
    Left = 40
    Top = 49
    object miAufruf: TMenuItem [1]
      Caption = '&Aufruf'
    end
    object miParametrierung: TMenuItem [2]
      Caption = 'Pa&rametrierung'
    end
  end
  object TimerAbrufStatus: TTimer
    Enabled = False
    OnTimer = TimerAbrufStatusTimer
    Left = 72
    Top = 49
  end
end
