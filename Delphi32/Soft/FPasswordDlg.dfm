inherited FormPassworteingabe: TFormPassworteingabe
  Caption = 'Passworteingabe'
  ClientHeight = 98
  ClientWidth = 335
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnClient: TPanel
    Width = 335
    Height = 57
    object ePassword: TEdit
      Left = 32
      Top = 16
      Width = 265
      Height = 21
      PasswordChar = '*'
      TabOrder = 0
      Text = 'ePassword'
      OnChange = ePasswordChange
    end
  end
  inherited pnBottom: TPanel
    Top = 57
    Width = 335
    inherited pnBottomBtnOk: TPanel
      Left = 44
    end
    object Panel1: TPanel
      Left = 180
      Top = 5
      Width = 100
      Height = 33
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object bbtnCancel: TBitBtn
        Left = 4
        Top = 4
        Width = 92
        Height = 25
        TabOrder = 0
        Kind = bkCancel
        Spacing = -1
      end
    end
  end
end
