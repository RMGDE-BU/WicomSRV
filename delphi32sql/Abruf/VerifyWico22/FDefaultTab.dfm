object FormVerifyWico22Tab: TFormVerifyWico22Tab
  Left = 185
  Top = 217
  Width = 952
  Height = 656
  Caption = 'Basis-Tabulatorfenster'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnButtons: TPanel
    Left = 759
    Top = 0
    Width = 185
    Height = 629
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnClient: TPanel
    Left = 0
    Top = 0
    Width = 759
    Height = 629
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    object pnModules: TPanel
      Left = 3
      Top = 3
      Width = 753
      Height = 623
      Align = alClient
      TabOrder = 0
      object gbExternalModules: TGroupBox
        Left = 1
        Top = 1
        Width = 751
        Height = 621
        Align = alClient
        Caption = 'Externe Programmmodule'
        TabOrder = 0
        object lvExternalModules: TListView
          Left = 2
          Top = 15
          Width = 747
          Height = 604
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Beschreibung'
            end
            item
              Caption = 'Dateiname'
            end>
          TabOrder = 0
          ViewStyle = vsReport
          OnResize = lvExternalModulesResize
        end
      end
    end
  end
end
