object FormStationsAuswahl: TFormStationsAuswahl
  Left = 279
  Top = 194
  Width = 870
  Height = 640
  Caption = 'Stationsauswahl'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Width = 3
    Height = 613
    Cursor = crHSplit
    AutoSnap = False
    Beveled = True
  end
  object pnTree: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 613
    Align = alLeft
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
  end
  object pnButtons: TPanel
    Left = 821
    Top = 0
    Width = 41
    Height = 613
    Align = alRight
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
  end
  object pnClient: TPanel
    Left = 188
    Top = 0
    Width = 633
    Height = 613
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
  end
end
