object formPen: TformPen
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Pen properties'
  ClientHeight = 185
  ClientWidth = 227
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 20
    Width = 25
    Height = 13
    Caption = 'Color'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 28
    Height = 13
    Caption = 'Width'
  end
  object Label3: TLabel
    Left = 16
    Top = 77
    Width = 24
    Height = 13
    Caption = 'Style'
  end
  object eWidth: TEdit
    Left = 72
    Top = 45
    Width = 49
    Height = 21
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 0
  end
  object cbStyle: TComboBox
    Left = 72
    Top = 74
    Width = 97
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    Items.Strings = (
      'Solid'
      'Dot'
      'Dash')
  end
  object cbColor: TColorBox
    Left = 72
    Top = 17
    Width = 97
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    TabOrder = 2
  end
  object bOK: TButton
    Left = 32
    Top = 120
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object bCancel: TButton
    Left = 113
    Top = 120
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
