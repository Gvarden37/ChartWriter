object Frame1: TFrame1
  Left = 0
  Top = 0
  Width = 278
  Height = 107
  TabOrder = 0
  object Label103: TLabel
    Left = 7
    Top = 46
    Width = 48
    Height = 13
    Caption = 'Label font'
  end
  object sbLabelFont: TSpeedButton
    Left = 171
    Top = 41
    Width = 33
    Height = 22
    Action = acFontDialog
  end
  object sbDivLines: TSpeedButton
    Left = 171
    Top = 81
    Width = 33
    Height = 22
    Hint = 'Divider line pen properties'
    Caption = '...'
    OnClick = sbDivLinesClick
  end
  object Label105: TLabel
    Left = 7
    Top = 85
    Width = 40
    Height = 13
    Caption = 'Line pen'
  end
  object lbDivLinePen: TLabel
    Left = 104
    Top = 85
    Width = 22
    Height = 13
    Caption = 'Solid'
  end
  object lbFont: TLabel
    Left = 104
    Top = 45
    Width = 47
    Height = 13
    Caption = 'Tahoma 8'
  end
  object Label1: TLabel
    Left = 7
    Top = 6
    Width = 40
    Height = 13
    Caption = 'Qualifier'
  end
  object cbxShowDividerLines: TCheckBox
    Left = 7
    Top = 62
    Width = 110
    Height = 17
    Hint = 'Shows / hides the label divider lines'
    Alignment = taLeftJustify
    Caption = 'Show divider lines'
    TabOrder = 2
    OnClick = cbxShowDividerLinesClick
  end
  object cbxShowLabels: TCheckBox
    Left = 8
    Top = 26
    Width = 110
    Height = 17
    Hint = 'Shows / hide the labels'
    Alignment = taLeftJustify
    Caption = 'Show labels'
    TabOrder = 1
    OnClick = cbxShowLabelsClick
  end
  object eQualifier: TEdit
    Left = 104
    Top = 3
    Width = 171
    Height = 21
    Hint = 'Text that explains the type of name or value'
    TabOrder = 0
    OnExit = eQualifierExit
  end
  object ActionList1: TActionList
    Left = 224
    Top = 29
    object acFontDialog: TFontEdit
      Category = 'Dialog'
      Caption = '...'
      Dialog.Font.Charset = DEFAULT_CHARSET
      Dialog.Font.Color = clWindowText
      Dialog.Font.Height = -11
      Dialog.Font.Name = 'Tahoma'
      Dialog.Font.Style = []
      Hint = 'Font Select'
      BeforeExecute = acFontDialogBeforeExecute
      OnAccept = acFontDialogAccept
    end
  end
end
