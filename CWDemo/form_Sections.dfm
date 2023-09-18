object formSections: TformSections
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Section properties'
  ClientHeight = 252
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 32
    Top = 16
    Width = 97
    Height = 22
    Hint = 'Caption font'
    Action = acSectionFont
  end
  object lb_Font: TLabel
    Left = 144
    Top = 20
    Width = 47
    Height = 13
    Caption = 'Tahoma 8'
  end
  object SpeedButton2: TSpeedButton
    Left = 32
    Top = 44
    Width = 97
    Height = 22
    Hint = 'Section line pen'
    HelpContext = 385
    Caption = 'Line pen ...'
    OnClick = SpeedButton2Click
  end
  object Label2: TLabel
    Left = 36
    Top = 75
    Width = 70
    Height = 13
    Caption = 'Caption layout'
  end
  object Label3: TLabel
    Left = 36
    Top = 102
    Width = 102
    Height = 13
    Caption = 'Caption horiz. margin'
  end
  object Label4: TLabel
    Left = 36
    Top = 129
    Width = 99
    Height = 13
    Caption = 'Caption vert. margin'
  end
  object bOK: TButton
    Left = 36
    Top = 208
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object bCancel: TButton
    Left = 117
    Top = 208
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbCaptionLayout: TComboBox
    Left = 144
    Top = 72
    Width = 145
    Height = 21
    Hint = 'Sets position of the caption row'
    HelpContext = 381
    Style = csDropDownList
    TabOrder = 2
    OnChange = cbCaptionLayoutChange
    Items.Strings = (
      'Same side as labels'
      'Oposite side of labels')
  end
  object eHorizMargin: TEdit
    Left = 144
    Top = 99
    Width = 57
    Height = 21
    Hint = 'Sets space between captions and graph area along the y axis'
    HelpContext = 382
    NumbersOnly = True
    TabOrder = 3
    OnExit = eHorizMarginExit
  end
  object eVertMargin: TEdit
    Left = 144
    Top = 126
    Width = 57
    Height = 21
    Hint = 'Sets space between captions and graph area along the x axis'
    HelpContext = 383
    NumbersOnly = True
    TabOrder = 4
    OnExit = eVertMarginExit
  end
  object cbxShowLines: TCheckBox
    Left = 36
    Top = 153
    Width = 121
    Height = 17
    Hint = 'Shows / hides the sections'
    HelpContext = 387
    Alignment = taLeftJustify
    Caption = 'Show lines'
    TabOrder = 5
    OnClick = cbxShowLinesClick
  end
  object cbxProlongedLines: TCheckBox
    Left = 36
    Top = 177
    Width = 121
    Height = 17
    Hint = 'Divides the captions by prolonging the section lines'
    HelpContext = 386
    Alignment = taLeftJustify
    Caption = 'Prolonged lines'
    TabOrder = 6
    OnClick = cbxProlongedLinesClick
  end
  object ActionList1: TActionList
    Left = 256
    Top = 192
    object acSectionFont: TFontEdit
      Category = 'Dialog'
      Caption = '&Font ...'
      Dialog.Font.Charset = DEFAULT_CHARSET
      Dialog.Font.Color = clWindowText
      Dialog.Font.Height = -11
      Dialog.Font.Name = 'Tahoma'
      Dialog.Font.Style = []
      Hint = 'Font Select'
      BeforeExecute = acSectionFontBeforeExecute
      OnAccept = acSectionFontAccept
    end
  end
end
