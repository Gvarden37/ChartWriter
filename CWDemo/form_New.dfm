object frm_New: Tfrm_New
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Create new chart'
  ClientHeight = 392
  ClientWidth = 389
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 8
    Top = 319
    Width = 363
    Height = 1
  end
  object sbNext: TSpeedButton
    Left = 101
    Top = 291
    Width = 75
    Height = 22
    Action = acNext
  end
  object sbPrevious: TSpeedButton
    Left = 197
    Top = 291
    Width = 75
    Height = 22
    Action = acPrev
  end
  object bOK: TButton
    Left = 8
    Top = 331
    Width = 75
    Height = 25
    Action = acOK
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button1: TButton
    Left = 89
    Top = 331
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object sb: TStatusBar
    Left = 0
    Top = 370
    Width = 389
    Height = 22
    Panels = <>
    SimplePanel = True
  end
  object pc: TPageControl
    Left = 0
    Top = 0
    Width = 389
    Height = 289
    ActivePage = tabGeneral
    Align = alTop
    TabOrder = 3
    object tabGeneral: TTabSheet
      Caption = 'General'
      ImageIndex = 1
      object Label5: TLabel
        Left = 9
        Top = 27
        Width = 52
        Height = 13
        Caption = 'Chart type'
      end
      object Label10: TLabel
        Left = 9
        Top = 139
        Width = 50
        Height = 26
        Caption = 'Number of legends'
        WordWrap = True
      end
      object Label11: TLabel
        Left = 9
        Top = 52
        Width = 49
        Height = 13
        Caption = 'Span type'
      end
      object eTitle: TLabeledEdit
        Left = 67
        Top = 0
        Width = 226
        Height = 21
        HelpContext = 312
        EditLabel.Width = 20
        EditLabel.Height = 13
        EditLabel.Caption = 'Title'
        LabelPosition = lpLeft
        LabelSpacing = 35
        TabOrder = 0
        Text = '<Title>'
      end
      object cbChartType: TComboBox
        Left = 67
        Top = 24
        Width = 150
        Height = 21
        HelpContext = 204
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 1
        Text = 'General flow chart'
        OnChange = cbChartTypeChange
        Items.Strings = (
          'Time or number span'
          'General flow chart'
          'Pie chart'
          'Category bar chart'
          '')
      end
      object cbxCorrelation: TCheckBox
        Left = 67
        Top = 74
        Width = 97
        Height = 17
        HelpContext = 267
        Caption = 'Correlation data'
        TabOrder = 3
        OnClick = cbxCorrelationClick
      end
      object cbxIncludeNSections: TCheckBox
        Left = 67
        Top = 95
        Width = 121
        Height = 17
        Caption = 'Include name sections'
        TabOrder = 4
      end
      object cbxIncludeVSections: TCheckBox
        Left = 67
        Top = 116
        Width = 121
        Height = 17
        Caption = 'Include value sections'
        TabOrder = 5
      end
      object eNumLegends: TEdit
        Left = 67
        Top = 140
        Width = 30
        Height = 21
        Alignment = taRightJustify
        MaxLength = 1
        NumbersOnly = True
        TabOrder = 6
        Text = '0'
      end
      object cbSpanType: TComboBox
        Left = 67
        Top = 49
        Width = 148
        Height = 21
        HelpContext = 324
        Style = csDropDownList
        Enabled = False
        TabOrder = 2
        Items.Strings = (
          'Month'
          'Date'
          'Hour'
          'Minute'
          'Second'
          'Number')
      end
    end
    object tabSeries: TTabSheet
      Caption = 'Series'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 10
        Top = 34
        Width = 29
        Height = 13
        Caption = 'Series'
      end
      object Label2: TLabel
        Left = 10
        Top = 155
        Width = 29
        Height = 13
        Caption = 'Graph'
      end
      object Label3: TLabel
        Left = 11
        Top = 177
        Width = 25
        Height = 13
        Caption = 'Color'
      end
      object Label4: TLabel
        Left = 10
        Top = 201
        Width = 53
        Height = 13
        Caption = 'Value scale'
      end
      object sbMoveUp: TSpeedButton
        Left = 298
        Top = 50
        Width = 64
        Height = 22
        Action = acMoveUp
      end
      object sbMoveDown: TSpeedButton
        Left = 298
        Top = 78
        Width = 64
        Height = 22
        Action = acMoveDown
      end
      object Label6: TLabel
        Left = 11
        Top = 223
        Width = 53
        Height = 26
        Caption = 'Alternative graph'
        WordWrap = True
      end
      object sbAdd: TSpeedButton
        Left = 70
        Top = 6
        Width = 47
        Height = 22
        Action = acAdd
      end
      object sbUpdate: TSpeedButton
        Left = 181
        Top = 6
        Width = 47
        Height = 22
        Action = acUpdate
      end
      object sbDelete: TSpeedButton
        Left = 125
        Top = 6
        Width = 47
        Height = 22
        Action = acDelete
      end
      object cbGraphs: TComboBox
        Left = 69
        Top = 149
        Width = 75
        Height = 21
        HelpContext = 211
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 2
        Text = 'Curve'
        Items.Strings = (
          'Curve'
          'Bar'
          'Pie')
      end
      object lbSeries: TListBox
        Left = 69
        Top = 34
        Width = 223
        Height = 86
        HelpContext = 309
        Style = lbOwnerDrawFixed
        TabOrder = 0
        OnClick = lbSeriesClick
        OnDrawItem = lbSeriesDrawItem
      end
      object eSeriesTitle: TLabeledEdit
        Left = 69
        Top = 124
        Width = 155
        Height = 21
        HelpContext = 412
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Series title'
        LabelPosition = lpLeft
        LabelSpacing = 9
        TabOrder = 1
        Text = 'Series 1'
      end
      object cbSeriesColor: TColorBox
        Left = 68
        Top = 174
        Width = 107
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 3
      end
      object cbValueScale: TComboBox
        Left = 68
        Top = 200
        Width = 107
        Height = 21
        HelpContext = 160
        Style = csDropDownList
        Enabled = False
        ItemIndex = 0
        TabOrder = 4
        Text = 'Value scale 1'
        Items.Strings = (
          'Value scale 1'
          'Value scale 2'
          'Not relevant')
      end
      object cbAltGraph: TComboBox
        Left = 69
        Top = 226
        Width = 75
        Height = 21
        HelpContext = 462
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 5
        Text = 'None'
        OnChange = cbAltGraphChange
        Items.Strings = (
          'None'
          'Curve'
          'Bar'
          'Pie')
      end
    end
    object tabCats: TTabSheet
      Caption = 'Categories'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label9: TLabel
        Left = 8
        Top = 235
        Width = 25
        Height = 13
        Caption = 'Color'
      end
      object Label8: TLabel
        Left = 8
        Top = 211
        Width = 27
        Height = 13
        Caption = 'Name'
      end
      object Label7: TLabel
        Left = 8
        Top = 34
        Width = 52
        Height = 13
        Caption = 'Categories'
      end
      object sbAddCat: TSpeedButton
        Left = 70
        Top = 3
        Width = 47
        Height = 22
        Action = acAddCat
      end
      object sbUpdateCat: TSpeedButton
        Left = 181
        Top = 3
        Width = 47
        Height = 22
        Action = acUpdateCat
      end
      object sbDeleteCat: TSpeedButton
        Left = 125
        Top = 3
        Width = 47
        Height = 22
        Action = acDeleteCat
      end
      object SpeedButton1: TSpeedButton
        Left = 234
        Top = 81
        Width = 64
        Height = 22
        Action = acMoveUp
      end
      object SpeedButton2: TSpeedButton
        Left = 234
        Top = 109
        Width = 64
        Height = 22
        Action = acMoveDown
      end
      object lbItems: TListBox
        Left = 72
        Top = 32
        Width = 156
        Height = 170
        HelpContext = 308
        Style = lbOwnerDrawFixed
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbItemsClick
        OnDrawItem = lbItemsDrawItem
      end
      object eItemName: TEdit
        Left = 72
        Top = 208
        Width = 171
        Height = 21
        HelpContext = 282
        TabOrder = 1
        Text = 'Category 1'
      end
      object cbChartItemColor: TColorBox
        Left = 72
        Top = 233
        Width = 113
        Height = 22
        HelpContext = 281
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 2
      end
    end
  end
  object ActionList1: TActionList
    Left = 341
    Top = 231
    object acAdd: TAction
      Caption = '&Add'
      OnExecute = acAddExecute
      OnUpdate = acAddUpdate
    end
    object acUpdate: TAction
      Caption = '&Update'
      OnExecute = acUpdateExecute
      OnUpdate = acUpdateUpdate
    end
    object acDelete: TAction
      Caption = '&Delete'
      OnExecute = acDeleteExecute
      OnUpdate = acDeleteUpdate
    end
    object acMoveUp: TAction
      Caption = 'Move up'
      OnExecute = acMoveUpExecute
      OnUpdate = acMoveUpUpdate
    end
    object acMoveDown: TAction
      Caption = 'Move down'
      OnExecute = acMoveDownExecute
      OnUpdate = acMoveDownUpdate
    end
    object acAddCat: TAction
      Caption = '&Add'
      OnExecute = acAddCatExecute
      OnUpdate = acAddCatUpdate
    end
    object acUpdateCat: TAction
      Caption = '&Update'
      OnExecute = acUpdateCatExecute
      OnUpdate = acUpdateCatUpdate
    end
    object acDeleteCat: TAction
      Caption = '&Delete'
      OnExecute = acDeleteCatExecute
      OnUpdate = acDeleteCatUpdate
    end
    object acNext: TAction
      Caption = '&Next'
      OnExecute = acNextExecute
      OnUpdate = acNextUpdate
    end
    object acPrev: TAction
      Caption = '&Back'
      OnExecute = acPrevExecute
      OnUpdate = acPrevUpdate
    end
    object acOK: TAction
      Caption = '&OK'
      OnUpdate = acOKUpdate
    end
  end
end
