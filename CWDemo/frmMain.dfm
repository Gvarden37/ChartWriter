object form_main: Tform_main
  Left = 0
  Top = 0
  Caption = 'ChartWriter demo'
  ClientHeight = 791
  ClientWidth = 1484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHelp = FormHelp
  PixelsPerInch = 96
  TextHeight = 13
  object lbNLabelFont: TLabel
    Left = 163
    Top = 103
    Width = 47
    Height = 13
    Caption = 'Tahoma 8'
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 1484
    Height = 29
    Caption = 'ToolBar1'
    Images = ImageList1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object ToolButton7: TToolButton
      Left = 0
      Top = 0
      Action = acNewChart
    end
    object ToolButton1: TToolButton
      Left = 23
      Top = 0
      Action = FileOpen1
    end
    object ToolButton8: TToolButton
      Left = 46
      Top = 0
      Action = FileSaveAs1
    end
    object ToolButton4: TToolButton
      Left = 69
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object ToolButton5: TToolButton
      Left = 77
      Top = 0
      Action = acDashbord
      Style = tbsCheck
    end
    object ToolButton6: TToolButton
      Left = 100
      Top = 0
      Action = acShowData
    end
    object ToolButton3: TToolButton
      Left = 123
      Top = 0
      Action = acUnzoom
    end
    object ToolButton10: TToolButton
      Left = 146
      Top = 0
      Action = acPrevPage
    end
    object ToolButton2: TToolButton
      Left = 169
      Top = 0
      Action = acNextPage
    end
    object ToolButton9: TToolButton
      Left = 192
      Top = 0
      Width = 8
      Caption = 'ToolButton9'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object ToolButton11: TToolButton
      Left = 200
      Top = 0
      Action = acReload
    end
  end
  object panChart: TPanel
    Left = 0
    Top = 29
    Width = 420
    Height = 743
    Align = alLeft
    Caption = 'panChart'
    ShowCaption = False
    TabOrder = 1
    Visible = False
    object Label80: TLabel
      Left = 174
      Top = 414
      Width = 40
      Height = 16
      Caption = 'Graphs'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object pgGraphs: TPageControl
      Left = 1
      Top = 291
      Width = 418
      Height = 451
      ActivePage = Curve
      Align = alClient
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Visible = False
      object Curve: TTabSheet
        Caption = 'Curve'
        object lbCurveAnimationSpeed: TLabel
          Left = 209
          Top = 173
          Width = 79
          Height = 13
          Caption = 'Animation speed'
        end
        object Label50: TLabel
          Left = 2
          Top = 243
          Width = 86
          Height = 13
          Caption = 'Min. point spacing'
        end
        object Label51: TLabel
          Left = 2
          Top = 268
          Width = 90
          Height = 13
          Caption = 'Max. point spacing'
        end
        object Label53: TLabel
          Left = 2
          Top = 318
          Width = 65
          Height = 13
          Caption = 'Point markers'
        end
        object sbFont: TSpeedButton
          Left = -16
          Top = 119
          Width = 79
          Height = 22
          Action = acCurveFont
        end
        object lbCurveFontName: TLabel
          Left = 96
          Top = 343
          Width = 47
          Height = 13
          Caption = 'Tahoma 8'
        end
        object Label56: TLabel
          Left = 209
          Top = 200
          Width = 79
          Height = 13
          Caption = 'Animation pause'
        end
        object Label43: TLabel
          Left = 358
          Top = 200
          Width = 32
          Height = 13
          Caption = 'm.sec.'
        end
        object Label105: TLabel
          Left = 209
          Top = 224
          Width = 150
          Height = 26
          Caption = '-1 = Manual stop.  Click mouse to resume'
          WordWrap = True
        end
        object Label108: TLabel
          Left = 3
          Top = 219
          Width = 51
          Height = 13
          Caption = 'Line shape'
        end
        object gbCurveStyles: TGroupBox
          Left = 2
          Top = 3
          Width = 199
          Height = 210
          Caption = 'Curve styles'
          Color = clCream
          ParentBackground = False
          ParentColor = False
          TabOrder = 0
          object lbStyle: TLabel
            Left = 4
            Top = 97
            Width = 24
            Height = 13
            Caption = 'Style'
          end
          object lbLineStyle: TLabel
            Left = 4
            Top = 124
            Width = 45
            Height = 13
            Caption = 'Line style'
          end
          object lbLineWidth: TLabel
            Left = 4
            Top = 151
            Width = 48
            Height = 13
            Caption = 'Line width'
          end
          object Label57: TLabel
            Left = 3
            Top = 20
            Width = 42
            Height = 13
            Caption = 'Apply on'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object sbApplyOnAll: TSpeedButton
            Left = 54
            Top = 177
            Width = 136
            Height = 22
            Action = acApply
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object cbCurveStyle: TComboBox
            Left = 56
            Top = 94
            Width = 134
            Height = 21
            HelpContext = 296
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 1
            Text = 'Line'
            OnChange = cbCurveStyleChange
            Items.Strings = (
              'Line'
              'ClientArea'
              'BaseLineArea'
              'NeighborArea'
              'Points')
          end
          object cbLineStyle: TComboBox
            Left = 56
            Top = 121
            Width = 134
            Height = 21
            HelpContext = 292
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 2
            Text = 'Solid'
            Items.Strings = (
              'Solid'
              'Dot'
              'Dash')
          end
          object eLineWidth: TEdit
            Left = 55
            Top = 148
            Width = 25
            Height = 21
            HelpContext = 295
            MaxLength = 1
            NumbersOnly = True
            TabOrder = 3
            Text = '1'
          end
          object lbApplyOn: TListBox
            Left = 54
            Top = 24
            Width = 136
            Height = 65
            Hint = 'The sereies that will be affected by the styles'
            HelpContext = 61
            ItemHeight = 13
            TabOrder = 0
            OnClick = lbApplyOnClick
          end
        end
        object cbxCurveAnimation: TCheckBox
          Left = 209
          Top = 146
          Width = 97
          Height = 17
          Hint = 'Run animation'
          HelpContext = 427
          Alignment = taLeftJustify
          Caption = 'Animation'
          TabOrder = 8
          OnClick = cbxCurveAnimationClick
        end
        object cbCurveAnimationSpeed: TComboBox
          Left = 294
          Top = 170
          Width = 100
          Height = 21
          HelpContext = 331
          Style = csDropDownList
          ItemIndex = 1
          TabOrder = 9
          Text = 'Medium fast'
          OnChange = cbCurvePointMarkersChange
          Items.Strings = (
            'Fast'
            'Medium fast'
            'Medium slow'
            'Slow')
        end
        object eCurveMinPointSpacing: TEdit
          Left = 95
          Top = 240
          Width = 65
          Height = 21
          Hint = 'Minimum space in pixels between the  points. 0 = compressed'
          HelpContext = 57
          TabOrder = 2
          OnExit = cbCurvePointMarkersChange
        end
        object eCurveMaxPointSpacing: TEdit
          Left = 95
          Top = 265
          Width = 65
          Height = 21
          Hint = 'Max space in pixels between the points. 0 = no limit'
          HelpContext = 56
          TabOrder = 3
          OnExit = cbCurvePointMarkersChange
        end
        object cbxCurveBeaconPoints: TCheckBox
          Left = 2
          Top = 292
          Width = 106
          Height = 17
          Hint = 'Displays a rolling point parker, following the mouse'
          HelpContext = 179
          Alignment = taLeftJustify
          Caption = 'Beacon points'
          TabOrder = 4
          OnClick = cbxCurveBeaconPointsClick
        end
        object cbCurvePointMarkers: TComboBox
          Left = 95
          Top = 315
          Width = 123
          Height = 22
          Hint = 'Images or text that marks up the points'
          HelpContext = 53
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'cbCurvePointMarkers'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 5
          Text = 'None'
          OnChange = cbCurvePointMarkersChange
          Items.Strings = (
            'None'
            'Small ball'
            'Big ball'
            'Small concentirc circels'
            'Big concentric circles'
            'Dot'
            'Text')
        end
        object eCurveAnimationPause: TEdit
          Left = 294
          Top = 197
          Width = 57
          Height = 21
          Hint = 
            'Pauses animation between each series. -1 = manual pause. Mouse c' +
            'lick resumes.'
          HelpContext = 340
          TabOrder = 10
        end
        object GroupBox1: TGroupBox
          Left = 207
          Top = 3
          Width = 200
          Height = 137
          Caption = 'Area  styles options'
          TabOrder = 7
          object Label49: TLabel
            Left = 7
            Top = 106
            Width = 71
            Height = 13
            Caption = 'Base line value'
            WordWrap = True
          end
          object Label54: TLabel
            Left = 7
            Top = 22
            Width = 69
            Height = 13
            Caption = 'Neighbor color'
          end
          object Label55: TLabel
            Left = 8
            Top = 71
            Width = 61
            Height = 26
            Caption = 'Area outline color'
            WordWrap = True
          end
          object eCurveBaseLine: TEdit
            Left = 85
            Top = 103
            Width = 65
            Height = 21
            Hint = 'Value that defines the base line split'
            HelpContext = 16
            TabOrder = 3
            OnExit = cbCurvePointMarkersChange
          end
          object cbNeighborAreaColor: TColorBox
            Left = 85
            Top = 19
            Width = 108
            Height = 22
            Hint = 'Color of neighboring area'
            HelpContext = 45
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
            TabOrder = 0
            OnChange = cbNeighborAreaColorChange
          end
          object cbxAreaOutLine: TCheckBox
            Left = 7
            Top = 46
            Width = 91
            Height = 17
            Hint = 'Draws a border around the area'
            HelpContext = 313
            Alignment = taLeftJustify
            Caption = 'Area outline'
            TabOrder = 1
            OnClick = cbxCurveBeaconPointsClick
          end
          object cbAreaOutLineColor: TColorBox
            Left = 85
            Top = 75
            Width = 108
            Height = 22
            HelpContext = 260
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
            TabOrder = 2
            OnChange = cbNeighborAreaColorChange
          end
        end
        object cbxCurveKeepFontColor: TCheckBox
          Left = 2
          Top = 360
          Width = 108
          Height = 17
          Hint = 'Overrides font color adaption, keeping the color of Text font.'
          Alignment = taLeftJustify
          Caption = 'Keep font color'
          TabOrder = 6
          OnClick = cbxCurveKeepFontColorClick
        end
        object cbLineShape: TComboBox
          Left = 94
          Top = 215
          Width = 66
          Height = 22
          Hint = 'Shape of line'
          HelpContext = 71
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'cbCurvePointMarkers'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 1
          Text = 'Straight'
          OnChange = cbLineShapeChange
          Items.Strings = (
            'Straight'
            'Bezier'
            'Step')
        end
        object cbxAntiAliasing: TCheckBox
          Left = 207
          Top = 256
          Width = 101
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Anti aliasing'
          TabOrder = 11
          OnClick = cbxAntiAliasingClick
        end
      end
      object Bar: TTabSheet
        Caption = 'Bar'
        ImageIndex = 1
        ParentShowHint = False
        ShowHint = True
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label58: TLabel
          Left = 6
          Top = 8
          Width = 42
          Height = 13
          Caption = 'Bar style'
        end
        object Label59: TLabel
          Left = 6
          Top = 79
          Width = 45
          Height = 13
          Caption = 'Bar width'
        end
        object Label61: TLabel
          Left = 6
          Top = 33
          Width = 33
          Height = 13
          Caption = 'Layout'
        end
        object Label62: TLabel
          Left = 6
          Top = 103
          Width = 61
          Height = 13
          Caption = 'Item spacing'
        end
        object Label63: TLabel
          Left = 6
          Top = 128
          Width = 68
          Height = 13
          Caption = 'Series spacing'
        end
        object Label67: TLabel
          Left = 209
          Top = 6
          Width = 54
          Height = 13
          Caption = 'Cube angle'
        end
        object Label68: TLabel
          Left = 306
          Top = 6
          Width = 56
          Height = 13
          Caption = 'Cube depth'
        end
        object sbBarTextFont: TSpeedButton
          Left = 210
          Top = 261
          Width = 71
          Height = 22
          Action = acCurveFont
        end
        object lbBarFont: TLabel
          Left = 293
          Top = 264
          Width = 47
          Height = 13
          Caption = 'Tahoma 8'
        end
        object cbBarStyle: TComboBox
          Left = 76
          Top = 5
          Width = 114
          Height = 21
          HelpContext = 332
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 0
          Text = 'Flat'
          OnChange = lbxBarOptionsClickCheck
          Items.Strings = (
            'Flat'
            'Cube'
            'Cylinder'
            'Gradient width'
            'Gradient length')
        end
        object eBarWidth: TEdit
          Left = 76
          Top = 76
          Width = 41
          Height = 21
          HelpContext = 19
          Alignment = taRightJustify
          NumbersOnly = True
          TabOrder = 3
          Text = '20'
          OnExit = lbxBarOptionsClickCheck
        end
        object cbLayout: TComboBox
          Left = 76
          Top = 30
          Width = 114
          Height = 21
          HelpContext = 18
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 1
          Text = 'Stacked'
          OnChange = lbxBarOptionsClickCheck
          Items.Strings = (
            'Stacked'
            'Side by side')
        end
        object eItemSpacing: TEdit
          Left = 76
          Top = 99
          Width = 41
          Height = 21
          Hint = 'Space between the series items'
          HelpContext = 17
          Alignment = taRightJustify
          NumbersOnly = True
          TabOrder = 4
          Text = '3'
          OnExit = lbxBarOptionsClickCheck
        end
        object eSeriesSpacing: TEdit
          Left = 76
          Top = 123
          Width = 41
          Height = 21
          Hint = 'In Side by side layout, space between the series'
          Alignment = taRightJustify
          TabOrder = 5
          Text = '0'
          OnExit = lbxBarOptionsClickCheck
        end
        object eCubeAngle: TEdit
          Left = 266
          Top = 3
          Width = 33
          Height = 21
          HelpContext = 266
          Alignment = taRightJustify
          NumbersOnly = True
          TabOrder = 8
          Text = '45'
          OnExit = lbxBarOptionsClickCheck
        end
        object eCubeDepth: TEdit
          Left = 368
          Top = 3
          Width = 33
          Height = 21
          HelpContext = 334
          Alignment = taRightJustify
          NumbersOnly = True
          TabOrder = 9
          Text = '0'
          OnExit = lbxBarOptionsClickCheck
        end
        object cbxHorizontalBar: TCheckBox
          Left = 6
          Top = 54
          Width = 84
          Height = 17
          HelpContext = 14
          Alignment = taLeftJustify
          Caption = 'Horizontal'
          TabOrder = 2
          OnClick = lbxBarOptionsClickCheck
        end
        object GroupBox2: TGroupBox
          Left = -1
          Top = 176
          Width = 205
          Height = 225
          Caption = 'Elements'
          TabOrder = 7
          object Label65: TLabel
            Left = 2
            Top = 102
            Width = 44
            Height = 13
            Caption = 'Bar texts'
          end
          object Label66: TLabel
            Left = 2
            Top = 80
            Width = 71
            Height = 13
            Caption = 'Base line value'
            WordWrap = True
          end
          object lbxBarOptions: TCheckListBox
            Left = 85
            Top = 16
            Width = 114
            Height = 57
            Hint = 'Optional add ins'
            HelpContext = 42
            OnClickCheck = lbxBarOptionsClickCheck
            ItemHeight = 13
            Items.Strings = (
              'Base line split'
              'Bar outlines'
              'Bar texts'
              'Bar images')
            TabOrder = 0
          end
          object lbxBarTexts: TCheckListBox
            Left = 83
            Top = 102
            Width = 114
            Height = 60
            Hint = 
              'Prints bar texts, if space allows. Bar texts in Elements must be' +
              ' checked'
            HelpContext = 5
            OnClickCheck = lbxBarOptionsClickCheck
            ItemHeight = 13
            Items.Strings = (
              'Values'
              'Names'
              'Series titles'
              'Percentages')
            TabOrder = 2
          end
          object eBarBaselineValue: TEdit
            Left = 84
            Top = 77
            Width = 41
            Height = 21
            Hint = 'Value that defines the base line split'
            HelpContext = 41
            TabOrder = 1
            OnExit = lbxBarOptionsClickCheck
          end
        end
        object gbBarAnimation: TGroupBox
          Left = 210
          Top = 33
          Width = 187
          Height = 200
          Caption = 'Animation'
          Color = clCream
          ParentBackground = False
          ParentColor = False
          TabOrder = 10
          object lbBarAnimationSpeed: TLabel
            Left = 7
            Top = 57
            Width = 30
            Height = 13
            Caption = 'Speed'
          end
          object Label70: TLabel
            Left = 8
            Top = 108
            Width = 29
            Height = 13
            Caption = 'Pause'
          end
          object Label72: TLabel
            Left = 8
            Top = 81
            Width = 37
            Height = 13
            Caption = 'Booster'
          end
          object sbBarAnimationApply: TSpeedButton
            Left = 70
            Top = 165
            Width = 65
            Height = 22
            Action = acApplyBarAnim
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label64: TLabel
            Left = 7
            Top = 17
            Width = 61
            Height = 13
            Caption = 'Performance'
          end
          object Label103: TLabel
            Left = 142
            Top = 108
            Width = 35
            Height = 13
            Caption = 'm. sec.'
          end
          object Label104: TLabel
            Left = 8
            Top = 132
            Width = 150
            Height = 26
            Caption = '-1 = Manual stop.  Click mouse to resume'
            WordWrap = True
          end
          object cbBarAnimationSpeed: TComboBox
            Left = 79
            Top = 54
            Width = 100
            Height = 21
            HelpContext = 331
            Style = csDropDownList
            ItemIndex = 1
            TabOrder = 2
            Text = 'Medium fast'
            Items.Strings = (
              'Fast'
              'Medium fast'
              'Medium slow'
              'Slow')
          end
          object eBarAnimationPause: TEdit
            Left = 77
            Top = 105
            Width = 57
            Height = 21
            Hint = 
              'Pauses animation between each series. -1 = manual pause. Mouse c' +
              'lick resumes.'
            HelpContext = 340
            TabOrder = 3
            Text = '0'
          end
          object lbxBarAnimations: TCheckListBox
            Left = 79
            Top = 14
            Width = 100
            Height = 35
            HelpContext = 310
            ItemHeight = 13
            Items.Strings = (
              'Flow'
              'Grow')
            TabOrder = 0
          end
          object seAnimationBooster: TSpinEdit
            Left = 80
            Top = 77
            Width = 49
            Height = 22
            Hint = 
              'Adds extra speed. Range from -5 to 5 (negative values slows down' +
              ')'
            HelpContext = 319
            MaxValue = 5
            MinValue = -5
            TabOrder = 1
            Value = 0
          end
        end
        object GroupBox3: TGroupBox
          Left = 210
          Top = 318
          Width = 187
          Height = 82
          Caption = 'Note'
          TabOrder = 11
          object Label47: TLabel
            Left = 2
            Top = 15
            Width = 181
            Height = 52
            Align = alClient
            Alignment = taCenter
            Caption = 
              'If bars are too thin, text elements might not show up. Also, som' +
              'e styles cannot be effected if space is to small.'
            Layout = tlCenter
            WordWrap = True
          end
        end
        object cbxAutoSize: TCheckBox
          Left = 6
          Top = 150
          Width = 83
          Height = 17
          HelpContext = 465
          Alignment = taLeftJustify
          Caption = 'Auto size'
          TabOrder = 6
          OnClick = cbxAutoSizeClick
        end
        object cbxShowQualifier: TCheckBox
          Left = 211
          Top = 238
          Width = 93
          Height = 17
          Hint = 
            'Show the qalifier term at the end of the value text (if selected' +
            ')'
          Alignment = taLeftJustify
          Caption = 'Show qualifier'
          TabOrder = 12
          OnClick = lbxBarOptionsClickCheck
        end
        object cbxBarKeepFontColor: TCheckBox
          Left = 211
          Top = 285
          Width = 93
          Height = 17
          Hint = 'Overrides font color adaption, keeping the color of Text font.'
          HelpContext = 430
          Alignment = taLeftJustify
          Caption = 'Keep font color'
          TabOrder = 13
          OnClick = cbxCurveKeepFontColorClick
        end
      end
      object Pie: TTabSheet
        Caption = 'Pie'
        ImageIndex = 2
        ParentShowHint = False
        ShowHint = True
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label12: TLabel
          Left = 3
          Top = 199
          Width = 71
          Height = 13
          Caption = 'Value precision'
        end
        object Label60: TLabel
          Left = 3
          Top = 14
          Width = 24
          Height = 13
          Caption = 'Style'
        end
        object Label69: TLabel
          Left = 229
          Top = 243
          Width = 79
          Height = 13
          Caption = 'Animation speed'
        end
        object Label71: TLabel
          Left = 229
          Top = 270
          Width = 79
          Height = 13
          Caption = 'Animation pause'
        end
        object SpeedButton1: TSpeedButton
          Left = 229
          Top = 139
          Width = 71
          Height = 22
          Action = acCurveFont
        end
        object lbPieFont: TLabel
          Left = 312
          Top = 142
          Width = 47
          Height = 13
          Caption = 'Tahoma 8'
        end
        object Label74: TLabel
          Left = 233
          Top = 2
          Width = 37
          Height = 13
          Caption = 'Options'
        end
        object Label75: TLabel
          Left = 3
          Top = 41
          Width = 41
          Height = 13
          Caption = 'Max size'
        end
        object Label76: TLabel
          Left = 3
          Top = 66
          Width = 68
          Height = 13
          Caption = 'Doughnut size'
        end
        object Label77: TLabel
          Left = 3
          Top = 91
          Width = 53
          Height = 13
          Caption = 'Start angle'
        end
        object Label78: TLabel
          Left = 3
          Top = 173
          Width = 26
          Height = 13
          Caption = 'Slope'
        end
        object Label79: TLabel
          Left = 3
          Top = 116
          Width = 60
          Height = 13
          Caption = 'Slice spacing'
        end
        object SpeedButton3: TSpeedButton
          Left = 229
          Top = 187
          Width = 97
          Height = 22
          Action = acPieTitleFont
          Caption = 'Series title font ...'
        end
        object lbPieTitleFont: TLabel
          Left = 332
          Top = 191
          Width = 47
          Height = 13
          Caption = 'Tahoma 8'
        end
        object Label82: TLabel
          Left = 3
          Top = 143
          Width = 50
          Height = 13
          Caption = 'Disc depth'
        end
        object Label73: TLabel
          Left = 139
          Top = 67
          Width = 62
          Height = 13
          Caption = '% of pie size'
        end
        object Label81: TLabel
          Left = 140
          Top = 92
          Width = 72
          Height = 13
          Caption = 'Whole degrees'
        end
        object Label83: TLabel
          Left = 140
          Top = 117
          Width = 66
          Height = 13
          Caption = 'Float degrees'
        end
        object Label84: TLabel
          Left = 140
          Top = 174
          Width = 72
          Height = 13
          Caption = 'Whole degrees'
        end
        object Label85: TLabel
          Left = 140
          Top = 41
          Width = 27
          Height = 13
          Caption = 'Pixels'
        end
        object Label87: TLabel
          Left = 140
          Top = 137
          Width = 64
          Height = 26
          Caption = 'Pixels or % (neg. values)'
          WordWrap = True
        end
        object Label88: TLabel
          Left = 371
          Top = 270
          Width = 28
          Height = 13
          Caption = 'm.sec'
        end
        object Label89: TLabel
          Left = 228
          Top = 294
          Width = 150
          Height = 26
          Caption = '-1 = Manual stop.  Click mouse to resume'
          WordWrap = True
        end
        object eValuePrecision: TEdit
          Left = 77
          Top = 196
          Width = 57
          Height = 21
          Hint = 'Number of value decimals '
          HelpContext = 439
          Alignment = taRightJustify
          NumbersOnly = True
          TabOrder = 7
          OnExit = cbPieStyleChange
        end
        object cbPieStyle: TComboBox
          Left = 77
          Top = 11
          Width = 91
          Height = 21
          HelpContext = 423
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 0
          Text = 'Flat'
          OnChange = cbPieStyleChange
          Items.Strings = (
            'Flat'
            '3D Disc')
        end
        object cbxPieAnimation: TCheckBox
          Left = 229
          Top = 217
          Width = 97
          Height = 17
          Hint = 'Starts animation'
          HelpContext = 427
          Alignment = taLeftJustify
          Caption = 'Animation'
          TabOrder = 10
          OnClick = cbxPieAnimationClick
        end
        object cbPieAnimationSpeed: TComboBox
          Left = 314
          Top = 240
          Width = 83
          Height = 21
          HelpContext = 331
          Style = csDropDownList
          ItemIndex = 1
          TabOrder = 11
          Text = 'Medium fast'
          OnChange = cbPieStyleChange
          Items.Strings = (
            'Fast'
            'Medium fast'
            'Medium slow'
            'Slow')
        end
        object ePieAnimationPause: TEdit
          Left = 314
          Top = 267
          Width = 51
          Height = 21
          Hint = 
            'Delay between each slice paint in milliseconds. -1 = Manual stop' +
            '/resume'
          HelpContext = 340
          TabOrder = 12
          OnExit = cbPieStyleChange
        end
        object lbxPieOptions: TCheckListBox
          Left = 229
          Top = 20
          Width = 136
          Height = 113
          HelpContext = 197
          OnClickCheck = cbPieStyleChange
          ItemHeight = 13
          Items.Strings = (
            'Print percentages'
            'Print names'
            'Print values'
            'Print series titles'
            'Print titles in doughnut'
            'Text background'
            'Pin text'
            'Client border')
          TabOrder = 8
        end
        object ePieSize: TEdit
          Left = 77
          Top = 37
          Width = 57
          Height = 21
          Hint = 'Maximum size of pie.'
          HelpContext = 51
          NumbersOnly = True
          TabOrder = 1
          OnExit = cbPieStyleChange
        end
        object eDoughnutSize: TEdit
          Left = 77
          Top = 63
          Width = 57
          Height = 21
          Hint = 'Size of doughnut ellipse, in percentage of pie diameter'
          HelpContext = 189
          NumbersOnly = True
          TabOrder = 2
          OnExit = cbPieStyleChange
        end
        object eStartAngle: TEdit
          Left = 77
          Top = 88
          Width = 57
          Height = 21
          Hint = 'Start angle of first pie slice'
          HelpContext = 424
          NumbersOnly = True
          TabOrder = 3
          OnExit = cbPieStyleChange
        end
        object eSlope: TEdit
          Left = 77
          Top = 170
          Width = 57
          Height = 21
          Hint = 'Tilts the pie by the given degrees'
          HelpContext = 422
          NumbersOnly = True
          TabOrder = 6
          OnExit = cbPieStyleChange
        end
        object eSliceSpacing: TEdit
          Left = 77
          Top = 113
          Width = 57
          Height = 21
          Hint = 'Spacing between pie slices, in degrees'
          HelpContext = 124
          TabOrder = 4
          OnExit = cbPieStyleChange
        end
        object eDiscDepth: TEdit
          Left = 77
          Top = 140
          Width = 57
          Height = 21
          Hint = 
            'Disc thickness.  Neg. values means percentage of major axis widt' +
            'h'
          HelpContext = 425
          TabOrder = 5
          OnExit = cbPieStyleChange
        end
        object cbxPieKeepFontColor: TCheckBox
          Left = 229
          Top = 164
          Width = 97
          Height = 17
          Hint = 'Overrides font color adaption, keeping the color of Text font.'
          HelpContext = 430
          Alignment = taLeftJustify
          Caption = 'Keep font color'
          TabOrder = 9
          OnClick = cbxCurveKeepFontColorClick
        end
      end
      object tabStats: TTabSheet
        Caption = 'Statistics'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label52: TLabel
          Left = 14
          Top = 16
          Width = 62
          Height = 13
          Caption = 'Statistic lines'
        end
        object Label110: TLabel
          Left = 14
          Top = 43
          Width = 59
          Height = 26
          Caption = 'Background blending'
          WordWrap = True
        end
        object Label111: TLabel
          Left = 13
          Top = 112
          Width = 59
          Height = 13
          Caption = 'SMA periods'
        end
        object Label112: TLabel
          Left = 14
          Top = 86
          Width = 48
          Height = 13
          Caption = 'Line width'
        end
        object cbCurveStatLine: TComboBox
          Left = 108
          Top = 13
          Width = 133
          Height = 21
          Hint = 'Displays lines based on some som kind of statistics'
          HelpContext = 58
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 0
          Text = 'None'
          OnChange = cbCurveStatLineChange
          Items.Strings = (
            'None'
            'Mean'
            'Median'
            'Linear regression'
            'Mode'
            'Simple moving average ')
        end
        object eSMAPeriods: TEdit
          Left = 108
          Top = 109
          Width = 51
          Height = 21
          NumbersOnly = True
          TabOrder = 3
          Text = '0'
          OnExit = eSMAPeriodsExit
        end
        object eStatLineWidth: TSpinEdit
          Left = 109
          Top = 83
          Width = 48
          Height = 22
          MaxValue = 3
          MinValue = 1
          TabOrder = 2
          Value = 1
          OnChange = eStatLineWidthChange
        end
        object tbStatBGBlending: TTrackBar
          Left = 101
          Top = 43
          Width = 150
          Height = 39
          Max = 128
          Frequency = 5
          TabOrder = 1
          OnChange = tbStatBGBlendingChange
        end
      end
    end
    object pgChart: TPageControl
      Left = 1
      Top = 1
      Width = 418
      Height = 290
      ActivePage = tabChartDefs
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object tabChartDefs: TTabSheet
        Caption = 'Chart definition'
        object pgChartDefs: TPageControl
          Left = 0
          Top = 0
          Width = 410
          Height = 262
          HelpContext = 309
          ActivePage = tabChartGeneral
          Align = alClient
          Enabled = False
          MultiLine = True
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          object tabChartGeneral: TTabSheet
            Caption = 'General'
            ImageIndex = 1
            ParentShowHint = False
            ShowHint = True
            object Label16: TLabel
              Left = 2
              Top = 131
              Width = 20
              Height = 13
              Caption = 'Title'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object Label86: TLabel
              Left = 1
              Top = 3
              Width = 76
              Height = 13
              Caption = 'Available charts'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object Label90: TLabel
              Left = 3
              Top = 206
              Width = 56
              Height = 26
              Caption = 'Background clolor'
              WordWrap = True
            end
            object SpeedButton6: TSpeedButton
              Left = 139
              Top = 181
              Width = 33
              Height = 22
              Action = acChartTitleFont
            end
            object lbTitleFont: TLabel
              Left = 81
              Top = 185
              Width = 47
              Height = 13
              Caption = 'Tahoma 8'
            end
            object Label92: TLabel
              Left = 1
              Top = 158
              Width = 47
              Height = 13
              Caption = 'Alignment'
            end
            object Label102: TLabel
              Left = 3
              Top = 185
              Width = 22
              Height = 13
              Caption = 'Font'
            end
            object eChartTitle: TEdit
              Left = 81
              Top = 128
              Width = 304
              Height = 21
              HelpContext = 312
              TabOrder = 1
              OnExit = eChartTitleExit
            end
            object lbCharts: TListBox
              Left = 81
              Top = 6
              Width = 304
              Height = 116
              ItemHeight = 13
              TabOrder = 0
              OnClick = lbChartsClick
            end
            object cbBGColor: TColorBox
              Left = 82
              Top = 207
              Width = 152
              Height = 22
              HelpContext = 27
              Selected = clWindow
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
              TabOrder = 3
              OnChange = cbBGColorChange
            end
            object cbTitleAlignment: TComboBox
              Left = 82
              Top = 155
              Width = 90
              Height = 21
              HelpContext = 337
              Style = csDropDownList
              TabOrder = 2
              OnChange = cbTitleAlignmentChange
              Items.Strings = (
                'Left justify'
                'Right justify'
                'Center'
                '')
            end
          end
          object tabSeries: TTabSheet
            Caption = 'Series'
            ImageIndex = 5
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label6: TLabel
              Left = 125
              Top = 55
              Width = 25
              Height = 13
              Caption = 'Color'
            end
            object Label7: TLabel
              Left = 125
              Top = 5
              Width = 20
              Height = 13
              Caption = 'Title'
            end
            object Label44: TLabel
              Left = 124
              Top = 84
              Width = 53
              Height = 13
              Caption = 'Value scale'
            end
            object lbValueQuality: TLabel
              Left = 125
              Top = 142
              Width = 61
              Height = 13
              Caption = 'Value quality'
              Visible = False
            end
            object Shape2: TShape
              Left = 127
              Top = 129
              Width = 272
              Height = 1
            end
            object Label17: TLabel
              Left = 125
              Top = 30
              Width = 54
              Height = 13
              Caption = 'Graph type'
            end
            object cbChartSeriesColor: TColorBox
              Left = 195
              Top = 52
              Width = 119
              Height = 22
              Hint = 
                'Series color. Affects the curve lines or bars that draws the ser' +
                'ies'
              HelpContext = 413
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
              TabOrder = 3
              OnChange = cbChartSeriesColorChange
            end
            object eSeriesTitle: TEdit
              Left = 194
              Top = 2
              Width = 205
              Height = 21
              HelpType = htKeyword
              HelpKeyword = '412'
              TabOrder = 1
              OnExit = eSeriesTitleExit
            end
            object cbxChartVisible: TCheckBox
              Left = 127
              Top = 163
              Width = 83
              Height = 17
              HelpContext = 415
              TabStop = False
              Alignment = taLeftJustify
              Caption = 'Visible'
              Checked = True
              State = cbChecked
              TabOrder = 5
              OnClick = cbxChartVisibleClick
            end
            object lbSeries: TListBox
              Left = 0
              Top = 0
              Width = 121
              Height = 234
              Align = alLeft
              ItemHeight = 13
              PopupMenu = pmSeries
              TabOrder = 0
              OnClick = lbSeriesClick
            end
            object cbScale: TComboBox
              Left = 194
              Top = 80
              Width = 120
              Height = 21
              Hint = 'Points to the value scale configuration (scales tab)'
              HelpContext = 414
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 4
              Text = 'Value scale 1'
              Items.Strings = (
                'Value scale 1'
                'Value scale 2')
            end
            object cbPercentages: TComboBox
              Left = 195
              Top = 139
              Width = 105
              Height = 21
              Hint = 'Kind of values in the series, real numbers or percentages'
              HelpContext = 446
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 6
              Text = 'Real values'
              Visible = False
              Items.Strings = (
                'Real values'
                'Percentages')
            end
            object cbSeriesGraphType: TComboBox
              Left = 195
              Top = 27
              Width = 120
              Height = 21
              HelpContext = 411
              Style = csDropDownList
              TabOrder = 2
              OnChange = cbSeriesGraphTypeChange
              Items.Strings = (
                'Curve'
                'Bar'
                'Pie')
            end
          end
          object tabItems: TTabSheet
            Caption = 'Categories'
            ImageIndex = 6
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label8: TLabel
              Left = 160
              Top = 7
              Width = 27
              Height = 13
              Caption = 'Name'
            end
            object Label9: TLabel
              Left = 160
              Top = 31
              Width = 25
              Height = 13
              Caption = 'Color'
            end
            object imgLabel: TImage
              Left = 208
              Top = 64
              Width = 64
              Height = 65
              Hint = 'Image appearing as labels on the name scale'
              Center = True
              Transparent = True
            end
            object Label109: TLabel
              Left = 160
              Top = 64
              Width = 30
              Height = 26
              Caption = 'Label Image'
              WordWrap = True
            end
            object SpeedButton2: TSpeedButton
              Left = 160
              Top = 96
              Width = 27
              Height = 22
              Caption = '...'
              OnClick = SpeedButton2Click
            end
            object lbItems: TListBox
              Left = 0
              Top = 0
              Width = 153
              Height = 234
              HelpContext = 308
              Style = lbOwnerDrawFixed
              Align = alLeft
              ItemHeight = 13
              PopupMenu = pmCategories
              TabOrder = 0
              OnClick = lbItemsClick
              OnDrawItem = lbItemsDrawItem
            end
            object cbChartItemColor: TColorBox
              Left = 207
              Top = 30
              Width = 113
              Height = 22
              Hint = 
                'Color of the bar or pie slice that is associated with the catego' +
                'ry'
              HelpContext = 281
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
              TabOrder = 2
              OnChange = cbChartItemColorChange
            end
            object eItemName: TEdit
              Left = 210
              Top = 3
              Width = 171
              Height = 21
              Hint = 'Name of category'
              HelpContext = 282
              TabOrder = 1
              OnExit = eItemNameExit
            end
          end
          object tabScales: TTabSheet
            Caption = 'Scales'
            ImageIndex = 7
            object pgScales: TPageControl
              Left = 0
              Top = 0
              Width = 402
              Height = 234
              ActivePage = tabNameScale
              Align = alClient
              Style = tsFlatButtons
              TabOrder = 0
              object tabNameScale: TTabSheet
                Caption = 'Name scale'
                ImageIndex = 1
                object Label14: TLabel
                  Left = 7
                  Top = 6
                  Width = 49
                  Height = 13
                  Caption = 'Span type'
                end
                object lbNamePrecision: TLabel
                  Left = 7
                  Top = 24
                  Width = 66
                  Height = 26
                  Caption = 'Number span precision'
                  WordWrap = True
                end
                object Label22: TLabel
                  Left = 6
                  Top = 55
                  Width = 76
                  Height = 13
                  Caption = 'Overflow action'
                end
                object cbSpanType: TComboBox
                  Left = 104
                  Top = 2
                  Width = 105
                  Height = 21
                  HelpKeyword = 'The type of span associated with a span chart.'
                  HelpContext = 324
                  Style = csDropDownList
                  Enabled = False
                  ItemIndex = 6
                  TabOrder = 0
                  Text = 'Not spanned'
                  Items.Strings = (
                    'Month span'
                    'Date span'
                    'Hourspan'
                    'Minute span'
                    'Second span'
                    'Number span'
                    'Not spanned')
                end
                object eNamePrecision: TEdit
                  Left = 104
                  Top = 26
                  Width = 45
                  Height = 21
                  Hint = 
                    'Defines the precision of the numbers in span is a number span ch' +
                    'art'
                  HelpContext = 323
                  Alignment = taRightJustify
                  TabOrder = 1
                  OnExit = eNamePrecisionExit
                end
                object cbOverflowAction: TComboBox
                  Left = 104
                  Top = 52
                  Width = 105
                  Height = 21
                  Hint = 
                    'Action to take when there is not enough space to render the diag' +
                    'ram completely'
                  HelpContext = 23
                  Style = csDropDownList
                  ItemIndex = 3
                  TabOrder = 2
                  Text = 'None'
                  OnChange = cbOverflowActionChange
                  Items.Strings = (
                    'Contraction'
                    'Compression'
                    'Scrolling'
                    'None')
                end
                inline Names: TFrame1
                  Left = 0
                  Top = 73
                  Width = 278
                  Height = 107
                  TabOrder = 3
                  ExplicitTop = 73
                  inherited cbxShowDividerLines: TCheckBox
                    Hint = 'Shows hides the name divider lines'
                    HelpContext = 488
                    Caption = 'Show dividerlines'
                  end
                  inherited cbxShowLabels: TCheckBox
                    Left = 7
                    Hint = 'Shows / hides the name labels'
                    HelpContext = 491
                    ExplicitLeft = 7
                  end
                  inherited eQualifier: TEdit
                    Hint = 'Name qualifier'
                    HelpContext = 173
                  end
                  inherited ActionList1: TActionList
                    Images = ImageList1
                    Left = 232
                    Top = 82
                  end
                end
                object cbxImageLabels: TCheckBox
                  Left = 6
                  Top = 180
                  Width = 110
                  Height = 17
                  Alignment = taLeftJustify
                  Caption = 'Image labels'
                  TabOrder = 4
                  OnClick = cbxImageLabelsClick
                end
              end
              object tabValueScale: TTabSheet
                Caption = 'Value scale 1'
                object Label2: TLabel
                  Left = 12
                  Top = 0
                  Width = 50
                  Height = 13
                  Caption = 'High value'
                end
                object Label10: TLabel
                  Left = 12
                  Top = 22
                  Width = 48
                  Height = 13
                  Caption = 'Low value'
                end
                object Label11: TLabel
                  Left = 12
                  Top = 44
                  Width = 43
                  Height = 13
                  Caption = 'Intervals'
                end
                object Label45: TLabel
                  Left = 12
                  Top = 66
                  Width = 71
                  Height = 13
                  Caption = 'Value precision'
                end
                object eHighValue: TEdit
                  Left = 104
                  Top = -3
                  Width = 66
                  Height = 21
                  Hint = 'Highest value of scale'
                  HelpContext = 325
                  Alignment = taRightJustify
                  TabOrder = 0
                  OnExit = eHighValueExit
                end
                object eLowValue: TEdit
                  Left = 104
                  Top = 19
                  Width = 66
                  Height = 21
                  Hint = 'Lowest value of scale'
                  HelpContext = 326
                  Alignment = taRightJustify
                  TabOrder = 1
                  OnExit = eLowValueExit
                end
                object cbxAutoSpan: TCheckBox
                  Left = 180
                  Top = 9
                  Width = 127
                  Height = 17
                  Hint = 
                    'Uses the high/low values in the series data as the upper and low' +
                    'er boundaries'
                  HelpContext = 328
                  Alignment = taLeftJustify
                  Caption = 'Get high/low from data'
                  TabOrder = 2
                  OnClick = cbxAutoSpanClick
                end
                object eValueIntervals: TEdit
                  Left = 104
                  Top = 41
                  Width = 46
                  Height = 21
                  Hint = 'Minimum value intervals in scale'
                  HelpContext = 327
                  Alignment = taRightJustify
                  TabOrder = 3
                  Text = '1'
                  OnExit = eValueIntervalsExit
                end
                object ePrecision1: TEdit
                  Left = 104
                  Top = 63
                  Width = 45
                  Height = 21
                  Hint = 'Max number of value decimals'
                  HelpContext = 72
                  Alignment = taRightJustify
                  TabOrder = 4
                  OnExit = ePrecision1Exit
                end
                inline V1: TFrame1
                  Left = 0
                  Top = 84
                  Width = 278
                  Height = 107
                  TabOrder = 5
                  ExplicitTop = 84
                  inherited Label103: TLabel
                    Left = 12
                    ExplicitLeft = 12
                  end
                  inherited sbDivLines: TSpeedButton
                    Top = 80
                    ExplicitTop = 80
                  end
                  inherited Label105: TLabel
                    Left = 11
                    Top = 84
                    ExplicitLeft = 11
                    ExplicitTop = 84
                  end
                  inherited lbDivLinePen: TLabel
                    Left = 105
                    Top = 84
                    ExplicitLeft = 105
                    ExplicitTop = 84
                  end
                  inherited Label1: TLabel
                    Left = 12
                    ExplicitLeft = 12
                  end
                  inherited cbxShowDividerLines: TCheckBox
                    Left = 11
                    Width = 109
                    HelpContext = 488
                    ExplicitLeft = 11
                    ExplicitWidth = 109
                  end
                  inherited cbxShowLabels: TCheckBox
                    Left = 12
                    Top = 25
                    Width = 106
                    HelpType = htKeyword
                    HelpKeyword = '491'
                    ExplicitLeft = 12
                    ExplicitTop = 25
                    ExplicitWidth = 106
                  end
                  inherited eQualifier: TEdit
                    HelpContext = 173
                  end
                  inherited ActionList1: TActionList
                    Images = ImageList1
                  end
                end
              end
              object tabValueScale2: TTabSheet
                Caption = 'Value scale 2'
                ImageIndex = 2
                object Label40: TLabel
                  Left = 12
                  Top = 2
                  Width = 50
                  Height = 13
                  Caption = 'High value'
                end
                object Label41: TLabel
                  Left = 12
                  Top = 24
                  Width = 48
                  Height = 13
                  Caption = 'Low value'
                end
                object Label42: TLabel
                  Left = 12
                  Top = 46
                  Width = 43
                  Height = 13
                  Caption = 'Intervals'
                end
                object Label46: TLabel
                  Left = 12
                  Top = 68
                  Width = 71
                  Height = 13
                  Caption = 'Value precision'
                end
                object eHighValue2: TEdit
                  Left = 108
                  Top = -1
                  Width = 66
                  Height = 21
                  Hint = 'Highest value of scale'
                  HelpContext = 325
                  Alignment = taRightJustify
                  TabOrder = 0
                  OnExit = eHighValueExit
                end
                object eLowValue2: TEdit
                  Left = 108
                  Top = 21
                  Width = 66
                  Height = 21
                  Hint = 'Lowest value of scale'
                  HelpContext = 326
                  Alignment = taRightJustify
                  TabOrder = 1
                  OnExit = eLowValueExit
                end
                object cbxAutoSpan2: TCheckBox
                  Left = 181
                  Top = 12
                  Width = 131
                  Height = 17
                  Hint = 
                    'Uses the high/low values in the series data as the upper and low' +
                    'er boundaries'
                  HelpContext = 328
                  Alignment = taLeftJustify
                  Caption = 'Get high/low from data'
                  TabOrder = 2
                  OnClick = cbxAutoSpanClick
                end
                object eValueIntervals2: TEdit
                  Left = 108
                  Top = 43
                  Width = 68
                  Height = 21
                  Hint = 'Minimum value intervals in scale'
                  HelpContext = 327
                  Alignment = taRightJustify
                  TabOrder = 3
                  Text = '1'
                  OnExit = eValueIntervalsExit
                end
                object ePrecision2: TEdit
                  Left = 108
                  Top = 65
                  Width = 45
                  Height = 21
                  Hint = 'Maximum nuber of value decimals'
                  HelpContext = 72
                  Alignment = taRightJustify
                  NumbersOnly = True
                  TabOrder = 4
                  OnExit = ePrecision2Exit
                end
                inline V2: TFrame1
                  Left = 5
                  Top = 87
                  Width = 278
                  Height = 107
                  TabOrder = 5
                  ExplicitLeft = 5
                  ExplicitTop = 87
                  inherited Label103: TLabel
                    Left = 8
                    ExplicitLeft = 8
                  end
                  inherited sbDivLines: TSpeedButton
                    Top = 78
                    ExplicitTop = 78
                  end
                  inherited Label105: TLabel
                    Left = 8
                    Top = 82
                    ExplicitLeft = 8
                    ExplicitTop = 82
                  end
                  inherited lbDivLinePen: TLabel
                    Top = 82
                    ExplicitTop = 82
                  end
                  inherited cbxShowDividerLines: TCheckBox
                    Left = 8
                    HelpContext = 488
                    ExplicitLeft = 8
                  end
                  inherited cbxShowLabels: TCheckBox
                    HelpType = htKeyword
                    HelpKeyword = '491'
                  end
                  inherited eQualifier: TEdit
                    Width = 177
                    HelpContext = 173
                    ExplicitWidth = 177
                  end
                  inherited ActionList1: TActionList
                    Images = ImageList1
                  end
                end
              end
            end
          end
          object tabAxisAttributes: TTabSheet
            Caption = 'Axis profile'
            ImageIndex = 3
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label15: TLabel
              Left = 3
              Top = 6
              Width = 54
              Height = 13
              Caption = 'Orientation'
              WordWrap = True
            end
            object Label18: TLabel
              Left = 3
              Top = 74
              Width = 58
              Height = 26
              Caption = 'Wall border color'
              WordWrap = True
            end
            object Label19: TLabel
              Left = 3
              Top = 130
              Width = 51
              Height = 13
              Caption = 'Text tilting'
            end
            object Label20: TLabel
              Left = 3
              Top = 31
              Width = 49
              Height = 13
              Caption = 'Wall width'
            end
            object Label21: TLabel
              Left = 3
              Top = 54
              Width = 46
              Height = 13
              Caption = 'Wall color'
            end
            object Label106: TLabel
              Left = 118
              Top = 29
              Width = 27
              Height = 13
              Caption = 'pixels'
            end
            object Label107: TLabel
              Left = 3
              Top = 187
              Width = 37
              Height = 13
              Caption = 'Borders'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object cbAxisOrientation: TComboBox
              Left = 68
              Top = 3
              Width = 162
              Height = 21
              Hint = 'Turns the axes around the clock'
              HelpContext = 14
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 0
              Text = 'Names bottom/Values left'
              OnChange = cbAxisOrientationChange
              Items.Strings = (
                'Names bottom/Values left'
                'Names bottom/Values right'
                'Names left/Values top'
                'Names top/Values left'
                'Names top/Values right'
                'Names right/Values top'
                'Names left/Values bottom'
                'Names right/Values bottom')
            end
            object cbAxisColor: TColorBox
              Left = 68
              Top = 78
              Width = 112
              Height = 22
              Hint = 'Wall border color'
              HelpContext = 525
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
              TabOrder = 3
              OnChange = cbAxisColorChange
            end
            object lbxTextTilting: TCheckListBox
              Left = 68
              Top = 130
              Width = 107
              Height = 48
              Hint = 'Activates label tilting for the selected label types'
              HelpContext = 48
              OnClickCheck = lbxTextTiltingClickCheck
              ItemHeight = 13
              Items.Strings = (
                'Names'
                'Values'
                'Section names')
              TabOrder = 5
            end
            object cbxGradientWall: TCheckBox
              Left = 3
              Top = 105
              Width = 78
              Height = 18
              Hint = 
                'Paints the wall background color as a gradient of wall color and' +
                ' graph color'
              HelpContext = 305
              Alignment = taLeftJustify
              Caption = 'Gradient wall'
              TabOrder = 4
              WordWrap = True
              OnClick = cbxGradientWallClick
            end
            object eWallWidth: TEdit
              Left = 69
              Top = 26
              Width = 43
              Height = 21
              Hint = 'Width of 3D wall. 0 = no wall'
              HelpContext = 304
              Alignment = taRightJustify
              NumbersOnly = True
              TabOrder = 1
              Text = '0'
              OnExit = eWallWidthExit
            end
            object cbWallColor: TColorBox
              Left = 68
              Top = 50
              Width = 111
              Height = 22
              Hint = 'Wall background color'
              HelpContext = 306
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
              TabOrder = 2
              OnChange = cbWallColorChange
            end
            object cbBorders: TComboBox
              Left = 69
              Top = 184
              Width = 152
              Height = 21
              HelpContext = 28
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 6
              Text = 'Axis sides only'
              OnChange = cbBordersChange
              Items.Strings = (
                'Axis sides only'
                'All sides'
                'No borders')
            end
          end
          object tabLegends: TTabSheet
            Caption = 'Legends'
            ImageIndex = 5
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label33: TLabel
              Left = 178
              Top = 6
              Width = 48
              Height = 13
              Caption = 'Anchoring'
            end
            object Label34: TLabel
              Left = 178
              Top = 49
              Width = 39
              Height = 13
              Caption = 'Content'
            end
            object Label35: TLabel
              Left = 178
              Top = 109
              Width = 39
              Height = 26
              Caption = 'Content flow'
              WordWrap = True
            end
            object Label36: TLabel
              Left = 178
              Top = 137
              Width = 22
              Height = 13
              Caption = 'Text'
            end
            object Label13: TLabel
              Left = 178
              Top = 29
              Width = 53
              Height = 13
              Caption = 'Point name'
            end
            object cbLegAnchoring: TComboBox
              Left = 234
              Top = 3
              Width = 117
              Height = 21
              Hint = 
                'Defines the position of the legend, relative to the sides of the' +
                ' graph area'
              HelpContext = 351
              Style = csDropDownList
              DropDownCount = 10
              ItemIndex = 4
              TabOrder = 2
              Text = 'Right outside'
              OnChange = cbLegAnchoringChange
              Items.Strings = (
                'Left outside'
                'Left inside'
                'Top outside'
                'Top inside'
                'Right outside'
                'Right inside'
                'Bottom outside'
                'Bottom inside'
                'Series outside'
                'Point inside')
            end
            object cbLegContentFlow: TComboBox
              Left = 234
              Top = 111
              Width = 116
              Height = 21
              Hint = 'Text flow direction'
              HelpType = htKeyword
              HelpKeyword = '353'
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 4
              Text = 'Top to bottom'
              OnChange = cbLegContentFlowChange
              Items.Strings = (
                'Top to bottom'
                'Left to right')
            end
            object memLegText: TMemo
              Left = 235
              Top = 138
              Width = 161
              Height = 75
              Hint = 'Free text added at the end of the legend'
              HelpContext = 357
              TabOrder = 5
              WordWrap = False
              OnExit = memLegTextExit
            end
            object panLegends: TPanel
              Left = 0
              Top = 0
              Width = 161
              Height = 234
              Align = alLeft
              BevelOuter = bvNone
              Caption = 'panLegends'
              ShowCaption = False
              TabOrder = 6
              object Label23: TLabel
                Left = 1
                Top = 125
                Width = 62
                Height = 13
                Caption = 'Horz margins'
              end
              object Label37: TLabel
                Left = 1
                Top = 148
                Width = 64
                Height = 13
                Caption = 'Vert. margins'
              end
              object Label38: TLabel
                Left = 1
                Top = 172
                Width = 47
                Height = 13
                Caption = 'Alignment'
              end
              object Label39: TLabel
                Left = 1
                Top = 196
                Width = 31
                Height = 13
                Caption = 'Bullets'
              end
              object Label1: TLabel
                Left = 2
                Top = 6
                Width = 40
                Height = 13
                Caption = 'Legends'
              end
              object lbLegends: TListBox
                Left = 1
                Top = 23
                Width = 156
                Height = 74
                Hint = 'Legends. Activate pop up menu to manage the list'
                HelpContext = 311
                ItemHeight = 13
                PopupMenu = pmLegends
                TabOrder = 0
                OnClick = lbLegendsClick
              end
              object eLegHorizMargins: TEdit
                Left = 68
                Top = 122
                Width = 89
                Height = 21
                Hint = 'Sets the horizontal margin relative to anchoring'
                HelpContext = 355
                NumbersOnly = True
                TabOrder = 1
                OnExit = eLegHorizMarginsExit
              end
              object eLegVertMargins: TEdit
                Left = 68
                Top = 145
                Width = 89
                Height = 21
                Hint = 'Sets the vertical margin relative to anchoring'
                HelpContext = 356
                NumbersOnly = True
                TabOrder = 2
                OnExit = eLegVertMarginsExit
              end
              object cbLegAlignment: TComboBox
                Left = 68
                Top = 169
                Width = 89
                Height = 21
                Hint = 'Sets alignment relative to anchoring'
                HelpContext = 359
                Style = csDropDownList
                TabOrder = 3
                OnChange = cbLegAlignmentChange
                Items.Strings = (
                  'Left or top'
                  'Center'
                  'Right or bottom')
              end
              object cbLegBullets: TComboBox
                Left = 68
                Top = 193
                Width = 89
                Height = 21
                Hint = 'Marks titles or categories with the selected bullet type'
                HelpType = htKeyword
                HelpKeyword = '358'
                Style = csDropDownList
                TabOrder = 4
                OnChange = cbLegBulletsChange
                Items.Strings = (
                  'None'
                  'Square'
                  'Circle'
                  'Line')
              end
            end
            object cbxLegVisible: TCheckBox
              Left = 1
              Top = 103
              Width = 81
              Height = 17
              HelpContext = 354
              Alignment = taLeftJustify
              Caption = 'Visible'
              TabOrder = 1
              OnClick = cbxLegVisibleClick
            end
            object lbxLegContent: TCheckListBox
              Left = 235
              Top = 50
              Width = 115
              Height = 58
              HelpContext = 352
              OnClickCheck = lbxLegContentClickCheck
              ItemHeight = 13
              Items.Strings = (
                'Series title'
                'Value'
                'Name'
                'Name span')
              TabOrder = 0
            end
            object eLegPointName: TEdit
              Left = 234
              Top = 26
              Width = 116
              Height = 21
              Hint = 'Name of series item when anchoring is Point inside'
              HelpContext = 363
              TabOrder = 3
              OnExit = eLegPointNameExit
            end
          end
          object tabSections: TTabSheet
            Caption = 'Sections'
            ImageIndex = 6
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object pgSections: TPageControl
              Left = 0
              Top = 0
              Width = 402
              Height = 234
              ActivePage = tabNameSections
              Align = alClient
              Style = tsFlatButtons
              TabOrder = 0
              object tabNameSections: TTabSheet
                Caption = 'Name sections'
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 0
                ExplicitHeight = 0
                object Label4: TLabel
                  Left = 1
                  Top = -1
                  Width = 65
                  Height = 13
                  Caption = 'Auto sections'
                end
                object Label26: TLabel
                  Left = 227
                  Top = -1
                  Width = 67
                  Height = 13
                  Caption = 'Time template'
                end
                object Label31: TLabel
                  Left = 0
                  Top = 41
                  Width = 240
                  Height = 13
                  Caption = 'Sections (activate pop up menu to manage the lis)'
                end
                object panNameSectionValues: TPanel
                  Left = 0
                  Top = 57
                  Width = 394
                  Height = 102
                  Caption = 'panNameSectionValues'
                  Color = clCream
                  ParentBackground = False
                  ShowCaption = False
                  TabOrder = 2
                  object Label3: TLabel
                    Left = 152
                    Top = 9
                    Width = 62
                    Height = 13
                    Caption = 'Start value *'
                  end
                  object Label5: TLabel
                    Left = 152
                    Top = 32
                    Width = 56
                    Height = 13
                    Caption = 'End value *'
                  end
                  object Label24: TLabel
                    Left = 152
                    Top = 55
                    Width = 70
                    Height = 13
                    Caption = 'Long caption *'
                  end
                  object Label25: TLabel
                    Left = 152
                    Top = 78
                    Width = 64
                    Height = 13
                    Caption = 'Short caption'
                  end
                  object lbNameSections: TListBox
                    Left = 1
                    Top = 1
                    Width = 145
                    Height = 100
                    HelpContext = 65
                    Align = alLeft
                    ItemHeight = 13
                    PopupMenu = pmSections
                    TabOrder = 0
                    OnClick = lbNameSectionsClick
                  end
                  object eNStartValue: TEdit
                    Left = 227
                    Top = 6
                    Width = 153
                    Height = 21
                    Hint = 'Start value of section'
                    HelpContext = 374
                    TabOrder = 1
                  end
                  object eNEndValue: TEdit
                    Left = 227
                    Top = 29
                    Width = 153
                    Height = 21
                    Hint = 'End value of section'
                    HelpContext = 375
                    TabOrder = 2
                  end
                  object eNLongCaption: TEdit
                    Left = 227
                    Top = 52
                    Width = 153
                    Height = 21
                    Hint = 'Long caption '
                    HelpType = htKeyword
                    HelpKeyword = '372'
                    TabOrder = 3
                  end
                  object eNShortCaption: TEdit
                    Left = 227
                    Top = 75
                    Width = 153
                    Height = 21
                    Hint = 'Short caption (optional)'
                    HelpContext = 373
                    TabOrder = 4
                  end
                end
                object cbAutoSections: TComboBox
                  Left = 1
                  Top = 14
                  Width = 143
                  Height = 21
                  Hint = 'Type of autosection (time spanned charts only)'
                  HelpContext = 390
                  Style = csDropDownList
                  ItemIndex = 0
                  TabOrder = 0
                  Text = 'No used'
                  OnChange = cbAutoSectionsChange
                  Items.Strings = (
                    'No used'
                    'Days'
                    'Dates'
                    'Weeks'
                    'Months'
                    'Years')
                end
                object cbTimeTemplate: TComboBox
                  Left = 227
                  Top = 14
                  Width = 143
                  Height = 21
                  Hint = 'Own defined time unit (time spanned charts only)'
                  HelpContext = 389
                  Style = csDropDownList
                  ItemIndex = 0
                  TabOrder = 1
                  Text = 'Not used'
                  OnChange = cbTimeTemplateChange
                  Items.Strings = (
                    'Not used'
                    'Month'
                    'Week'
                    'Date'
                    'Hour'
                    'Second')
                end
                object sbMoreNSections: TButton
                  Left = 316
                  Top = 161
                  Width = 75
                  Height = 25
                  Caption = 'More ...'
                  TabOrder = 3
                  OnClick = sbMoreNSectionsClick
                end
                object cbxNSectionsVisible: TCheckBox
                  Left = 8
                  Top = 166
                  Width = 51
                  Height = 17
                  HelpContext = 406
                  Caption = 'Visible '
                  TabOrder = 4
                  OnClick = cbxNSectionsVisibleClick
                end
              end
              object tabValueSections: TTabSheet
                Caption = 'Value sections'
                ImageIndex = 1
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 0
                ExplicitHeight = 0
                object Label32: TLabel
                  Left = 2
                  Top = 1
                  Width = 244
                  Height = 13
                  Caption = 'Sections (activate pop up menu to manage the list)'
                end
                object Panel1: TPanel
                  Left = 0
                  Top = 20
                  Width = 394
                  Height = 132
                  Caption = 'panNameSectionValues'
                  Color = clCream
                  ParentBackground = False
                  ShowCaption = False
                  TabOrder = 0
                  object Label27: TLabel
                    Left = 152
                    Top = 11
                    Width = 62
                    Height = 13
                    Caption = 'Start value *'
                  end
                  object Label28: TLabel
                    Left = 152
                    Top = 31
                    Width = 56
                    Height = 13
                    Caption = 'End value *'
                  end
                  object Label29: TLabel
                    Left = 152
                    Top = 55
                    Width = 70
                    Height = 13
                    Caption = 'Long caption *'
                  end
                  object Label30: TLabel
                    Left = 152
                    Top = 79
                    Width = 64
                    Height = 13
                    Caption = 'Short caption'
                  end
                  object Label48: TLabel
                    Left = 152
                    Top = 103
                    Width = 141
                    Height = 13
                    Caption = 'Start / End values in numbers'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clBlue
                    Font.Height = -11
                    Font.Name = 'Tahoma'
                    Font.Style = []
                    ParentFont = False
                  end
                  object lbValueSections: TListBox
                    Left = 1
                    Top = 1
                    Width = 145
                    Height = 130
                    HelpContext = 343
                    Align = alLeft
                    ItemHeight = 13
                    PopupMenu = pmSections
                    TabOrder = 0
                    OnClick = lbValueSectionsClick
                  end
                  object eVStartValue: TEdit
                    Left = 227
                    Top = 5
                    Width = 153
                    Height = 21
                    HelpContext = 374
                    TabOrder = 1
                  end
                  object eVEndValue: TEdit
                    Left = 227
                    Top = 28
                    Width = 153
                    Height = 21
                    HelpContext = 375
                    TabOrder = 2
                  end
                  object eVLongCaption: TEdit
                    Left = 228
                    Top = 49
                    Width = 153
                    Height = 21
                    HelpContext = 372
                    TabOrder = 3
                  end
                  object eVShortCaption: TEdit
                    Left = 227
                    Top = 76
                    Width = 153
                    Height = 21
                    HelpContext = 373
                    TabOrder = 4
                  end
                end
                object cbxVSectionsVisible: TCheckBox
                  Left = 9
                  Top = 163
                  Width = 51
                  Height = 17
                  Caption = 'Visible '
                  TabOrder = 1
                  OnClick = cbxVSectionsVisibleClick
                end
                object sbMoreVSections: TButton
                  Left = 315
                  Top = 158
                  Width = 75
                  Height = 25
                  Caption = 'More ...'
                  TabOrder = 2
                  OnClick = sbMoreVSectionsClick
                end
              end
            end
          end
        end
      end
      object tab_Commons: TTabSheet
        Caption = 'Commons'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label91: TLabel
          Left = 3
          Top = 13
          Width = 30
          Height = 13
          Caption = 'Rulers'
        end
        object Label101: TLabel
          Left = 3
          Top = 56
          Width = 52
          Height = 13
          Caption = 'Mouse info'
        end
        object cbRulers: TComboBox
          Left = 97
          Top = 8
          Width = 121
          Height = 21
          Hint = 
            'The rulers displayed when pressing the Ctrl button and sliding t' +
            'he mouse over the diagram'
          HelpContext = 52
          Style = csDropDownList
          TabOrder = 0
          OnChange = cbRulersChange
          Items.Strings = (
            'Vertical'
            'Horizontal'
            'Both'
            'None')
        end
        object GroupBox4: TGroupBox
          Left = 3
          Top = 81
          Width = 122
          Height = 120
          Hint = 'Margins from the outer borders of the chart'
          HelpContext = 12
          Caption = 'Border margins'
          TabOrder = 1
          object Label93: TLabel
            Left = 8
            Top = 20
            Width = 19
            Height = 13
            Caption = 'Left'
          end
          object Label94: TLabel
            Left = 8
            Top = 42
            Width = 18
            Height = 13
            Caption = 'Top'
          end
          object Label95: TLabel
            Left = 8
            Top = 65
            Width = 25
            Height = 13
            Caption = 'Right'
          end
          object Label96: TLabel
            Left = 8
            Top = 93
            Width = 34
            Height = 13
            Caption = 'Bottom'
          end
          object eIMargLeft: TEdit
            Left = 51
            Top = 16
            Width = 57
            Height = 21
            NumbersOnly = True
            TabOrder = 0
            OnExit = eIMargLeftExit
          end
          object eIMargTop: TEdit
            Left = 51
            Top = 39
            Width = 57
            Height = 21
            NumbersOnly = True
            TabOrder = 1
            OnExit = eIMargTopExit
          end
          object eIMargRight: TEdit
            Left = 51
            Top = 62
            Width = 57
            Height = 21
            NumbersOnly = True
            TabOrder = 2
            OnExit = eIMargRightExit
          end
          object eIMargBottom: TEdit
            Left = 51
            Top = 90
            Width = 57
            Height = 21
            NumbersOnly = True
            TabOrder = 3
            OnExit = eIMargBottomExit
          end
        end
        object GroupBox5: TGroupBox
          Left = 144
          Top = 82
          Width = 122
          Height = 120
          Hint = 
            'Distance between the edges of the graph area and the printing ar' +
            'ea'
          HelpContext = 15
          Caption = 'Graph margins'
          TabOrder = 2
          object Label97: TLabel
            Left = 8
            Top = 19
            Width = 19
            Height = 13
            Caption = 'Left'
          end
          object Label98: TLabel
            Left = 8
            Top = 43
            Width = 18
            Height = 13
            Caption = 'Top'
          end
          object Label99: TLabel
            Left = 8
            Top = 68
            Width = 25
            Height = 13
            Caption = 'Right'
          end
          object Label100: TLabel
            Left = 8
            Top = 93
            Width = 34
            Height = 13
            Caption = 'Bottom'
          end
          object eGmargLeft: TEdit
            Left = 51
            Top = 16
            Width = 57
            Height = 21
            NumbersOnly = True
            TabOrder = 0
            OnExit = eGmargLeftExit
          end
          object eGMargTop: TEdit
            Left = 51
            Top = 40
            Width = 57
            Height = 21
            NumbersOnly = True
            TabOrder = 1
            OnExit = eGMargTopExit
          end
          object eGMargRight: TEdit
            Left = 51
            Top = 65
            Width = 57
            Height = 21
            NumbersOnly = True
            TabOrder = 2
            OnExit = eGMargRightExit
          end
          object eGMargBottom: TEdit
            Left = 51
            Top = 90
            Width = 57
            Height = 21
            NumbersOnly = True
            TabOrder = 3
            OnExit = eGMargBottomExit
          end
        end
        object cbMouseInfo: TComboBox
          Left = 98
          Top = 52
          Width = 120
          Height = 21
          Hint = 
            'The kind of info displayed when pressing the Ctrl button and pos' +
            'itioning the mouse over the diagram'
          HelpContext = 38
          Style = csDropDownList
          TabOrder = 3
          OnChange = cbMouseInfoChange
          Items.Strings = (
            'Names'
            'Values'
            'Values and Names'
            'None')
        end
        object cbxCenterChart: TCheckBox
          Left = 3
          Top = 32
          Width = 107
          Height = 17
          Hint = 'Centers the diagram within the chart'
          HelpContext = 122
          Alignment = taLeftJustify
          Caption = 'Center chart'
          TabOrder = 4
          OnClick = cbxCenterChartClick
        end
        object cbxLiveResize: TCheckBox
          Left = 3
          Top = 207
          Width = 69
          Height = 17
          Hint = 'Centers the diagram within the chart'
          HelpContext = 122
          Alignment = taLeftJustify
          Caption = 'Live resize'
          Checked = True
          State = cbChecked
          TabOrder = 5
          OnClick = cbxLiveResizeClick
        end
      end
    end
  end
  object panGraph: TPanel
    Left = 420
    Top = 29
    Width = 1064
    Height = 743
    Align = alClient
    Caption = 'panGraph'
    ShowCaption = False
    TabOrder = 2
    object CW: TChartWriter
      Left = 1
      Top = 1
      Width = 1062
      Height = 741
      Align = alClient
      Color = clBtnFace
      ParentColor = False
      AfterBuildChart = CWAfterBuildChart
      BezierMargin = 20
      Centered = True
      Chart = Chart_CO2
      MouseTimeFormat = 'mmmm" "yyyyy'
      OnDataChange = CWDataChange
      OnQuerySpace = CWQuerySpace
      Rulers = ruNames
    end
  end
  object sb: TStatusBar
    Left = 0
    Top = 772
    Width = 1484
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object ActionList1: TActionList
    Images = ImageList1
    OnUpdate = ActionList1Update
    Left = 705
    Top = 481
    object FileOpen1: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Dialog.Filter = 'CharWriter rich fild|*.CWR'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 0
      ShortCut = 16463
      OnAccept = FileOpen1Accept
    end
    object acCurveFont: TFontEdit
      Category = 'Dialog'
      Caption = 'Text font ...'
      Dialog.Font.Charset = DEFAULT_CHARSET
      Dialog.Font.Color = clWindowText
      Dialog.Font.Height = -11
      Dialog.Font.Name = 'Tahoma'
      Dialog.Font.Style = []
      Hint = 'Font used to draw text associated withe the curve'
      BeforeExecute = acCurveFontBeforeExecute
      OnAccept = acCurveFontAccept
    end
    object acApply: TAction
      Caption = 'Apply'
      Hint = 'Apply line styles'
      OnExecute = acApplyExecute
      OnUpdate = acApplyUpdate
    end
    object acApplyBarAnim: TAction
      Caption = 'Apply'
      Enabled = False
      Hint = 'Run animation'
      OnExecute = acApplyBarAnimExecute
      OnUpdate = acApplyBarAnimUpdate
    end
    object acPieTitleFont: TFontEdit
      Category = 'Dialog'
      Caption = 'Title font ...'
      Dialog.Font.Charset = DEFAULT_CHARSET
      Dialog.Font.Color = clWindowText
      Dialog.Font.Height = -11
      Dialog.Font.Name = 'Tahoma'
      Dialog.Font.Style = []
      Hint = 'Font Select'
      BeforeExecute = acPieTitleFontBeforeExecute
      OnAccept = acPieTitleFontAccept
    end
    object acNLabelFont: TFontEdit
      Category = 'Dialog'
      Caption = '...'
      Dialog.Font.Charset = DEFAULT_CHARSET
      Dialog.Font.Color = clWindowText
      Dialog.Font.Height = -11
      Dialog.Font.Name = 'Tahoma'
      Dialog.Font.Style = []
      Hint = 'Font Select'
      BeforeExecute = acNLabelFontBeforeExecute
      OnAccept = acNLabelFontAccept
    end
    object acVLabelFont: TFontEdit
      Category = 'Dialog'
      Caption = '...'
      Dialog.Font.Charset = DEFAULT_CHARSET
      Dialog.Font.Color = clWindowText
      Dialog.Font.Height = -11
      Dialog.Font.Name = 'Tahoma'
      Dialog.Font.Style = []
      Hint = 'Font Select'
      BeforeExecute = acVLabelFontBeforeExecute
      OnAccept = acVLabelFontAccept
    end
    object acChartTitleFont: TFontEdit
      Category = 'Dialog'
      Caption = '...'
      Dialog.Font.Charset = DEFAULT_CHARSET
      Dialog.Font.Color = clWindowText
      Dialog.Font.Height = -11
      Dialog.Font.Name = 'Tahoma'
      Dialog.Font.Style = []
      Hint = 'Font Select'
      BeforeExecute = acChartTitleFontBeforeExecute
      OnAccept = acChartTitleFontAccept
    end
    object acNextPage: TAction
      Category = 'Dialog'
      Caption = 'Next page'
      Hint = 'Next page'
      ImageIndex = 8
      ShortCut = 16462
      OnExecute = acNextPageExecute
      OnUpdate = acNextPageUpdate
    end
    object acPrevPage: TAction
      Category = 'Dialog'
      Caption = 'Previous page'
      Hint = 'Previous page'
      ImageIndex = 7
      ShortCut = 16464
      OnExecute = acPrevPageExecute
      OnUpdate = acPrevPageUpdate
    end
    object acUnzoom: TAction
      Caption = 'Unzoom'
      Hint = 'Unzoom'
      ImageIndex = 2
      ShortCut = 16469
      OnExecute = acUnzoomExecute
      OnUpdate = acUnzoomUpdate
    end
    object acDashbord: TAction
      AutoCheck = True
      Caption = 'View dashboard'
      Hint = 'View dashboard'
      ImageIndex = 3
      ShortCut = 16452
      OnExecute = acDashbordExecute
    end
    object acV2LabelFont: TFontEdit
      Category = 'Dialog'
      Caption = '...'
      Dialog.Font.Charset = DEFAULT_CHARSET
      Dialog.Font.Color = clWindowText
      Dialog.Font.Height = -11
      Dialog.Font.Name = 'Tahoma'
      Dialog.Font.Style = []
      Hint = 'Font Select'
      BeforeExecute = acVLabelFontBeforeExecute
      OnAccept = acVLabelFontAccept
    end
    object acShowData: TAction
      Caption = 'Edit data ...'
      Hint = 'Data editor'
      ImageIndex = 4
      OnExecute = acShowDataExecute
      OnUpdate = acShowDataUpdate
    end
    object acNewChart: TAction
      Caption = 'Create new chart ...'
      Hint = 'Create a new chart'
      ImageIndex = 5
      OnExecute = acNewChartExecute
    end
    object FileSaveAs1: TFileSaveAs
      Category = 'File'
      Caption = 'Save &As...'
      Dialog.Filter = 'CharWriter rich file|*.CWR'
      Hint = 'Save As|Saves the active file with a new name'
      ImageIndex = 6
      ShortCut = 16467
      OnAccept = FileSaveAs1Accept
      OnUpdate = FileSaveAs1Update
    end
    object acAddNameSections: TAction
      Caption = 'Add name sections'
      OnExecute = acAddNameSectionsExecute
      OnUpdate = acAddNameSectionsUpdate
    end
    object acAddValueSections: TAction
      Caption = 'Add value sections'
      OnExecute = acAddValueSectionsExecute
      OnUpdate = acAddValueSectionsUpdate
    end
    object acAddSectionList: TAction
      Caption = 'Add section'
      OnExecute = acAddSectionListExecute
      OnUpdate = acAddSectionListUpdate
    end
    object acDeleteSectionList: TAction
      Caption = 'Delete section'
      OnExecute = acDeleteSectionListExecute
      OnUpdate = acDeleteSectionListUpdate
    end
    object acUpdateSection: TAction
      Caption = 'Update section'
      OnExecute = acUpdateSectionExecute
      OnUpdate = acUpdateSectionUpdate
    end
    object acAddLegend: TAction
      Caption = 'Add'
      OnExecute = acAddLegendExecute
    end
    object acDeleteLegend: TAction
      Caption = 'Delete'
      OnExecute = acDeleteLegendExecute
      OnUpdate = acDeleteLegendUpdate
    end
    object acReload: TAction
      Caption = 'Reload'
      Hint = 'Reload'
      ImageIndex = 9
      OnExecute = acReloadExecute
      OnUpdate = acReloadUpdate
    end
    object acAddCategory: TAction
      Caption = 'Add'
      OnExecute = acAddCategoryExecute
      OnUpdate = acAddCategoryUpdate
    end
    object acDeleteCategory: TAction
      Caption = 'Delete'
      OnExecute = acDeleteCategoryExecute
      OnUpdate = acDeleteCategoryUpdate
    end
    object acDeleteSeries: TAction
      Caption = 'Delete'
      OnExecute = acDeleteSeriesExecute
      OnUpdate = acDeleteSeriesUpdate
    end
    object acAddSeries: TAction
      Caption = 'Add'
      OnExecute = acAddSeriesExecute
      OnUpdate = acAddSeriesUpdate
    end
    object acContract: TAction
      Caption = 'Contraction ...'
      OnExecute = acContractExecute
      OnUpdate = acContractUpdate
    end
    object acResetContraction: TAction
      Caption = 'Reset contraction'
      OnExecute = acResetContractionExecute
      OnUpdate = acResetContractionUpdate
    end
  end
  object ImageList1: TImageList
    Left = 705
    Top = 537
    Bitmap = {
      494C01010B00D8064C0610001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000325648006896800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000226048002A7A570065957B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000257151003CA4750035835A00699175000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00000000000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000015663F0040B57C003FAA72002F7E5200709A830000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF000000
      0000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000338358003FB9790034B270003CB0750020774B006DAC8C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000080800000808000008080000080800000808000008080000080
      8000008080000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003F8D5E0050C985001CA65E002BB6720045B97E00237A4E00719B84000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003B90630068C1890045B57B0030AF7B0048C385002987580055856B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003EA1710069CF950051C1870058CB99003694650076BC9800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002C9D6B006CDEA20062CD950046A1740086B69C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000040B685005ACD94005BB788008BC8A6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003EA97E0055B586008FD1AE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000065B4930097D3B10000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000000
      000080808000808080008080800000FFFF0000FFFF0080808000808080008080
      80008080800000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E4BFA300DBAB8800D8A4
      7C00D6A17800D39D7300D19A7000D0976C00CC946800CC946800CC946800CC94
      6800CC946800CC946800D7AE900000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DDAE8900F9F4F000F8F3
      EE00F8F0EA00F6EEE700F5EBE300F3E8E100F3E7DE00F3E7DE00F3E7DE00F3E7
      DE00F3E7DE00F3E7DE00CE9A7300000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000759B890026574300000000000000
      00000000000000000000000000000000000000000000DFB18D00FAF5F100EED9
      C90000000000EEDACA00000000000000000000000000EED0BA00000000000000
      000000000000F3E7DE00CF9B7400000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000008080800000000000000000000000000000000000000000000000
      0000008080000080800000000000000000000000000000000000C0C0C0000000
      0000008080000000000000000000000000000000000000000000000000000000
      000000000000000000000000000088A89500396B530029664C00000000000000
      00000000000000000000000000000000000000000000E3B59000FAF5F200EED7
      C500EED7C600EED7C600EED7C600EED7C600EED8C800EED5C100EED5C100EED5
      C100EED5C100F3E7DE00CF9A7100000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000008080800000000000000000000000000000000000000000000000
      0000008080000080800000000000000000000000000000000000C0C0C0000000
      0000008080000000000000000000000000000000000000000000000000000000
      00000000000000000000649E8500157A4D0041AE760010743800000000000000
      00000000000000000000000000000000000000000000E5B69400FAF5F200EED6
      C30000000000EED7C600000000000000000000000000EED7C500FCF8F4000000
      000000000000F3E7DE00D19C7400000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000008080800000000000000000000000000000000000000000000000
      0000008080000080800000000000000000000000000000000000000000000000
      0000008080000000000000000000000000000000000000000000000000000000
      000000000000729983002979540043AE7D0041AE76001C7F4700000000000000
      00000000000000000000000000000000000000000000E7BA9900FBF6F300EED3
      BE00EED4BF00EED4BF00EED4BF00EED4BF00EED6C300ECD0B900ECD0B900ECD1
      BD00ECD1BB00F3E7DE00CD966A000000000000FFFF0000FFFF0000FFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000FFFF0000FFFF00000000000000000000000000000000000000
      0000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      000063997C00288155003EAE780031A66D0048B57D0021834F00000000000000
      00000000000000000000000000000000000000000000E8BD9D00FBF8F300EED1
      BB0000000000EED1BD00000000000000000000000000ECD0B900000000000000
      000000000000F3E9E100CF966B00000000000000000000FFFF0000FFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000008080000080800000000000000000000000000000000000000000000080
      8000008080000000000000000000000000000000000000000000000000006889
      74002D82560038B2760029B06C0032AF71004FBB8600298C5C00000000000000
      00000000000000000000000000000000000000000000EABFA000FBF8F400EDCF
      B700EDCFB900EED0B900EDD0BA00EDD1BB00EDD1BD00ECD0B900EDD1BD00ECD1
      BD00ECD4C100F4EBE300D19A6F00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000080800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000008080000000000000000000000000000000000000000000000000006084
      6E0030875B0043BF830032B975003BB87A005DC9940033966600000000000000
      00000000000000000000000000000000000000000000ECC2A400FCF8F600EDCC
      B40000000000ECCDB600000000000000000000000000ECD0B900000000000000
      000000000000F8F3EE00D39E7400000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000080800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000008080000000000000000000000000000000000000000000000000000000
      00006EA68900449F720054C6900047BF850063D29A00369B6700000000000000
      00000000000000000000000000000000000000000000EEC4A800FCF8F600EDCC
      B400EDCCB400EDCCB400EDCCB400EDCCB400EDCCB400EDCCB400EDCCB400EDCC
      B400EDCCB400FCF8F600D6A27900000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000080800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000083AF98004CA17B0065D4A2005FD097003FA66D00000000000000
      00000000000000000000000000000000000000000000EFC7AB00FCF8F6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FCF8F600D8A57E00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      00000080800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      000000000000000000008FD0B60047B0830068D9A00048B07300000000000000
      00000000000000000000000000000000000000000000F0C9AD00FCF8F600A8D9
      B200A4D7AE00A1D5AB009DD3A60099D1A20095CF9D0090CC99008DCA950089C8
      910084C58D00FCF8F600DCA9830000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000096D1B50041AC7B002BB57500000000000000
      00000000000000000000000000000000000000000000F3CEB600FCF8F600FCF8
      F600FCF8F600FCF8F600FCF8F600FCF8F600FCF8F600FCF8F600FCF8F600FCF8
      F600FCF8F600FCF8F600DFAF8B00000000000000000000FFFF0000FFFF000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      00000000000000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097D7B40060B88A00000000000000
      00000000000000000000000000000000000000000000F8E5D900F3D0B900F1CA
      AE00EFC8AD00EFC6AA00EEC4A600ECC2A400EBC0A100E9BD9D00E8BB9A00E6B8
      9600E3B69200E3B28F00E6C1A5000000000000FFFF0000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008C8580FFB2A9A4FFE9DDD3FF0000000000000000000000000000
      000000000000827D7A004F4B46003E3B36004A47420056524D00AFACA4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848383FF36587FFF3E5878FFE8DAD2FF0000000000000000000000000000
      000000000000888380007C7975007E7B770084817C0068656000ACA8A3000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABA3
      9EFF3B5A80FF325B84FF4B627DFF0000000000000000D2CCC70046403B00C4BD
      BA008883800057524F008A878300989591009F9E9A0086858100595651009794
      8F009B9792004B474200CBC5BE00000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C8BEB5FF425D
      7CFF376088FF3B5475FFD0C9C4FF0000000000000000443E39007E7873005954
      510088858100C8C5C100B4B3AF008E8C8B0088868500B8B6B500BFBEBA008683
      7F0077746F007D7A75004B484000000000000000000000FFFF00000000000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003E5C7EFF365A
      83FF3B5677FFC1B9B5FF00000000000000004A443D008B85800086807B00827D
      7A00BBB8B40088848300848281007F7D7D008586840080817F008A8B8900A7A5
      A400908F8B007D7C780078746F004B443B0000000000FFFFFF0000FFFF000000
      0000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BEB9B4FFA3A2A0FFA09F9DFFA4A1A0FFB4B0ABFF6F7E8CFF4A6787FF3655
      76FFB9B3B1FF000000000000000000000000534D460078726D007A767100B3B0
      AC009E9B97009E9C9B00A0A19F0097979700A6A6A60092939100A2A3A100AEAC
      AB00A09E9D0074727100827F7A00544D44000000000000FFFF00FFFFFF0000FF
      FF00000000000080800000808000008080000080800000808000008080000080
      8000008080000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009C9B
      99FF8E8E8DFFC5C0B9FFD5D1CCFFCDC9C5FFACA7A4FF7D7E80FF96979FFFA7A7
      AAFF00000000000000000000000000000000958F8A00605B580087827F00B1AD
      AC00BEBAB900C6C4C400989898003838380027292900A4A6A600B5B7B700BFBF
      BF00A8A9A70075767400514E4A008982790000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009D9B99FFA09C
      98FFF8EFDDFFF9F2E4FFF9F1E6FFF9F2E6FFFAF2E8FFD1C7C5FF969493FFE7DA
      D1FF00000000000000000000000000000000000000007A7572006B666300BFBB
      BA00E4E2E100E4E4E40028282800E4E6E600F0F2F200282A2A00E9EBEB00DDDD
      DD00C0C0C000727371006E6D6900000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0B9B5FF979594FFF8E2
      C7FFF9E7D6FFFAEADDFFF9E8DBFFF9E9DCFFF8EADBFFFAE6DCFFB8B0ADFFBCB5
      B0FF0000000000000000000000000000000000000000736E6B006D686500CECA
      C900F1EFEE00F3F3F3002F2F2F00FDFFFF00EBEDEE0026282900F0F2F300F5F7
      F700CCCECE0066686800706F6B000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009D9C9BFFC4B19EFFFAE2
      C8FFFAE8D9FFF9E9DAFFF9E7D7FFF9E7D8FFF9E9D8FFF9E7D8FFE0CAC1FFA5A4
      A2FF0000000000000000000000000000000087837E006863600074716D00AFAD
      AC00D9DAD800D9D9D900B5B5B500232526002A2C2D00B3B5B600DADCDC00D0D2
      D200A7A7A7007373730052514D00868178000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009A9A9AFFDDBB95FFFBE3
      CAFFFF000000FF000000FF000000FF000000FF000000FBEBDAFFEED3C4FFA5A4
      A2FF0000000000000000000000000000000056534F007A767500787675009997
      9700B7B7B700BFBEC000C3C5C600C6C8C900BDBFC000C9C9C900BEBEBE00B7B7
      B70092908F007C7A79007C797400524B42000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A09F9FFFCDAF90FFFCE0
      BFFFFF000000FF000000FF000000FF000000FF000000FCE7D0FFE2CAB7FF9F9E
      9DFF000000000000000000000000000000003B3834008A868500757372007C7A
      7A009B9B9B0091919100DBDDDD00CAC9CB00DADADA00D0D0D0009B9C9A00A19F
      9E008A89850076736F0087837E00554E45000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B7B4B2FFB1A294FFF1CA
      A0FFFEF2E0FFFDEAD8FFFDE9D6FFFDE9D7FFFDEDDCFFFBE5CEFFCAB7A7FFB0B0
      ADFF0000000000000000000000000000000000000000413E3A0085848000716F
      6E0089878600BCBAB9008A8B890087858500817F7E00A5A3A200B0AEAD007E7A
      7900605D590079747100514B4400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009C9B99FFD7B7
      95FFE9CDACFFF5E3D0FFF8E8D6FFFAEBD9FFF3DCC6FFDFC0A5FFA49E95FFD1CD
      C8FF000000000000000000000000000000000000000000000000575450009996
      92009D9B9A005B5958008D8B8A009A9897009C9A99008D8B8A0064615D009693
      8F00C3BFBA004A464100D2CCC500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BEBBB8FF9C9A
      97FFCABAADFFDBC4AFFFDFC0A1FFDDC8B1FFD3BEA8FFACA094FFAEACAAFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009B9894007B787400817E7A007A777300787571007B7772000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BBB7B4FFA4A29FFFA5A29EFFA4A19EFFB7B4B1FFD6D0CBFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000099959000494641003D3835005C58530047433E008B857E000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF00FFFFFF7FFFFF0000FFFFFE7FFFFF0000
      F3FFFCFF001F0000F1FFF81F000F0000F0FFF80F00070000F07FCCE700030000
      F03F9E7300010000F01F9F7300000000F01F3FF9001F0000F03F3FF9001F0000
      F07F9FF3001F0000F0FF9FF38FF10000F1FFCFE7FFF90000F3FFE38FFF750000
      FFFFF01FFF8F0000FFFFFC7FFFFF0000FFFF9001FF7EFFFF8001C003BFFFFFFF
      8001E003F003FF3F8BB9E003E003FE3F8001E003E003FC3F8B99E003E003F83F
      80010001E003F03F8BB980002003E03F8001E007E002E03F8BB9E00FE003F03F
      8001E00FE003F83F9FF9E027E003FC3F8001C073E003FE3F80019E79FFFFFF3F
      80017EFEBF7DFFFFFFFFFFFF7F7EFFFFFFFFFFFFFFF8F81FFFFFFFFFFFF0F81F
      001FFFFFFFE18001000FFFFFFFC180010007FFFFFFC300000003FFFFF0070000
      0001FFF7E00F00000000FFF3C00F8001001F8001800F8001001FFFF3800F0000
      001FFFF7800F00008FF1FFFF800F0000FFF9FFFF800F8001FF75FFFFC00FC001
      FF8FFFFFC01FF81FFFFFFFFFF03FF81F00000000000000000000000000000000
      000000000000}
  end
  object Curve_CO2: TCWCurve
    AnimationSpeed = asMediumSlow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    KeepFontColor = False
    StatLineWidth = 0
    Animation = True
    LineWidth = 2
    MinPointSpacing = 1
    SeriesStyles = <>
    Left = 580
    Top = 477
  end
  object Leg_CO2: TCWLegend
    Anchoring = anLeftInside
    Border = True
    Bullets = lbNone
    Contents = []
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    HorizMargins = 20
    Text.Strings = (
      'Welcome to the ChartWriter demo app!'
      ''
      
        'Here you will be able to explore most of the features offered by' +
        ' CW. '
      
        'Help youself from the demo folder; there you will find a collect' +
        'ion of premade '
      'charts with which you can experiment.'
      ''
      'We start with one of the most famous graphs for the time being. '
      ''
      'To zoom, press Ctrl key, then click and drag the mouse.'
      ''
      'God luck!'
      '')
    Transparency = 128
    VertMargins = 20
    Left = 580
    Top = 589
  end
  object legCO2Pandemic: TCWLegend
    Anchoring = anPointInside
    Border = True
    Bullets = lbNone
    Contents = []
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    HorizMargins = 0
    PointItemIndex = 768
    PointName = '3/1/2022'
    PointOptions = [poShowConnectionLine, poThinConnectionLine]
    Text.Strings = (
      'Pandemic')
    Transparency = 128
    Left = 580
    Top = 645
  end
  object Chart_CO2: TCWSpanChart
    FileName = 'CO2Graph.CWD'
    GraphBGColor = clSkyBlue
    InnerMargins.Right = 20
    Legends = <
      item
        Legend = Leg_CO2
      end
      item
        Legend = legCO2Pandemic
      end>
    SeriesDefs = <
      item
        Color = clRed
        Graph = Curve_CO2
        Title = 'Hockey stick'
      end>
    Title = 'CO2 in atmosphere 1958 - 2023'
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBlue
    TitleFont.Height = -16
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    GraphBorders = gbAxis
    MouseTimeFormat = 'mmmm" "yyyyy'
    NameScale.Font.Charset = DEFAULT_CHARSET
    NameScale.Font.Color = clWindowText
    NameScale.Font.Height = -11
    NameScale.Font.Name = 'Tahoma'
    NameScale.Font.Style = []
    NameScale.Qualifier = 'Years'
    NameScale.ShowLabels = False
    NameScale.OverflowAction = ovContraction
    NameSectionDefs = Sect_Year
    SpanType = ntMonthSpan
    TimeFormat = 'yy" "mm'
    ValueScale1.Font.Charset = DEFAULT_CHARSET
    ValueScale1.Font.Color = clWindowText
    ValueScale1.Font.Height = -11
    ValueScale1.Font.Name = 'Tahoma'
    ValueScale1.Font.Style = []
    ValueScale1.Qualifier = 'PPM'
    ValueScale1.ScaleRounding = False
    ValueScale1.ValueHigh = 424.000000000000000000
    ValueScale1.ValueIntervals = 1.000000000000000000
    ValueScale1.ValueLow = 312.429992675781300000
    ValueScale1.ValueSpanFromData = True
    ValueScale2.Font.Charset = DEFAULT_CHARSET
    ValueScale2.Font.Color = clWindowText
    ValueScale2.Font.Height = -11
    ValueScale2.Font.Name = 'Tahoma'
    ValueScale2.Font.Style = []
    ValueScale2.ScaleRounding = False
    ValueScale2.ValueHigh = 1.000000000000000000
    ValueScale2.ValueIntervals = 1.000000000000000000
    Left = 580
    Top = 421
  end
  object Sect_Year: TCWNameSectionDefs
    CaptionLayout = clSameSideAsLabels
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Pen.Color = clMedGray
    Sections = <>
    AutoSections = autYears
    Left = 580
    Top = 533
  end
  object MainMenu1: TMainMenu
    Images = ImageList1
    Left = 705
    Top = 422
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Action = FileOpen1
      end
      object SaveAs1: TMenuItem
        Action = FileSaveAs1
      end
    end
    object Chart1: TMenuItem
      Caption = 'Chart'
      object Createnewchart1: TMenuItem
        Action = acNewChart
      end
      object acShowData1: TMenuItem
        Action = acShowData
      end
      object Reload1: TMenuItem
        Action = acReload
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Addnamesections1: TMenuItem
        Action = acAddNameSections
      end
      object Addvaluesections1: TMenuItem
        Action = acAddValueSections
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Nextpage1: TMenuItem
        Action = acNextPage
      end
      object Previouspage1: TMenuItem
        Action = acPrevPage
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Contraction1: TMenuItem
        Action = acContract
      end
      object Resetcontraction1: TMenuItem
        Action = acResetContraction
      end
      object Unzoom1: TMenuItem
        Action = acUnzoom
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Viewdashboard1: TMenuItem
        Action = acDashbord
        AutoCheck = True
      end
    end
  end
  object pmSections: TPopupMenu
    Left = 705
    Top = 592
    object Addsectiontolist1: TMenuItem
      Action = acAddSectionList
    end
    object Updatesection1: TMenuItem
      Action = acUpdateSection
    end
    object Deletesection1: TMenuItem
      Action = acDeleteSectionList
    end
  end
  object pmLegends: TPopupMenu
    Left = 705
    Top = 645
    object MenuItem1: TMenuItem
      Action = acAddLegend
    end
    object MenuItem3: TMenuItem
      Action = acDeleteLegend
    end
  end
  object pmCategories: TPopupMenu
    Left = 796
    Top = 645
    object acAddCategory1: TMenuItem
      Action = acAddCategory
    end
    object acDeleteCategory1: TMenuItem
      Action = acDeleteCategory
    end
  end
  object pmSeries: TPopupMenu
    Left = 796
    Top = 591
    object MenuItem2: TMenuItem
      Action = acAddSeries
    end
    object MenuItem4: TMenuItem
      Action = acDeleteSeries
    end
  end
  object dlgImage: TOpenPictureDialog
    Left = 868
    Top = 589
  end
end
