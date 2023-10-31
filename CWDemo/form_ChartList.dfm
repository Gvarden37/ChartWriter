object frm_ChartList: Tfrm_ChartList
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Available charts'
  ClientHeight = 170
  ClientWidth = 222
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbCharts: TListBox
    Left = 0
    Top = 0
    Width = 222
    Height = 137
    Align = alTop
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbChartsClick
    ExplicitWidth = 262
  end
  object BitBtn1: TBitBtn
    Left = 72
    Top = 140
    Width = 75
    Height = 25
    Action = form_main.FileOpen1
    Caption = '&Open...'
    TabOrder = 1
  end
end
