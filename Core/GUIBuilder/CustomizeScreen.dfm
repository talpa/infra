object CustomizeScreen: TCustomizeScreen
  Left = 0
  Top = 0
  Caption = 'Customize screen'
  ClientHeight = 523
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lsvControlList: TListView
    Left = 0
    Top = 127
    Width = 719
    Height = 361
    Align = alClient
    BorderWidth = 1
    Columns = <
      item
        Caption = 'Name'
        Width = 138
      end
      item
        Caption = 'Caption'
        Width = 180
      end
      item
        Caption = 'Visible'
        Width = 55
      end
      item
        Caption = 'Caption Visible'
        Width = 85
      end
      item
        Caption = 'Put After'
        Width = 65
      end
      item
        Caption = 'Put Before'
        Width = 65
      end
      item
        Caption = 'Height'
        Width = 55
      end
      item
        Caption = 'Width'
        Width = 55
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lsvControlListDblClick
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 488
    Width = 719
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btbtCancel: TButton
      Left = 165
      Top = 5
      Width = 75
      Height = 25
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = btbtCancelClick
    end
    object btbtOK: TButton
      Left = 85
      Top = 5
      Width = 75
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      OnClick = btbtOKClick
    end
    object btbtEditControl: TButton
      Left = 5
      Top = 5
      Width = 75
      Height = 25
      Caption = '&Edit control'
      Default = True
      TabOrder = 2
      OnClick = btbtEditControlClick
    end
  end
  object LayoutManager1: TLayoutManager
    Left = 0
    Top = 0
    Width = 719
    Height = 127
    AlignMode = alTop
    ItemDefPadding.Bottom = 5
    ItemDefPadding.Left = 5
    ItemDefPadding.Right = 5
    ItemDefPadding.Top = 5
    ItemSpacing.Bottom = 5
    ItemSpacing.Left = 5
    ItemSpacing.Right = 5
    ItemSpacing.Top = 5
    object Title: TLayoutManagerItem
      Left = 5
      Top = 5
      Width = 709
      Height = 31
      Caption = 'Title'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 20
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editTitle
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 100.000000000000000000
      object editTitle: TEdit
        Left = 85
        Top = 5
        Width = 619
        Height = 21
        AutoSize = False
        TabOrder = 0
      end
    end
    object CaptionPosition: TLayoutManagerItem
      Left = 5
      Top = 46
      Width = 206
      Height = 31
      Caption = 'Caption Position'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 77
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = combCaptionPosition
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 30.000000000000000000
      object combCaptionPosition: TComboBox
        Left = 85
        Top = 5
        Width = 116
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        Items.Strings = (
          'Above'
          'Below'
          'Left'
          'Right')
      end
    end
    object Height: TLayoutManagerItem
      Left = 221
      Top = 46
      Width = 134
      Height = 31
      Caption = 'Height'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 31
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editHeight
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 20.000000000000000000
      object editHeight: TMaskEdit
        Left = 39
        Top = 5
        Width = 90
        Height = 21
        AutoSize = False
        TabOrder = 0
      end
    end
    object Width: TLayoutManagerItem
      Left = 365
      Top = 46
      Width = 134
      Height = 31
      Caption = 'Width'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 28
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editWidth
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 20.000000000000000000
      object editWidth: TMaskEdit
        Left = 36
        Top = 5
        Width = 93
        Height = 21
        AutoSize = False
        TabOrder = 0
      end
    end
    object ItemLayout: TLayoutManagerItem
      Left = 509
      Top = 46
      Width = 206
      Height = 31
      Caption = 'Item Layout'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 58
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = combItemLayout
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 30.000000000000000000
      object combItemLayout: TComboBox
        Left = 66
        Top = 5
        Width = 135
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        Items.Strings = (
          'Horizontal'
          'Vertical')
      end
    end
    object PaddingLeft: TLayoutManagerItem
      Left = 5
      Top = 87
      Width = 169
      Height = 31
      Caption = 'Padding - Left'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 67
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editPaddingLeft
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 24.895688456189150000
      object editPaddingLeft: TMaskEdit
        Left = 85
        Top = 5
        Width = 79
        Height = 21
        AutoSize = False
        EditMask = '99;1; '
        MaxLength = 2
        TabOrder = 0
        Text = '  '
      end
    end
    object PaddingTop: TLayoutManagerItem
      Left = 184
      Top = 87
      Width = 169
      Height = 31
      Caption = 'Padding - Top'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 66
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editPaddingTop
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 24.895688456189150000
      object editPaddingTop: TMaskEdit
        Left = 74
        Top = 5
        Width = 90
        Height = 21
        AutoSize = False
        EditMask = '99;1; '
        MaxLength = 2
        TabOrder = 0
        Text = '  '
      end
    end
    object PaddingRight: TLayoutManagerItem
      Left = 363
      Top = 87
      Width = 169
      Height = 31
      Caption = 'Padding - Right'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 73
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editPaddingRight
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 24.895688456189150000
      object editPaddingRight: TMaskEdit
        Left = 81
        Top = 5
        Width = 83
        Height = 21
        AutoSize = False
        EditMask = '99;1; '
        MaxLength = 2
        TabOrder = 0
        Text = '  '
      end
    end
    object PaddingBottom: TLayoutManagerItem
      Left = 542
      Top = 87
      Width = 169
      Height = 31
      Caption = 'Padding - Bottom'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 82
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editPaddingBottom
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 24.895688456189150000
      object editPaddingBottom: TMaskEdit
        Left = 90
        Top = 5
        Width = 74
        Height = 21
        AutoSize = False
        EditMask = '99;1; '
        MaxLength = 2
        TabOrder = 0
        Text = '  '
      end
    end
  end
  object actlActions: TActionList
    Left = 264
    Top = 208
    object actnClose: TAction
      Caption = 'Close'
      ShortCut = 27
      OnExecute = actnCloseExecute
    end
  end
end
