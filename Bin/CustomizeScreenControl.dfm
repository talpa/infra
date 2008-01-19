object CustomizeScreenControl: TCustomizeScreenControl
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Customize screen control'
  ClientHeight = 281
  ClientWidth = 586
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
  object pnlBottom: TPanel
    Left = 0
    Top = 246
    Width = 586
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btbtCancel: TButton
      Left = 85
      Top = 5
      Width = 75
      Height = 25
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = btbtCancelClick
    end
    object btbtOK: TButton
      Left = 5
      Top = 5
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      TabOrder = 0
      OnClick = btbtOKClick
    end
  end
  object LayoutManager1: TLayoutManager
    Left = 0
    Top = 0
    Width = 586
    Height = 246
    AlignMode = alClient
    ItemDefPadding.Bottom = 5
    ItemDefPadding.Left = 5
    ItemDefPadding.Right = 5
    ItemDefPadding.Top = 5
    ItemSpacing.Bottom = 5
    ItemSpacing.Left = 5
    ItemSpacing.Right = 5
    ItemSpacing.Top = 5
    object Name: TLayoutManagerItem
      Left = 5
      Top = 5
      Width = 576
      Height = 31
      Caption = 'Name'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 27
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editName
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 100.000000000000000000
      object editName: TEdit
        Left = 64
        Top = 5
        Width = 507
        Height = 21
        TabStop = False
        AutoSize = False
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
    end
    object Caption: TLayoutManagerItem
      Left = 5
      Top = 46
      Width = 576
      Height = 31
      Caption = 'Caption'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 37
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editCaption
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 100.000000000000000000
      object editCaption: TEdit
        Left = 64
        Top = 5
        Width = 507
        Height = 21
        AutoSize = False
        TabOrder = 0
      end
    end
    object Visible: TLayoutManagerItem
      Left = 5
      Top = 87
      Width = 136
      Height = 31
      Caption = 'CheckBox1'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 53
      CaptionVisible = False
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = ckbxVisible
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 9
      UseDefPadding = False
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 25.000000000000000000
      object ckbxVisible: TCheckBox
        Left = 5
        Top = 9
        Width = 126
        Height = 17
        Caption = 'Visible'
        TabOrder = 0
      end
    end
    object CaptionVisible: TLayoutManagerItem
      Left = 151
      Top = 87
      Width = 136
      Height = 31
      Caption = 'CheckBox1'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 53
      CaptionVisible = False
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = ckbxCaptionVisible
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 9
      UseDefPadding = False
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 25.000000000000000000
      object ckbxCaptionVisible: TCheckBox
        Left = 5
        Top = 9
        Width = 126
        Height = 17
        Caption = 'Caption Visible'
        TabOrder = 0
      end
    end
    object CaptionPosition: TLayoutManagerItem
      Left = 297
      Top = 87
      Width = 283
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
      WidthOptions.Size = 50.000000000000000000
      object combCaptionPosition: TComboBox
        Left = 135
        Top = 5
        Width = 143
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
    object ItemHeight: TLayoutManagerItem
      Left = 5
      Top = 128
      Width = 282
      Height = 31
      Caption = 'Item Height'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 56
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editItemHeight
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 49.840764331210190000
      object editItemHeight: TMaskEdit
        Left = 64
        Top = 5
        Width = 213
        Height = 21
        AutoSize = False
        EditMask = '999;1; '
        MaxLength = 3
        TabOrder = 0
        Text = '   '
      end
    end
    object ItemHeightMeasureType: TLayoutManagerItem
      Left = 297
      Top = 128
      Width = 283
      Height = 31
      Caption = 'Item Height Measure Type'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 127
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = combItemHeightMeasureType
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 50.000000000000000000
      object combItemHeightMeasureType: TComboBox
        Left = 135
        Top = 5
        Width = 143
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        Items.Strings = (
          'Fix'
          'Percent')
      end
    end
    object ItemWidth: TLayoutManagerItem
      Left = 5
      Top = 169
      Width = 282
      Height = 31
      Caption = 'Item Width'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 53
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editItemWidth
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 49.840764331210190000
      object editItemWidth: TMaskEdit
        Left = 64
        Top = 5
        Width = 213
        Height = 21
        AutoSize = False
        EditMask = '999;1; '
        MaxLength = 3
        TabOrder = 0
        Text = '   '
      end
    end
    object ItemWidthMeasureType: TLayoutManagerItem
      Left = 297
      Top = 169
      Width = 283
      Height = 31
      Caption = 'Item Width Measure Type'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 124
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = combItemWidthMeasureType
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 50.000000000000000000
      object combItemWidthMeasureType: TComboBox
        Left = 135
        Top = 5
        Width = 143
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        Items.Strings = (
          'Fix'
          'Percent')
      end
    end
    object PutAfter: TLayoutManagerItem
      Left = 5
      Top = 210
      Width = 283
      Height = 31
      Caption = 'Put After'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 44
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editPutAfter
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 50.000000000000000000
      object editPutAfter: TEdit
        Left = 64
        Top = 5
        Width = 214
        Height = 21
        AutoSize = False
        TabOrder = 0
      end
    end
    object PutBefore: TLayoutManagerItem
      Left = 298
      Top = 210
      Width = 283
      Height = 31
      Caption = 'Put Before'
      CaptionOptions.Font.Charset = DEFAULT_CHARSET
      CaptionOptions.Font.Color = clWindowText
      CaptionOptions.Font.Height = -11
      CaptionOptions.Font.Name = 'Tahoma'
      CaptionOptions.Font.Style = []
      CaptionOptions.Width = 51
      CaptionVisible = True
      HeightOptions.MeasureType = mtFix
      HeightOptions.Size = 31.000000000000000000
      ItemControl = editPutBefore
      Padding.Bottom = 5
      Padding.Left = 5
      Padding.Right = 5
      Padding.Top = 5
      WidthOptions.MeasureType = mtPercent
      WidthOptions.Size = 50.000000000000000000
      object editPutBefore: TEdit
        Left = 59
        Top = 5
        Width = 219
        Height = 21
        AutoSize = False
        TabOrder = 0
      end
    end
  end
  object actlActions: TActionList
    Left = 184
    Top = 336
    object actnClose: TAction
      Caption = 'Close'
      ShortCut = 27
      OnExecute = actnCloseExecute
    end
  end
end
