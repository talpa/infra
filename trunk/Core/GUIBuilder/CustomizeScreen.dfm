object CustomizeScreen: TCustomizeScreen
  Left = 0
  Top = 0
  Caption = 'Customize screen'
  ClientHeight = 405
  ClientWidth = 673
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
    Top = 0
    Width = 673
    Height = 370
    Align = alClient
    BorderWidth = 1
    Columns = <
      item
        Caption = 'Name'
        Width = 155
      end
      item
        Caption = 'Caption'
        Width = 185
      end
      item
        Caption = 'Control class'
        Width = 132
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 370
    Width = 673
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btbtCancel: TButton
      Left = 167
      Top = 5
      Width = 75
      Height = 25
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = btbtCancelClick
    end
    object btbtOK: TButton
      Left = 89
      Top = 5
      Width = 75
      Height = 25
      Caption = '&OK'
      TabOrder = 0
    end
    object btbtEditControl: TButton
      Left = 8
      Top = 5
      Width = 75
      Height = 25
      Caption = '&Edit control'
      Default = True
      TabOrder = 2
      OnClick = btbtEditControlClick
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
