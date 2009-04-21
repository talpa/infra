object CustomizeScreen: TCustomizeScreen
  Left = 374
  Top = 35
  Width = 727
  Height = 557
  Caption = 'Customize screen'
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lsvControlList: TListView
    Left = 0
    Top = 232
    Width = 719
    Height = 256
    Align = alBottom
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
