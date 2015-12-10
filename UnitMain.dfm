object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Image Viewer'
  ClientHeight = 511
  ClientWidth = 885
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDragDrop = FormDragDrop
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 8
    Top = 8
    Width = 873
    Height = 464
    Cursor = crCross
    OnMouseDown = ImageMouseDown
    OnMouseLeave = ImageMouseLeave
    OnMouseUp = ImageMouseUp
  end
  object ShapeColor: TShape
    Left = 462
    Top = 480
    Width = 27
    Height = 23
  end
  object LabeledEditX: TLabeledEdit
    Left = 65
    Top = 482
    Width = 121
    Height = 21
    EditLabel.Width = 6
    EditLabel.Height = 13
    EditLabel.Caption = 'X'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 0
  end
  object LabeledEditY: TLabeledEdit
    Left = 208
    Top = 480
    Width = 121
    Height = 21
    EditLabel.Width = 6
    EditLabel.Height = 13
    EditLabel.Caption = 'Y'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 1
  end
  object LabeledEditColor: TLabeledEdit
    Left = 343
    Top = 482
    Width = 113
    Height = 21
    EditLabel.Width = 7
    EditLabel.Height = 13
    EditLabel.Caption = 'C'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 2
  end
  object MemoJSON: TMemo
    Left = 552
    Top = 478
    Width = 191
    Height = 25
    ReadOnly = True
    TabOrder = 3
  end
  object ButtonLoad: TButton
    Left = 802
    Top = 478
    Width = 75
    Height = 25
    Caption = '&Open'
    TabOrder = 4
    OnClick = ButtonLoadClick
  end
  object FileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'PNG Image'
        FileMask = '*.png'
      end>
    Options = []
    Left = 24
    Top = 448
  end
end
