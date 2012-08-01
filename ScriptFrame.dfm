object frmScripts: TfrmScripts
  Left = 0
  Top = 0
  Width = 495
  Height = 233
  TabOrder = 0
  DesignSize = (
    495
    233)
  object ListView1: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 489
    Height = 188
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = #1053#1072#1079#1074#1072#1085#1080#1077' '#1089#1082#1088#1080#1087#1090#1072
      end
      item
        Alignment = taCenter
        Caption = #1040#1074#1090#1086#1088
      end
      item
        Alignment = taCenter
        Caption = #1054#1087#1080#1089#1072#1085#1080#1077
      end
      item
        Alignment = taCenter
        Caption = #1055#1072#1087#1082#1072' '#1089#1082#1088#1080#1087#1090#1072
      end>
    Groups = <
      item
        Header = #1057#1082#1072#1095#1072#1085#1085#1099#1077' '#1089#1082#1088#1080#1087#1090#1099
        GroupID = 0
        State = [lgsNormal, lgsCollapsible]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = #1044#1086#1089#1090#1091#1087#1085#1099#1077' '#1089#1082#1088#1080#1087#1090#1099
        GroupID = 1
        State = [lgsNormal, lgsCollapsible]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end>
    Items.ItemData = {
      05E00000000200000000000000FFFFFFFFFFFFFFFF0300000000000000000000
      000B440065006D006F002000530063007200690070007400044B004400650076
      0020C87B1607440065006D006F0029002900290098AA7B1604440065006D006F
      0070D77B1600000000FFFFFFFFFFFFFFFF0300000001000000000000000F4400
      6F0077006E006C006F0061006400200053006300720069007000740006410075
      00740068006F007200B0C77B160B440065007300630072006900700074006900
      6F006E00D0CD7B160744006F0077006E00530063007200B0CE7B16FFFFFFFFFF
      FFFFFFFFFFFFFF}
    GroupView = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object btnDownScript: TButton
    Left = 152
    Top = 197
    Width = 105
    Height = 25
    Anchors = [akBottom]
    Caption = #1057#1082#1072#1095#1072#1090#1100' '#1089#1082#1088#1080#1087#1090
    TabOrder = 1
    OnClick = btnDownScriptClick
  end
  object btnDelScript: TButton
    Left = 274
    Top = 197
    Width = 99
    Height = 25
    Anchors = [akBottom]
    Caption = #1059#1076#1072#1083#1080#1090#1100' '#1089#1082#1088#1080#1087#1090
    TabOrder = 2
    OnClick = btnDelScriptClick
  end
  object btnCleanScr: TButton
    Left = 387
    Top = 197
    Width = 95
    Height = 25
    Anchors = [akRight, akBottom]
    BiDiMode = bdLeftToRight
    Caption = #1054#1095#1080#1089#1090#1080#1090#1100
    ParentBiDiMode = False
    TabOrder = 3
    OnClick = btnCleanScrClick
  end
  object btnUpdateScr: TButton
    Left = 16
    Top = 197
    Width = 118
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #1054#1073#1085#1086#1074#1080#1090#1100' '#1089#1087#1080#1089#1086#1082
    TabOrder = 4
    OnClick = btnUpdateScrClick
  end
end
