object frmCompileFirmware: TfrmCompileFirmware
  Left = 0
  Top = 0
  Width = 560
  Height = 230
  TabOrder = 0
  DesignSize = (
    560
    230)
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 554
    Height = 13
    Align = alTop
    Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '#1086' '#1087#1088#1086#1096#1080#1074#1082#1077':'
    ExplicitWidth = 143
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 30
    Top = 19
    Width = 527
    Height = 94
    Margins.Left = 30
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    TabOrder = 0
  end
  object btnStartCompile: TButton
    Left = 240
    Top = 202
    Width = 109
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = #1050#1086#1084#1087#1080#1083#1080#1088#1086#1074#1072#1090#1100
    TabOrder = 1
    OnClick = btnStartCompileClick
  end
  object chkNoApp: TCheckBox
    Left = 30
    Top = 202
    Width = 171
    Height = 17
    Align = alCustom
    Anchors = [akLeft, akRight, akBottom]
    Caption = #1053#1077' '#1087#1077#1088#1077#1074#1086#1076#1080#1090#1100' '#1087#1088#1080#1083#1086#1078#1077#1085#1080#1103
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 30
    Top = 119
    Width = 527
    Height = 72
    Anchors = [akLeft, akRight, akBottom]
    Caption = #1056#1077#1075#1080#1086#1085#1072#1083#1100#1085#1099#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1099
    TabOrder = 3
    object Label2: TLabel
      Left = 16
      Top = 16
      Width = 165
      Height = 13
      Caption = #1063#1072#1089#1086#1074#1086#1081' '#1087#1086#1103#1089' ('#1087#1086'-'#1091#1084#1086#1083#1095#1072#1085#1080#1102'):'
    end
    object Label3: TLabel
      Left = 93
      Top = 39
      Width = 88
      Height = 13
      Caption = #1071#1079#1099#1082' '#1087#1088#1086#1096#1080#1074#1082#1080':'
    end
    object cbbTimeZoneSelect: TComboBox
      Left = 187
      Top = 12
      Width = 326
      Height = 21
      Style = csDropDownList
      Sorted = True
      TabOrder = 0
      OnChange = cbbTimeZoneSelectChange
    end
    object cbbLangSelect: TComboBox
      Left = 187
      Top = 39
      Width = 326
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = #1056#1091#1089#1089#1082#1080#1081
      Items.Strings = (
        #1056#1091#1089#1089#1082#1080#1081
        #1059#1082#1088#1072#1080#1085#1089#1082#1080#1081
        #1040#1085#1075#1083#1080#1081#1089#1082#1080#1081' ('#1057#1064#1040')')
    end
  end
end
