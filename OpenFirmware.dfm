object frmOpenFirmware: TfrmOpenFirmware
  Left = 0
  Top = 0
  Width = 567
  Height = 287
  TabOrder = 0
  object grpGeneralGroup: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 561
    Height = 62
    Align = alTop
    Caption = #1059#1082#1072#1078#1080#1090#1077' '#1080#1089#1093#1086#1076#1085#1099#1081' '#1092#1072#1081#1083' '#1087#1088#1086#1096#1080#1074#1082#1080' '#1080#1083#1080' '#1087#1072#1087#1082#1091' '#1076#1083#1103' '#1084#1091#1083#1100#1090#1080#1082#1086#1084#1087#1080#1083#1103#1094#1080#1080' '
    TabOrder = 0
    DesignSize = (
      561
      62)
    object btnBrowseFirmware: TButton
      Left = 520
      Top = 17
      Width = 26
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 0
      OnClick = btnBrowseFirmwareClick
    end
    object edtFirmwareFile: TEdit
      Left = 145
      Top = 19
      Width = 369
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edtFirmwareFileChange
    end
    object CheckBox2: TCheckBox
      Left = 22
      Top = 21
      Width = 117
      Height = 17
      Caption = 'MultiCompile/One:'
      TabOrder = 2
      OnClick = CheckBox2Click
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 71
    Width = 561
    Height = 213
    Align = alClient
    Caption = ' '#1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1072#1103' '#1085#1072#1089#1090#1088#1086#1081#1082#1072' MIUI '
    TabOrder = 1
    DesignSize = (
      561
      213)
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 123
      Height = 13
      Caption = #1057#1077#1090#1082#1072' '#1088#1072#1073#1086#1095#1077#1075#1086' '#1089#1090#1086#1083#1072': '
    end
    object Label2: TLabel
      Left = 192
      Top = 24
      Width = 5
      Height = 13
      Caption = 'x'
    end
    object Label3: TLabel
      Left = 53
      Top = 76
      Width = 188
      Height = 13
      Caption = #1042#1077#1073'-'#1089#1090#1088#1072#1085#1080#1094#1072' '#1089' '#1087#1088#1077#1076#1091#1087#1088#1077#1078#1076#1077#1085#1080#1077#1084':'
    end
    object SpinEdit1: TSpinEdit
      Left = 145
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 6
      MinValue = 4
      TabOrder = 0
      Value = 4
    end
    object SpinEdit2: TSpinEdit
      Left = 200
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 6
      MinValue = 4
      TabOrder = 1
      Value = 4
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 49
      Width = 225
      Height = 17
      Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1089#1086#1086#1090#1074#1077#1090#1089#1090#1074#1080#1103' '#1103#1076#1088#1072':    '#1042#1077#1088#1089#1080#1103':'
      TabOrder = 2
    end
    object Edit1: TEdit
      Left = 247
      Top = 47
      Width = 121
      Height = 21
      TabOrder = 3
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 92
      Width = 257
      Height = 17
      Caption = #1042#1082#1083#1102#1095#1080#1090#1100' hw-'#1088#1077#1085#1076#1077#1088#1080#1085#1075' '#1088#1072#1073#1086#1095#1077#1075#1086' '#1089#1090#1086#1083#1072
      TabOrder = 4
    end
    object Button1: TButton
      Left = 170
      Top = 177
      Width = 120
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1087#1072#1090#1095#1077#1081
      TabOrder = 5
      OnClick = Button1Click
    end
    object Edit2: TEdit
      Left = 247
      Top = 72
      Width = 121
      Height = 21
      TabOrder = 6
    end
    object Button2: TButton
      Left = 296
      Top = 177
      Width = 95
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1087#1072#1090#1095#1080
      TabOrder = 7
      OnClick = Button2Click
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.zip'
    Filter = 'Firmware File|*.zip'
    Left = 232
    Top = 24
  end
  object JvBrowseForFolderDialog1: TJvBrowseForFolderDialog
    Left = 264
    Top = 24
  end
end
