object MidiGriff: TMidiGriff
  Left = 0
  Top = 0
  Caption = 'MIDI Recorder'
  ClientHeight = 284
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 553
    Height = 116
    Align = alClient
    Caption = 'MIDI'
    TabOrder = 0
    ExplicitTop = 73
    ExplicitWidth = 549
    ExplicitHeight = 141
    DesignSize = (
      553
      116)
    object Label17: TLabel
      Left = 35
      Top = 70
      Width = 83
      Height = 13
      Caption = 'Synthesizer (out)'
    end
    object lblKeyboard: TLabel
      Left = 35
      Top = 27
      Width = 69
      Height = 13
      Caption = 'Harmonica (in)'
    end
    object btnStart: TButton
      Left = 431
      Top = 63
      Width = 98
      Height = 25
      Caption = 'Start Recording'
      TabOrder = 0
      OnClick = btnStartClick
    end
    object cbxMidiInput: TComboBox
      Left = 135
      Top = 24
      Width = 269
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = cbxMidiInputChange
      ExplicitWidth = 265
    end
    object cbxMidiOut: TComboBox
      Left = 135
      Top = 67
      Width = 269
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = cbxMidiOutChange
      ExplicitWidth = 265
    end
  end
  object gbHeader: TGroupBox
    Left = 0
    Top = 116
    Width = 553
    Height = 168
    Align = alBottom
    Caption = 'Taktangaben'
    TabOrder = 1
    ExplicitTop = 139
    DesignSize = (
      553
      168)
    object Label8: TLabel
      Left = 24
      Top = 32
      Width = 21
      Height = 13
      Caption = 'Takt'
    end
    object Label12: TLabel
      Left = 24
      Top = 59
      Width = 84
      Height = 13
      Caption = 'Viertel pro Minute'
    end
    object Label2: TLabel
      Left = 24
      Top = 115
      Width = 48
      Height = 13
      Caption = 'Metronom'
    end
    object lbBegleitung: TLabel
      Left = 24
      Top = 95
      Width = 51
      Height = 13
      Caption = 'Lautst'#228'rke'
    end
    object Label10: TLabel
      Left = 24
      Top = 135
      Width = 41
      Height = 13
      Caption = 'Nur Takt'
    end
    object cbxViertel: TComboBox
      Left = 198
      Top = 29
      Width = 70
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Viertel'
      OnChange = cbxViertelChange
      Items.Strings = (
        'Viertel'
        'Achtel')
    end
    object cbxTakt: TComboBox
      Left = 122
      Top = 29
      Width = 70
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 0
      Text = '4'
      OnChange = cbxTaktChange
      Items.Strings = (
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8'
        '9'
        '10'
        '11'
        '12')
    end
    object edtBPM: TEdit
      Left = 122
      Top = 56
      Width = 70
      Height = 21
      Alignment = taRightJustify
      TabOrder = 2
      Text = '120'
      OnExit = edtBPMExit
    end
    object cbxMetronom: TCheckBox
      Left = 122
      Top = 114
      Width = 25
      Height = 17
      TabOrder = 4
      OnClick = cbxMetronomClick
    end
    object sbMetronom: TScrollBar
      Left = 122
      Top = 91
      Width = 282
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Max = 120
      Min = 20
      PageSize = 0
      Position = 80
      TabOrder = 3
      OnChange = sbMetronomChange
      ExplicitWidth = 278
    end
    object cbxNurTakt: TCheckBox
      Left = 122
      Top = 134
      Width = 25
      Height = 17
      TabOrder = 5
      OnClick = cbxNurTaktClick
    end
  end
  object SaveDialog1: TSaveDialog
    FileName = 'Recorded.mid'
    Filter = 'MIDI|*.mid'
    InitialDir = '.'
    Left = 464
    Top = 16
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 424
    Top = 176
  end
end
