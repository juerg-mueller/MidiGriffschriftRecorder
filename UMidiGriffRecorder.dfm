object MidiGriff: TMidiGriff
  Left = 0
  Top = 0
  Caption = 
    'MIDI Griffschrift Recorder (Steirische Harmonika / Schwyzer'#246'rgel' +
    'i)'
  ClientHeight = 108
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    545
    108)
  PixelsPerInch = 96
  TextHeight = 13
  object lblKeyboard: TLabel
    Left = 35
    Top = 27
    Width = 69
    Height = 13
    Caption = 'Harmonica (in)'
  end
  object Label17: TLabel
    Left = 35
    Top = 70
    Width = 83
    Height = 13
    Caption = 'Synthesizer (out)'
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
    Width = 261
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object cbxMidiOut: TComboBox
    Left = 135
    Top = 67
    Width = 261
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object SaveDialog1: TSaveDialog
    FileName = 'Recorded.mid'
    Filter = 'MIDI|*.mid'
    InitialDir = '.'
    Left = 464
    Top = 16
  end
end
