unit UMidiGriffRecorder;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  UMidi,
{$ifdef mswindows}
  Windows,
{$endif}
  Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, UMidiEvent, syncobjs,
  ExtCtrls;

type
  TMidiGriff = class(TForm)
    GroupBox1: TGroupBox;
    btnStart: TButton;
    cbxMidiInput: TComboBox;
    cbxMidiOut: TComboBox;
    Label17: TLabel;
    lblKeyboard: TLabel;
    SaveDialog1: TSaveDialog;
    gbHeader: TGroupBox;
    Label8: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    lbBegleitung: TLabel;
    Label10: TLabel;
    cbxViertel: TComboBox;
    cbxTakt: TComboBox;
    edtBPM: TEdit;
    cbxMetronom: TCheckBox;
    sbMetronom: TScrollBar;
    cbxNurTakt: TCheckBox;
    Timer1: TTimer;
    procedure btnStartClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbxMidiOutChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbxMidiInputChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxTaktChange(Sender: TObject);
    procedure cbxViertelChange(Sender: TObject);
    procedure edtBPMExit(Sender: TObject);
    procedure cbxMetronomClick(Sender: TObject);
    procedure cbxNurTaktClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure sbMetronomChange(Sender: TObject);
  private
    OutputDevIndex: integer;
    InRecord: boolean;
    procedure SaveMidi;
  public
    Header: TDetailHeader;
    Metronom: TMetronom;

    procedure OnMidiInData(InDeviceIndex: integer; Status, Data1, Data2: byte; Timestamp: Int64);
  end;

var
  MidiGriff: TMidiGriff;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
{$if defined(mswindows) and not defined(Win64)}
  Midi,
{$else}
  URtMidi,
{$endif}
  UMidiSaveStream, UMidiDataStream, UMidiDataIn;


var
  MidiEventRecorder: TMidiEventRecorder;

procedure TMidiGriff.SaveMidi;
var
  name: string;
  SaveStream: TMidiDataStream;
  Simple: TSimpleDataStream;
  i: integer;
begin
  SaveStream := MidiEventRecorder.MakeRecordStream;

  if SaveStream <> nil then
  begin
    name := 'griff_recorder';
    if FileExists(name+'.mid') then
    begin
      name := name + '_';
      i := 1;
      while FileExists(name + IntToStr(i) + '.mid', false) do
        inc(i);

      name := name + IntToStr(i);
    end;
    SaveStream.SaveToFile(name+'.mid');
    Simple := TSimpleDataStream.Create;
    if Simple.MakeSimpleFile(SaveStream) then
    begin
      Simple.SaveToFile(name + '.txt');
    end;
    Application.MessageBox(PChar(name + ' saved'), '');
    SaveStream.Free;
  end;                
end;

procedure TMidiGriff.sbMetronomChange(Sender: TObject);
var
  s: string;
  p: double;
begin
  with Sender as TScrollBar do
  begin
    s := Format('Lautstärke  (%d %%)', [Position]);
    p := Position / 100.0;
  end;
  if Sender = sbMetronom then begin
    lbBegleitung.Caption := s;
    VolumeMetronom := p;
  end;
end;

procedure TMidiGriff.Timer1Timer(Sender: TObject);
begin
  if Metronom.DoPip(Header) then
  begin
    if InRecord and Metronom.IsFirst then // Taktbeginn
      MidiEventRecorder.Append(Metronom.MidiEvent);
    SendMidiEvent(Metronom.MidiEvent);
  end;
end;

procedure TMidiGriff.btnStartClick(Sender: TObject);
var
  i, j: integer;
begin
  if btnStart.Caption = 'Aufnahme stoppen' then
  begin
    InRecord := false;
    btnStart.Caption := 'Aufnahme starten';
    if OutputDevIndex > 0 then
    begin
      for j := 1 to 7 do
        MidiOutput.Send(OutputDevIndex-1, $B0 + j, 120, 0);
    end;
    MidiEventRecorder.Stop;
    SaveMidi;
  end else
  if cbxMidiInput.ItemIndex < 1 then
  begin
    Application.MessageBox('Bitte, MIDI Input wählen', 'Fehler');
  end else begin
    MidiEventRecorder.Start;
    InRecord := true;
    btnStart.Caption := 'Aufnahme stoppen';
  end;
end;

procedure TMidiGriff.cbxMetronomClick(Sender: TObject);
begin
  Metronom.SetOn(cbxMetronom.Checked);
end;

procedure TMidiGriff.cbxMidiInputChange(Sender: TObject);
begin
  MidiInput.CloseAll;
  MidiInput.OnMidiData := nil;
  if cbxMidiInput.ItemIndex > 0 then
  begin
    MidiInput.Open(cbxMidiInput.ItemIndex-1);
    MidiInput.OnMidiData := OnMidiInData;
  end;
end;

procedure TMidiGriff.cbxMidiOutChange(Sender: TObject);
var
  i: integer;
begin
  if OutputDevIndex > 0 then
    MidiOutput.Close(OutputDevIndex-1);
  OutputDevIndex := cbxMidiOut.ItemIndex;
  if OutputDevIndex > 0 then
  begin
    MidiOutput.Open(OutputDevIndex-1);
    for i := 0 to 7 do
    begin
      OnMidiInData(0, $b0 + i,  0, 0, 0);  // 0x32, LSB Bank);
      OnMidiInData(0, $C0 + i, 21, 0, 0);
    end;
  end
end;

procedure TMidiGriff.cbxNurTaktClick(Sender: TObject);
begin
  NurTakt := cbxNurTakt.Checked;
end;

procedure TMidiGriff.cbxTaktChange(Sender: TObject);
begin
  Header.measureFact := cbxTakt.ItemIndex + 2;
  MidiEventRecorder.Header := Header;
end;

procedure TMidiGriff.cbxViertelChange(Sender: TObject);
var
  q: integer;
begin
  case cbxViertel.ItemIndex of
    0: q := 4;
    1: q := 8;
    else q := 4;
  end;
  Header.MeasureDiv :=  q;
  MidiEventRecorder.Header := Header;
end;

procedure TMidiGriff.edtBPMExit(Sender: TObject);
begin
  Header.QuarterPerMin := StrToInt(edtBPM.Text);
  cbxViertelChange(Sender);
end;

procedure TMidiGriff.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MidiInput.CloseAll;
  MidiEventRecorder.Free;
end;

procedure TMidiGriff.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not InRecord;
end;

procedure InsertList(Combo: TComboBox; arr: array of string);
var
  i: integer;
begin
  for i := 0 to Length(arr)-1 do
    Combo.AddItem(arr[i], nil);
end;

procedure AddLine(cbx: TComboBox);
begin
  cbx.Items.Insert(0, '');
  cbx.ItemIndex := 0;
end;

procedure TMidiGriff.FormCreate(Sender: TObject);
begin
  InRecord := false;
  MidiEventRecorder := TMidiEventRecorder.Create;
  MidiOutput.GenerateList;
  MidiInput.GenerateList;
  Header.Clear;
  MidiEventRecorder.Header := Header;
  InsertList(cbxMidiOut, MidiOutput.DeviceNames);
  AddLine(cbxMidiOut);
  cbxMidiOut.ItemIndex := 0;
  InsertList(cbxMidiInput, MidiInput.DeviceNames);
  AddLine(cbxMidiInput);
  cbxMidiInput.ItemIndex := 0;
  OutputDevIndex := 0;

  Header.Clear;
end;

procedure TMidiGriff.OnMidiInData(InDeviceIndex: integer; Status, Data1, Data2: byte; Timestamp: Int64);
var
  Event: TMidiEvent;
begin
  Event.SetEvent(Status, Data1, Data2);
  if InRecord then
    MidiEventRecorder.Append(Event);
{$ifdef CONSOLE}
//  writeln(Status, '  ', Data1, '  ', Data2);
{$endif}
  if OutputDevIndex > 0 then
    MidiOutput.Send(OutputDevIndex-1, Status, Data1, Data2);
end;


end.
