unit UMidiGriffRecorder;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, UMidiEvent;

type
  TMidiGriff = class(TForm)
    btnStart: TButton;
    cbxMidiInput: TComboBox;
    cbxMidiOut: TComboBox;
    lblKeyboard: TLabel;
    Label17: TLabel;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    OutputDevIndex: integer;
    OldTime: TTime;
    procedure RegenerateMidi;
    procedure SaveMidi;  
  public
    Header: TDetailHeader; 

    procedure OnMidiInData(aDeviceIndex: integer; aStatus, aData1, aData2: byte; Timestamp: integer);
  end;

var
  MidiGriff: TMidiGriff;

implementation

{$R *.dfm}

uses
  Midi, UMidiSaveStream;

var
  eventCount: cardinal;
  hasOns: boolean;
  MidiEvents: array of TMidiEvent;

procedure TMidiGriff.SaveMidi;
var
  i, newCount: integer;
  Event: TMidiEvent;
  SaveStream: TMidiSaveStream;
  name: string;
  inpush: boolean;
begin 
  // stream bereinigen
  i := 0;
  while (i < eventCount-2) and
        (MidiEvents[i].Event = 11) and (MidiEvents[i+1].Event = 11) do
    inc(i);
  inpush := true;
  if (MidiEvents[i].Event = 11) and (MidiEvents[i].d1 = $1f) then
  begin
    inpush := MidiEvents[i].d2 <> 0;
    MidiEvents[i].var_len := 0;
  end;
  inc(i);

  while (i < eventCount) and (MidiEvents[eventCount-1].Event = 11) do
    dec(eventCount);    

  newCount := 1;
  while i < eventCount do
  begin
    if (MidiEvents[i].Event = 11) and (MidiEvents[i].d1 = $1f) and
       (inpush = (MidiEvents[i].d2 <> 0)) then
    begin
    end else begin
      MidiEvents[newCount] := MidiEvents[i];
      inc(newCount);
    end;
    inc(i);
  end;
  
  SaveStream := TMidiSaveStream.Create;
  try
    SaveStream.SetSize(6*newCount + 10000);
    SaveStream.Title := 'juerg5524.ch';
    SaveStream.SetHead;
    SaveStream.AppendTrackHead(0);
    SaveStream.AppendHeaderMetaEvents(Header);
    SaveStream.AppendTrackEnd(false);
    SaveStream.AppendTrackHead(0);
    for i := 0 to 6 do
    begin 
       SaveStream.WriteByte($C0 + i);
       SaveStream.WriteByte(21);
       SaveStream.WriteByte(0);
    end;
    for i := 0 to newCount-1 do
      SaveStream.AppendEvent(MidiEvents[i]);
    SaveStream.AppendTrackEnd(true);
    name := 'griff_recorder';
    if FileExists(name+'.mid') then
    begin  
      name := name + '_';
      i := 1;
      while FileExists(name + IntToStr(i) + '.mid') do
        inc(i);
      
      name := name + IntToStr(i) + '.mid';
    end;
    SaveStream.SaveToFile(name);      
    Application.MessageBox(PWideChar(name + ' saved'), '');
    
  finally
    SaveStream.Free;
  end;                
end;

procedure TMidiGriff.btnStartClick(Sender: TObject);
var
  i: integer;
begin
  if btnStart.Caption = 'Stop Recording' then
  begin
    btnStart.Caption := 'Start Recording';
    MidiInput.OnMidiData := nil;
    MidiInput.CloseAll;
    MidiOutput.CloseAll;    
    if hasOns then
      SaveMidi;
  end else
  if cbxMidiInput.ItemIndex < 1 then
  begin
    Application.MessageBox('Please, choose a Midi Input', 'Error');
  end else begin
    hasOns := false;
    if cbxMidiOut.ItemIndex > 0 then
      MidiOutput.Open(cbxMidiOut.ItemIndex-1);

    MidiInput.OnMidiData := OnMidiInData;
    OutputDevIndex := cbxMidiInput.ItemIndex;
    MidiInput.Open(OutputDevIndex-1);
    btnStart.Caption := 'Stop Recording';
  end;
end;

procedure TMidiGriff.FormCreate(Sender: TObject);
begin
  Header.Clear;
end;

procedure TMidiGriff.FormShow(Sender: TObject);
begin
  RegenerateMidi;
end;

procedure InsertList(Combo: TComboBox; arr: TStringList);
var
  i: integer;
begin
  for i := 0 to arr.Count-1 do
    Combo.AddItem(arr[i], nil);
end;

procedure AddLine(cbx: TComboBox);
begin
  cbx.Items.Insert(0, '');
  cbx.ItemIndex := 0;
  if cbx.Items.Count > 1 then
    cbx.ItemIndex := 1;
end;

procedure TMidiGriff.RegenerateMidi;
begin
  MidiOutput.GenerateList;
  MidiInput.GenerateList;

  InsertList(cbxMidiOut, MidiOutput.DeviceNames);
  AddLine(cbxMidiOut);
  InsertList(cbxMidiInput, MidiInput.DeviceNames);
  AddLine(cbxMidiInput);
end;

procedure TMidiGriff.OnMidiInData(aDeviceIndex: integer; aStatus, aData1, aData2: byte; Timestamp: integer);
var
  Event: TMidiEvent;
  time, delta: TDateTime;
begin
  if eventCount >= Length(MidiEvents)-1 then
    SetLength(MidiEvents, 2*Length(MidiEvents));

  time := Now;
  if eventCount = 0 then
    OldTime := time;
  Event.Clear;
  Event.command := aStatus;
  if Event.Event = 9 then
    hasOns := true;
  Event.d1 := aData1;
  Event.d2 := aData2;
  MidiEvents[eventCount] := Event;
  delta := 24*3600000.0*(time - OldTime); // ms
  OldTime := time;
  if eventCount > 0 then
    MidiEvents[eventCount-1].var_len := Header.MsDelayToTicks(round(delta));
  inc(eventCount);
  if OutputDevIndex > 0 then
    MidiOutput.Send(OutputDevIndex-1, aStatus, aData1, aData2);
end;

initialization
  SetLength(MidiEvents, 1000000);

finalization
  SetLength(MidiEvents, 0);
  
end.
