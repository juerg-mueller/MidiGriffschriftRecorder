unit UMidiGriffRecorder;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, UMidiEvent, syncobjs;

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
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    OutputDevIndex: integer;
    OldTime: TTime;
    Ons: array [1..6, 0..127] of boolean;
    CriticalMidiIn: syncobjs.TCriticalSection;
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
  eventCount: integer;
  hasOns: boolean;
  MidiEvents: array of TMidiEvent;

procedure TMidiGriff.SaveMidi;
var
  i, newCount: integer;
  SaveStream: TMidiSaveStream;
  name: string;
  inpush, isEvent: boolean;
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
    isEvent := (MidiEvents[i].Event = 11) and (MidiEvents[i].d1 = $1f);
    if not isEvent or (inpush <> (MidiEvents[i].d2 <> 0)) then
    begin
      if isEvent then
        inpush := (MidiEvents[i].d2 <> 0);
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
    for i := 0 to newCount-1 do
      SaveStream.AppendEvent(MidiEvents[i]);
    SaveStream.AppendTrackEnd(true);
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
    Application.MessageBox(PWideChar(name + ' saved'), '');
    
  finally
    SaveStream.Free;
  end;                
end;

procedure TMidiGriff.btnStartClick(Sender: TObject);
var
  i, j: integer;
begin
  if btnStart.Caption = 'Stop Recording' then
  begin
    btnStart.Caption := 'Start Recording';
    MidiInput.OnMidiData := nil;
    MidiInput.CloseAll;
    if OutputDevIndex > 0 then
    begin
      for j := 1 to 6 do
        for i := 0 to 127 do
          if Ons[j, i] then
            MidiOutput.Send(OutputDevIndex-1, $80 + j, i, $40);
    end;
    MidiOutput.CloseAll;    
    if hasOns then
      SaveMidi;
  end else
  if cbxMidiInput.ItemIndex < 1 then
  begin
    Application.MessageBox('Please, choose a Midi Input', 'Error');
  end else begin
    hasOns := false;
    OutputDevIndex := cbxMidiOut.ItemIndex;
    if cbxMidiOut.ItemIndex > 0 then
      MidiOutput.Open(cbxMidiOut.ItemIndex-1);

    for j := 1 to 6 do
      for i := 0 to 127 do
        Ons[j, i] := false;
    for i := 0 to 6 do
      OnMidiInData(0, $C0 + i, 21, 0, 0);
    MidiInput.OnMidiData := OnMidiInData;
    MidiInput.Open(cbxMidiInput.ItemIndex-1);
    btnStart.Caption := 'Stop Recording';
  end;
end;

procedure TMidiGriff.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := btnStart.Caption = 'Start Recording';
end;

procedure TMidiGriff.FormCreate(Sender: TObject);
begin
  CriticalMidiIn := TCriticalSection.Create;
  Header.Clear;
end;

procedure TMidiGriff.FormDestroy(Sender: TObject);
begin
  CriticalMidiIn.Free;
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
end;

procedure TMidiGriff.RegenerateMidi;
begin
  MidiOutput.GenerateList;
  MidiInput.GenerateList;

  InsertList(cbxMidiOut, MidiOutput.DeviceNames);
  AddLine(cbxMidiOut);
  InsertList(cbxMidiInput, MidiInput.DeviceNames);
  AddLine(cbxMidiInput);
  if cbxMidiInput.Items.Count > 1 then
    cbxMidiInput.ItemIndex := 1;
end;

procedure TMidiGriff.OnMidiInData(aDeviceIndex: integer; aStatus, aData1, aData2: byte; Timestamp: integer);
var
  Event: TMidiEvent;
  time, delta: TDateTime;
begin
  CriticalMidiIn.Acquire;
  try
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
  {$ifdef CONSOLE}
    writeln(aStatus, '  ', aData1, '  ', aData2);
  {$endif}
    if Event.command in [$81..$86, $91..$96] then
      Ons[Event.Channel, Event.d1 and $7f] := Event.Event = 9;
  finally
    CriticalMidiIn.Release;
  end;
end;

initialization
  SetLength(MidiEvents, 1000000);

finalization
  SetLength(MidiEvents, 0);
  
end.
