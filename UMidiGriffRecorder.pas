unit UMidiGriffRecorder;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, UMidiEvent, syncobjs,
  Vcl.ExtCtrls;

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
    VolumeDiscant: double;
    VolumeMetronom: double;
    Header: TDetailHeader;
    Metronom: boolean;
    NurTakt: boolean;
    pipCount: integer;
    nextPip: TTime;
    pipDelay: TTime;


    procedure OnMidiInData(InDeviceIndex: integer; Status, Data1, Data2: byte; Timestamp: integer);
  end;

var
  MidiGriff: TMidiGriff;

implementation

{$R *.dfm}

uses
  Midi, UMidiSaveStream, UMidiDataStream;


var
  MidiRec: TMidiRecord;

  pipFirst: byte = 59;
  pipSecond: byte = 69;
  pipChannel: byte = 9;

  VolumeDiscant: double = 0.9;
  VolumeMetronom: double = 0.8;


procedure TMidiGriff.SaveMidi;
var
  name: string;
  SaveStream: UMidiSaveStream.TMidiSaveStream;
  Simple: TSimpleDataStream;
  i: integer;
begin
  SaveStream := UMidiSaveStream.TMidiSaveStream.BuildSaveStream(MidiRec);
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
    Application.MessageBox(PWideChar(name + ' saved'), '');
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
var
  Event: TMouseEvent;
  Key: word;
  BPM, mDiv: integer;
  sec: boolean;
  pip: byte;
  vol: double;

  procedure SendMidiOut(Status, Data1, Data2: byte);
  begin
    Status := Status + pipChannel;
    OnMidiInData(MicrosoftIndex, Status, Data1, Data2, 0);
    MidiOutput.Send(MicrosoftIndex, Status, Data1, Data2);
  end;

begin
  if Metronom then
  begin
    BPM := Header.beatsPerMin;
    mDiv := Header.measureDiv; // ist 4 oder 8
    if NurTakt then
      sec := false
    else
    if mDiv = 8 then
    begin
      BPM := 2*BPM;
      sec := (Header.measureFact = 6) and (pipCount = 3);
    end else
      sec := true;
    pip := 0;
    if pipCount = 0 then
      pip := pipFirst
    else
    if sec then
      pip := pipSecond;
    if Now >= nextPip then
    begin
      vol := 100*VolumeMetronom;
      if vol > 126 then
        vol := 126;
      if pip > 0 then
      begin
        SendMidiOut($90, pip, trunc(vol));
        if pipCount = 0 then
        begin
       //   pipPaint(true);
        end;
      end;
      nextPip := Now + 1/(24.0*60.0)/BPM;
      pipDelay := Now + 0.1/(24*3600);
    end else
    if (Now >= pipDelay) and (pipDelay > 0) then
    begin
      pipDelay := 0;
      if pip > 0 then
      begin
        if pip > 0 then
          SendMidiOut($80, pip, 64);
        if pipCount = 0 then
        begin
          //pipPaint(false);
        end;
      end;
      inc(pipCount);
      if pipCount >= Header.measureFact then
        pipCount := 0;
    end;
  end;
end;

procedure TMidiGriff.btnStartClick(Sender: TObject);
var
  i, j: integer;
begin
  if btnStart.Caption = 'Stop Recording' then
  begin
    InRecord := false;
    btnStart.Caption := 'Start Recording';
    if OutputDevIndex > 0 then
    begin
      for j := 1 to 7 do
        MidiOutput.Send(OutputDevIndex-1, $B0 + j, 120, 0);
    end;
    MidiOutput.CloseAll;    
    SaveMidi;
    FreeAndNil(MidiRec);
  end else
  if cbxMidiInput.ItemIndex < 1 then
  begin
    Application.MessageBox('Please, choose a Midi Input', 'Error');
  end else begin
    MidiRec := TMidiRecord.Create(Header);
    InRecord := true;
    for i := 0 to 7 do
      OnMidiInData(0, $C0 + i, 21, 0, 0);
    btnStart.Caption := 'Stop Recording';
  end;
end;

procedure TMidiGriff.cbxMetronomClick(Sender: TObject);
begin
  Metronom := cbxMetronom.Checked;
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
end;

procedure TMidiGriff.edtBPMExit(Sender: TObject);
begin
  Header.beatsPerMin := StrToInt(edtBPM.Text);
  cbxViertelChange(Sender);
end;

procedure TMidiGriff.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MidiInput.CloseAll;
end;

procedure TMidiGriff.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := btnStart.Caption = 'Start Recording';
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

procedure TMidiGriff.FormCreate(Sender: TObject);
begin
  MidiOutput.GenerateList;
  MidiInput.GenerateList;
  InRecord := false;

  InsertList(cbxMidiOut, MidiOutput.DeviceNames);
  AddLine(cbxMidiOut);
  cbxMidiOut.ItemIndex := 0;
  InsertList(cbxMidiInput, MidiInput.DeviceNames);
  AddLine(cbxMidiInput);
  cbxMidiInput.ItemIndex := 0;
  OutputDevIndex := 0;

  Header.Clear;
end;

procedure TMidiGriff.OnMidiInData(InDeviceIndex: integer; Status, Data1, Data2: byte; Timestamp: integer);
begin
  if InRecord then
    MidiRec.OnMidiInData(Status, Data1, Data2);
{$ifdef CONSOLE}
  writeln(Status, '  ', Data1, '  ', Data2);
{$endif}
  if OutputDevIndex > 0 then
    MidiOutput.Send(OutputDevIndex-1, Status, Data1, Data2);
end;


end.
