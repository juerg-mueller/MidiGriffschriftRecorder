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
    procedure btnStartClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbxMidiOutChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbxMidiInputChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    OutputDevIndex: integer;
    InRecord: boolean;
    procedure SaveMidi;
  public

    procedure OnMidiInData(InDeviceIndex: integer; Status, Data1, Data2: byte; Timestamp: integer);
  end;

var
  MidiGriff: TMidiGriff;

implementation

{$R *.dfm}

uses
  Midi, UMidiSaveStream;


var
  MidiRec: TMidiRecord;

procedure TMidiGriff.SaveMidi;
var
  name: string;
  SaveStream: TMidiSaveStream;
  i: integer;
begin
  SaveStream := TMidiSaveStream.BuildSaveStream(MidiRec);
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
    Application.MessageBox(PWideChar(name + ' saved'), '');
    SaveStream.Free;
  end;                
end;

procedure TMidiGriff.btnStartClick(Sender: TObject);
var
  i, j: integer;
  Header: TDetailHeader;
begin
  if btnStart.Caption = 'Stop Recording' then
  begin
    InRecord := false;
    btnStart.Caption := 'Start Recording';
    if OutputDevIndex > 0 then
    begin
      for j := 1 to 6 do
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
    Header.Clear;
    MidiRec := TMidiRecord.Create(Header);
    InRecord := true;
    for i := 0 to 6 do
      OnMidiInData(0, $C0 + i, 21, 0, 0);
    btnStart.Caption := 'Stop Recording';
  end;
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
  OutputDevIndex := cbxMidiOut.ItemIndex;
  if OutputDevIndex > 0 then
  begin
    MidiOutput.Open(OutputDevIndex-1);
    for i := 0 to 6 do
      OnMidiInData(0, $C0 + i, 21, 0, 0);
  end;
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
