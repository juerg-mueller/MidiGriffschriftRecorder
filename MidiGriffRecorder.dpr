program MidiGriffRecorder;

uses
  Vcl.Forms,
  Midi in 'Midi.pas',
  teVirtualMIDIdll in 'teVirtual\teVirtualMIDIdll.pas',
  UMidiGriffRecorder in 'UMidiGriffRecorder.pas' {MidiGriff},
  UVirtual in 'UVirtual.pas',
  UMidiSaveStream in 'UMidiSaveStream.pas',
  UMidiEvent in 'UMidiEvent.pas',
  UMyMemoryStream in 'UMyMemoryStream.pas',
  UMyMidiStream in 'UMyMidiStream.pas',
  UMidiDataStream in 'UMidiDataStream.pas',
  UEventArray in 'UEventArray.pas';


{$ifdef DEBUG}
  {$APPTYPE CONSOLE}
{$endif}

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMidiGriff, MidiGriff);
  Application.CreateForm(TMidiGriff, MidiGriff);
  Application.Run;
end.
