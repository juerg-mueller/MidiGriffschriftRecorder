program MidiGriffRecorder;

//
// funktioniert für Win32
//
// Midi.pas verwendet die Winmm.dll. Sie ist eine 32-Bit Library, für 64-Bit gibt es keinen Ersatz.
//

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$ifdef unix}
  cthreads,
  pthreads,
{$endif}
  Interfaces,
  Forms,
  LCLIntf, LCLType, LMessages,
  UMidi,
{$if defined(mswindows) and not defined(Win64)}
  Midi in 'Midi.pas',
{$else}
  rtmidi,
  urtmidi,
{$endif}
//  teVirtualMIDIdll in 'teVirtual\teVirtualMIDIdll.pas',
  UMidiGriffRecorder in 'UMidiGriffRecorder.pas' {MidiGriff},
  UMidiEvent in 'UMidiEvent.pas',
  UMyMemoryStream in 'UMyMemoryStream.pas',
  UMyMidiStream in 'UMyMidiStream.pas',
  UMidiDataStream in 'UMidiDataStream.pas',
  UEventArray in 'UEventArray.pas', UMidiDataIn, CriticalSection;


{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMidiGriff, MidiGriff);
  Application.CreateForm(TMidiGriff, MidiGriff);
  Application.Run;
end.
