unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Spin;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    btnWriteWAV: TButton;
    btnClose: TButton;
    lblDutyCycle: TLabel;
    Panel1: TPanel;
    seDuration: TFloatSpinEdit;
    lblFrequency: TLabel;
    lblFrequencyValue: TLabel;
    lblAmplitude: TLabel;
    lblAmplitudeValue: TLabel;
    lblDuration: TLabel;
    rgChannels: TRadioGroup;
    rgWaveForm: TRadioGroup;
    seDutyCycle: TFloatSpinEdit;
    tbFrequency: TTrackBar;
    tbAmplitude: TTrackBar;
    procedure btnCloseClick(Sender: TObject);
    procedure btnWriteWAVClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgWaveFormClick(Sender: TObject);
    procedure tbAmplitudeChange(Sender: TObject);
    procedure tbFrequencyChange(Sender: TObject);
  private
    procedure EnableControls(Enable: Boolean);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  FREQUENCIES: array[0..8] of Double = (
    200, 300, 400, 600, 1000,
    2000, 3000, 4000, 6000);

type
  TWaveForm = (wfSine, wfTri, wfRect);

  TWavRiffHeader = packed record
    ID: array[0..3] of ansichar;        // "RIFF" for little-endian, "RIFX" for big-endian
    ChunkSize: LongInt;                 // Filesize - 8
    RiffType: array[0..3] of ansiChar;  // "WAVE"
  end;

  TWavFmtSubchunk = packed record
    ID: array[0..3] of ansichar;  // "fmt "
    SubChunkSize: LongInt;   // 16
    AudioFormat: SmallInt;   // PCM = 1 --> uncompressed
    NumChannels: SmallInt;   // Mono = 1, Stereo = 2
    SampleRate: LongInt;     // 8000, 44100 etc.
    ByteRate: LongInt;       // SampleRate * NumChannels * BitsPerSample / 8
    BlockAlign: SmallInt;    // NumChannels * BitsPerSample / 8
    BitsPerSample: SmallInt; // 8, 16
  end;

  TWavDataSubchunk = packed record
    ID: array[0..3] of ansichar;  // "data"
    DataSize: LongInt;            // NumSamples * NumChannels * BitsPerSample / 8
                                  // or: size of following data part.
  end;

procedure WriteWavHeader(AStream: TStream;
  ANumChannels, ASampleRate, ABitsPerSample: Integer);
var
  riff: TWavRiffHeader;
  fmt: TWavFmtSubchunk;
  data: TWavDataSubchunk;
begin
  riff.ID := 'RIFF';
  riff.ChunkSize := 0;   // Write later!!!!
  riff.RiffType := 'WAVE';
  AStream.WriteBuffer(riff, Sizeof(riff));

  fmt.ID := 'fmt ';
  fmt.SubchunkSize := 16;
  fmt.AudioFormat := 1;  // 1 = PCM = uncompressed
  fmt.NumChannels := ANumChannels;
  fmt.SampleRate := ASampleRate;
  fmt.BitsPerSample := ABitsPerSample;
  fmt.ByteRate := ANumchannels * ASampleRate * fmt.BitsPerSample div 8;
  fmt.BlockAlign := ANumChannels * fmt.BitsPerSample div 8;
  AStream.WriteBuffer(fmt, SizeOf(fmt));

  data.ID := 'data';
  AStream.WriteBuffer(data, SizeOf(data));
  // Later: write the size of data data part following here to the field
  // data.DataSize, i.e. 4 bytes below current stream position.
end;


{ Frequency in Hz, Amplitude = 0..1, ADuration in s, ASampleRate in samples/sec
  AChannel = 0 or 3 --> mono, 1 --> left only, 2 --> right only }
procedure WriteWavStream(AStream: TStream; Frequency, Amplitude, ADuration: Double;
  ASampleRate, AChannel: Integer; AWaveForm: TWaveForm; ADutyCycle: Double);
const
  BITS_PER_SAMPLE = 16;
  MAX_WAV = 32760;
  MIN_WAV = -MAX_WAV;
var
  i: Integer;
  n: Integer;  // Number of samples
  p: Int64;
  t, omega: Double;
  phi, phiDC: Double;
  two_pi: Double;
  numch: Integer;

  procedure WriteValue(phi, Ampl: Double);
  var
    y: Double;
    value: SmallInt;
  begin
    case AWaveForm of
      wfSine : y := Ampl*sin(phi);
      wfTri  : if phi <= phiDC then
                 y := 2*Ampl*phi/phiDC - Ampl
               else
                 y := Ampl*(two_pi+phiDC-phi*2)/(two_pi-phiDC);
      wfRect : if phi <= phiDC then
                 y := Ampl
               else
                 y := -Ampl;
    end;
    value := round(y*MAX_WAV);
    if value > MAX_WAV then value := MAX_WAV;
    if value < MIN_WAV then value := MIN_WAV;
    AStream.WriteWord(word(value));
  end;

begin
  if abs(Amplitude) > 1.0 then
    raise Exception.Create('Wav writer: amplitude must be <= 1.0');

  if not (AChannel in [0..3]) then
    raise Exception.Create('Wav writer: 1 or 2 channels supported only.');

  if (ADutyCycle <= 0.0) or (ADutyCycle >= 1.0) then
    raise Exception.Create('Wav writer: Duty cycle must be > 0 and < 1');

  if AChannel in [0, 3] then numch := 1 else numch := 2;

  two_pi := 2.0 * pi;
  WriteWavHeader(AStream, numch, ASampleRate, BITS_PER_SAMPLE);
  p := AStream.Position;  // remember begin of data section

  omega := two_pi * Frequency;
  phiDC := ADutyCycle * two_pi;
  n := round(ADuration * ASampleRate);           // Number of samples

  for i:=0 to n-1 do begin
    t := ADuration * i / n;                      // time in s
    phi := omega*t;                              // phase angle ...
    phi := phi - trunc(phi/two_pi)*two_pi;       // ... in range 0..2pi
    if numch = 1 then
      WriteValue(phi, Amplitude)
    else
      case AChannel of
        1: // left channel only
          begin
            WriteValue(phi, Amplitude);  // L
            WriteValue(phi, 0);          // R
          end;
        2: // right channel only
          begin
            WriteValue(phi, 0);         // L
            WriteValue(phi, Amplitude); // R
          end;
      end;
  end;

  // Complete missing header data
  AStream.Position := p - 4;
  AStream.WriteDWord(n * numch * BITS_PER_SAMPLE div 8); // Size of data part
  AStream.Position := 4;
  AStream.WriteDWord(AStream.Size - 8);  // File size - RIFF chunk size
end;



{ TMainForm }

procedure TMainForm.btnWriteWAVClick(Sender: TObject);
const
  SAMPLE_RATE = 44100;
var
  fs: TFileStream;
  fn: String;
  freq: Double;
  channels: Integer;  // 1 = mono, 2 = stereo, 3= left only, 4 = right only
  amplitude: Double;  // 0..1
  duration: Double;   // seconds
  duty_cycle: Double; // >0 .. <1, for rectangular and triangular waves
begin
  fn := 'test.wav';
  fs := TFileStream.Create(fn, fmCreate);
  Screen.Cursor := crHourglass;
  EnableControls(false);
  try
    Application.ProcessMessages;
    freq := FREQUENCIES[tbFrequency.Position];
    channels := rgChannels.ItemIndex;
    amplitude := tbAmplitude.Position / tbAmplitude.Max;
    duration := seDuration.Value;
    duty_cycle := seDutyCycle.Value;
    WriteWavStream(fs, freq, amplitude, duration, SAMPLE_RATE, channels,
      TWaveForm(rgWaveForm.ItemIndex), duty_cycle);
    ShowMessage('File "' + fn + '" written.');
  finally
    fs.Free;
    Screen.Cursor := crDefault;
    EnableControls(true);
  end;
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EnableControls(Enable: Boolean);
begin
  {
  Panel1.Enabled := Enable;
  rgChannels.Enabled := Enable;
  rgWaveForm.Enabled := Enable;
  }
  btnWriteWAV.Enabled := Enable;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  tbFrequencyChange(nil);
  tbAmplitudeChange(nil);
end;

procedure TMainForm.rgWaveFormClick(Sender: TObject);
begin
  lblDutyCycle.Visible := rgWaveForm.ItemIndex <> 0;
  seDutyCycle.Visible := rgWaveForm.ItemIndex <> 0;
end;

procedure TMainForm.tbAmplitudeChange(Sender: TObject);
begin
  lblAmplitudeValue.Caption := Format('%d%%', [tbAmplitude.Position*10]);
end;

procedure TMainForm.tbFrequencyChange(Sender: TObject);
begin
  lblFrequencyValue.Caption := Format('%.0f Hz', [FREQUENCIES[tbFrequency.Position]]);
end;


end.

