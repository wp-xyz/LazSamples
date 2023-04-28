program colorreduction;

uses
  crt,
  SysUtils,
  fpimage, fpimgcanv, fpquantizer, fpditherer,
  fpreadbmp, fpwritebmp,
  fpreadpng, fpwritepng,
  fpreadjpeg, fpwritejpeg;

procedure WriteSyntax;
begin
  WriteLn('Syntax: color_reduction [-bpp=1|4|8] -if=<filename> [-of=<filename>]');
  WriteLn('  -bpp .. bits per pixel in the output file (bw, ega, 1, 4, or 8 only, default 8).');
  WriteLn('  -q=mc|ot .. quantizer: mc=MedianCut, ot=Octree');
  WriteLn('  -if ... input file, image file to be converted. bmp, png, jpg allowed');
  WriteLn('  -of ... output file, name of the result file. Optional.');
  WriteLn('  -h  ... this help');
end;

procedure Error(AMsg: String);
begin
  WriteLn(AMsg);
  WriteSyntax;
  Halt;
end;

function abs(x: Integer): Integer;
begin
  if x < 0 then Result := -x else Result := x;
end;

type
  TProgress = object
    procedure QuantizerProgress(Sender: TObject; Stage: TFPImgProgressStage;
      PercentDone: Byte; const Msg: AnsiString; var AContinue: Boolean);
    procedure DitherProgress(Sender: TObject; Stage: TFPImgProgressStage;
      PercentDone: Byte; const Msg: AnsiString; var AContinue: Boolean);
  end;

var
  progress: TProgress;
  progressX, progressY: DWord;

procedure TProgress.QuantizerProgress(Sender: TObject; Stage: TFPImgProgressStage;
  PercentDone: Byte; const Msg: AnsiString; var AContinue: Boolean);
begin
  case Stage of
    psStarting:
      begin
        Write('Quantization ');
        progressX := WhereX32;
        progressY := WhereY32;
      end;
    psRunning:
      if PercentDone mod 10 = 0 then
      begin
        GotoXY(progressX, progressY);
        Write(PercentDone, '%');
      end;
    psEnding:
      begin
        GotoXY(progressX, progressY);
        WriteLn('done.');
      end;
  end;
end;

procedure TProgress.DitherProgress(Sender: TObject; Stage: TFPImgProgressStage;
  PercentDone: Byte; const Msg: AnsiString; var AContinue: Boolean);
begin
  case Stage of
    psStarting:
      begin
        Write('Dithering ');
        progressX := WhereX32;
        progressY := WhereY32;
      end;
    psRunning:
      if PercentDone mod 10 = 0 then
      begin
        GotoXY(progressX, progressY);
        Write(PercentDone, '%');
      end;
    psEnding:
      begin
        GotoXY(progressX, progressY);
        WriteLn('done.');
      end;
  end;
end;

procedure ReduceColors(SourceFile, TargetFile:String; BitsPerPixel: Integer;
  QuantizerKind: String);
var
  ext: string;
  sourceImg, targetImg: TFPMemoryImage;
  reader: TFPCustomImageReader;
  writer: TFPCustomImageWriter;
  quantizer: TFPColorQuantizer = nil;
  palette: TFPPalette = nil;
  ditherer: TFPFloydSteinbergDitherer = nil;
begin
  ext := lowercase(ExtractFileExt(TargetFile));
  case ext of
    '.bmp':
        begin
          writer := TFPWriterBMP.Create;
          TFPWriterBMP(writer).BitsPerPixel:= abs(BitsPerPixel);
        end;
    '.png':
        begin
          writer := TFPWriterPNG.Create;
          TFPWriterPNG(writer).Indexed := true;
        end;
    '.jpg', '.jpeg':
        Error('Image with color palette cannot be written to JPEG.');
  end;

  ext := lowercase(ExtractFileExt(SourceFile));
  case ext of
    '.bmp':
        reader := TFPReaderBMP.Create;
    '.png':
        reader := TFPReaderPNG.Create;
    '.jpg', '.jpeg':
        reader := TFPReaderJPeg.Create;
  end;

  sourceImg := TFPMemoryImage.Create(0,0);
  targetImg := TFPMemoryImage.Create(0,0);
  try
    sourceImg.LoadFromFile(SourceFile);
    if BitsPerPixel > 0 then
    begin
      case QuantizerKind of
        'mc': quantizer  := TFPMedianCutQuantizer.Create;
        'ot': quantizer := TFPOctreeQuantizer.Create;
      end;
      quantizer.ColorNumber := 1 shl BitsPerPixel;
      quantizer.OnProgress := @progress.QuantizerProgress;
      quantizer.Add(sourceImg);
      palette := quantizer.Quantize;
    end else
    begin
      palette := TFPPalette.Create(1 shl (-BitsPerPixel));
      palette.Add(colBlack);
      if BitsPerPixel = -4 then
      begin
        palette.Add(colDkBlue);
        palette.Add(colDkGreen);
        palette.Add(colDkCyan);
        palette.Add(colDkRed);
        palette.Add(colDkMagenta);
        palette.Add(colMaroon);
        palette.Add(colLtGray);
        palette.Add(colDkGray);
        palette.Add(colBlue);
        palette.Add(colGreen);
        palette.Add(colCyan);
        palette.Add(colRed);
        palette.Add(colMagenta);
        palette.Add(colYellow);
      end;
      palette.Add(colWhite);
    end;
    ditherer := TFPFloydSteinbergDitherer.Create(palette);
    ditherer.OnProgress := @progress.DitherProgress;
    ditherer.Dither(sourceImg, targetImg);
    targetImg.SaveToFile(TargetFile, writer);
    WriteLn('Color-reduced image saved as "', TargetFile, '"');
  finally
    ditherer.Free;
    palette.Free;
    quantizer.Free;
    writer.Free;
    reader.Free;
    targetImg.Free;
    sourceImg.Free;
  end;
end;

procedure ParseCmdLine(out InputFile, OutputFile: String;
  out BitsPerPixel: Integer; out Quantizer: String);
var
  i: Integer;
  p: Integer;
  param, value: String;
  s: String;
begin
  InputFile := '';
  OutputFile := '';
  BitsPerPixel := 8;
  Quantizer := 'mc';
  for i := 1 to ParamCount do
  begin
    if ParamStr(i)[1] <> '-' then
      Error('Parameter syntax error.');
    p := pos('=', ParamStr(i));
    if p > 0 then
    begin
      param := trim(Copy(ParamStr(i), 1, p-1));
      value := trim(Copy(ParamStr(i), p+1));
    end else
    begin
      param := ParamStr(i);
      value := '';
    end;

    case lowercase(param) of
      '-if':
        InputFile := value;
      '-of':
        OutputFile := value;
      '-bpp':
        case lowercase(value) of
          'bw' : BitsPerPixel := -1;
          'ega': BitsPerPixel := -4;
          else   if TryStrToInt(value, BitsPerPixel) then
                 begin
                   if not (BitsPerPixel in [1, 4, 8]) then
                     Error('Illegal parameter value.');
                 end else
                   Error('Parameter value error.');
        end;
      '-q':
        begin
          value := LowerCase(value);
          if not ((value = 'mc') or (value = 'ot')) then
            Error('Illegal quantizer specification.');
          end;
      '-h':
        begin
          WriteSyntax;
          halt;
        end;
    end;
  end;
  if InputFile = '' then
    Error('Input file name not specified.');
  if not FileExists(InputFile) then
    Error('Input file "' + InputFile + '" not found.');
  if OutputFile = '' then
  begin
    case BitsPerPixel of
      -1: s := 'bw';
      -4: s := 'ega';
      else s := IntToStr(BitsPerPixel) + 'bpp';
    end;
    OutputFile := Format('%s_%s%s', [ChangeFileExt(InputFile, ''), s, ExtractFileExt(InputFile)]);
  end;
end;

var
  InputFile, OutputFile: String;
  bpp: Integer;
  quantizer: string;
begin
  if ParamCount = 0 then
  begin
    WriteSyntax;
    halt;
  end;
  ParseCmdLine(inputFile, outputFile, bpp, quantizer);
  ReduceColors(inputFile, outputFile, bpp, quantizer);
end.
