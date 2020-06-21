program project1;

uses
  fpimage, fpreadjpeg, fpwritejpeg, math;

procedure GammaCorrection(const ASrcFile, ADestFile: String; AGamma: Double);
var
  img: TFPMemoryImage;
  reader: TFPCustomImageReader;
  writer: TFPCustomImageWriter;
  i, j: Integer;
  clr: TFPColor;
begin
  Assert(AGamma <> 0.0);

  img := TFPMemoryImage.Create(0, 0);
  try
    reader := TFPReaderJpeg.Create;
    try
      img.LoadFromFile(ASrcFile, reader);
    finally
      reader.Free;
    end;

    for j := 0 to img.Height - 1 do
      for i := 0 to img.Width - 1 do
      begin
        clr := img.Colors[i, j];
        clr.Red := round(((clr.Red / 65535)**AGamma)*65535);
        clr.Green := round(((clr.Green / 65535)**AGamma)*65535);
        clr.Blue := round(((clr.Blue / 65535)**AGamma)*65535);
        img.Colors[i, j] := clr;
      end;

    writer := TFPWriterJpeg.Create;
    try
      img.SaveToFile(ADestFile, writer);
    finally
      writer.Free;
    end;
  finally
    img.Free;
  end;
end;

const
  src = 'C:\lazarus-trunk_fpc304\images\splash_source\cheetah.jpg';
  dest1 = './cheetah-gamma20.jpg';
  dest2 = './cheetah-gamma05.jpg';
begin
  GammaCorrection(src, dest1, 2.0);
  GammaCorrection(src, dest2, 0.5);
end.



begin
end.

