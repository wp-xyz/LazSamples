program imgsize;

uses
  Classes, SysUtils,
  fpImage, fpReadJpeg, fpReadBMP, fpReadPNG, fpReadGIF, fpReadPCX, fpReadTIFF, fpReadTGA,
  fixImageSize;

function GetImageSize(AFileName: String; out ASize: TPoint): Boolean;
var
  stream: TStream;
  readerClass: TFPCustomImageReaderClass;
begin
  Result := false;
  ASize := Point(0, 0);
  stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    readerClass := TFPCustomImage.FindReaderFromStream(stream);
    if readerClass <> nil then
    begin
      ASize := readerClass.ImageSize(stream);
      Result := (ASize.X > 0) and (ASize.Y > 0);
    end;
  finally
    stream.Free;
  end;
end;

var
  Size: TPoint;
  fn: String;
begin
  if ParamCount = 0 then
  begin
    WriteLn('Syntax: imgsize filename');
    Halt;
  end;
  fn := ParamStr(1);
  Write('Image ' + fn + ': ');
  if GetImageSize(fn, size) then
    WriteLn(size.X, ' x ', size.Y)
  else
    WriteLn('File not found or format not supported.');

//    Readln;
end.

