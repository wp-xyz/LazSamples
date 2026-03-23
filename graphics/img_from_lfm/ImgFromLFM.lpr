{ Extracts the image data embedded in a Lazarus lfm file for a TImage component
  and writes them into a file. }

program ImgFromLFM;
uses
  SysUtils, StrUtils, Classes, FPImage, FPReadPNG, FPReadBMP, FPReadJPEG,
  FPReadTIFF;

function ByteValueAt(const ALine: String; AIndex: Integer): Byte;
var
  s: string[3] = '$--';
  code: Integer;
begin
  s[2] := ALine[AIndex];
  s[3] := ALine[AIndex + 1];
  val(s, Result, code);
end;

function ZeroByteAt(const ALine: String; AIndex: Integer): Boolean;
begin
  Result := (ALine[AIndex] = '0') and (ALine[AIndex + 1] = '0');
end;

function FindImageExtension(AStream: TStream): String;
var
  readerClass: TFPCustomImageReaderClass;
  typeName: String;
  i: Integer;
begin
  Result := '';
  AStream.Position := 0;
  readerClass := TFPCustomImage.FindReaderFromStream(AStream);
  if readerClass <> nil then
    for i := 0 to ImageHandlers.Count-1 do
    begin
      typeName := ImageHandlers.TypeNames[i];
      if ImageHandlers.ImageReader[typeName] = readerClass then
      begin
        Result := '.' + ImageHandlers.DefaultExtension[typeName];
        break;
      end;
    end;
  AStream.Position := 0;
end;

procedure ExtractImageFromLFM(const AFileName: String);
var
  i, j: Integer;
  s: String;
  list: TStringList;
  stream: TMemoryStream;
  fn: String;
  counter: Integer = 0;
  inPicture: Boolean = false;
  inHeader: Boolean = true;
begin
  list := TStringList.Create;
  try
    list.LoadFromFile(AFileName);
    for i := 0 to list.Count-1 do
    begin
      s := Trim(list[i]);
      if (not inPicture) and (s = 'Picture.Data = {') then
      begin
        inPicture := true;
        inHeader := true;
        inc(counter);
        stream := TMemoryStream.Create;
      end else
      if inPicture then
      begin
        if (s = '}') then
        begin
          inPicture := false;
          fn := AFileName + '-' + IntToStr(counter) + FindImageExtension(stream);
          stream.SaveToFile(fn);
          FreeAndNil(stream);
        end else
        begin
          j := 1;
          while j < Length(s) do
          begin
            if inHeader then
            begin
              if ZeroByteAt(s, j) then
                inHeader := false;
            end else
              stream.WriteByte(ByteValueAt(s, j));
            inc(j, 2);
          end;
        end;
      end;
    end;
  finally
    stream.Free;
    list.Free;
  end;
end;

procedure WriteHelp;
begin
  WriteLn('Syntax: ImgFromLFM <lfmfile>');
  WriteLn('The extracted images are stored in the original folder under their original name');
  WriteLn('with numeric appendices and the correct image format extension.');
  WriteLn('  e.g. unit1.lfm-1.bmp');
end;

begin
  if ParamCount = 0 then
  begin
    WriteHelp;
    halt;
  end;

  ExtractImageFromLFM(ParamStr(1));
end.

