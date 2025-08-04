program PrintOfficeFileType;
uses
  SysUtils, classes, uVirtualLayer_OLE;

function IsOLEStream(AStream: TStream;
  const AStreamName: String = 'Book'): Boolean;
var
  fsOLE: TVirtualLayer_OLE;
  VLAbsolutePath: UTF8String;
begin
  VLAbsolutePath := '/' + AStreamName;
  fsOLE := TVirtualLayer_OLE.Create(AStream);
  try
    fsOLE.Initialize(); //Initialize the OLE container.
    Result := fsOLE.FileExists(VLAbsolutePath);
  finally
    fsOLE.Free;
  end;
end;

var
  fn: String;
  stream: TStream;
begin
  if ParamCount = 0 then
  begin
    WriteLn('Syntax: PrintOfficeFileType filename');
    Halt;
  end;

  fn := ParamStr(1);
  if not FileExists(fn) then
  begin
    WriteLn('File "', fn, '" not found.');
    Halt;
  end;

  stream := TFileStream.Create(fn, fmOpenRead);
  try
    if IsOLEStream(stream, 'Workbook') then
      WriteLn('File "', fn, '" is an Excel file in BIFF8 format.')
    else
    if IsOLEStream(stream, 'Book') then
      WriteLn('File "', fn, '" is an Excel file in BIFF5 format.')
    else
    if IsOLEStream(stream, 'WordDocument') then
      WriteLn('File "', fn, '" is a Word file in .doc format.')
    else
    if IsOLEStream(stream, 'PowerPoint Document') then
      WriteLn('File "', fn, '" is a PowerPoint file in .ppt format.')
    else
      WriteLn('Formaat for file "', fn, '" could not be detected.');
  finally
    stream.Free;
  end;
end.

