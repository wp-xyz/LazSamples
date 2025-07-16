unit fixImageSize;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpImage, fpReadBMP, fpReadGIF, fpReadTIFF;

type
  {$IF FPC_FullVersion <= 30300}
  TFPReaderBMP = class(fpReadBMP.TFPReaderBMP)
  protected
    class function  InternalSize  (Stream:TStream) : TPoint; override;
  end;

  TFPReaderGIF = class(fpReadGIF.TFPReaderGIF)
  protected
    class function  InternalSize  (Stream:TStream) : TPoint; override;
  end;
  {$IFEND}

  TFPReaderTIFF = class(fpReadTIFF.TFPReaderTIFF)
  protected
    class function InternalSize(Stream: TStream): TPoint; override;
  end;


implementation

uses
  bmpcomn, FPTiffCmn;

{$IF FPC_FullVersion <= 30300}
class function TFPReaderBMP.InternalSize (Stream: TStream): TPoint;
var
  fileHdr: TBitmapFileHeader;
  infoHdr: TBitmapInfoHeader;
  n: Int64;
  StartPos: Int64;
begin
  Result := Point(0, 0);

  StartPos := Stream.Position;
  try
    n := Stream.Read(fileHdr, SizeOf(fileHdr));
    if n <> SizeOf(fileHdr) then exit;
    if {$IFDEF ENDIAN_BIG}swap(fileHdr.bfType){$ELSE}fileHdr.bfType{$ENDIF} <> BMmagic then exit;
    n := Stream.Read(infoHdr, SizeOf(infoHdr));
    if n <> SizeOf(infoHdr) then exit;
    {$IFDEF ENDIAN_BIG}
    Result := Point(swap(infoHdr.Width), swap(infoHdr.Height));
    {$ELSE}
    Result := Point(infoHdr.Width, infoHdr.Height);
    {$ENDIF}
  finally
    Stream.Position := StartPos;
  end;
end;

class function TFPReaderGif.InternalSize(Stream:TStream): TPoint;

  function LocalSkipBlock(Stream: TStream): byte;
  var
    Introducer,
    Labels,
    SkipByte : byte;
  begin
    Stream.read(Introducer,1);
    if Introducer = $21 then
    begin
       Stream.read(Labels,1);
       Case Labels of
         $FE, $FF :     // Comment Extension block or Application Extension block
              while true do
              begin
                Stream.Read(SkipByte, 1);
                if SkipByte = 0 then Break;
                Stream.Seek(SkipByte, soFromCurrent);
              end;
         $F9 :         // Graphics Control Extension block
              begin
                Stream.Seek(SizeOf(TGifGraphicsControlExtension), soFromCurrent);
              end;
         $01 :        // Plain Text Extension block
              begin
                Stream.Read(SkipByte, 1);
                Stream.Seek(SkipByte, soFromCurrent);
                while true do
                begin
                  Stream.Read(SkipByte, 1);
                  if SkipByte = 0 then Break;
                  Stream.Seek(SkipByte, soFromCurrent);
                end;
              end;
        end;
    end;
    Result:=Introducer;
  end;

var
  hdr: TGIFHeader;
  introducer: Byte;
  b: Byte = 0;
  skipByte: Byte = 0;
  descr: TGifImageDescriptor;
  n: Integer;
begin
  Result := Point(-1, 1);

  Stream.Read(hdr, SizeOf(hdr));

  // Skip global palette if there is one
  if (hdr.Packedbit and $80) <> 0 then
  begin
    n := hdr.Packedbit and 7 + 1;
    Stream.Seek(1 shl n, soFromCurrent);
  end;
  if Stream.Position >= Stream.Size then
    exit;

  // Skip extensions until image descriptor is found ($2C)
  repeat
    introducer := LocalSkipBlock(Stream);
  until (introducer = $2C) or (Stream.Position>=Stream.Size);
  if Stream.Position>=Stream.Size then
    Exit;

  descr := Default(TGifImageDescriptor);
  Stream.Read(descr, SizeOf(descr));
  with descr do
  begin
   {$IFDEF ENDIAN_BIG}
    Width := LEtoN(Width);
    Height := LEtoN(Height);
   {$ENDIF}
    Result.X := Width;
    Result.Y := Height;
  end;
end;
{$IFEND}

class function TFPReaderTIFF.InternalSize(Stream: TStream): TPoint;
{$IF FPC_FullVersion <= 30300}
type
  TTiffHeader = packed record
    ByteOrder: Word;
    case Version:Word of
    42 : (IFDStart:DWord);
    43 : (BigTIFF_padA, BigTiff_padB:Word) //Follow a 64 Bit IFDStart
  end;

const
  TIFF_ByteOrderBIG = $4D4D;   //'MM';
  TIFF_ByteOrderNOBIG = $4949; //'II';
{$IFEND}
var
  FFirstIFDStart: SizeUInt;
  FStartPos: SizeUInt;
  FReverseEndian: Boolean;
  FBigTiff: Boolean = false;

  function FixEndian(w: Word): Word;
  begin
    Result := w;
    if FReverseEndian then
      Result := ((Result and $ff) shl 8) or (Result shr 8);
  end;

  function FixEndian(d: DWord): DWord;
  begin
    Result := d;
    if FReverseEndian then
      Result := ((Result and $ff) shl 24)
             or ((Result and $ff00) shl 8)
             or ((Result and $ff0000) shr 8)
             or (Result shr 24);
  end;

  {$ifdef CPU64}
  function FixEndian(q: QWord): QWord;
  begin
    Result := q;
    if FReverseEndian then
      Result := SwapEndian(q);
  end;
  {$endif}

  function ReadByte: Byte;
  begin
    Result := Stream.ReadByte;
  end;

  function ReadWord: Word;
  begin
    Result := FixEndian(Stream.ReadWord);
  end;

  function ReadDWord: DWord;
  begin
    Result := FixEndian(Stream.ReadDWord);
  end;

  function ReadQWord: SizeUInt;
  begin
    {$ifdef CPU64}
     Result := FixEndian(Stream.ReadQWord);
    {$else}
     Result := FixEndian(Stream.ReadDWord);
    {$endif}
  end;

  function ReadEntryOffset: SizeUInt;
  begin
    if FBigTiff then
      Result := ReadQWord
    else
      Result := ReadDWord;
  end;

  function ReadEntryUnsigned(out Value: DWord): Boolean;
  var
    EntryCount: SizeUInt;
    EntryType: Word;
  begin
    Result := False;
    Value := 0;
    EntryType := ReadWord;
    EntryCount := ReadEntryOffset;
    if EntryCount<>1 then
      exit;  // EntryCount = 1 expected

    case EntryType of
      1: begin
          // byte: 8bit unsigned
          Value := ReadByte;
        end;
      3: begin
          // short: 16bit unsigned
          Value := ReadWord;
        end;
      4: begin
          // long: 32bit unsigned long
          Value := ReadDWord;
        end;
      else
        exit;
    end;

    Result := true;
  end;

  function ReadTiffHeader(out IFDStart: SizeUInt): boolean;
  var
    BigEndian: Boolean;
    TIFHeader: TTiffHeader;
  begin
    Result := false;

    TifHeader := Default(TTiffHeader);
    Stream.Read(TIFHeader, SizeOf(TTiffHeader));

    if TIFHeader.ByteOrder = TIFF_ByteOrderBIG then
      BigEndian := true
    else
    if TIFHeader.ByteOrder=TIFF_ByteOrderNOBIG then
      BigEndian := false
    else
      exit;

    FReverseEndian := {$ifdef FPC_BIG_ENDIAN}not{$endif} BigEndian;
    FBigTiff := false;

    // Read offset to first IFD
    case FixEndian(TIFHeader.Version) of
      42 : IFDStart := FixEndian(TIFHeader.IFDStart);
      43 : {$ifdef CPU64}
           begin
             IFDStart:=ReadQWord;
             FBigTiff:=true;
           end;
           {$else}
             exit;  // Big Tiff supported only on 64 bit architecture
           {$endif}
      else exit;
    end;
    Result := true;
  end;

  function LoadHeaderFromStream: Boolean;
  begin
    FFirstIFDStart := 0;
    FStartPos := Stream.Position;
    Result := ReadTiffHeader(FFirstIFDStart);
  end;

  function ReadSizeFromIFD(IFDStart: SizeUInt): TPoint;
  var
    Count: SizeUInt;
    i: Integer;
    entryTag: Word = 0;
    p: Int64;
    value: DWord;
  begin
    Result := Point(-1, -1);

    Stream.Position := Int64(IFDStart) + FStartPos;

    if FBigTiff then
      Count := ReadQWord
    else
      Count := ReadWord;

    p := Stream.Position;
    for i:=1 to Count do begin
      entryTag := ReadWord;
      case entryTag of
        256: if ReadEntryUnsigned(value) then Result.X := value else exit;
        257: if ReadEntryUnsigned(value) then Result.Y := value else exit;
      end;
      if (Result.X > -1) and (Result.Y > -1) then
        exit;
      if FBigTiff then
        inc(p, 20)
      else
        inc(p, 12);
      Stream.Position := p;
    end;
  end;

begin
  Result := Point(-1, -1);
  if LoadHeaderFromStream then
    Result := ReadSizeFromIFD(FFirstIFDStart);
end;

initialization
  {$IF FPC_FullVersion <= 30300}
  ImageHandlers.UnRegisterImageHandlers('BMP Format', true, false);
  ImageHandlers.RegisterImageReader ('BMP Format', 'bmp', TFPReaderBMP);

  ImageHandlers.UnRegisterImageHandlers('GIF Graphics', true, false);
  ImageHandlers.RegisterImageReader ('GIF Graphics', 'gif', TFPReaderGif);
  {$IFEND}

  ImageHandlers.UnRegisterImageHandlers(TiffHandlerName, true, false);
  ImageHandlers.RegisterImageReader (TiffHandlerName, 'tif;tiff', TFPReaderTiff);

end.

