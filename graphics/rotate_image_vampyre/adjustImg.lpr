program AdjustImg;

uses
  SysUtils, Classes,
  Imaging, ImagingTypes,
  fpeMetaData, fpeExifData, fpeTags, fpeGlobal;

procedure AdjustImageOrientation(ASrcFile, ADestFile: String);
const
  THUMBNAIL_SIZE = 200;
var
  srcImg: TImageData;
  destImg: TImageData;
  thumbnail: TImageData;
  thumbnailWidth, thumbnailHeight: Integer;
  stream: TMemoryStream;
  imgInfo: TImgInfo;
  lTag: TTag;
  orientation: TExifOrientation;
begin
  // Read EXIF from source file
  imgInfo := TImgInfo.Create;
  imgInfo.LoadFromFile(ASrcfile);
  if imgInfo.HasExif then
    orientation := imgInfo.ExifData.ImgOrientation
  else
    orientation := eoUnknown;

  // Read source file...
  InitImage(srcImg);
  LoadImageFromFile(ASrcFile, srcImg);

  // ... and manipulate it to adjust image orientation
  InitImage(destImg);
  CloneImage(srcImg, destImg);
  case orientation of
    eoUnknown: ;
    eoNormal: ;
    eoMirrorHor:
      MirrorImage(destImg);
    eoRotate180:
      RotateImage(destImg, 180.0);
    eoMirrorVert:
      FlipImage(destImg);
    eoMirrorHorRot270:
      begin
        MirrorImage(destImg);
        RotateImage(destImg, 90.0);
      end;
    eoRotate90:
      RotateImage(destImg, 270);
    eoMirrorHorRot90:
      begin
        MirrorImage(destImg);
        RotateImage(destImg, 270);
      end;
    eoRotate270:
      RotateImage(destImg, 90);
  end;

  // ... and save modified jpeg to file
  SaveImageToFile(ADestFile, destImg);

  // Update relevant exif data (width, height, thumbnail)
  if imgInfo.HasExif then
  begin
    // Find the predefined tag names in fpeExifData
    // Update tags related to image width
    lTag := imgInfo.ExifData.TagByID[FULLTAG_IMAGEWIDTH];
    if lTag <> nil then lTag.AsInteger := destImg.Width;
    lTag := imgInfo.ExifData.TagByID[EXIFTAG_IMAGEWIDTH];
    if lTag <> nil then lTag.AsInteger := destImg.Width;
    lTag := imgInfo.ExifData.TagByID[TAGPARENT_EXIF or $A002];
    if lTag <> nil then lTag.AsInteger := destImg.Width;

    // Update tags related to image height
    lTag := imgInfo.Exifdata.TagByID[FULLTAG_IMAGELENGTH];
    if lTag <> nil then lTag.AsInteger := destImg.Height;
    lTag := imgInfo.ExifData.TagByID[EXIFTAG_IMAGELENGTH];
    if lTag <> nil then lTag.AsInteger := destImg.Height;
    lTag := imgInfo.ExifData.TagByID[TAGPARENT_EXIF or $A003];
    if lTag <> nil then lTag.AsInteger := destImg.Height;

    // Update tag for image orientation (needed if image has been rotated)
    // The ID for the "orientation" tag is $0112; the tag value is an integer
    // and must be 1 for "normal (=horizontal)" orientation.
    // See https://exiftool.org/TagNames/EXIF.html
    imgInfo.ExifData.AddOrReplaceTagByName('Orientation').AsInteger := 1;

    // Update thumbnail image
    if imgInfo.HasThumbnail then
    begin
      // Create new thumbnail
      InitImage(thumbnail);
      try
        CloneImage(destImg, thumbnail);
        lTag := imgInfo.ExifData.TagByID[FULLTAG_THUMBWIDTH];
        if lTag <> nil then thumbnailWidth := lTag.AsInteger
          else thumbnailWidth := THUMBNAIL_SIZE;
        lTag := imgInfo.ExifData.TagByID[FULLTAG_THUMBHEIGHT];
        if lTag <> nil then thumbnailHeight := lTag.AsInteger
          else thumbnailHeight := round(THUMBNAIL_SIZE * destImg.Height/destImg.Width);
        // Stretch destination image to thumbnail size
        if ((destImg.Width > destImg.Height) and (thumbnailWidth < thumbnailHeight)) or
           ((destImg.Width < destImg.Height) and (thumbnailWidth > thumbnailHeight))
        then
          ResizeImage(thumbnail, thumbnailHeight, thumbnailWidth, rfBiCubic)
        else
          ResizeImage(thumbnail, thumbnailWidth, thumbnailHeight, rfBiCubic);
        // Save thumbnail to stream
        stream := TMemoryStream.Create;
        try
          SaveImageToStream('jpg', stream, thumbnail);
          stream.Position := 0;
          // ... and add to exif
          imgInfo.ExifData.LoadThumbnailFromStream(stream);
        finally
          stream.Free;
        end;
      finally
        FreeImage(thumbnail);
      end;
    end;

    // Merge modified exif into destination file
    imgInfo.SaveToFile(ADestFile, ADestFile); // 1st argument: result file, 2nd argument: file with image data
  end;

  // Clean-up
  imgInfo.Free;
  FreeImage(destImg);
  FreeImage(srcImg);
end;

var
  srcFile, destfile: String;
begin
  if ParamCount = 0 then
  begin
    WriteLn('Syntax: adjustImg sourcefile');
    WriteLn('  Reads image orientation from meta data and adjusts orientation accordingly.');
    WriteLn('  The output file is renamed with appended "-modified".');
  end;
  srcFile := ExpandFileName(ParamStr(1));
  destFile := ChangeFileExt(srcFile, '') + '-modified' + ExtractFileExt(ParamStr(1));
  AdjustImageOrientation(srcFile, destFile);
end.

