{-------------------------------------------------------------------------------
  Rotates a bitmap image by an arbitrary angle.

  Usage:  rotate_image filename angle  // where angle is in degrees.
-------------------------------------------------------------------------------}

program rotate_image;

uses
  SysUtils, Types, Math, FPImage, FPReadPNG, FPWritePNG;

{ Rotates point P around the origin (0, 0) by the given angle (in radians).
  Borrowed from Lazarus GraphMath. }
function RotatePoint(const APoint: TPoint; AAngle: Double): TPoint;
var
  sa, ca: Double;
begin
  SinCos(AAngle, sa, ca);
  Result.X := Round( ca * APoint.X + sa * APoint.Y);
  Result.Y := Round(-sa * APoint.X + ca * APoint.Y);
end;

{ Rotates the rectangle (0, 0, AWidth, AHeight) around its top-left corner (0,0)
  by the angle AAngle (in radians). Borrowed from Lazarus GraphMath. }
function RotateRect(AWidth, AHeight: Integer; AAngle: Double): TRect;

  procedure GetMinMax(x: Integer; var min, max: Integer);
  begin
    if x < min then min := x;
    if x > max then max := x;
  end;

var
  P1, P2, P3: TPoint;
begin
  if AAngle = 0 then
    Result := Rect(0, 0, AWidth, AHeight)
  else
  begin
    P1 := RotatePoint(Point(AWidth, 0), AAngle);
    P2 := RotatePoint(Point(0, AHeight), AAngle);
    P3 := P1 + P2;

    Result := Rect(0, 0, 0, 0);
    GetMinMax(P1.X, Result.Left, Result.Right);
    GetMinMax(P2.X, Result.Left, Result.Right);
    GetMinMax(P3.X, Result.Left, Result.Right);
    GetMinMax(P1.Y, Result.Top, Result.Bottom);
    GetMinMax(P2.Y, Result.Top, Result.Bottom);
    GetMinMax(P3.Y, Result.Top, Result.Bottom);
  end;
end;

{ Rotates the source image around its center by the given angle (in radians).
  The size of the resulting image is adjust to tightly contain the
  rotated image. }
function CreateRotatedImage(AImage: TFPMemoryImage; Angle: Double): TFPMemoryImage;
var
  R: TRect;
  x, y: Integer;
  C, P: TPoint;
  delta: TPoint;
begin
  // Calculate bounds of the rotated image
  R := RotateRect(AImage.Width, AImage.Height, Angle);
  OffsetRect(R, -R.Left, -R.Top);

  // Calculate center of rotated image
  C := Point((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2);

  // Create the resulting image.
  Result := TFPMemoryImage.Create(R.Right - R.Left, R.Bottom - R.Top);

  // Correction needed to center the rotated pixels
  delta := Point((AImage.Width - Result.Width) div 2, (AImage.Height - Result.Height) div 2);

  // Iterate over all pixels in the result image and look up the corresponding
  // pixels in the unrotated image. Pixels in the rotated image which are outside
  // the unrotated image are set to be transparent.
  for y := 0 to Result.Height-1 do
    for x := 0 to Result.Width-1 do
    begin
      P := RotatePoint(Point(x, y) - C, -Angle) + C;
      P := P + delta;
      if (P.X >= 0) and (P.Y >= 0) and (P.X < AImage.Width) and (P.Y < AImage.Height) then
        Result.Colors[x, y] := AImage.Colors[P.X, P.Y]
      else
        Result.Colors[x, y] := colTransparent;
    end;
end;

var
  src, dest: TFPMemoryImage;
  filename: String;
  angle: Double;
  writer: TFPWriterPNG;
begin
  if ParamCount < 2 then begin
    WriteLn('Syntax: rotate_image filename angle');
    WriteLn('  (angle in degrees)');
    Halt;
  end;

  filename := ParamStr(1);
  angle := DegToRad(StrToFloat(ParamStr(2)));

  src := TFPMemoryImage.Create(0, 0);
  try
    src.LoadFromFile(filename);
    dest := CreateRotatedImage(src, angle);
    try
      writer := TFPWriterPNG.create;
      try
        writer.UseAlpha := true;
        dest.SaveToFile(ExtractFilePath(filename) + 'rotated_' + ExtractFileName(fileName), writer);
      finally
        writer.Free;
      end;
    finally
      dest.Free;
    end;
  finally
    src.Free;
  end;
end.

