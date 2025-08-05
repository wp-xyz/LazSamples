program demo_3of9;
uses
  Types, fpImage, fpBarCode, fpImgBarCode, fpWritePng;
const
  HEIGHT = 60;
  NUMERIC_TEXT = '0123456789';
  ENCODING = be39Extended;    // or be39Extended?
  UNIT_WIDTH = 2;
  WEIGHT = 4;
  FILE_NAME = '3of9.png';
var
  barcode: TFPDrawBarcode;
  width: Integer;
begin
  barcode := TFPDrawBarcode.Create;
  try
    barcode.Encoding := ENCODING;
    barcode.UnitWidth := UNIT_WIDTH;
    barcode.Weight := WEIGHT;
    barcode.Text := NUMERIC_TEXT;
    width := barcode.CalcWidth;
    barcode.Rect := Rect(0, 0, width, HEIGHT);
    barcode.Image := TFPCompactImgGray8Bit.Create(barcode.Rect.Width, barcode.Rect.Height);
    try
      if barcode.Draw then
      begin
        barcode.Image.SaveToFile(FILE_NAME);
        WriteLn('Saved to file ', FILE_NAME);
      end else
        WriteLn('Error generating barcode.');
    finally
      barcode.Image.Free;
    end;
  finally
    barcode.Free;
  end;
end.

