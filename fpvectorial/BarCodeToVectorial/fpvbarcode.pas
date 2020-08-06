unit fpvBarCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPVectorial, FPImage, FPCanvas, uBarcodes;

type
  TvBarcodePageHelper = class helper for TvVectorialPage
  public
    procedure AddBarCode(ABarCode: TLazBarcodeCustomBase; AWidth, AHeight: Double;
      out AErrorMsg: String);
    procedure AddFilledRect(X1, Y1, X2, Y2: Double; AColor: TFPColor;
      ABrushStyle: TFPBrushStyle);
  end;

procedure BarcodeToFPVectorialStream(AStream: TStream; ABarCode: TLazBarcodeCustomBase;
  AWidth, AHeight: Integer; AFormat: TvVectorialFormat; out AErrMsg: String);

implementation

uses
  lbc_render, zint;

type
  TMyLazBarcode = class(TLazBarcodeCustomBase);

// Duplicated from Graphics to avoid using this unit.
function TColorToFPColor(const c: LongInt): TFPColor;
begin
  Result.Red:=(c and $ff);
  Result.Red:=Result.Red+(Result.Red shl 8);
  Result.Green:=(c and $ff00);
  Result.Green:=Result.Green+(Result.Green shr 8);
  Result.Blue:=(c and $ff0000) shr 8;
  Result.Blue:=Result.Blue+(Result.Blue shr 8);
  Result.Alpha:=FPImage.alphaOpaque;
end;

procedure TvBarcodePageHelper.AddBarcode(ABarcode: TLazBarcodeCustomBase;
  AWidth, AHeight: Double; out AErrorMsg: String);
var
  barcode: TMyLazBarCode;
  qr: PointerTo_zint_symbol;
  errorCode: integer;
  line: PointerTo_zint_render_line;
  baseX, baseY: integer;
  X, Y: integer;
  clr: TFPColor;
begin
  barcode := TMyLazBarCode(ABarCode);
  qr := barcode.FQR;
  if not Assigned(qr^.rendered) then begin
    X := round(AWidth) + 1;
    Y := round(AHeight) + 1;
    if not barcode.FStrictSize then begin
      errorCode := render_plot(qr, X, Y);
    end else begin
      baseX := qr^.width + qr^.border_width*2;
      baseY := qr^.rows + qr^.border_width*2;
      errorCode := render_plot(qr, X - (X mod BaseX), (Y - (Y mod BaseY)));
    end;
    if errorCode <> 1 then begin
      AErrorMsg := qr^.errtxt;
      exit;
    end else begin
      AErrorMsg := '';
    end;
  end;

  if Assigned(qr^.rendered) then
  begin
    line := qr^.rendered^.lines;
    Clear;
    clr := TColorToFPColor(barcode.ForegroundColor);
    while Assigned(Line) do begin
      AddFilledRect(
        line^.x,
        line^.y,
        line^.x + line^.width,
        line^.y + line^.length,
        clr,
        bsSolid
      );
      line := line^.next;
    end;
  end;
end;

procedure TvBarcodePageHelper.AddFilledRect(X1, Y1, X2, Y2: Double;
  AColor: TFPColor; ABrushStyle: TFPBrushStyle);
begin
  StartPath(X1, Y1);
  AddLineToPath(X1, Y2);
  AddLineToPath(X2, Y2);
  AddLineToPath(X2, Y1);
  AddLineToPath(X1, Y1);
  SetBrushColor(AColor);
  SetBrushStyle(ABrushStyle);
  SetPenStyle(psClear);
  EndPath();
end;

procedure BarcodeToFPVectorialStream(AStream: TStream; ABarCode: TLazBarcodeCustomBase;
  AWidth, AHeight: Integer; AFormat: TvVectorialFormat; out AErrMsg: String);
var
  vec: TvVectorialDocument;
  page: TvVectorialPage;
begin
  vec := TvVectorialDocument.Create;
  try
    vec.Width := AWidth;
    vec.Height := AHeight;
    page := Vec.AddPage();
    page.UseTopLeftCoordinates := true;
    page.AddBarcode(ABarCode, AWidth, AHeight, AErrMsg);
    vec.WriteToStream(AStream, AFormat);
  finally
    vec.Free;
  end;
end;

end.

