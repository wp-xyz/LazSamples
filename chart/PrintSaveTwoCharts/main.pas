{ This demo shows how two charts can be printed or copied to the clipboard. }

unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, PrintersDlgs, StdCtrls, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSaveToJpeg: TButton;
    btnCopyToClipboard: TButton;
    Chart1LineSeries2: TLineSeries;
    Chart2: TChart;
    PrintScaledPen: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Panel1: TPanel;
    PrintDialog1: TPrintDialog;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure btnSaveToJpegClick(Sender: TObject);
    procedure PrintClick(Sender: TObject);
  private
    function CombineCharts(AImageClass: TRasterImageClass): TRasterImage;

  end;

var
  Form1: TForm1; 

implementation

uses
  OSPrinters, TAPrint, Printers, Clipbrd;

{$R *.lfm}

function mmToPix_x(mm: Integer): Integer;
begin
   Result := round(mm / 25.4 * Printer.XDpi);
end;

function mmToPix_y(mm: Integer): Integer;
begin
   Result := round(mm/25.4 * Printer.YDpi);
end;

{ TForm1 }

function TForm1.CombineCharts(AImageClass: TRasterImageClass): TRasterImage;
const
  DISTANCE = 16;
var
  img: TRasterImage;
begin
  Result := AImageClass.Create;

  // Adjust the size of the jpeg such that it can contain both charts
  // stacked above each other.
  // It is assumed here that both charts have the same width.
  Result.Width := Chart1.Width;
  Result.Height := Chart1.Height + Chart2.Height + DISTANCE;
//  Result.SetSize(Chart1.Width, Chart1.Width + Chart2.Height + DISTANCE);
  Result.Canvas.Brush.Color := clWhite;
  Result.Canvas.FillRect(0, 0, Result.Width, Result.Height);

  // Create a bitmap of the first chart...
  Chart1.Legend.Visible := true;
  img := Chart1.SaveToImage(TBitmap);
  Chart1.Legend.Visible := false;
  try
    // ... and draw it in the upper part of the jpeg
    Result.Canvas.Draw(0, 0, img);
  finally
    img.Free;
  end;

  // Create a bitmap of the second chart...
  img := Chart2.SaveToImage(TBitmap);
  try
    // ... and draw it below the first chart onto the jpeg's canvas.
    Result.Canvas.Draw(0, Chart1.Height + DISTANCE, img);
  finally
    img.Free;
  end;
end;

procedure TForm1.btnCopyToClipboardClick(Sender: TObject);
var
  img: TRasterImage;
begin
  img := CombineCharts(TBitmap);
  try
    Clipboard.Assign(img);
  finally
    img.Free;
  end;
end;

procedure TForm1.btnSaveToJpegClick(Sender: TObject);
var
  img: TRasterImage;
begin
  img := CombineCharts(TJpegImage);
  try
    img.SaveToFile('test.jpg');
  finally
    img.Free;
  end;
end;

procedure TForm1.PrintClick(Sender: TObject);
const
  MARGIN = 20; // 20 mm margin around the charts
var
  R: TRect;
  d: Integer;
begin
  if not PrintDialog1.Execute then exit;
  Printer.BeginDoc;
  try
    // First chart
    // Adjust the output rectangle so that it fills the upper part of the page
    // and keeps the aspect ratio of the top chart
    r := Rect(0, 0, Printer.PageWidth, Printer.PageHeight div 2);
    r.Left := mmToPix_x(MARGIN);
    r.Right := Printer.PageWidth - r.Left;
    r.Top := mmToPix_y(MARGIN);
    r.Bottom := r.Top + round(Chart1.Height/Chart1.Width*r.Width);
    Chart1.Legend.Visible := true;
    Chart1.Draw(TPrinterDrawer.Create(Printer, true), r);
    Chart1.Legend.Visible := false;

    // Second chart
    // Adjust the output rectangle so that the second chart fills the remainder
    // of the page. Leave a distance of 1 cm between both charts.
    r.Top := r.Bottom + mmToPix_y(10);  // 10 --> 1 cm distance
    r.Bottom := Printer.PageHeight - mmToPix_y(MARGIN);
    Chart2.Draw(TPrinterDrawer.Create(Printer, true), r);

  finally
    Printer.EndDoc;
  end;
end;

end.

