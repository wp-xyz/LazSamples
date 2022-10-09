unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, TAGraph;

type

  { TForm1 }

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    procedure PaintBox1Paint(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math;

{ TForm1 }

function RotatePointX(AX, AAngle: Double): TPoint;
var
  sa, ca: Extended;
begin
  SinCos(AAngle, sa, ca);
  Result.X := Round(ca * AX);
  Result.Y := Round(sa * AX);
end;

procedure DrawArrowLine(ACanvas: TCanvas; FromPt, ToPt: TPoint;
  ArrowWidth: Integer = 5; ArrowLength: Integer = 10;
  ArrowBaseLength: Integer = 10);
var
  angle: Double;
  dir: Double;
  diag: Integer;
  pt1, pt2, ptBase: TPoint;
begin
  // Draw the connection line between FromPt and ToPt
  ACanvas.Line(FromPt, ToPt);

  // Draw the arrow at the end point (ToPt)
  dir := ArcTan2(ToPt.Y - FromPt.Y, ToPt.X - FromPt.X);  // Direction of line
  angle := ArcTan2(ArrowWidth, ArrowLength);             // Opening angle of arrow
  diag := -Round(sqrt(sqr(ArrowLength) + sqr(ArrowWidth)));
  pt1 := ToPt + RotatePointX(diag, dir - angle);
  pt2 := ToPt + RotatePointX(diag, dir + angle);
  if ArrowBaseLength > 0 then
  begin
    ptBase := ToPt + RotatePointX(-ArrowBaseLength, dir);
    ACanvas.Polygon([pt1, ToPt, pt2, ptBase]);
  end else
    ACanvas.PolyLine([pt1, ToPt, pt2]);
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  P1, P2: TPoint;
  i: Integer;
begin
  Paintbox1.Canvas.Brush.Color := clWhite;
  Paintbox1.Canvas.FillRect(0, 0, Paintbox1.Width, Paintbox1.Height);

  // Two red diagonal lines
  Paintbox1.Canvas.Pen.Color := clRed;
  Paintbox1.Canvas.Brush.Color := clRed;
  DrawArrowLine(Paintbox1.Canvas, Point(10, 10), Point(Paintbox1.Width-10, Paintbox1.Height-10), 10, 20, 0);
  DrawArrowLine(Paintbox1.Canvas, Point(Paintbox1.Width-20, 20), Point(20, Paintbox1.Height-20), 10, 20, 15);

  // Ten random lines
  RandSeed := 202210;
  for i := 1 to 10 do
  begin
    P1 := Point(Random(Paintbox1.Width), Random(Paintbox1.Height));
    P2 := Point(Random(Paintbox1.Width), Random(Paintbox1.Height));
    Paintbox1.Canvas.Brush.Color := RgbToColor(Random(256), Random(256), Random(256));
    Paintbox1.Canvas.Pen.Color := Paintbox1.Canvas.Brush.Color;
    DrawArrowLine(Paintbox1.Canvas, P1, P2);
  end;
end;

end.

