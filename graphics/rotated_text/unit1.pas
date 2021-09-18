unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    TrackBar1: TTrackBar;
    procedure PaintBox1Paint(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLIntf;

function Min4(a, b, c, d: Integer): Integer;
begin
  Result := a;
  if b < Result then Result := b;
  if c < Result then Result := c;
  if d < Result then Result := d;
end;

function Max4(a, b, c, d: Integer): Integer;
begin
  Result := a;
  if b > Result then Result := b;
  if c > Result then Result := c;
  if d > Result then Result := d;
end;

function RotatePoint(const APoint: TPoint; Angle: Double): TPoint;
var
  sa, ca: Double;
begin
  sa := sin(Angle * pi/180);
  ca := cos(Angle * pi/180);
  Result.X := Round( ca * APoint.X + sa * APoint.Y);
  Result.Y := Round(-sa * APoint.X + ca * APoint.Y);
end;

function RotateRect(const Width, Height: Integer; Angle: Double): TRect;
var
  P0, P1, P2, P3: TPoint;
begin
  P0 := Point(0, 0);
  P1 := RotatePoint(Point(0, Height), Angle);
  P2 := RotatePoint(Point(Width, 0), Angle);
  P3 := RotatePoint(Point(Width, Height), Angle);
  Result.Left := Min4(P0.X, P1.X, P2.X, P3.X);
  Result.Top := Min4(P0.Y, P1.Y, P2.Y, P3.Y);
  Result.Right := Max4(P0.X, P1.X, P2.X, P3.X);
  Result.Bottom := Max4(P0.Y, P1.Y, P2.Y, P3.Y);
end;

{ TForm1 }

procedure TForm1.PaintBox1Paint(Sender: TObject);
const
  lText: String = 'Hallo';
var
  C: TPoint;
  R: TRect;
  P1, P2, P3, P4: TPoint;
begin
  // Fill background
  Paintbox1.Canvas.Brush.Color := clWhite;
  Paintbox1.Canvas.FillRect(0, 0, Paintbox1.Width, Paintbox1.Height);

  // Center of drawing area
  C := Point(Paintbox1.Width div 2, Paintbox1.Height div 2);

  // Draw rotated text
  Paintbox1.Canvas.Font.Orientation := Trackbar1.Position * 10;
  Paintbox1.Canvas.TextOut(C.X, C.Y, lText);

  // Draw starting point of text
  Paintbox1.Canvas.Pen.Color := clRed;
  Paintbox1.Canvas.Pen.Style := psSolid;
  Paintbox1.Canvas.Line(C.X - 10, C.Y, C.X + 10, C.Y);
  Paintbox1.Canvas.line(C.X, C.Y - 10, C.X, C.Y + 10);

  // Calculate rectangle enclosing the text...
  R.TopLeft := Point(0, 0);
  R.BottomRight := TPoint(Paintbox1.Canvas.TextExtent(lText));
  P1 := RotatePoint(Point(R.Left, R.Top), Trackbar1.Position);
  P2 := RotatePoint(Point(R.Right, R.Top), Trackbar1.Position);
  P3 := RotatePoint(Point(R.Right, R.Bottom), Trackbar1.Position);
  P4 := RotatePoint(Point(R.Left, R.Bottom), Trackbar1.Position);
  R := RotateRect(R.Right, R.Bottom, Trackbar1.Position);

  // ... and draw it.
  Paintbox1.Canvas.Line(P1.X+C.X, P1.Y+C.Y, P2.X+C.X, P2.Y+C.Y);
  Paintbox1.Canvas.Line(P2.X+C.X, P2.Y+C.Y, P3.X+C.X, P3.Y+C.Y);
  Paintbox1.Canvas.line(P3.X+C.X, P3.Y+C.Y, P4.X+C.X, P4.Y+C.Y);
  Paintbox1.Canvas.Line(P4.X+C.X, P4.Y+C.Y, P1.X+C.X, P1.Y+C.Y);

  if (Trackbar1.Position mod 90 <> 0) then
  begin
    OffsetRect(R, C.X, C.Y);
    Paintbox1.Canvas.Brush.Style := bsClear;
    Paintbox1.Canvas.Pen.Color := clBlue;
    Paintbox1.Canvas.Pen.Style := psDash;
    Paintbox1.Canvas.Rectangle(R);
  end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Paintbox1.Invalidate;
end;

end.

