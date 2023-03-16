unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, FPImage;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorButton1: TColorButton;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1Resize(Sender: TObject);
  private
    P: array[0..6] of TPoint;
    C: array[0..6] of TFPColor;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math, IntfGraphics;

{ Calculates the interpolated color at the point P inside the triangle spanned
  by the three vertices P1, P2 and P3. Interpolation occurs by bilinear
  algorithm based on barycentric coordinates.
  For details see: https://codeplea.com/triangular-interpolation }
function BarycentricBlend(P, P1, P2, P3: TPoint; C1, C2, C3: TFPColor): TFPColor;
var
  w1, w2, w3: Double;
  c: Int64;
begin
  w1 := EnsureRange(
          ((P2.Y - P3.Y)*(P.X  - P3.X) + (P3.X - P2.X)*(P.Y  - P3.Y)) /
          ((P2.Y - P3.Y)*(P1.X - P3.X) + (P3.X - P2.X)*(P1.Y - P3.Y)),
          0.0, 1.0);
  w2 := EnsureRange(
          ((P3.Y - P1.Y)*(P.X  - P3.X) + (P1.X - P3.X)*(P.Y  - P3.Y)) /
          ((P2.Y - P3.Y)*(P1.X - P3.X) + (P3.X - P2.X)*(P1.Y - P3.Y)),
          0.0, 1.0);
  w3 := EnsureRange(
          1.0 - w1 - w2,
          0.0, 1.0);

  c := EnsureRange(round(w1 * C1.Red + w2 * C2.Red + w3 * C3.Red), 0, $FFFF);
  Result.Red := c;

  c := EnsureRange(round(w1 * C1.Green + w2 * C2.Green + w3 * C3.Green), 0, $FFFF);
  Result.Green := c;

  c := EnsureRange(round(w1 * C1.Blue + w2 * C2.Blue + w3 * C3.Blue), 0, $FFFF);
  Result.Blue := c;
end;

{ Calculates the x coordinate of the intersection of a horizontal line at the
  given y value with the line running through the points A and B.
  In the case that the line AB is horizontal as well the end points are returned
  in x1 and x2. In the "usual" case of nonhorizontal direction, there is only
  a single intersection point which is returned in both x1 and x2.
  Note that y always must be inside the range defined by A.Y and B.Y; otherwise
  an intersection may be reported although there is none (e.g.
  A.Y = B.Y and y <> A.Y) }
procedure HorIntersection(y: Integer; A, B: TPoint; out x1, x2: Integer);
begin
  if A.Y = B.Y then
  begin
    if A.X < B.X then
    begin
      x1 := A.X;
      x2 := B.X;
    end else
    begin
      x1 := B.X;
      x2 := A.X;
    end;
  end else
  begin
    x1 := round(A.X + (B.X - A.X) / (B.Y - A.Y) * (y - A.Y));
    x2 := x1;
  end;
end;

// Make sure that the points are ordered from top to bottom
procedure SortVertices(var P1, P2, P3: TPoint; var C1, C2, C3: TFPColor);

  procedure Exchange(var P1, P2: TPoint; var C1, C2: TFPColor);
  var
    P: TPoint;
    C: TFPColor;
  begin
    P := P1;
    P1 := P2;
    P2 := P;
    C := C1;
    C1 := C2;
    C2 := C;
  end;

begin
  if P1.Y > P2.Y then Exchange(P1, P2, C1, C2);
  if P1.Y > P3.Y then Exchange(P1, P3, C1, C3);
  if P2.Y > P3.Y then Exchange(P2, P3, C2, C3);
end;

{ Fills the triangle spanned by the points P1, P2 and P3 by a color gradient
  defined by bilinear interpolation between the colors C1, C2 and C3 at the
  three vertices. }
procedure TriangleGradient(AImage: TFPCustomImage; P1, P2, P3: TPoint;
  C1, C2, C3: TFPColor);
var
  x, y: Integer;
  x11, x12, x21, x22: Integer;
  xL, xR: Integer;
  P, Q1, Q2: TPoint;
begin
  SortVertices(P1, P2, P3, C1, C2, C3);

  Q1 := P1;
  Q2 := P2;
  for y := P1.Y to P3.Y do
  begin
    if (y = P2.Y) then
    begin
      Q1 := P2;
      Q2 := P3;
    end;
    if y < 0 then
      Continue;
    if y >= AImage.Height then
      exit;
    HorIntersection(y, Q1, Q2, x11, x12);
    HorIntersection(y, P1, P3, x21, x22);
    if x11 <> x12 then
    begin
      xL := x11;
      xR := x12;
    end else
    if x21 <> x22 then
    begin
      xL := x21;
      xR := x22;
    end else
    if x11 < x22 then
    begin
      xL := x12;
      xR := x22;
    end else
    begin
      xL := x22;
      xR := x11;
    end;
    if xL < 0 then xL := 0;
    if xR >= AImage.Width then xR := AImage.Width - 1;
    for x := xL to xR do
    begin
      P := Point(x, y);
      AImage.Colors[x, y] := BarycentricBlend(P, P1, P2, P3, C1, C2, C3);
    end;
  end;
end;


{ TForm1 }

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  bmp: TBitmap;
  img: TLazIntfImage;
  i,j: Integer;
  t: TDateTime;
begin
  t := Now();
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf32Bit;
    bmp.SetSize(Paintbox1.Width, Paintbox1.Height);
    bmp.Canvas.Brush.Color := ColorToRGB(Paintbox1.Color);
    bmp.Canvas.FillRect(0, 0, Paintbox1.Width, Paintbox1.Height);
    img := bmp.CreateIntfImage;
    for i := 1 to 6 do
    begin
      j := i+1;
      if j = 7 then j := 1;
      TriangleGradient(img, P[0], P[i], P[j], C[0], C[i], C[j]);
    end;

    bmp.LoadFromIntfImage(img);
    Paintbox1.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
  Caption := 'Painting time: ' + FormatDateTime('s.zzz" ms"', Now-t);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  C[0] := TColorToFPColor(ColorButton1.ButtonColor);
  C[1] := colRed;
  C[2] := colYellow;
  C[3] := colGreen;
  C[4] := colAqua;
  C[5] := colBlue;
  C[6] := colFuchsia;
end;

procedure TForm1.ColorButton1ColorChanged(Sender: TObject);
begin
  C[0] := TColorToFPColor(ColorButton1.ButtonColor);
  Paintbox1.Invalidate;
end;

procedure TForm1.PaintBox1Resize(Sender: TObject);
var
  ctr: TPoint;
  R: Integer;
  i: Integer;
  sinphi, cosphi: Double;
begin
  ctr := Point(Paintbox1.Width div 2, Paintbox1.Height div 2);
  R := Min(Paintbox1.Width, Paintbox1.Height) * 9 div 20;

  P[0] := ctr;
  for i := 1 to 6 do begin
    sinCos(pi/3*(i-1), sinphi, cosphi);
    P[i] := Point(round(R * cosphi + ctr.x), round(R * sinphi + ctr.Y));
  end;
  C[0] := TColorToFPColor(ColorButton1.ButtonColor);
  C[1] := colRed;
  C[2] := colYellow;
  C[3] := colGreen;
  C[4] := colAqua;
  C[5] := colBlue;
  C[6] := colFuchsia;
end;

end.

