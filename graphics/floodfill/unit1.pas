{ Demonstrates application of the floodfill procedure }

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

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

{ TForm1 }

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  with Paintbox1 do begin
    // Background
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(0, 0, Width, Height);

    // Draw some shape, here: an ellipse
    Canvas.Pen.Color := clRed;
    Canvas.Ellipse(50, 50, 100, 100);

    // Define the replacement color
    Canvas.Brush.Color := clGreen;

    // Do the flood fill which picks the color at some interior point of the
    // shape and replaces it within the shape by the brush color
    Canvas.FloodFill(60, 60, Canvas.Pixels[60,60], fsSurface);
  end;
end;

end.

