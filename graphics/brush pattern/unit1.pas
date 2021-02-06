unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ColorBox, StdCtrls, FPCanvas;

type

  { TForm1 }

  TForm1 = class(TForm)
    CbFGColor: TColorBox;
    CbBGColor: TColorBox;
    CbStyle: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    PaintBox1: TPaintBox;
    procedure BrushChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { private declarations }
    FPattern: TBitmap;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BrushChanged(Sender: TObject);
begin
  Paintbox1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPattern := TBitmap.Create;
  FPattern.Transparent := true;
  FPattern.SetSize(4, 4);
  FPattern.Canvas.Brush.Color := clWhite;
  FPattern.canvas.FillRect(0, 0, FPattern.Width, FPattern.Height);
  // Fill pattern   (r = red, b = blue)
  //    r . . .
  //    . . b .
  //    . b b b
  //    . . b .
  FPattern.Canvas.Pixels[0, 0] := clRed;
  FPattern.canvas.Pixels[2, 1] := clBlue;
  FPattern.Canvas.Pixels[1, 2] := clBlue;
  FPattern.Canvas.Pixels[2, 2] := clBlue;
  FPattern.Canvas.Pixels[3, 2] := clBlue;
  FPattern.Canvas.Pixels[2, 3] := clBlue;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPattern.Free;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  with Paintbox1.Canvas do begin
    // First draw background
    if (CbBGColor.Selected = clNone) then
      Brush.Style := bsClear
    else
      Brush.Style := bsSolid;
    Brush.Color := CbBGColor.Selected;
    FillRect(0, 0, Paintbox1.Width, Paintbox1.Height);

    // Then draw fill pattern
    Brush.Style := TBrushStyle(CbStyle.ItemIndex);
    Brush.Color := CbFGColor.Selected;
    if Brush.Style in [bsImage, bsPattern] then
      Brush.Bitmap := FPattern;
    FillRect(0, 0, Paintbox1.Width, Paintbox1.Height);
  end;
end;

end.

