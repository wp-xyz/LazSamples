unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    sbRed: TScrollBar;
    sbGreen: TScrollBar;
    sbBlue: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure sbColorChange(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  FPImage, IntfGraphics, LCLType;

{ TForm1 }

function Distance2(i, j: Integer; P: TPoint): Integer;
begin
  Result := sqr(i-P.X) + sqr(j-P.Y);
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  P1, P2, P3: TPoint;
  R, Rsq: Integer;
  bmp: TBitmap;
  img: TLazIntfImage;
  HBmp, HMask: HBitmap;
  clr: TFPColor;
  i, j: Integer;
begin
  R := Paintbox1.Width div 3;
  Rsq := R*R;
  P1 := Point(R, R);
  P2 := Point(Paintbox1.Width - R, R);
  P3 := Point(Paintbox1.Width div 2, PaintBox1.Height - R);

  bmp := TBitmap.Create;
  try
    bmp.SetSize(Paintbox1.Width, Paintbox1.Height);
    bmp.PixelFormat := pf24bit;
    bmp.Canvas.Brush.Color := clBlack;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);

    img := bmp.CreateIntfImage;
    try
      for j := 0 to img.Height-1 do
        for i := 0 to img.Width - 1 do
        begin
          clr := img.Colors[i, j];
          if Distance2(i, j, P1) <= Rsq then
            clr.Red := sbRed.Position shl 8;
          if Distance2(i, j, P2) <= Rsq then
            clr.Green := sbGreen.Position shl 8;
          if Distance2(i, j, P3) <= Rsq then
            clr.Blue := sbBlue.Position shl 8;
          img.Colors[i, j] := clr;
        end;
      img.CreateBitmaps(HBmp, HMask);
      bmp.BitmapHandle := HBmp;
      bmp.MaskHandle := HMask;
    finally
      img.Free;
    end;
    Paintbox1.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  sbColorChange(nil);
end;

procedure TForm1.sbColorChange(Sender: TObject);
begin
  Paintbox1.Invalidate;
  Panel1.Color := RGBToColor(sbRed.Position, sbGreen.Position, sbBlue.Position);
  Panel1.Caption := Format('#%.2x%.2x%.2x', [sbRed.Position, sbGreen.Position, sbBlue.Position]);
  if sbRed.Position + sbGreen.Position + sbBlue.Position > 3*128 then
    Panel1.Font.Color := clBlack
  else
    Panel1.Font.Color := clWhite;
end;

end.

