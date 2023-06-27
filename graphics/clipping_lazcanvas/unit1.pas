unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure RedrawHandler(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, LCLVersion,
  FPImage, FPCanvas, IntfGraphics, LazCanvas, LazRegions;

const
  CHECKER_BOARD: TBrushPattern = (
    $FFFF0000, $FFFF0000, $FFFF0000, $FFFF0000, $FFFF0000, $FFFF0000, $FFFF0000, $FFFF0000,
    $FFFF0000, $FFFF0000, $FFFF0000, $FFFF0000, $FFFF0000, $FFFF0000, $FFFF0000, $FFFF0000,
    $0000FFFF, $0000FFFF, $0000FFFF, $0000FFFF, $0000FFFF, $0000FFFF, $0000FFFF, $0000FFFF,
    $0000FFFF, $0000FFFF, $0000FFFF, $0000FFFF, $0000FFFF, $0000FFFF, $0000FFFF, $0000FFFF
  );
var
  FillImg: TFPCustomImage = nil;

{ TForm1 }

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  bmp: TBitmap;
  img: TLazIntfImage;
  cnv: TLazCanvas;
  rgn: TLazRegion;
  Rc: TRect;
  pc: array of TPoint = nil;
  p: array of TPoint = nil;
  w2: Integer;
  h2: Integer;
begin
  RandSeed := 1;

  // Shape to be clipped, here: a polygon with inner hole which is filled or not, depending on the filling rule
  w2 := Paintbox1.Width div 2;
  h2 := Paintbox1.Height div 2;
  SetLength(p, 5);
  P[0] := Point(w2, 10);
  P[1] := Point(w2 div 3, PaintBox1.Height - 10);
  P[2] := Point(Paintbox1.Width - 10, h2 * 2 div 3);
  P[3] := Point(10, h2 * 2 div 3);
  P[4] := Point(Paintbox1.Width - w2 div 3, Paintbox1.Height - 10);

  // Clipping geometry
  Rc := Rect(10, 10, Paintbox1.Width-100, Paintbox1.Height - 100);
  SetLength(pc, 3);
  pc[0] := Point(Paintbox1.Width - 10, 10);
  pc[1] := Point(Paintbox1.Width - 10, Paintbox1.Height - 10);
  pc[2] := Point(10, Paintbox1.Top + Paintbox1.Height div 2);

  // Buffer bitmap
  bmp := TBitmap.Create;
  try
    bmp.SetSize(Paintbox1.Width, Paintbox1.Height);
    img := bmp.CreateIntfImage;
    try
      img.FillPixels(colWhite);
      cnv := TLazCanvas.Create(img);

      // Draw outline of clipping region
      cnv.Brush.Style := bsClear;
      if Checkbox1.Checked then
      begin
        cnv.Pen.Style := psDot;
        case Combobox1.ItemIndex of
          0: ;
          1: cnv.Rectangle(Rc);
          2: cnv.Ellipse(Rc);
          3: cnv.Polygon(pc);
        end;
      end;

      // Draw outline of polygon to be clipped
      if Checkbox2.Checked then
      begin
        cnv.Pen.Style := psSolid;
        cnv.Polygon(p);
      end;

      // Create the clipping region
      rgn := TLazRegion.Create;
      try
        case Combobox1.ItemIndex of
          0: FreeAndNil(rgn);
          1: rgn.AddRectangle(Rc);
          2: rgn.AddEllipse(Rc.Left, Rc.Top, Rc.Right, Rc.Bottom);
          3: rgn.AddPolygon(pc, rfmOddEven);
        end;
        if Assigned(rgn) then
          cnv.SetLazClipRegion(rgn);

        // Draw the polygon to be clipped
        cnv.Brush.FPColor := colRed;
        cnv.Brush.Style := TBrushStyle(Combobox2.ItemIndex);
        if cnv.Brush.Style = bsPattern then
          cnv.Brush.Pattern := CHECKER_BOARD
        else
        if cnv.Brush.Style = bsImage then
        begin
          cnv.Brush.Image := FillImg;
          cnv.Pen.FPColor := colWhite;
        end else
          cnv.Brush.Image := nil;
        cnv.Pen.Style := psSolid;
        cnv.Pen.FPColor := colBlack;
        cnv.Polygon(p {$IF LCL_FullVersion >= 2030000}, RadioButton2.Checked{$ENDIF});
      finally
        cnv.Free;
      end;
      bmp.LoadFromIntfImage(img);
      Paintbox1.Canvas.Draw(0, 0, bmp);
    finally
      img.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  png: TCustomBitmap;
begin
  png := TPortableNetworkGraphic.Create;
  try
    png.Clear;
    png.LoadFromFile('paw-red.png');
    FillImg := png.CreateIntfImage;
  finally
    png.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FillImg.Free;
end;

procedure TForm1.RedrawHandler(Sender: TObject);
begin
  Paintbox1.Invalidate;
end;

end.

