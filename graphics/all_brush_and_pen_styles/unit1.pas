unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes,ComCtrls,ExtCtrls,SysUtils,Forms,Controls,FPCanvas,Graphics,Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    PageControl1:TPageControl;
    PaintBox1:TPaintBox;
    PaintBox2:TPaintBox;
    TabSheet1:TTabSheet;
    TabSheet2:TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender:TObject);
    procedure PaintBox2Paint(Sender:TObject);
  private
    FImage: TCustomBitmap;
    FPattern: TCustomBitmap;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLType,
  Types, TypInfo;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  PatternBits: array[0..7] of Word = (
    %00010000,
    %00111000,
    %01101100,
    %11000110,
    %01101100,
    %00111000,
    %00010000,
    %00000001
  );
begin
  FImage := TPortableNetworkGraphic.Create;
  FImage.LoadFromFile('debugger.png');

  FPattern := TBitmap.Create;
  FPattern.SetHandles(CreateBitmap(8, 8, 1, 1, @PatternBits), 0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPattern.Free;
  FImage.Free;
end;

procedure TForm1.PaintBox1Paint(Sender:TObject);
var
  R: TRect;
  bs: TBrushStyle;
  styleName: String;
  x, y, h: Integer;
begin
  R := Rect(8, 8, 68, 68);
  h := Paintbox1.Canvas.TextHeight('Tg');

  // Set fill color
  Paintbox1.Canvas.Brush.Color := clRed;   // Must be set before Brush.Style.

  // Set border color
  Paintbox1.Canvas.Pen.Color := clBlack;

  // Iterate over all available brush styles
  for bs in TBrushStyle do
  begin
    // Fill a rectangle with the selected brush
    case bs of
      bsImage   : Paintbox1.Canvas.Brush.Bitmap := FImage;
      bsPattern : begin
                    Paintbox1.Canvas.Brush.Bitmap := FPattern;
                    Paintbox1.Canvas.Font.Color := clRed;
                  end;
      else        Paintbox1.Canvas.Brush.Style := bs;   // Set brush style
    end;
    // Set background color
    SetBkMode(Paintbox1.Canvas.Handle, OPAQUE);
    SetBkColor(Paintbox1.Canvas.Handle, clYellow);
    // Draw rectangle
    Paintbox1.Canvas.Rectangle(R);
    // Get name of brush style and draw it below the rectangle
    styleName := GetEnumName(TypeInfo(TBrushStyle), ord(bs));
    x := (R.Left + R.Right - Paintbox1.Canvas.TextWidth(styleName)) div 2;
    y := R.Bottom + 4;
    // No text background
    SetBkMode(Paintbox1.Canvas.Handle, TRANSPARENT);
    // Draw the brush name
    Paintbox1.Canvas.Font.Color := clBlack;
    Paintbox1.Canvas.TextOut(x, y, styleName);
    // Position the rectangle for next brush
    if ord(bs) = ord(High(bs)) div 2 then
      OffsetRect(R, -R.Left + 8, R.Height + 16 + h)
    else
      OffsetRect(R, R.Width + 8, 0);
  end;
end;

procedure TForm1.PaintBox2Paint(Sender:TObject);
var
  ps: TPenStyle;
  styleName: String;
  x, y, w, h: Integer;
  PenPattern: TPenPattern = nil;
begin
  h := Paintbox2.Canvas.TextHeight('Tg');

  SetLength(PenPattern, 6);
  PenPattern[0] := 4;  // line
  PenPattern[1] := 4;  // space
  PenPattern[2] := 8;  // line
  PenPattern[3] := 8;  // space
  PenPattern[4] := 16; // line
  PenPattern[5] := 16; // space

  // Iterate over all available pen styles
  x := 100;
  y := h div 2;
  Paintbox2.Canvas.Pen.Width := 3;
  for ps in TPenStyle do
  begin
    // Get name of brush style and draw it at the left of the line sample
    styleName := GetEnumName(TypeInfo(TPenStyle), ord(ps));
    w := Paintbox2.Canvas.TextWidth(styleName);
    // No text background
    SetBkMode(Paintbox2.Canvas.Handle, TRANSPARENT);
    // Draw the pen name
    Paintbox2.Canvas.TextOut(x- w - 8, y, styleName);

    // Draw the line sample
    Paintbox2.Canvas.Pen.Style := ps;
    if ps = psPattern then
      Paintbox2.Canvas.Pen.SetPattern(PenPattern);
    Paintbox2.Canvas.Line(x, y + h div 2, Paintbox2.ClientWidth - 8, y + h div 2);

    // next line
    inc(y, h+8);
  end;
end;


end.

