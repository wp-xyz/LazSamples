unit Unit1;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  LCLVersion, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph,
  TATransformations, TASources, TAChartAxisUtils, TASeries;

{$IF LCL_FullVersion >= 3990000}
 {$DEFINE NEEDS_MINORMARKS_SOURCE}
{$ENDIF}

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    LeftAxisTransformations: TChartAxisTransformations;
    LeftAxisLogTransform: TLogarithmAxisTransform;
    BottomAxisTransformations: TChartAxisTransformations;
    BottomAxisLogTransform: TLogarithmAxisTransform;
    MajorMarksChartSource: TListChartSource;
    xMinorMarksSource: TListChartSource;
    yMinorMarksSource: TListChartSource;
    procedure FormCreate(Sender: TObject);
  private
    procedure GetMarkTextHandler(Sender: TObject; var AText: String; AMark: Double);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math,
  TAChartUtils, TATypes, TACustomSource, TATextElements;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  XMIN = 100;
  XMAX = 10*1000;
var
  x, y: Double;
  i: Integer;
  {$IFDEF NEEDS_MINORMARKS_SOURCE}
  xm: Double;
  j: Integer;
  {$ENDIF}
begin
  // Prepare the axis marks
  x := 1.0;
  for i := 0 to 10 do
  begin
    MajorMarksChartSource.Add(x, x);
    x := x * 10.0;
    {$IFDEF NEEDS_MINORMARKS_SOURCE}
    for j := 2 to 9 do
    begin
      xm := x * j;
      xMinorMarksSource.Add(xm, xm, IntToStr(j));
      if j in [7, 9] then
        // Do not label the upper y marks per decade; they are too close to each other.
        yMinorMarksSource.Add(xm, xm)
      else
        yMinorMarksSource.Add(xm, xm, IntToStr(j));
    end;
    {$ENDIF}
  end;

  // Setup the y axis
  Chart1.LeftAxis.Title.Caption := 'y Axis';
  Chart1.LeftAxis.Title.Visible := true;
  Chart1.LeftAxis.Title.LabelFont.Size := 12;
  Chart1.LeftAxis.Title.LabelFont.Style := [fsBold];
  Chart1.LeftAxis.Marks.Source := MajorMarksChartSource;
  Chart1.LeftAxis.Marks.Style := smsValue;
  Chart1.LeftAxis.Marks.Distance := 10;
  Chart1.LeftAxis.Marks.TextFormat := tfHTML;
  Chart1.LeftAxis.Marks.LabelFont.Size := 11;
  Chart1.LeftAxis.Grid.Color := clSilver;
  Chart1.LeftAxis.Grid.Style := psSolid;
  Chart1.LeftAxis.OnGetMarkText := @GetMarkTextHandler;
  with Chart1.LeftAxis.Minors.Add do begin
    Intervals.Count := 9;                    // fixed number of 9 minor intervals
    Intervals.Options := [aipUseCount];
    Grid.Color := clSilver;
    {$IFDEF NEEDS_MINORMARKS_SOURCE}
    Marks.Source := yMinorMarksSource;
    Marks.Style := smsLabel;
    {$ELSE}
    Marks.Style := smsValue;
    Marks.OverlapPolicy := opHideNeighbour;
    {$ENDIF}
    Marks.Distance := 4;
    Marks.LabelFont.Size := 7;
  end;

  // Set up the x axis
  Chart1.BottomAxis.Range.Min := XMIN;
  Chart1.BottomAxis.Range.Max := XMAX;
  Chart1.BottomAxis.Range.UseMin := true;
  Chart1.BottomAxis.Range.UseMax := true;

  Chart1.BottomAxis.Title.Caption := 'x Axis';
  Chart1.BottomAxis.Title.Visible := true;
  Chart1.BottomAxis.Title.LabelFont.Size := 12;
  Chart1.BottomAxis.Title.LabelFont.Style := [fsBold];
  Chart1.BottomAxis.Marks.Source := MajorMarksChartSource;
  Chart1.BottomAxis.Marks.Style := smsValue;
  Chart1.BottomAxis.Marks.Distance := 8;  // avoid overlap with minors
  Chart1.BottomAxis.Marks.TextFormat := tfHTML;
  Chart1.BottomAxis.Marks.LabelFont.Size := 11;
  Chart1.BottomAxis.Grid.Color := clSilver;
  Chart1.BottomAxis.Grid.Style := psSolid;
  Chart1.BottomAxis.OnGetMarkText := @GetMarkTextHandler;
  with Chart1.BottomAxis.Minors.Add do begin
    Intervals.Count := 9;                  // fixed number of 9 minor intervals
    Intervals.Options := [aipUseCount];
    Grid.Color := clSilver;
    {$IFDEF NEEDS_MINORMARKS_SOURCE}
    Marks.Source := xMinorMarksSource;
    Marks.Style := smsLabel;
    {$ELSE}
    Marks.Style := smsValue;
    Marks.OverlapPolicy := opHideNeighbour;
    {$ENDIF}
    Marks.LabelFont.Size := 7;
  end;

  // Setup the series
  Chart1LineSeries1.AxisIndexX := Chart1.BottomAxis.Index;      // IMPORTANT!
  Chart1LineSeries1.AxisIndexY := Chart1.LeftAxis.Index;        // IMPORTANT!
  Chart1LineSeries1.Pointer.Brush.Color := clBlue;
  Chart1LineSeries1.Pointer.HorizSize := 4;
  Chart1LineSeries1.Pointer.VertSize := 4;
  Chart1LineSeries1.Pointer.Style := psCircle;
  Chart1LineSeries1.ShowLines := false;
  Chart1LineSeries1.ShowPoints := true;

  // Generate data for plotting
  for i := 0 to 100 do
  begin
    x := Random * (XMAX - XMIN) + XMIN;
    y := x*x;
    y := y*RandG(1, 0.1);          // Add 10% noise
    Chart1LineSeries1.AddXY(x, y);
  end;
end;

{ Display main axis labels as powers of 10: e.g. 10², 10³, ...
  Marks.TextFormat must be set to tfHTML to achieve this effect. }
procedure TForm1.GetMarkTextHandler(Sender: TObject; var AText: String;
  AMark: Double);
begin
  {$IFDEF NEEDS_MINORMARKS_SOURCE}
  AText := Format('10<sup>%d</sup>', [round(Log10(AMark))]);
  {$ENDIF}
end;

end.

