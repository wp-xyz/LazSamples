unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LvlGraphCtrl,
  TAGraph, TASources, TAIntervalSources,
  TACustomSeries, TASeries, TAFuncSeries, TAExpressionSeries,  TAPolygonSeries,
  TAMultiSeries, TARadialSeries, TADbSource, TAAnimatedSource, TAAxisSource;

type

  TClassDisplay = (cdNone, cdSeries, cdSource);

  { TForm1 }

  TForm1 = class(TForm)
    btnSeriesHierarchy: TButton;
    btnSourceHierarchy: TButton;
    cbStraightenGraph: TCheckBox;
    cbReduceBackEdges: TCheckBox;
    cbHighLevels: TCheckBox;
    ImageList1: TImageList;
    LvlGraphControl1: TLvlGraphControl;
    Panel1: TPanel;
    procedure btnSeriesHierarchyClick(Sender: TObject);
    procedure btnSourceHierarchyClick(Sender: TObject);
    procedure cbStraightenGraphChange(Sender: TObject);
    procedure cbReduceBackEdgesChange(Sender: TObject);
    procedure cbHighLevelsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDisplay: TClassDisplay;
    FRootClassName: String;
    function AddClassNode(AClass: TClass; ARootClassName: String = ''): TLvlGraphNode;
    function GetClassImageIndex(AClassName: String): Integer;
    procedure DefaultLvlGraphSettings;
    procedure RecreateGraph;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  IDX_COMPONENT = 0;
  IDX_AREASERIES = 1;
  IDX_BARSERIES = 2;
  IDX_LINESERIES = 3;
  IDX_PIESERIES = 4;
  IDX_MANHATTANSERIES = 5;
  IDX_OHLCSERIES = 6;
  IDX_BOXWHISKERSERIES = 6;
  IDX_RADIALSERIES = 7;
  IDX_FUNCSERIES = 8;
  IDX_COLORMAPSERIES = 9;
  IDX_SPLINESERIES = 10;
  IDX_BUBBLESERIES = 11;
  IDX_FIELDSERIES = 12;
  IDX_CONSTANTLINE = 13;
  IDX_POINTSERIES = 14;
  IDX_POLYGONSERIES = 22;

  IDX_CALCULATED_CHARTSOURCE = 15;
  IDX_DATETIMEINTERVAL_CHARTSOURCE = 16;
  IDX_DB_CHARTSOURCE = 17;
  IDX_INTERVAL_CHARTSOURCE = 18;
  IDX_LIST_CHARTSOURCE = 19;
  IDX_RANDOM_CHARTSOURCE = 20;
  IDX_USERDEFINED_CHARTSOURCE = 21;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  LvlGraphControl1.Images := ImageList1;
end;


procedure TForm1.cbReduceBackEdgesChange(Sender: TObject);
var
  i: Integer;
begin
  LvlGraphControl1.Clear;

  for i:=0 to LvlGraphControl1.Graph.NodeCount-1 do
    LvlGraphControl1.Graph.Nodes[i].DrawPosition := 0;

  if cbReduceBackEdges.Checked then
    LvlGraphControl1.Options := LvlGraphControl1.Options + [lgoReduceBackEdges]
  else
    LvlGraphControl1.Options := LvlGraphControl1.Options - [lgoReduceBackEdges];

  RecreateGraph;
end;

procedure TForm1.cbHighLevelsChange(Sender: TObject);
var
  i: Integer;
begin
  LvlGraphcontrol1.Clear;

  for i:=0 to LvlGraphControl1.Graph.NodeCount-1 do
    LvlGraphControl1.Graph.Nodes[i].DrawPosition := 0;

  if cbHighLevels.Checked then
    LvlGraphControl1.Options := LvlGraphControl1.Options + [lgoHighLevels]
  else
    LvlGraphControl1.Options := LvlGraphControl1.Options - [lgoHighLevels];

  RecreateGraph;
end;

procedure TForm1.cbStraightenGraphChange(Sender: TObject);
var
  i: Integer;
begin
  LvlGraphControl1.Clear;

  for i:=0 to LvlGraphControl1.Graph.NodeCount-1 do
    LvlGraphControl1.Graph.Nodes[i].DrawPosition := 0;

  if cbStraightenGraph.Checked then
    LvlGraphControl1.Options := LvlGraphControl1.Options + [lgoStraightenGraph]
  else
    LvlGraphControl1.Options := LvlGraphControl1.Options - [lgoStraightenGraph];

  RecreateGraph;
end;

function TForm1.GetClassImageIndex(AClassName: String): Integer;
begin
  case AClassName of
    'TAreaSeries':
      Result := IDX_AREASERIES;
    'TBarSeries':
      Result := IDX_BARSERIES;
    'TBasicPointSeries':
      Result := IDX_POINTSERIES;
    'TBoxAndWhisherSeries':
      Result := IDX_BOXWHISKERSERIES;
    'TBSplineSeries':
      Result := IDX_SPLINESERIES;
    'TBubbleSeries':
      Result := IDX_BUBBLESERIES;
    'TColorMapSeries', 'TCustomColorMapSeries', 'TExpressionColorMapSeries':
       Result := IDX_COLORMAPSERIES;
    'TConstantLine':
      Result := IDX_CONSTANTLINE;
    'TCubicSplineSeries':
      Result := IDX_SPLINESERIES;
    'TFieldSeries':
      Result := IDX_FIELDSERIES;
    'TFitSeries':
      Result := IDX_SPLINESERIES;
    'TFuncSeries', 'TCustomFuncSeries', 'TBasicFuncSeries', 'TExpressionSeries':
      Result := IDX_FUNCSERIES;
    'TLineSeries':
      Result := IDX_LINESERIES;
    'TManhattanSeries':
      Result := IDX_MANHATTANSERIES;
    'TOHLCSeries':
      Result := IDX_OHLCSERIES;
    'TParametricCurveSeries':
      Result := IDX_FUNCSERIES;
    'TPieSeries', 'TCustomPieSeries':
      Result := IDX_PIESERIES;
    'TRadialSeries':
      Result := IDX_RADIALSERIES;
    'TPolygonSeries':
      Result := IDX_POLYGONSERIES;

    'TCalculatedChartSource':
      Result := IDX_CALCULATED_CHARTSOURCE;
    'TDateTimeIntervalChartSource':
      Result := IDX_DATETIMEINTERVAL_CHARTSOURCE;
    'TDbChartSource':
      Result := IDX_DB_CHARTSOURCE;
    'TIntervalChartSource':
      Result := IDX_INTERVAL_CHARTSOURCE;
    'TListChartSource':
      Result := IDX_LIST_CHARTSOURCE;
    'TRandomChartSource':
      Result := IDX_RANDOM_CHARTSOURCE;
    'TUserDefinedChartSource':
      Result := IDX_USERDEFINED_CHARTSOURCE;

    else
      Result := IDX_COMPONENT;
  end;
end;

function TForm1.AddClassNode(AClass: TClass; ARootClassName: String = ''): TLvlGraphNode;
var
  parentclass: TClass;
begin
  if ARootClassName <> '' then
    FRootClassName := ARootClassName;
  Result := LvlGraphControl1.Graph.GetNode(AClass.ClassName, true);
  Result.ImageIndex := GetClassImageIndex(AClass.ClassName);
  if AClass.ClassName = FRootClassName then
    exit;
  parentclass := AClass.ClassParent;
  AddClassNode(parentclass);
  LvlGraphControl1.Graph.GetEdge(parentclass.ClassName, AClass.ClassName, true);
end;

procedure TForm1.btnSeriesHierarchyClick(Sender: TObject);
begin
  FDisplay := cdSeries;

  LvlGraphControl1.Clear;
  AddClassNode(TBasicPointSeries, 'TBasicChartSeries');
  AddClassNode(TLineSeries);
  AddClassNode(TAreaSeries);
  AddClassNode(TBSplineSeries);
  AddClassNode(TCubicSplineSeries);
  AddClassNode(TFitSeries);
  AddClassNode(TBarSeries);
  AddClassNode(TPolarSeries);
  AddClassNode(TOpenHighLowCloseSeries);
  AddClassNode(TBoxAndWhiskerSeries);
  AddClassNode(TBubbleSeries);
  AddClassNode(TManhattanSeries);
  AddClassNode(TFieldSeries);
  AddClassNode(TPieSeries);
  AddClassNode(TConstantLine);
  AddClassNode(TParametricCurveSeries);
  AddClassNode(TFuncSeries);
  AddClassNode(TExpressionSeries);
  AddClassNode(TColorMapSeries);
  AddClassNode(TExpressionColorMapSeries);
  AddClassNode(TPolygonSeries);

  DefaultLvlGraphSettings;
end;

procedure TForm1.btnSourceHierarchyClick(Sender: TObject);
begin
  FDisplay := cdSource;

  LvlGraphControl1.Clear;
  AddClassNode(TDateTimeIntervalChartSource, 'TBasicChartSource');
  AddClassNode(TListChartSource);
  AddClassNode(TRandomChartSource);
  AddClassNode(TUserDefinedChartSource);
  AddClassNode(TCalculatedChartSource);
  AddClassNode(TDbChartSource);
  AddClassNode(TIntervalChartSource);
  AddClassNode(TCustomAnimatedChartSource);
  AddClassNode(TCustomAxisChartSource);

  DefaultLvlGraphSettings;
end;

procedure TForm1.DefaultLvlGraphSettings;
begin
  with LvlGraphControl1 do begin
    NodeStyle.Shape := lgnsNone;
    NodeStyle.Width := 15;
    NodeStyle.GapTop := 12;
    NodeStyle.GapBottom := 0;
    NodeStyle.CaptionPosition := lgncBottom;
    NodeStyle.CaptionScale := 1.0;

    EdgeStyle.Shape := lgesStraight;

    Limits.MaxLevelHeightAbs := MaxInt;

    Font.Size:= 10;
  end;
end;

procedure TForm1.RecreateGraph;
begin
  case FDisplay of
    cdNone: ;
    cdSeries: btnSeriesHierarchyClick(nil);
    cdSource: btnSourceHierarchyClick(nil);
  end;
end;

end.

