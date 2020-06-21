unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TAGraph, TASeries, TAFuncSeries, Math,
  utypes, uerrors, unlfit;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    btnFit: TButton;
    btnCreateData: TButton;
    Chart1: TChart;
    cbFirstPeak: TCheckBox;
    cbSecondPeak: TCheckBox;
    FittedSeries1: TFuncSeries;
    FittedSeries2: TFuncSeries;
    cmbAlgorithm: TComboBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit13: TEdit;
    Edit12: TEdit;
    Edit11: TEdit;
    edNumPoints: TEdit;
    edIterations: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    edNoise: TEdit;
    Edit7: TEdit;
    Edit6: TEdit;
    Edit5: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    FittedSeries: TFuncSeries;
    DataSeries: TLineSeries;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    lblNumPoints: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    Panel1: TPanel;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure btnFitClick(Sender: TObject);
    procedure btnCreateDataClick(Sender: TObject);
    procedure cbFirstPeakChange(Sender: TObject);
    procedure FittedSeries1Calculate(const AX: Double; out AY: Double);
    procedure FittedSeries2Calculate(const AX: Double; out AY: Double);
    procedure FittedSeriesCalculate(const AX: Double; out AY: Double);
    procedure FormCreate(Sender: TObject);
  private
    XData, YData: TVector;
    FitParams: TVector;
    DataOK: boolean;
    procedure CalcFit;
    procedure CreateData;
    procedure DisplayResults(B: TVector);
    function ValidFloat(AEdit: TEdit; out AValue: Double): Boolean;
    function ValidInt(AEdit: TEdit; out AValue: Integer): Boolean;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function GaussFunc1(x: Double; a, b, c: Double): Double;
begin
  Result := a * exp( -sqr(x - b) / (2 * sqr(c)));
end;

function GaussFunc(x: double; b: TVector): Double;
begin
  Result := GaussFunc1(x, b[0], b[1], b[2]);
  if Length(b) = 6 then
    Result := Result + GaussFunc1(x, b[3], b[4], b[5]);
end;

procedure DerivGaussProc1(x, y: Double; a, b, c: Double; out da, db, dc: Double);
begin
  da := y / a;                          // partial derivative dy/da
  db := (x - b) / sqr(c) * y;           // partial derivative dy/db
  dc := sqr(x - b) * y / (c * c * c);   // partial derivative dy/dc
end;

procedure DerivGaussProc(x, y: Double; b, d: TVector);
begin
  DerivGaussProc1(x, y, b[0], b[1], b[2], d[0], d[1], d[2]);
  if Length(b) = 6 then
    DerivGaussProc1(x, y, b[3], b[4], b[5], d[3], d[4], d[5]);
end;

procedure TForm1.btnFitClick(Sender: TObject);
begin
  CalcFit;
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  btnFit.Enabled := dataOK;
end;

procedure TForm1.btnCreateDataClick(Sender: TObject);
begin
  CreateData;
end;

procedure TForm1.CalcFit;
var
  B: TVector;
  V: TMatrix;
  i: Integer;
  np: Integer;
  msg: String;
  nIt: Integer;
begin
  np := 0;
  SetLength(FitParams, 0);
  if cbFirstPeak.Checked then
  begin
    SetLength(FitParams, Length(FitParams) + 3);
    if not ValidFloat(Edit8, FitParams[np+0]) then exit;
    if not ValidFloat(Edit9, FitParams[np+1]) then exit;
    if not ValidFloat(Edit10, FitParams[np+2]) then exit;
    np := 3;
  end;

  if cbSecondPeak.Checked then
  begin
    SetLength(Fitparams, Length(FitParams) + 3);
    if not ValidFloat(Edit11, FitParams[np+0]) then exit;
    if not ValidFloat(Edit12, FitParams[np+1]) then exit;
    if not ValidFloat(Edit13, FitParams[np+2]) then exit;
  end;

  if not ValidInt(edIterations, nIt) then
    exit;

  np := Length(FitParams);
  SetLength(B, np);
  SetLength(V, np, np);

  // Initial guessed parameters
  for i := 0 to np-1 do B[i] := FitParams[i];

  SetOptAlgo(TOptAlgo(cmbAlgorithm.ItemIndex));
  NLFit(@GaussFunc, @DerivGaussProc, XData, YData, Low(XData), High(XData), nIt, 1E-3, B, 0, np-1, V);

  if MathErr = matOK then
    DisplayResults(B)
  else begin
    B := nil;
    FittedSeries.Active := false;
    FittedSeries1.Active := false;
    FittedSeries2.Active := false;
    msg := ErrorMessage[mathErr];
    Listbox1.Items.Clear;
    Listbox1.Items.Add(msg);
  end;
end;

procedure TForm1.cbFirstPeakChange(Sender: TObject);
begin
  Edit1.Visible := cbFirstPeak.Checked;
  Edit2.Visible := cbFirstPeak.Checked;
  Edit3.Visible := cbFirstPeak.Checked;
  Label1.Visible := cbFirstPeak.Checked;
  Label2.Visible := cbFirstPeak.Checked;
  Label3.Visible := cbFirstPeak.Checked;

  Edit5.Visible := cbSecondPeak.Checked;
  Edit6.Visible := cbSecondPeak.Checked;
  Edit7.Visible := cbSecondPeak.Checked;
  Label5.Visible := cbSecondPeak.Checked;
  Label6.Visible := cbSecondPeak.Checked;
  Label7.Visible := cbSecondPeak.Checked;

  Edit8.Visible := cbFirstPeak.Checked;
  Edit9.Visible := cbFirstPeak.Checked;
  Edit10.Visible := cbFirstPeak.Checked;
  Label8.Visible := cbFirstPeak.Checked;
  Label9.Visible := cbFirstPeak.Checked;
  Label10.Visible := cbFirstPeak.Checked;
  Label15.Visible := cbFirstPeak.Checked;

  Edit11.Visible := cbSecondPeak.Checked;
  Edit12.Visible := cbSecondPeak.Checked;
  Edit13.Visible := cbSecondPeak.Checked;
  Label11.Visible := cbSecondPeak.Checked;
  Label12.Visible := cbSecondPeak.Checked;
  Label13.Visible := cbSecondPeak.Checked;
  Label16.Visible := cbSecondPeak.Checked;
end;

procedure TForm1.DisplayResults(B: TVector);
var
  peak: Integer;
begin
  if B = nil then begin
    FittedSeries.Active := false;
    ListBox1.Items.Clear;
    exit;
  end;

  SetLength(FitParams, Length(B));
  Move(B[0], FitParams[0], Length(B) * SizeOf(B[0]));

  Listbox1.Items.Clear;
  if cbFirstPeak.Checked then peak := 1 else peak := 2;
  Listbox1.Items.Add('a%d = %.6f', [peak, FitParams[0]]);
  Listbox1.Items.Add('b%d = %.6f', [peak, FitParams[1]]);
  Listbox1.Items.Add('c%d = %.6f', [peak, FitParams[2]]);

  if Length(B) = 6 then
  begin
    peak := 2;
    Listbox1.Items.Add('a%d = %.6f', [peak, FitParams[3]]);
    Listbox1.Items.Add('b%d = %.6f', [peak, FitParams[4]]);
    Listbox1.Items.Add('c%d = %.6f', [peak, FitParams[5]]);
  end;

  FittedSeries.Active := true;
  FittedSeries1.Active := cbFirstPeak.Checked and cbSecondPeak.Checked;
  Fittedseries2.Active := cbFirstPeak.Checked and cbSecondPeak.Checked;
end;

procedure TForm1.FittedSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := GaussFunc1(AX, FitParams[0], FitParams[1], FitParams[2]);
end;

procedure TForm1.FittedSeries2Calculate(const AX: Double; out AY: Double);
begin
  AY := GaussFunc1(AX, FitParams[3], FitParams[4], FitParams[5]);
end;

procedure TForm1.FittedSeriesCalculate(const AX: Double; out AY: Double);
begin
  AY := GaussFunc(AX, FitParams);
end;

function TForm1.ValidFloat(AEdit: TEdit; out AValue: Double): Boolean;
var
  fs: TFormatSettings;
begin
  Result := TryStrToFloat(AEdit.Text, AValue);
  if Result then
    exit;

  fs := DefaultFormatSettings;
  if fs.DecimalSeparator = '.' then fs.DecimalSeparator := ',' else fs.decimalSeparator := '.';
  Result := TryStrToFloat(AEdit.Text, AValue, fs);

  if not Result then
  begin
    AEdit.SetFocus;
    ShowMessage('No valid number.');
  end;
end;

function TForm1.ValidInt(AEdit: TEdit; out AValue: Integer): Boolean;
var
  fs: TFormatSettings;
begin
  Result := TryStrToInt(AEdit.Text, AValue);
  if Result then
    exit;

  if not Result then
  begin
    AEdit.SetFocus;
    ShowMessage('No valid number.');
  end;
end;

procedure TForm1.CreateData;
var
  i, n: Integer;
  x, y: Double;
  xmin, xmax: Double;
  noise: Double;
  b: TVector;
  np: Integer;
begin
  DataOK := false;

  if not cbFirstPeak.Checked and not cbSecondPeak.Checked then
  begin
    MessageDlg('Please select at least 1 peak.', mtError, [mbOK], 0);
    exit;
  end;

  xmin := 1E308;
  xmax := -1E308;

  np := 0;
  SetLength(b, 0);
  if cbFirstPeak.Checked then
  begin
    SetLength(b, Length(b) + 3);
    if not ValidFloat(Edit1, b[np+0]) then exit;
    if not ValidFloat(Edit2, b[np+1]) then exit;
    if not ValidFloat(Edit3, b[np+2]) then exit;
    xmin := b[np+1] - 5*b[np+2];
    xmax := b[np+1] + 5*b[np+2];
    np := 3;
  end;

  if cbSecondPeak.checked then
  begin
    SetLength(b, Length(b) + 3);
    if not ValidFloat(Edit5, b[np+0]) then exit;
    if not ValidFloat(Edit6, b[np+1]) then exit;
    if not ValidFloat(Edit7, b[np+2]) then exit;
    x := b[np+1] - 5 * b[np+2];
    if x < xmin then xmin := x;
    x := b[np+1] + 5 * b[np+2];
    if x > xmax then xmax := x;
  end;

  if not ValidInt(edNumPoints, n) then
    exit;
  if not ValidFloat(edNoise, noise) then
    exit;

  SetLength(XData, N);
  SetLength(YData, N);
  for i := 0 to N-1 do begin
    x := xmin + i / (N-1) * (xmax - xmin);
    y := GaussFunc(x, b);
    XData[i] := x;
    YData[i] := y + RandG(0, noise); //RanGauss(0, noise);
  end;

  DataSeries.Clear;
  for i := 0 to N-1 do
    DataSeries.AddXY(XData[i], YData[i]);

  FittedSeries.Active := false;
  FittedSeries1.Active := false;
  FittedSeries2.Active := false;
  DataOK := true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DefaultFormatSettings.DecimalSeparator := '.';
  CreateData;
end;

end.

