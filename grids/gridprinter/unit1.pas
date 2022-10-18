unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, PrintersDlgs, GridPrn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    cbPageOrientation: TComboBox;
    cbPrintOrder: TComboBox;
    cbMonochrome: TCheckBox;
    cbHeaderLine: TCheckBox;
    cbFooterLine: TCheckBox;
    clbHeaderLinecolor: TColorButton;
    clbFooterLineColor: TColorButton;
    edFooterTextCenter: TEdit;
    edHeaderTextleft: TEdit;
    edHeaderTextCenter: TEdit;
    edFooterTextLeft: TEdit;
    edHeaderTextRight: TEdit;
    edFooterTextRight: TEdit;
    gbHeader: TGroupBox;
    gbFooter: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    PrintDialog1: TPrintDialog;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure cbFooterLineChange(Sender: TObject);
    procedure cbHeaderLineChange(Sender: TObject);
    procedure cbMonochromeChange(Sender: TObject);
    procedure cbPageOrientationChange(Sender: TObject);
    procedure cbPrintOrderChange(Sender: TObject);
    procedure clbHeaderLinecolorColorChanged(Sender: TObject);
    procedure clbFooterLineColorColorChanged(Sender: TObject);
    procedure edFooterTextCenterChange(Sender: TObject);
    procedure edFooterTextLeftChange(Sender: TObject);
    procedure edFooterTextRightChange(Sender: TObject);
    procedure edHeaderTextCenterChange(Sender: TObject);
    procedure edHeaderTextleftChange(Sender: TObject);
    procedure edHeaderTextRightChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    FGridPrinter: TGridPrinter;
    procedure PopulateGrid;
    procedure PopulateGrid_Columns;
    procedure PrinterGetCellText(Sender: TObject; AGrid: TCustomGrid; ACol,
      ARow: Integer; var AText: String);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Printers;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
    FGridPrinter.Print;
end;

procedure TForm1.cbFooterLineChange(Sender: TObject);
begin
  FGridPrinter.FooterLine := cbFooterLine.Checked;
end;

procedure TForm1.cbHeaderLineChange(Sender: TObject);
begin
  FGridPrinter.HeaderLine := cbHeaderLine.Checked;
end;

procedure TForm1.cbMonochromeChange(Sender: TObject);
begin
  FGridPrinter.Monochrome := cbMonochrome.Checked;
end;

procedure TForm1.cbPageOrientationChange(Sender: TObject);
begin
  FGridPrinter.Orientation := TPrinterOrientation(cbPageOrientation.ItemIndex);
end;

procedure TForm1.cbPrintOrderChange(Sender: TObject);
begin
  FGridPrinter.PrintOrder := TGridPrnOrder(cbPrintOrder.ItemIndex);
end;

procedure TForm1.clbHeaderLinecolorColorChanged(Sender: TObject);
begin
  FGridPrinter.HeaderLineColor := clbHeaderLineColor.ButtonColor;
end;

procedure TForm1.clbFooterLineColorColorChanged(Sender: TObject);
begin
  FGridPrinter.FooterLineColor := clbFooterLineColor.ButtonColor;
end;

procedure TForm1.edFooterTextCenterChange(Sender: TObject);
begin
  FGridPrinter.Footer[hfpCenter] := edFooterTextCenter.Text;
end;

procedure TForm1.edFooterTextLeftChange(Sender: TObject);
begin
  FGridPrinter.Footer[hfpLeft] := edFooterTextLeft.Text;
end;

procedure TForm1.edFooterTextRightChange(Sender: TObject);
begin
  FGridPrinter.Footer[hfpRight] := edFooterTextRight.Text;
end;

procedure TForm1.edHeaderTextCenterChange(Sender: TObject);
begin
  FGridPrinter.Header[hfpCenter] := edHeaderTextCenter.Text;
end;

procedure TForm1.edHeaderTextleftChange(Sender: TObject);
begin
  FGridPrinter.Header[hfpLeft] := edHeaderTextLeft.Text;
end;

procedure TForm1.edHeaderTextRightChange(Sender: TObject);
begin
  FGridPrinter.Header[hfpRight] := edHeaderTextRight.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PopulateGrid;
  //PopulateGrid_Columns;

  FGridPrinter := TGridPrinter.Create(self);
  FGridPrinter.Grid := StringGrid1;
  FGridPrinter.BorderLineColor := clRed;
  FGridPrinter.Header[hfpLeft] := 'This is a print test.';
  FGridPrinter.Footer[hfpCenter] := 'Page $PAGE of $PAGECOUNT';
  FgridPrinter.FooterLine := false;
//  FGridPrinter.OnGetCellText := @PrinterGetCellText;
  FGridPrinter.OnPrepareCanvas := @StringGrid1PrepareCanvas;

  edHeaderTextLeft.Text := FGridPrinter.Header[hfpLeft];
  edHeaderTextCenter.Text := FGridPrinter.Header[hfpCenter];
  edHeaderTextRight.Text := FGridPrinter.Header[hfpRight];
  cbHeaderLine.Checked := FGridPrinter.HeaderLine;
  clbHeaderLineColor.ButtonColor := FGridPrinter.HeaderLineColor;

  edFooterTextLeft.Text := FGridPrinter.Footer[hfpLeft];
  edFooterTextCenter.Text := FGridPrinter.Footer[hfpCenter];
  edFooterTextRight.Text := FGridPrinter.Footer[hfpRight];
  cbFooterLine.Checked := FGridPrinter.FooterLine;
  clbFooterLineColor.ButtonColor := FGridPrinter.FooterLineColor;
end;

procedure TForm1.StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  lCanvas: TCanvas;
  ts: TTextStyle;
begin
  if Sender is TStringGrid then
    lCanvas := TStringGrid(Sender).Canvas
  else if Sender is TGridPrinter then
    lCanvas := Printer.Canvas
  else
    exit;

  // Cell col=3/row=3 should have a red background color
  if (aCol = 3) and (aRow = 3) then
    lCanvas.Brush.Color := clRed;

  // Cell col=2 and row=5 contains a string for its background color
  if (aCol = 2) and (aRow = 5) then
    lCanvas.Brush.Color := StringToColorDef(StringGrid1.Cells[2, 5], lCanvas.Brush.Color);

  // Cell col=4/row=4 should have underlined text in "Times New Roman", 18 pt
  if (aCol = 4) and (aRow = 4) then
  begin
    lCanvas.Font.Name := 'Times New Roman';
    lCanvas.Font.Size := 12;
    lCanvas.Font.Style := [fsUnderline];
  end;

  if (aCol = 7) and (aRow = 2) then
  begin
    ts := lCanvas.TextStyle;
    ts.WordBreak := true;
    ts.SingleLine := false;
    lCanvas.TextStyle := ts;
  end;

  // Col=0 should be bold and centered;
  if (aCol = 0) then
  begin
    lCanvas.Font.Style := [fsBold];
    ts := lCanvas.TextStyle;
    ts.Alignment := taCenter;
    lCanvas.TextStyle := ts;
  end;
end;

procedure TForm1.PrinterGetCellText(Sender: TObject; AGrid: TCustomGrid;
  ACol, ARow: Integer; var AText: String);
begin
  if AGrid is TStringGrid then
    AText := TStringGrid(AGrid).Cells[ACol, ARow];
end;

procedure TForm1.PopulateGrid;
var
  r, c: Integer;
begin
  StringGrid1.Columns.Clear;
  StringGrid1.RowCount := 100;
  StringGrid1.ColCount := 20;
  StringGrid1.FixedCols := 2;

  // Populate column headers
  for c := StringGrid1.FixedCols to StringGrid1.ColCount-1 do
    StringGrid1.Cells[c, 0] := 'Column ' + IntToStr(c - StringGrid1.FixedCols + 1);
  // Populate row headers
  for r := 1 to StringGrid1.RowCount-1 do
  begin
    StringGrid1.Cells[0, r] := 'Row ' + IntToStr(r);
    StringGrid1.Cells[1, r] := 'Test';
  end;
  // Populate cells with some dummy text
  for r := 1 to StringGrid1.RowCount-1 do
    for c := 1 to StringGrid1.ColCount-1 do
      StringGrid1.Cells[c, r] := Format('C%d R%d', [c, r]);

  // String in col=2, row=5 is interpreted as cell background color
  StringGrid1.cells[2, 5] := 'clYellow';

  // Long text in col=5 and 6, row=2
  StringGrid1.Cells[5, 2] := StringGrid1.Cells[5, 2] + ' This is a long text.';
  StringGrid1.Cells[6, 2] := StringGrid1.Cells[6, 2] + ' This is a long text.';
  StringGrid1.Cells[7, 2] := StringGrid1.Cells[7, 2] + ' This is a long text.';

  // Different row height in row 2 and 4
  StringGrid1.RowHeights[2] := 2* StringGrid1.DefaultRowHeight;
  StringGrid1.RowHeights[4] := 40;

  // Different col width in col 6
  StringGrid1.ColWidths[6] := 120;
end;

procedure TForm1.PopulateGrid_Columns;
var
  r, c: Integer;
begin
  StringGrid1.RowCount := 100;

  StringGrid1.Columns.Clear;
  for c := 0 to 19 do
    with StringGrid1.Columns.Add do
    begin
      Title.Caption := 'COLUMN ' + IntToStr(c+1);
      if c = 4 then Width := 140 else Width := 90;
      if c = 2 then ButtonStyle := cbsCheckboxColumn;
    end;

  StringGrid1.FixedCols := 2;

  // Populate row headers
  for r := 1 to StringGrid1.RowCount-1 do
  begin
    StringGrid1.Cells[0, r] := 'ROW ' + IntToStr(r);
    StringGrid1.Cells[1, r] := 'Test';
  end;
  // Populate cells with some dummy text
  for r := 1 to StringGrid1.RowCount-1 do
    for c := 0 to StringGrid1.Columns.Count-1 do
      if c = 2 then                        // this is the checkbox column
        StringGrid1.Cells[c+StringGrid1.FixedCols, r] := IntToStr(r mod 3)
      else
        StringGrid1.Cells[c+StringGrid1.FixedCols, r] := Format('C%d R%d', [c, r]);

  // String in col=2, row=5 is interpreted as cell background color
  StringGrid1.cells[2, 5] := 'clYellow';

  // Long text in col=5 and 6, row=2
  StringGrid1.Cells[5, 2] := StringGrid1.Cells[5, 2] + ' This is a long text.';
  StringGrid1.Cells[6, 2] := StringGrid1.Cells[6, 2] + ' This is a long text.';
  StringGrid1.Cells[7, 2] := StringGrid1.Cells[7, 2] + ' This is a long text.';

  // Different row height in row 2 and 4
  StringGrid1.RowHeights[2] := 2*StringGrid1.DefaultRowHeight;
  StringGrid1.RowHeights[4] := 40;
end;

end.

