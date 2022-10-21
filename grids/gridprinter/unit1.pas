unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, ComCtrls, Spin, Buttons, PrintersDlgs, GridPrn, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    btnPrintPreviewform: TButton;
    cbDefaultFixedCellsDividerLineColor: TCheckBox;
    cbDefaultGridLineColor: TCheckBox;
    cbPageOrientation: TComboBox;
    cbPrintOrder: TComboBox;
    cbMonochrome: TCheckBox;
    cbHeaderLine: TCheckBox;
    cbFooterLine: TCheckBox;
    cbDefaultBorderLinecolor: TCheckBox;
    clbFixedCellsDividerLineColor: TColorButton;
    clbGridLineColor: TColorButton;
    clbHeaderLinecolor: TColorButton;
    clbFooterLineColor: TColorButton;
    cmbPercent: TComboBox;
    clbBorderLineColor: TColorButton;
    edHeaderText: TEdit;
    edFooterText: TEdit;
    FontDialog1: TFontDialog;
    gbFixedCellsDividerLine: TGroupBox;
    gbGridLines: TGroupBox;
    gbHeader: TGroupBox;
    gbFooter: TGroupBox;
    gbBorderLine: TGroupBox;
    Image1: TImage;
    ImageList1: TImageList;
    lblHeaderText: TLabel;
    lblFooterText: TLabel;
    lblBorderLineWidth: TLabel;
    lblFixedCellsDividerLineWidth: TLabel;
    lblGridLineWidth: TLabel;
    PageInfo: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    PrintDialog1: TPrintDialog;
    ScrollBox1: TScrollBox;
    seBorderLineWidth: TSpinEdit;
    seFixedCellsDividerLineWidth: TSpinEdit;
    seGridLineWidth: TSpinEdit;
    sbHeaderFont: TSpeedButton;
    sbFooterFont: TSpeedButton;
    StringGrid1: TStringGrid;
    pgGrid: TTabSheet;
    pgPreview: TTabSheet;
    ToolBar1: TToolBar;
    tbFirstPage: TToolButton;
    tbPrevPage: TToolButton;
    tbNextPage: TToolButton;
    tbLastPage: TToolButton;
    tbPrint: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    tbZoomIn: TToolButton;
    tbZoomOut: TToolButton;
    ToolButton5: TToolButton;
    procedure btnPrintPreviewformClick(Sender: TObject);
    procedure cbDefaultFixedCellsDividerLineColorChange(Sender: TObject);
    procedure cbDefaultGridLineColorChange(Sender: TObject);
    procedure cbFooterLineChange(Sender: TObject);
    procedure cbHeaderLineChange(Sender: TObject);
    procedure cbMonochromeChange(Sender: TObject);
    procedure cbPageOrientationChange(Sender: TObject);
    procedure cbPrintOrderChange(Sender: TObject);
    procedure cbDefaultBorderLinecolorChange(Sender: TObject);
    procedure clbBorderLineColorColorChanged(Sender: TObject);
    procedure clbFixedCellsDividerLineColorColorChanged(Sender: TObject);
    procedure clbGridLineColorColorChanged(Sender: TObject);
    procedure clbHeaderLinecolorColorChanged(Sender: TObject);
    procedure clbFooterLineColorColorChanged(Sender: TObject);
    procedure cmbPercentEditingDone(Sender: TObject);
    procedure edFooterTextChange(Sender: TObject);
    procedure edHeaderTextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PageControl1Change(Sender: TObject);
    procedure seBorderLineWidthChange(Sender: TObject);
    procedure seFixedCellsDividerLineWidthChange(Sender: TObject);
    procedure seGridLineWidthChange(Sender: TObject);
    procedure sbHeaderFontClick(Sender: TObject);
    procedure sbFooterFontClick(Sender: TObject);
    procedure StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure tbFirstPageClick(Sender: TObject);
    procedure tbLastPageClick(Sender: TObject);
    procedure tbNextPageClick(Sender: TObject);
    procedure tbPrevPageClick(Sender: TObject);
    procedure tbPrintClick(Sender: TObject);
    procedure tbZoomInClick(Sender: TObject);
    procedure tbZoomOutClick(Sender: TObject);
  private
    FGridPrinter: TGridPrinter;
    FPageNo: Integer;
    FZoom: Integer;
    procedure PopulateGrid;
    procedure PopulateGrid_Columns;
    procedure PrinterGetCellText(Sender: TObject; AGrid: TCustomGrid; ACol,
      ARow: Integer; var AText: String);
    procedure ShowPreview(APageNo: Integer);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Printers, GridPrnPreviewForm;

const
  ZOOM_MULTIPLIER = 1.05;

{ TForm1 }

procedure TForm1.ShowPreview(APageNo: Integer);
var
  bmp: TBitmap;
begin
  FPageNo := APageNo;
  bmp := FGridPrinter.CreatePreviewBitmap(FPageNo, FZoom);
  try
    Image1.Width := bmp.Width;
    Image1.Height := bmp.Height;
    Image1.Picture.Bitmap.Assign(bmp);
  finally
    bmp.Free;
  end;
  PageControl1.ActivePage := pgPreview;

  tbFirstPage.Enabled := FPageNo > 1;
  tbPrevPage.Enabled := FPageNo > 1;
  tbNextPage.Enabled := FPageNo < FGridPrinter.PageCount;
  tbLastPage.Enabled := FPageNo < FGridPrinter.PageCount;
  PageInfo.Caption := Format('Page %d of %d', [FPageNo, FGridPrinter.PageCount]);

  cmbPercent.Text := IntToStr(FZoom) + '%';
end;

procedure TForm1.cbFooterLineChange(Sender: TObject);
begin
  FGridPrinter.FooterLine := cbFooterLine.Checked;
  ShowPreview(FPageNo);
end;

procedure TForm1.cbDefaultFixedCellsDividerLineColorChange(Sender: TObject);
begin
  clbFixedCellsDividerLineColor.Enabled := not cbDefaultFixedCellsDividerLinecolor.Checked;
  if cbDefaultFixedCellsDividerLineColor.Checked then
    FGridPrinter.FixedLineColor := clDefault
  else
    FGridPrinter.FixedLineColor := clbFixedCellsDividerLinecolor.ButtonColor;
  ShowPreview(FPageNo);
end;

procedure TForm1.btnPrintPreviewformClick(Sender: TObject);
var
  F: TGridPrintPreviewForm;
begin
  F := TGridPrintPreviewForm.Create(nil);
  try
    F.GridPrinter := FGridPrinter;
    if (F.ShowModal = mrOK) and PrintDialog1.Execute then
      FGridPrinter.Print;
  finally
    F.Free;
  end;
end;

procedure TForm1.cbDefaultGridLineColorChange(Sender: TObject);
begin
  clbGridLineColor.Enabled := not cbDefaultGridLinecolor.Checked;
  if cbDefaultGridLineColor.Checked then
    FGridPrinter.GridLineColor := clDefault
  else
    FGridPrinter.GridLineColor := clbGridLinecolor.ButtonColor;
  ShowPreview(FPageNo);
end;

procedure TForm1.cbHeaderLineChange(Sender: TObject);
begin
  FGridPrinter.HeaderLine := cbHeaderLine.Checked;
  ShowPreview(FPageNo);
end;

procedure TForm1.cbMonochromeChange(Sender: TObject);
begin
  FGridPrinter.Monochrome := cbMonochrome.Checked;
  ShowPreview(FPageNo);
end;

procedure TForm1.cbPageOrientationChange(Sender: TObject);
begin
  FGridPrinter.Orientation := TPrinterOrientation(cbPageOrientation.ItemIndex);
  ShowPreview(FPageNo);
end;

procedure TForm1.cbPrintOrderChange(Sender: TObject);
begin
  FGridPrinter.PrintOrder := TGridPrnOrder(cbPrintOrder.ItemIndex);
  ShowPreview(FPageno);
end;

procedure TForm1.cbDefaultBorderLinecolorChange(Sender: TObject);
begin
  clbBorderLineColor.Enabled := not cbDefaultBorderLinecolor.Checked;
  if cbDefaultBorderLineColor.Checked then
    FGridPrinter.BorderLineColor := clDefault
  else
    FGridPrinter.BorderLineColor := clbBorderLinecolor.ButtonColor;
  ShowPreview(FPageNo);
end;

procedure TForm1.clbBorderLineColorColorChanged(Sender: TObject);
begin
  if not cbDefaultBorderLineColor.Checked then
  begin
    FGridPrinter.BorderLineColor := clbBorderLineColor.ButtonColor;
    ShowPreview(FPageNo);
  end;
end;

procedure TForm1.clbFixedCellsDividerLineColorColorChanged(Sender: TObject);
begin
  if not cbDefaultFixedCellsDividerLineColor.Checked then
  begin
    FGridPrinter.FixedLineColor := clbFixedCellsDividerLineColor.ButtonColor;
    ShowPreview(FPageNo);
  end;
end;

procedure TForm1.clbGridLineColorColorChanged(Sender: TObject);
begin
  if not cbDefaultGridLineColor.Checked then
  begin
    FGridPrinter.GridLineColor := clbGridLineColor.ButtonColor;
    ShowPreview(FPageNo);
  end;
end;

procedure TForm1.clbHeaderLinecolorColorChanged(Sender: TObject);
begin
  FGridPrinter.HeaderLineColor := clbHeaderLineColor.ButtonColor;
  ShowPreview(FPageNo);
end;

procedure TForm1.clbFooterLineColorColorChanged(Sender: TObject);
begin
  FGridPrinter.FooterLineColor := clbFooterLineColor.ButtonColor;
  ShowPreview(FPageNo);
end;

procedure TForm1.cmbPercentEditingDone(Sender: TObject);
var
  zoomStr: String;
begin
  zoomStr := cmbPercent.Text;
  while (zoomStr <> '') and (zoomStr[Length(zoomStr)] in [' ', '%']) do
    Delete(zoomStr, Length(zoomStr), 1);
  if TryStrToInt(zoomStr, FZoom) then
    ShowPreview(FPageNo);
end;

procedure TForm1.edFooterTextChange(Sender: TObject);
begin
  FGridPrinter.Footer := edFooterText.Text;
  ShowPreview(FPageNo);
end;

procedure TForm1.edHeaderTextChange(Sender: TObject);
begin
  FGridPrinter.Header := edHeaderText.Text;
  ShowPreview(FPageNo);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PopulateGrid;
  //PopulateGrid_Columns;

  FGridPrinter := TGridPrinter.Create(self);
  FGridPrinter.Grid := StringGrid1;
  FGridPrinter.HeaderPart[hfpLeft] := 'This is a print test.';
  FGridPrinter.FooterPart[hfpCenter] := 'Page $PAGE of $PAGECOUNT';
  FgridPrinter.FooterLine := false;
//  FGridPrinter.OnGetCellText := @PrinterGetCellText;
  FGridPrinter.OnPrepareCanvas := @StringGrid1PrepareCanvas;

  edHeaderText.Text := FGridPrinter.Header;
  cbHeaderLine.Checked := FGridPrinter.HeaderLine;
  clbHeaderLineColor.ButtonColor := FGridPrinter.HeaderLineColor;

  edFooterText.Text := FGridPrinter.Footer;
  cbFooterLine.Checked := FGridPrinter.FooterLine;
  clbFooterLineColor.ButtonColor := FGridPrinter.FooterLineColor;

  cbDefaultBorderLineColor.Checked := FGridPrinter.BorderLineColor = clDefault;
  seBorderLineWidth.Value := FGridPrinter.BorderlineWidth;

  cbDefaultFixedCellsDividerLineColor.Checked := FGridPrinter.FixedLineColor = clDefault;
  seFixedCellsDividerLineWidth.Value := FGridPrinter.FixedLineWidth;

  cbDefaultGridLineColor.Checked := FGridPrinter.GridLineColor = clDefault;
  seGridLineWidth.Value := FGridPrinter.GridLineWidth;

  FZoom := 100;
  ShowPreview(1);
end;

procedure TForm1.Image1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (ssCtrl in Shift) then
  begin
    if WheelDelta > 0 then
      FZoom := round(FZoom * ZOOM_MULTIPLIER)
    else
      FZoom := round(FZoom / ZOOM_MULTIPLIER);
    ShowPreview(FPageNo);
  end;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = pgPreview then
    ShowPreview(FPageNo);
end;

procedure TForm1.seBorderLineWidthChange(Sender: TObject);
begin
  FGridPrinter.BorderLineWidth := seBorderLineWidth.Value;
  ShowPreview(FPageNo);
end;

procedure TForm1.seFixedCellsDividerLineWidthChange(Sender: TObject);
begin
  FGridPrinter.FixedLineWidth := seFixedCellsDividerLineWidth.Value;
  ShowPreview(FPageNo);
end;

procedure TForm1.seGridLineWidthChange(Sender: TObject);
begin
  FGridPrinter.GridLineWidth := seGridLineWidth.Value;
  ShowPreview(FPageNo);
end;

procedure TForm1.sbHeaderFontClick(Sender: TObject);
begin
  FontDialog1.Font := FGridPrinter.HeaderFont;
  if FontDialog1.Execute then
  begin
    FGridPrinter.HeaderFont.Assign(FontDialog1.Font);
    ShowPreview(FPageNo);
  end;
end;

procedure TForm1.sbFooterFontClick(Sender: TObject);
begin
  FontDialog1.Font := FGridPrinter.FooterFont;
  if FontDialog1.Execute then
  begin
    FGridPrinter.FooterFont.Assign(FontDialog1.Font);
    ShowPreview(FPageNo);
  end;
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
    lCanvas := FGridPrinter.Canvas
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

  // Cell col=6/row=3: no endellipsis
  if (aCol=6) and (aRow = 3) then
  begin
    ts := lCanvas.TextStyle;
    ts.EndEllipsis := false;
    lCanvas.TextStyle := ts;
  end;
end;

procedure TForm1.tbFirstPageClick(Sender: TObject);
begin
  ShowPreview(1);
end;

procedure TForm1.tbLastPageClick(Sender: TObject);
begin
  ShowPreview(FGridPrinter.PageCount);
end;

procedure TForm1.tbNextPageClick(Sender: TObject);
begin
  ShowPreview(FPageNo+1);
end;

procedure TForm1.tbPrevPageClick(Sender: TObject);
begin
  ShowPreview(FPageNo-1);
end;

procedure TForm1.tbPrintClick(Sender: TObject);
begin
  if PrintDialog1.Execute then
    FGridPrinter.Print;
end;

procedure TForm1.tbZoomInClick(Sender: TObject);
begin
  FZoom := round(FZoom * ZOOM_MULTIPLIER);
  ShowPreview(FPageNo);
end;

procedure TForm1.tbZoomOutClick(Sender: TObject);
begin
  FZoom := round(FZoom / ZOOM_MULTIPLIER);
  ShowPreview(FPageNo);
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
  for r := StringGrid1.FixedRows to StringGrid1.RowCount-1 do
    for c := StringGrid1.FixedCols to StringGrid1.ColCount-1 do
      StringGrid1.Cells[c, r] := Format('C%d R%d', [c - StringGrid1.FixedCols + 1, r]);

  // String in col=2, row=5 is interpreted as cell background color
  StringGrid1.cells[2, 5] := 'clYellow';

  // Long text in cols=5, 6, 7, row=2
  StringGrid1.Cells[5, 2] := StringGrid1.Cells[5, 2] + ' This is a long text.';
  StringGrid1.Cells[6, 2] := StringGrid1.Cells[6, 2] + ' This is a long text.';
  StringGrid1.Cells[6, 3] := StringGrid1.Cells[6, 3] + ' This is a long text.';
  StringGrid1.Cells[7, 2] := StringGrid1.Cells[7, 2] + ' This is a long text.';
  StringGrid1.Cells[7, 3] := '';         // there must not be overflow text from prev cell.

  // Different row height in row 2 and 4
  StringGrid1.RowHeights[2] := 2* StringGrid1.DefaultRowHeight;
  StringGrid1.RowHeights[4] := 40;

  // Different col width in col 5
  StringGrid1.ColWidths[5] := 150;
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

