unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ComboEx,
  LCLTranslator, Translations,
  PrintersDlgs, GridPrn, GridPrnPreviewForm, GridPrnPreviewDlg;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnPrint: TButton;
    btnPreview: TButton;
    Button2: TButton;
    CheckComboBox1: TCheckComboBox;
    ComboBox1: TComboBox;
    GridPrinter1: TGridPrinter;
    GridPrintPreviewDialog1: TGridPrintPreviewDialog;
    PageSetupDialog1: TPageSetupDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    StringGrid1: TStringGrid;
    procedure btnPrintClick(Sender: TObject);
    procedure btnPreviewClick(Sender: TObject);
    procedure CheckComboBox1ItemChange(Sender: TObject; AIndex: Integer);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnPrintClick(Sender: TObject);
begin
  case Combobox1.ItemIndex of
    0: GridPrinter1.PrintDialogs := gpdNone;
    1: GridPrinter1.PrintDialogs := gpdPageSetup;
    2: GridPrinter1.PrintDialogs := gpdPrintDialog;
  end;
  GridPrinter1.Print;
end;

procedure TForm1.btnPreviewClick(Sender: TObject);
begin
  GridPrintPreviewDialog1.Execute;
end;

procedure TForm1.CheckComboBox1ItemChange(Sender: TObject; AIndex: Integer);
var
  optns: TGridPrintPreviewOptions;
begin
  optns := [];
  if CheckCombobox1.Checked[0] then Include(optns, ppoNavigationBtns);
  if CheckCombobox1.Checked[1] then Include(optns, ppoNavigationEdit);
  if CheckCombobox1.Checked[2] then Include(optns, ppoZoomBtns);
  if CheckCombobox1.Checked[3] then Include(optns, ppoPageOrientationBtns);
  if CheckCombobox1.Checked[4] then Include(optns, ppoMarginsBtn);
  if CheckCombobox1.Checked[5] then Include(optns, ppoHeaderFooterBtn);
  if CheckCombobox1.Checked[6] then Include(optns, ppoPrintOrderBtns);
  GridPrintPreviewDialog1.Options := optns;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  NUM_ROWS = 100;
  NUM_COLS = 20;
var
  i: Integer;
  r, c: Integer;
  languagesDir: String;
begin
  languagesDir := ExpandFileName(Application.Location + '../../languages/');
  SetDefaultLang('de', languagesDir);
  TranslateUnitResourceStrings('GridPrnStrings', languagesDir + 'GridPrnStrings.de.po');

  for i := 0 to CheckComboBox1.Items.Count-1 do
    CheckComboBox1.Checked[i] := True;

  StringGrid1.BeginUpdate;
  try
    StringGrid1.Clear;
    StringGrid1.RowCount := NUM_ROWS + StringGrid1.FixedRows;
    StringGrid1.ColCount := NUM_COLS + StringGrid1.FixedCols;
    for r := StringGrid1.FixedRows to StringGrid1.RowCount-1 do
      StringGrid1.Cells[0, r] := 'Row ' + IntToStr(r);
    for c := StringGrid1.FixedCols to StringGrid1.ColCount-1 do
    begin
      StringGrid1.Cells[c, 0] := 'Column ' + IntToStr(c);
      for r := StringGrid1.FixedRows to StringGrid1.RowCount-1 do
        StringGrid1.cells[c, r] := Format('C%d R%d', [c, r]);
    end;
  finally
    StringGrid1.EndUpdate;
  end;
end;

end.

