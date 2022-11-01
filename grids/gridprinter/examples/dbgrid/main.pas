unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, DB, BufDataset, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, StdCtrls, GridPrn, GridPrnPreviewDlg, Grids;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    Button1: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    GridPrinter1: TGridPrinter;
    GridPrintPreviewDialog1: TGridPrintPreviewDialog;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridPrinter1GetCellText(Sender: TObject; AGrid: TCustomGrid;
      ACol, ARow: Integer; var AText: String);
    procedure GridPrinter1GetRowCount(Sender: TObject; AGrid: TCustomGrid;
      var ARowCount: Integer);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Create some dummy dataset
  BufDataset1.FieldDefs.Add('Text', ftString, 20);
  BufDataset1.FieldDefs.Add('Value', ftInteger);
  BufDataset1.FieldDefs.Add('Date', ftDate);
  BufDataset1.CreateDataset;
  BufDataset1.Open;
  for i := 1 to 100 do
    BufDataset1.AppendRecord(['Record ' + IntToStr(i), 100*i, Date()-i]);
  BufDataset1.First;

  // Since the GridPrinter accesses the DBGrid assign it to the Grid property
  // only after the Dataset is ready and the DBGrid can display valid data.
  GridPrinter1.Grid := DBGrid1;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  bm: TBookmark;
begin
  // Store currently active record so that we can return to it after preview/print.
  bm := BufDataset1.GetBookmark;
  try
    // Disable scrolling of grid
    BufDataset1.DisableControls;
    try
      // Show the grid printpreview
      GridPrintPreviewDialog1.Execute;
    finally
      // Allow scrolling again
      BufDataset1.EnableControls;
    end;
    // Return to the stored record position.
    BufDataset1.GotoBookmark(bm);
  finally
    BufDataset1.FreeBookmark(bm);
  end;
end;

procedure TForm1.GridPrinter1GetCellText(Sender: TObject; AGrid: TCustomGrid;
  ACol, ARow: Integer; var AText: String);
var
  field: TField;
  dbGrid: TDBGrid;
begin
  dbGrid := AGrid as TDBGrid;
  if ACol >= dbGrid.FixedCols then
  begin
    // We need something to find the row to be printed and use the dataset's
    // RecNo for this purpose which is a number starting at 1.
    // BUT BEWARE: RecNo is no good parameter for many dataset types!
    BufDataset1.RecNo := ARow;
    // Using the field from the DBGrid.Columns rather than from the dataset
    // directly accounts for rearranging the column order in the grid.
    field := dbGrid.Columns[ACol - dbGrid.FixedCols].Field;
    AText := field.AsString;
  end;
end;

procedure TForm1.GridPrinter1GetRowCount(Sender: TObject; AGrid: TCustomGrid;
  var ARowCount: Integer);
begin
  // Since the DBGrid does not load all records, but we want to print all
  // of them, we must tell the printer the real number of rows to print
  ARowCount := BufDataset1.RecordCount + 1;  // added 1 for the header row
end;

end.

