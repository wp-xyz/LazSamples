unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, DB, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, StdCtrls, GridPrn, GridPrnPreviewDlg, Grids;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    GridPrinter1: TGridPrinter;
    GridPrintPreviewDialog1: TGridPrintPreviewDialog;
    MemDataset1: TMemDataset;
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
  MemDataset1.FieldDefs.Add('Text', ftString, 20);
  MemDataset1.FieldDefs.Add('Value', ftInteger);
  MemDataset1.FieldDefs.Add('Date', ftDate);
  MemDataset1.CreateTable;
  MemDataset1.Open;
  for i := 1 to 100 do
    MemDataset1.AppendRecord(['Record ' + IntToStr(i), 100*i, Date()-i]);

  // Since the GridPrinter accesses the DBGrid assign it to the Grid property
  // only after the Dataset is ready and the DBGrid can display valid data.
  GridPrinter1.Grid := DBGrid1;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  GridPrintPreviewDialog1.Execute;
end;

procedure TForm1.GridPrinter1GetCellText(Sender: TObject; AGrid: TCustomGrid;
  ACol, ARow: Integer; var AText: String);
var
  F: TField;
  dbGrid: TDBGrid;
begin
  dbGrid := AGrid as TDBGrid;
  if ACol < dbGrid.FixedCols then
    AText := ''
  else
  begin
    MemDataset1.Locate('Text', 'Record ' + IntToStr(ARow), []);
    F := dbGrid.Columns[ACol - dbGrid.FixedCols].Field;
    AText := F.AsString;
  end;
end;

procedure TForm1.GridPrinter1GetRowCount(Sender: TObject; AGrid: TCustomGrid;
  var ARowCount, AFixedRows: Integer);
begin
  ARowCount := MemDataset1.RecordCount;
end;

end.

