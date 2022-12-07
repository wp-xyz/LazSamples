unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure StringGrid1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FStartDragCol: Integer;
    FStartDragRow: Integer;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  InterfaceBase;

var
  crDragCopy: Integer;

{ TForm1 }

procedure TForm1.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (ssAlt in Shift) then
  begin
    StringGrid1.MouseToCell(X, Y, FStartDragCol, FStartDragRow);
    StringGrid1.BeginDrag(false, 5);
  end;
end;

procedure TForm1.StringGrid1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  overCol, overRow: Integer;
begin
  Accept := false;

  if not (goEditing in StringGrid1.Options) then
    exit;

  if (ssCtrl in GetKeyShiftState) then
    StringGrid1.DragCursor := crDragCopy
  else
    StringGrid1.DragCursor := crDrag;

  StringGrid1.MouseToCell(X, Y, overCol, overRow);

  if (overCol = -1) or (overRow = -1) then
    exit;

  Accept := (overCol >= StringGrid1.FixedCols) and (overRow >= StringGrid1.FixedRows) and
    not ((overCol = FStartDragCol) and (overRow = FStartDragRow));
end;

procedure TForm1.StringGrid1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  dropCol, dropRow: Integer;
begin
  StringGrid1.MouseToCell(X, Y, dropCol, dropRow);
  if (dropCol = -1) or (dropRow = -1) then
    exit;

  if not ((dropCol = FStartDragCol) and (dropRow = FStartDragRow)) then
  begin
    StringGrid1.Cells[dropCol, dropRow] := StringGrid1.Cells[FStartDragCol, FStartDragRow];
    if not (ssCtrl in GetKeyShiftState) then
      StringGrid1.Cells[FStartDragCol, FStartDragRow] := '';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  StringGrid1.AllowOutBoundEvents := false;
end;


initialization
  crDragCopy := 1;
  // Note: the cursor image "cur_dragcopy" has been added to "Project option" > "Resources"
  Screen.Cursors[crDragCopy] := LoadCursorFunction(HINSTANCE, PChar('cur_dragcopy'));

end.

