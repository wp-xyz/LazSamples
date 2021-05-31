unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, ExtCtrls,
  ComCtrls, DBCtrls, DBGrids, Grids, paradoxds, db, LCLVersion;

{$UNDEF SUPPORTS_SHELL_ICONS}
{$IF LCL_FullVersion >= 2010000}
  {$IFDEF WINDOWS}
    {$DEFINE SUPPORTS_SHELL_ICONS}
  {$ENDIF}
{$IFEND}

type

  { TMainForm }

  TMainForm = class(TForm)
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBImage: TDBImage;
    Images: TImageList;
    Memo: TDBMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SplitterMemoImage: TSplitter;
    Splitter4: TSplitter;
    FieldGrid: TStringGrid;
    SplitterFieldGrid: TSplitter;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ParadoxAfterClose(DataSet: TDataSet);
    procedure ParadoxAfterOpen(DataSet: TDataSet);
    procedure ShellListViewClick(Sender: TObject);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
  private
    FParadox: TParadoxDataSet;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TypInfo;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ShellTreeview.Path := Application.Location;
  {$IFNDEF SUPPORTS_SHELL_ICONS}
  ShellListView.SmallImages := Images;
  ShellTreeView.Images := Images;
  ShellTreeView.GetImageIndex := @ShellTreeViewGetImageIndex;
  ShellTreeView.GetSelectedIndex :=@ShellTreeViewGetSelectedIndex;
  {$ENDIF}

  FParadox := TParadoxDataset.Create(self);
  FParadox.AfterOpen := @ParadoxAfterOpen;
  FParadox.AfterClose := @ParadoxAfterClose;
  
  Datasource.Dataset := FParadox;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  FieldGrid.Align := alClient;
end;

procedure TMainForm.ParadoxAfterClose(DataSet: TDataSet);
begin
  Memo.DataField := '';
  DBImage.DataField := '';
end;

procedure TMainForm.ParadoxAfterOpen(DataSet: TDataSet);
var
  i, j: Integer;
  memoCount, graphicCount: Integer;
begin
  FieldGrid.RowCount := FParadox.FieldCount + FieldGrid.FixedRows;
  memoCount := 0;
  graphicCount := 0;
  j := FieldGrid.FixedRows;
  for i:=0 to FParadox.FieldCount-1 do begin
    FieldGrid.Cells[0, j] := FParadox.Fields[i].FieldName;
    FieldGrid.Cells[1, j] := GetEnumName(TypeInfo(TFieldType), ord(FParadox.Fields[i].DataType));
    FieldGrid.Cells[2, j] := IntToStr(FParadox.Fields[i].Size);
    inc(j);
    if (FParadox.Fields[i].DataType = ftMemo) then begin
      if memoCount = 0 then
        Memo.DataField := FParadox.Fields[i].FieldName;
      inc(memoCount);
    end;
    if (FParadox.Fields[i].DataType = ftGraphic) then begin
      if graphicCount = 0 then
        DBImage.DataField := FParadox.Fields[i].FieldName;
      inc(graphicCount);
    end;
  end;
  Memo.Visible := (memoCount > 0) and (FParadox.RecordCount > 0);
  DBImage.Visible := (graphicCount > 0) and (FParadox.RecordCount > 0);
  SplitterMemoImage.Visible := Memo.Visible and DBImage.Visible;
  SplitterMemoImage.Top := DBImage.Top - 1;
  SplitterFieldGrid.Visible := Memo.Visible or DBImage.Visible;
  SplitterFieldGrid.Top := 0;
end;

procedure TMainForm.ShellListViewClick(Sender: TObject);
begin
  if ShellListView.ItemFocused = nil then
    exit;
  FParadox.Close;
  try
    FParadox.TableName := ShellListView.GetPathFromItem(ShellListView.ItemFocused);
    FParadox.Open;
  except
  end;
end;

procedure TMainForm.ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  if Node.Level = 0 then
    Node.ImageIndex := 0
  else if Node.Selected then
    Node.ImageIndex := 2
  else
    Node.ImageIndex := 1;
end;

procedure TMainForm.ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode
  );
begin
  if Node.Level = 0 then
    Node.SelectedIndex := 0
  else if Node.Selected then
    Node.SelectedIndex := 2
  else
    Node.SelectedIndex := 1;
end;

end.

