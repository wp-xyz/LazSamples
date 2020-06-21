unit mainformu;

///////////////
///
///
///   Drag and Drop in DBGrid
///
///   Copyright (c) 2013 Cary Jensen. All Rights Reserved
///
///   See http://caryjensen.blogspot.com/2013/08/dragging-and-dropping-in-dbgrids.html
///   for a description of this project.
///
///   No guarantees or warranties are expressed or implied concerning
///   the applicability of techniques or code included in this example.
///   If you wish to use techniques or code included in this example,
///   it is your responsibility to test and certify any code,
///   techniques, or design adopted as a result of this project.
///
///////////////

interface

{.$DEFINE CLONE_CURSOR}   // NOTE: THIS IS NOT WORKING WITH LAZARUS !!!

uses
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Grids, DBGrids,
  DB, Menus, ComCtrls,
  BufDataset;

type
//  The interceptor class
//  TDBGrid = class(DBGrids.TDBGrid)
//  public
//    function RecNoFromVisibleRow(Value: Integer): Integer;
//  end;
  TForm1 = class(TForm)
    ListBox: TListBox;
    DBGrid: TDBGrid;
    OKBtn: TButton;
    CloseBtn: TButton;
    RestartBtn: TButton;
    DataSource1: TDataSource;
    PopupMenu1: TPopupMenu;
    Remove1: TMenuItem;
    DisplaySequenceCB: TCheckBox;
    StatusBar1: TStatusBar;
    ShowHeaderCB: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DBGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DBGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure RestartBtnClick(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure DisplaySequenceCBClick(Sender: TObject);
    procedure ShowHeaderCBClick(Sender: TObject);
  private
    { Private declarations }
    BufDataSet: TBufDataSet;
    procedure EmptyDataset;
    procedure ResequenceBufDS(BufDS: TBufDataset; FromRow: Integer);
    procedure MoveRecord(BufDS: TBufDataset; OldPos: Integer; NewPos: Integer);
    procedure RemoveFromSequence(BufDS: TBufDataset; APosition: Integer);
    procedure PopulateListBox;
    procedure CreateBufDataSet;
    procedure SetGridColumns;
    procedure Setup;
  public
    { Public declarations }
  end;

  //The class helper
  TDBGridHelper = class helper for TDBGrid
  public
    function RecNoFromVisibleRow(Value: Integer): Integer;
    function AreRowsHidden: Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses 
  Math;  //for RandomRange

procedure TForm1.OKBtnClick(Sender: TObject);
begin
  ShowMessage('In real life, you now use the contents of ' +
              'the BufDataSet to insert or update ' +
              'records in the underlying database.');
  Close;
end;

procedure TForm1.DisplaySequenceCBClick(Sender: TObject);
begin
  DBGrid.columns[0].Visible := DisplaySequenceCB.Checked;
end;

procedure TForm1.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.CreateBufDataSet;
begin
  BufDataSet := TBufDataSet.Create(Self);

  //This example assumes that the field defining the
  //order of records is named Sequence. This field
  //can appear in any position in the table structure.
  BufDataSet.FieldDefs.Add('Sequence', ftInteger);
  BufDataSet.FieldDefs.Add('Field', ftString, 30);

  //The BufDataSet can have any number of fields
  //The following field is just for demonstration
  BufDataSet.FieldDefs.Add('RandomNumber', ftInteger);
  BufDataSet.CreateDataSet;

  //This next line is critical. It displays the records
  //in the order of the integer Sequence field
  BufDataSet.IndexFieldNames := 'Sequence';

  DataSource1.DataSet := BufDataSet;
end;

procedure TForm1.DBGridDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  GridRow: Integer;
  OriginalRow: Integer;
  HiddenRows: Boolean;
begin
  ///
  ///  WARNING: This routine does not work correct as written if
  ///  the dgTitles flag is not in the Options property of the DBGrid
  ///

  GridRow := DBGrid.MouseCoord(X,Y).Y;
  if (Source is TListBox) then
  begin
    GridRow := DBGrid.RecNoFromVisibleRow(GridRow);
    //An item is being dropped into the DBGrid
    if BufDataSet.IsEmpty then
    begin
      //The grid is empty. Add the item in the first position
      BufDataSet.AppendRecord([1, TListBox(Source).Items[TListBox(Source).ItemIndex], RandomRange(1, 101)]);
    end
    else
    begin
      //Insert the item at the position of the drop
      if GridRow = -1 then  //the drop is at the end of the DBGrid
        GridRow := BufDataSet.RecordCount + 1
      else //the drop needs to be inserted into the DBGrid. Make room
        ResequenceBufDS(BufDataSet, GridRow);
      //Insert the new item at the drop position
      BufDataSet.InsertRecord( [GridRow,  TListBox(Source).Items[TListBox(Source).ItemIndex], RandomRange(1, 101)]);
      //Make the dropped record the current record. This is new
      BufDataSet.RecNo := GridRow;
    end;
    //Remove the dropped item from the source (optional)
    TListBox(Source).Items.Delete(TListBox(Source).ItemIndex);
  end
  else
  if (Source = Sender) then
  begin
    if BufDataSet.IsEmpty then exit;
    if (GridRow = 0) then
      GridRow := 1;
      OriginalRow := BufDataSet.RecNo;
    HiddenRows := DBGrid.AreRowsHidden;
    if HiddenRows then
    begin
      if GridRow = -1 then
        GridRow := DBGrid.RecNoFromVisibleRow(GridRow) - 1 //adjust for mid stream
      else
        GridRow := DBGrid.RecNoFromVisibleRow(GridRow);
    end;
    if (OriginalRow = GridRow) or (GridRow = -1) or (GridRow > BufDataSet.RecordCount) then
      exit;
    MoveRecord(BufDataSet, OriginalRow, GridRow);
  end;
end;

procedure TForm1.DBGridDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
 //Test for acceptable drag origin classes or objects
 Accept :=  (Source is TListBox) or (Source is TDBGrid);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Setup;
end;

procedure TForm1.MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
const //this constant is typically declared with a higher scope
  MouseMovePixels = 15;
begin
  //MouseMovePixels is a constant
  if ssLeft in Shift then
    TListBox(Sender).BeginDrag(False, MouseMovePixels)
end;

{$IFDEF CLONE_CURSOR}
{ --- version with cloned cursor }
procedure TForm1.MoveRecord(BufDS: TBufDataSet; OldPos, NewPos: Integer);
var
  clone: TBufDataSet;
  SequenceFld: TField;
begin
  clone := TBufDataSet.Create(nil);
  try
    clone.CloneCursor(BufDS, True);
    SequenceFld := clone.FieldByName('Sequence');
    clone.RecNo := OldPos;
    clone.Edit;
    //Move the record being moved to the end of the sequence
    SequenceFld.AsInteger := BufDS.RecordCount + 1;
    clone.Post;
    if OldPos < NewPos then
    begin
      //Shift the records after the original old position up one position
      clone.RecNo := OldPos;
      while (clone.RecNo < NewPos) do
      begin
      //TODO: Bug!!!
         clone.Edit;
        SequenceFld.AsInteger := SequenceFld.AsInteger - 1;
        clone.Post;
        clone.Next;
      end;
    end
    else
    begin
      //Shift the record before the original position down one position
      clone.IndexFieldNames := 'Sequence';
      clone.RecNo := OldPos - 1;
      while (clone.RecNo >= NewPos) and (not clone.bof) do
      begin
        clone.Edit;
        SequenceFld.AsInteger := SequenceFld.AsInteger + 1;
        clone.Post;
        clone.Prior;
      end;
    end;
    //Move the record being moved to its new position
    clone.RecNo := BufDS.RecordCount;
    clone.Edit;
    SequenceFld.AsInteger := NewPos;
    clone.Post;
  finally
    clone.Free;
  end;
  BufDS.RecNo := NewPos;
end;
{$ELSE}
//This is a version that does not use a cloned cursor
procedure TForm1.MoveRecord(BufDS: TBufDataSet; OldPos, NewPos: Integer);
var
   SequenceFld: TField;
begin
  try
    BufDS.DisableControls;
    SequenceFld := BufDs.FieldByName('Sequence');
    BufDS.RecNo := OldPos;
    BufDS.Edit;

    //Stick the record being moved at the end of the sequence
    SequenceFld.AsInteger := BufDS.RecordCount + 1;
    BufDS.Post;
    if OldPos < NewPos then
    begin
      //Shift the records after the original old position up one position
      BufDS.RecNo := OldPos;
      while (BufDS.RecNo < NewPos)  do
      begin
        BufDS.Edit;
        SequenceFld.AsInteger := SequenceFld.AsInteger - 1;
        BufDS.Post;
        BufDS.Next;
      end;
    end
    else
    begin
      //Shift the record before the original position down one position
      BufDS.RecNo := OldPos - 1;
      while (BufDS.RecNo >= NewPos) and (not BufDS.bof) do
      begin
        BufDS.Edit;
        SequenceFld.AsInteger := SequenceFld.AsInteger + 1;
        BufDS.Post;
        BufDS.Prior;
      end;
    end;
    //Move the record being moved to its new position
    BufDS.RecNo := BufDS.RecordCount;
    BufDS.Edit;
    SequenceFld.AsInteger := NewPos;
    BufDS.Post;
  finally
    BufDS.EnableControls;
  end;
  BufDS.RecNo := NewPos;
end;
{$ENDIF}

procedure TForm1.PopulateListBox;
begin
  ListBox.Clear;
  ListBox.Items.Add('One');
  ListBox.Items.Add('Two');
  ListBox.Items.Add('Three');
  ListBox.Items.Add('Four');
  ListBox.Items.Add('Five');
  ListBox.Items.Add('Six');
  ListBox.Items.Add('Seven');
  ListBox.Items.Add('Eight');
  ListBox.Items.Add('Nine');
  ListBox.Items.Add('Ten');
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  Remove1.Visible := DBGrid.DataSource.DataSet.RecordCount > 0;
end;

procedure TForm1.Remove1Click(Sender: TObject);
var
  BufDS: TBufDataSet;
begin
  BufDS := TBufDataSet(DBGrid.DataSource.DataSet);
  ListBox.Items.Add(BufDS.FieldByName('Field').AsString);
  RemoveFromSequence(BufDS, BufDS.RecNo);
end;

{$IFDEF CLONED_CURSOR}
//This version uses a cloned cursor
procedure TForm1.RemoveFromSequence(BufDS: TBufDataSet; APosition: Integer);
var
   clone: TBufDataSet;
   SeqFld: TField;
begin
  clone := TBufDataSet.Create(nil);
  try
    clone.CloneCursor(BufDS, True);
    SeqFld := clone.FieldByName('Sequence');
    clone.RecNo := APosition;
    if (APosition = 1) and (clone.RecordCount = 1) then
    begin
      //There is just one record. Delete it,
      //but do not try to set a new record position
      clone.Delete;
    end
    else
    begin
      if clone.RecNo = clone.RecordCount then
      begin
        clone.Delete;
        BufDS.RecNo := BufDS.RecordCount;
      end
      else
      begin
        clone.Delete;
        while not clone.eof do
        begin
          clone.Edit;
          SeqFld.AsInteger := SeqFld.AsInteger - 1;
          clone.Post;
          clone.Next;
        end;
        BufDS.RecNo := APosition;
      end;
    end;
  finally
    clone.Free;
  end;
end;
{$ELSE}
//This version does not use a cloned cursor
procedure TForm1.RemoveFromSequence(BufDS: TBufDataSet; APosition: Integer);
var
   SeqFld: TField;
begin
  SeqFld := BufDS.FieldByName('Sequence');
  BufDS.DisableControls;
  try
    BufDS.RecNo := APosition;
    if (APosition = 1) and (BufDS.RecordCount = 1) then
    begin
      //There is just one record. Delete it,
      //but do not try to set a new record position
      BufDS.Delete;
    end
    else
    begin
      if BufDS.RecNo = BufDS.RecordCount then
      begin
        BufDS.Delete;
        BufDS.RecNo := BufDS.RecordCount;
      end
      else
      begin
        BufDS.Delete;
        while not BufDS.eof do
        begin
          BufDS.Edit;
          SeqFld.AsInteger := SeqFld.AsInteger - 1;
          BufDS.Post;
          BufDS.Next;
        end;
        BufDS.RecNo := APosition;
      end;
    end;
  finally
    BufDS.EnableControls;
  end;
end;
{$ENDIF}

{$IFDEF CLONED_CURSOR}
//This version uses a cloned cursor
procedure TForm1.ResequenceBufDS(BufDS: TBufDataSet; FromRow: Integer);
var
  clone: TBufDataSet;
  SequenceFld: TField;
begin
  clone := TBufDataSet.Create(nil);
  try
    clone.CloneCursor(BufDS, True);
    SequenceFld := clone.FieldByName('Sequence');
    begin
      //Shift all records down to make room in the sequence
      //for the record being inserted
      clone.Last;
      while (SequenceFld.AsInteger >= FromRow) and not clone.bof do
      begin
        clone.Edit;
        SequenceFld.AsInteger := SequenceFld.AsInteger + 1;
        clone.Post;
        clone.Prior;
      end;
    end
  finally
    clone.Free;
  end;
end;
{$ELSE}
//This is a version that does not use a cloned cursor
procedure TForm1.ResequenceBufDS(BufDS: TBufDataSet; FromRow: Integer);
var
  SequenceFld: TField;
begin
  try
    BufDS.DisableControls;
    SequenceFld := BufDS.FieldByName('Sequence');
    begin
      //Shift all records down to make room in the sequence
      //for the record being inserted
      BufDS.Last;
      while (SequenceFld.AsInteger >= FromRow)  and not BufDS.bof do
      begin
        BufDS.Edit;
        SequenceFld.AsInteger := SequenceFld.AsInteger + 1;
        BufDS.Post;
        BufDS.Prior;
      end;
    end
  finally
    BufDS.EnableControls;
  end;
end;
{$ENDIF}

procedure TForm1.Setup;
begin
  if BufDataSet <> nil then
    EmptyDataset;
  PopulateListBox;
  CreateBufDataSet;
  SetGridColumns;
end;

procedure TForm1.ShowHeaderCBClick(Sender: TObject);
begin
  if ShowHeaderCB.Checked then
    DBGrid.Options := DBGrid.Options + [dgTitles]
  else
  begin
    MessageDlg('This example does not work when the grid does not use titles. I''m working on fixing this.', mtWarning, [mbOK], 0);
    DBGrid.Options := DBGrid.Options - [dgTitles]
  end;
end;

procedure TForm1.RestartBtnClick(Sender: TObject);
begin
  PopulateListBox;
  EmptyDataset;
end;

procedure TForm1.SetGridColumns;
begin
  DBGrid.Columns[0].Width := 60;
  DBGrid.Columns[1].Width := 40;
  DBGrid.Columns[2].Width := 80;
end;

procedure TForm1.EmptyDataset;
begin
  BufDataset.Free;
  CreateBufDataset;
end;

{ TDBGridHelper }
//This is the implementation of the class helper method
function TDBGridHelper.AreRowsHidden: Boolean;
begin
  Result := VisibleRowCount < DataSource.DataSet.RecordCount;
end;

function TDBGridHelper.RecNoFromVisibleRow(Value: Integer): Integer;
begin
  if Value = -1 then
  begin
    Result := DataSource.DataSet.RecNo - Row + TopRow + VisibleRowCount
  end
  else
  begin
    Result := DataSource.DataSet.RecNo - Row + TopRow + Value;
    if dgTitles in Options then
      Dec(Result);
  end;
end;

{ TDBGrid }
// If you want to use the interceptor class, uncomment this method
//function TDBGrid.RecNoFromVisibleRow(Value: Integer): Integer;
//begin
//  if Value = -1 then
//  begin
//    Result := DataSource.DataSet.RecNo - Row + TopRow + VisibleRowCount
//  end
//  else
//  begin
//    Result := DataSource.DataSet.RecNo - Row + TopRow + Value;
//    if dgTitles in Options then
//      Dec(Result);
//  end;
//end;

initialization
  Randomize;

end.
