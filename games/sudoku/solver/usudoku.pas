unit uSudoku;

{$mode ObjFPC}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils;

const
  EMPTY = 0;

type
  TBoard = array[0..8, 0..8] of byte;

  TBoardHelper = type helper for TBoard
    procedure Clear;
    function ToNiceString: String;
    function ToString: String;
  end;

  TSudoku = class
  private
    FBoard: TBoard;
    FSolutions: array of TBoard;
    function GetSolution(AIndex: Integer): TBoard;
    function GetSolutionCount: Integer;
  protected
    function FindFreeCell(out ARow, ACol: byte): Boolean;
    function IsSafe(ARow, ACol, ANumber: Byte): Boolean;
  public
    SolveCounter: Integer;
  public
    constructor Create(ABoard: TBoard); overload;
    procedure Solve;
    function SolveSingle: Boolean;
    property Board: TBoard read FBoard;
    property SolutionCount: Integer read GetSolutionCount;
    property Solutions[AIndex: Integer]: TBoard read GetSolution;
  end;

function BoardFromStr(ABoardStr: String): TBoard;

implementation

function BoardFromStr(ABoardStr: String): TBoard;
var
  r, c: Byte;

  procedure InvalidSudokuError(Msg: String);
  begin
    raise Exception.Create('Invalid Sudoku: ' + Msg);
  end;

  procedure NewRow;
  begin
    c := 0;
    inc(r);
  end;

var
  i, i1, i2: Integer;
begin
  Result{%H-}.Clear;

  if ABoardStr = '' then
    exit;

  i1 := 1;
  if ABoardStr[1] = '"' then
    inc(i1);

  i2 := Length(ABoardStr);
  if ABoardStr[i2] = '"' then
    dec(i2);

  r := 0;
  c := 0;
  for i := i1 to i2 do
  begin
    case ABoardStr[i] of
      ',', ';':
        NewRow;
      '0', '.':
        begin
          if c = 9 then
            NewRow;
          inc(c);
        end;
      '1'..'9':
        begin
          if c = 9 then
            NewRow;
          if (r = 9) then
            InvalidSudokuError('Too many rows.');
          Result[r, c] := ord(ABoardStr[i]) - ord('0');
          inc(c);
        end;
      #13, #10, '|', '-', '+', '*':
        ; // Skip
      else
        InvalidSudokuError('Invalid character "' + ABoardStr[i] + '" at position ' + IntToStr(i) + ' of the Sudoku input string.');
    end;
  end;
end;


{ TBoardHelper }

procedure TBoardHelper.Clear;
var
  r, c: Byte;
begin
  for r := 0 to 8 do
    for c := 0 to 8 do
      Self[r, c] := EMPTY;
end;

function TBoardHelper.ToString: String;
var
  r, c: byte;
begin
  Result := '';
  for r := 0 to 8 do
  begin
    for c := 0 to 8 do
      Result := Result + char(ord('0') or Self[r,c]);
    if r < 8 then
      Result := Result + ',';
  end;
end;

function TBoardHelper.ToNiceString: String;

  procedure AddNumber(ANumber: Byte);
  begin
    if ANumber = EMPTY then
      Result := Result + ' '
    else
      Result := Result + char(ord('0') + ANumber);
  end;

  procedure AddColSpacer(ACol: Byte);
  begin
    if ACol in [2, 5] then
      Result := Result + ' | '
    else
      Result := Result + ' ';
  end;

  procedure BeginLine;
  begin
    Result := Result + '| ';
  end;

  procedure EndLine;
  begin
    Result := Result + '|' + LineEnding;
  end;

const
  DIVIDER = '+-------+-------+-------+';
var
  r, c: Integer;
begin
  Result := DIVIDER + LineEnding;
  for r := 0 to 8 do
  begin
    BeginLine;
    for c := 0 to 8 do
    begin
      AddNumber(self[r, c]);
      AddColSpacer(c);
    end;
    EndLine;
    if r in [2, 5] then
      Result := Result + DIVIDER + LineEnding;
  end;
  Result := Result + DIVIDER;
end;

{ TSudoku }

constructor TSudoku.Create(ABoard: TBoard);
begin
  inherited Create;
  FBoard := ABoard;
  SolveCounter := 0;
end;

{  Find an empty cell.
   - ARow: the row of the cell found, if any
   - ACol: the columns of the cell found, if any
   - returns: true if an empty cell could be found, false otherwise }
function TSudoku.FindFreeCell(out ARow, ACol: Byte): Boolean;
var
  r, c: Byte;
begin
  for r := 0 to 8 do
    for c := 0 to 8 do
      if FBoard[r, c] = EMPTY then
      begin
        ARow := r;
        ACol := c;
        Result := true;
        exit;
      end;
  Result := false;
end;

function TSudoku.GetSolution(AIndex: Integer): TBoard;
begin
  Result := FSolutions[AIndex];
end;

function TSudoku.GetSolutionCount: Integer;
begin
  Result := Length(FSolutions);
end;

{ Check if placing a number at the designated destinaton is safe.
  The function checks if the given number is either present in
  the given row or column or 3x3 box.

  ARow: row to place into
  ACol: column to place into
  ANumber: number to place
  Returns: true if safe, false otherwise }
function TSudoku.IsSafe(ARow, ACol, ANumber: Byte): Boolean;
var
  i, r, c, rowStart, colStart: Byte;
begin
  Result := false;

  // Check row and column
  for i := 0 to 8 do
    if (FBoard[i, ACol] = ANumber) or (FBoard[ARow, i] = ANumber) then
      exit;

  // Check 3x3 block
  rowStart := (ARow div 3) * 3;
  colStart := (ACol div 3) * 3;
  for r := rowStart to rowStart+2 do
    for c := colStart to colStart+2 do
      if FBoard[r, c] = ANumber then
        exit;

  Result := true;
end;

{ Solves the Sudoku.
  Recursive procedure implementing a backtracking algorithm.
  Each solution found is added to the array FSolutions. }
procedure TSudoku.Solve;
var
  row, col, number: Byte;
begin
  inc(SolveCounter);

  // Find empty cells. If there is no more empty cell the Sudoku is solved, and
  // we can store the board in the Solutions array.
  if not FindFreeCell(row, col) then
  begin
    SetLength(FSolutions, Length(FSolutions) + 1);
    FSolutions[High(FSolutions)] := FBoard;
    exit;
  end;

  // Place a number between 1 and 9 into the empty cell found and try to solve
  // the Sudoku in a recursive way.
  for number := 1 to 9 do
  begin
    if IsSafe(row, col, number) then
    begin
      FBoard[row, col] := number;
      Solve();                    // Important: () needed !!!
      FBoard[row, col] := EMPTY;  // Back-track
    end;
  end;
end;

function TSudoku.SolveSingle: Boolean;
var
  row, col, number: Byte;
begin
  inc(SolveCounter);

  if not FindFreeCell(row, col) then
  begin
    Result := true;
    exit;
  end;

  for number := 1 to 9 do
  begin
    if IsSafe(row, col, number) then
    begin
      FBoard[row, col] := number;
      if SolveSingle() then       // Important: () needed !!!
      begin
        Result := true;
        exit;
      end;
      FBoard[row, col] := EMPTY;  // Back-track
    end;
  end;

  Result := false;
end;

end.

