program sudoku;

uses
  SysUtils, uSudoku;

{ Some Sudoku boards for testing...

const
  Board1: TBoard = (
    (0, 7, 6,   0, 1, 3,   0, 0, 0),
    (0, 4, 0,   0, 0, 0,   0, 0, 0),
    (0, 0, 8,   6, 9, 0,   7, 0, 0),

    (0, 5, 0,   0, 6, 9,   0, 3, 0),
    (0, 0, 0,   0, 0, 0,   5, 4, 0),
    (0, 8, 0,   7, 3, 0,   0, 0, 0),

    (5, 1, 0,   0, 2, 6,   8, 0, 0),
    (0, 0, 7,   1, 0, 0,   9, 0, 0),
    (0, 0, 0,   0, 4, 0,   0, 6, 0)
  );

  Board1Str = '076013000,040000000,008690700,050069030,000000540,080730000,510026800,007100900,000040060';

  Board2: TBoard = (
    (0, 0, 3,   0, 7, 0,   0, 6, 1),
    (0, 0, 0,   0, 0, 0,   0, 8, 0),
    (0, 0, 0,   0, 0, 0,   0, 2, 3),

    (0, 0, 0,   7, 0, 0,   0, 0, 4),
    (0, 0, 0,   2, 0, 0,   0, 0, 0),
    (5, 0, 6,   0, 0, 0,   0, 0, 9),

    (9, 0, 0,   0, 4, 0,   0, 0, 5),
    (0, 8, 0,   3, 0, 0,   0, 0, 0),
    (2, 0, 0,   0, 0, 8,   0, 0, 0)
  );
  Board2Str = '003070061,000000080,000000023,000700004,000200000,506000009,900040005,080300000,200008000';
}

procedure WriteHelp;
begin
  WriteLn('Syntax: sudoku SUDOKU-STRING [-all]');
  WriteLn('  SUDOKU-STRING: String of 0..9 where 0 is an empty cell.');
  WriteLn('                 Rows can be separated by comma.');
  WriteLn('                 If SUDOKU-STRING begins with "@" is will be interpreted');
  WriteLn('                 as the name of a file with the SODUKU-STRING to be used.');
  WriteLn('  -all           Get all solutions');
  Halt;
end;

function ReadSudokuFile(AFileName: String): String;
var
  F: TextFile;
  s: String;
begin
  Result := '';
  if not FileExists(AFileName) then
  begin
    WriteLn('File ' + AFileName + ' not found.');
    Halt;
  end;

  AssignFile(F, AFileName);
  try
    Reset(F);
    while not EoF(F) do
    begin
      ReadLn(F, s);
      if (s = '') or (s[1] = '#') then
        Continue;
      Result := Result + s;
    end;
  finally
    CloseFile(F);
  end;
end;

var
  s: TSudoku = nil;
  solved: Boolean = false;
  board: TBoard;
  sudokuStr: String;
  all: Boolean;
  i: Integer;
begin
  if ParamCount = 0 then
    WriteHelp;

  // Find all solutions
  all := (ParamCount > 1) and (Lowercase(ParamStr(2)) = '-all');

  try
    sudokuStr := ParamStr(1);
    if sudokuStr = '' then
      WriteHelp;
    if sudokuStr[1] = '@' then
      sudokuStr := ReadSudokuFile(Copy(sudokuStr, 2));

    board := BoardFromStr(sudokuStr);

    s := TSudoku.Create(board);
    try
      WriteLn('Sudoku to be solved');
      WriteLn(s.Board.ToNiceString);
      WriteLn;

      if all then
      begin
        s.Solve;
        if s.SolutionCount > 0 then
        begin
          solved := true;
          WriteLn(s.SolutionCount, ' solution(s) found after ', s.SolveCounter, ' recursive calls:');
          WriteLn;
          for i := 0 to s.SolutionCount-1 do
          begin
            WriteLn('Solution #', i+1);
            WriteLn(s.Solutions[i].ToNiceString);
            WriteLn;
          end;
        end;
      end else
      begin
        if s.SolveSingle then
        begin
          solved := true;
          WriteLn('Solution after ', s.SolveCounter, ' recursive calls:');
          WriteLn(s.Board.ToNiceString);
        end;
      end;
      if not solved then
      begin
        WriteLn('No solution found after ', s.SolveCounter, ' recursive calls to SolveSingle()');
      end;
    finally
      s.Free;
    end;
  except
    on E: Exception do
      WriteLn(E.Message);
  end;
end.

