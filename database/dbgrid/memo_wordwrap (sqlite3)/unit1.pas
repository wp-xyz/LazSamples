unit Unit1;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DbCtrls, DBGrids, Grids;

type

  { TForm1 }

  TForm1 = class(TForm)
    DBMemo1: TDBMemo;
    DsArticles: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    SQLite3Connection1: TSQLite3Connection;
    QryArticles: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid1GetCellHint(Sender: TObject; Column: TColumn;
      var AText: String);
    procedure FormCreate(Sender: TObject);
  private
    procedure CreateTables;
    procedure PopulateTables;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, LazUTF8;

const
  DB_NAME = 'test.sqlite';
  MAX_HINT_WIDTH = 500;

type
  TMyDBGrid = class(TDBGrid);

type
  TMyHintWindow = class(THintWindow)
  public
    function CalcHintRect(MaxWidth: Integer; const AHint: String;
      AData: Pointer): TRect; override;
  end;

// Trims width of the hint window to MAX_HINT_WIDTH pixels
function TMyHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: String;
  AData: Pointer): TRect;
begin
  Result := inherited;
  if Result.Width > MAX_HINT_WIDTH then
    Result.Right := Result.Left + MAX_HINT_WIDTH;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  h: Integer;
begin
  SQLite3Connection1.DatabaseName := DB_NAME;
  if not FileExists(DB_NAME) then begin
    CreateTables;
    PopulateTables;
  end;
  SQLite3Connection1.Connected := true;
  QryArticles.SQL.Add('SELECT * FROM Articles');
  QryArticles.Open;

  DBMemo1.DataField := 'Comments';

  h := DBGrid1.DefaultRowHeight;
  // Cell height for 3 lines of text
  DBGrid1.DefaultRowHeight := DBGrid1.Canvas.TextHeight('A') * 3 + 2*varCellPadding;;
  TMyDBGrid(DBGrid1).RowHeights[0] := h;

  // Width of the Article column
  DBGrid1.Columns[1].Width := 100;

  // Width of the memo column
  for i:=0 to DBGrid1.Columns.Count-1 do
    if DBGrid1.Columns[i].Field.DataType = ftMemo then
      DBGrid1.Columns[i].Width := 200;

  // Nice display of Price column
  DBGrid1.Columns[3].DisplayFormat := '0.00';
  //better (but requires FPC 3.2+):
  //DBGrid1.Columns.ColumnByFieldName('Price').DisplayFormat := '0.00';

  // Introduce a new HintWindow class to reduce hint window width
  HintWindowClass := TMyHintWindow;
end;

procedure TForm1.CreateTables;
begin
  SQLTransaction1.StartTransaction;
  SQLite3Connection1.Connected := true;
  SQLite3Connection1.ExecuteDirect('CREATE TABLE Articles (' +
    'ArticleID INTEGER UNIQUE NOT NULL, '+
    'ArticleName VARCHAR(50), '+
    'Comments TEXT, '+
    'Price FLOAT)');
  SQLTransaction1.Commit;
  SQLite3Connection1.Connected := false;
end;

procedure TForm1.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  field: TField;
  ts: TTextStyle;
  s: String;
  h, hRect: Integer;
  i, dn, n, nprev: Integer;
  R: TRect;
  p: PChar;
begin
  field := DBGrid1.Columns[DataCol].Field;
  if field.DataType = ftMemo then begin
    DBGrid1.Canvas.FillRect(Rect);
    ts := DBGrid1.Canvas.TextStyle;
    ts.WordBreak := true;
    ts.SingleLine := false;
    DBGrid1.Canvas.TextStyle := ts;
    h := 0;
    hRect := Rect.Bottom - Rect.Top;
    s := field.AsString;
    n := 0;
    nprev := 0;
    p := @s[1];
    for i:=1 to Length(s) do begin
      dn := UTF8CodepointSize(p);
      n := n + dn;
      R := Rect;
      InflateRect(R, -constCellPadding, -constCellPadding);
      DrawText(DBGrid1.Canvas.Handle, PChar(s), n, R, DT_CALCRECT + DT_WORDBREAK);
      h := R.Bottom - R.Top + 2*constCellPadding;
      if h > hRect then begin
        n := nprev;
        break;
      end;
      nprev := n;
      inc(p, dn);
    end;
    DBGrid1.Canvas.FillRect(Rect);
    R := Rect;
    InflateRect(R, -constCellPadding, -constCellPadding);
    DrawText(DBGrid1.Canvas.Handle, PChar(s), n, R, DT_WORDBREAK + DT_LEFT + DT_VCENTER);
  end;
end;

procedure TForm1.DBGrid1GetCellHint(Sender: TObject; Column: TColumn;
  var AText: String);
begin
  if Length(AText) > 200 then
    AText := Copy(AText, 1, 1000) + ' [...]';
end;

procedure TForm1.PopulateTables;
const
  N = 10;
var
  i, j: Integer;
  id: Integer;
  s: String = '';
  sc: String;
  p: Double;
  fs: TFormatSettings;
begin
  SQLTransaction1.StartTransaction;
  fs := FormatSettings;
  fs.DecimalSeparator := '.';
  for i:=0 to N-1 do begin
    id := Random(100000);
    p := Random * 1000;
    SetLength(s, 3 + Random(5));
    for j:=1 to Length(s) do
      s[j] := char(Random(26) + ord('A'));
    sc := IntToStr(i) + LineEnding +
      'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.';
    SQLite3Connection1.ExecuteDirect(Format(
      'INSERT INTO Articles (ArticleID, ArticleName, Comments, Price) VALUES (%d, "%s", "%s", %.2f)', [
      id, s, sc, p
    ], fs));
  end;
  SQLTransaction1.Commit;
end;

end.

