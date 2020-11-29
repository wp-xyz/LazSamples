{ This demo shows how the table names of an Access database can be determined. }

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, StdCtrls, ExtCtrls,
  odbcconn, sqldb, db, DBGrids, DbCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    DataSource: TDatasource;
    DBGrid2: TDBGrid;
    DBNavigator2: TDBNavigator;
    ListBox1: TListBox;
    ODBCConnection1: TODBCConnection;
    Query: TSQLQuery;
    Splitter2: TSplitter;
    SQLTransaction1: TSQLTransaction;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
  public
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ListBox1Click(Sender: TObject);
var
  tblName: string;
  sql: String;
begin
  if Listbox1.ItemIndex = -1 then
    exit;

  tblName := Listbox1.Items[Listbox1.ItemIndex];
  sql := 'SELECT * FROM ' + tblName;
  Query.Close;
  Query.SQL.Text := sql;
  Query.Open;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  dbName: String;
begin
  dbName := Application.Location + 'data.mdb';

  // connection
  ODBCConnection1.Driver:= 'Microsoft Access Driver (*.mdb)';
  ODBCConnection1.Params.Text := 'DBQ=' + dbName;
  ODBCConnection1.Connected:= True;
  ODBCConnection1.KeepConnection := True;

  // transaction
  SQLTransaction1.DataBase := ODBCConnection1;
  SQLTransaction1.Action := caCommit;
  SQLTransaction1.Active := false;
  SQLTransaction1.StartTransaction;

  // Get table names
  ODBCConnection1.GetTableNames(Listbox1.Items, false);

  // Important:
  Query.Database := ODBCConnection1;
  Query.Transaction := SQLTransaction1;
  Query.UsePrimaryKeyAsKey := false;
end;


end.

