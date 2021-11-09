// Based on code by forum user Zvoni.

program sqlite3_console;

uses
  sysutils, classes, sqldb, db, sqlite3conn;

var
  fn: String;
  SQLiteConn: TSQLite3Connection;
  Transaction: TSQLTransaction;
  SQLQuery: TSQLQuery;

  function RandomString(ALen: Integer): String;
  var
    i: Integer;
  begin
    SetLength(Result, ALen);
    for i := 1 to ALen do
      Result[i] := char(ord('A') + Random(26));
  end;

begin
  Randomize;

  fn := GetCurrentDir + '/test.db';
  //DeleteFile(fn);  // uncomment to begin with new empty db

  { Set up databse components }
  SQLiteConn := TSQLite3Connection.Create(nil);
  SQLiteConn.DatabaseName := fn;
  Transaction := TSQLTransaction.Create(SQLiteConn);
  Transaction.DataBase := SQLiteConn;
  SQLQuery := TSQLQuery.Create(nil);
  SQLQuery.DataBase := SQLiteConn;
  SQLQuery.Transaction := Transaction;
  SQLiteConn.Open;

  { Create database, if not yet existing }
  SQLQuery.SQL.Text := 'CREATE TABLE IF NOT EXISTS "tbl_data" ('+
    '"ID" INTEGER NOT NULL UNIQUE, '+
    '"StringData" TEXT, '+
    'PRIMARY KEY("ID")'+
  ')';
  SQLQuery.ExecSQL;
  Transaction.Commit;

  { Add three records }
  SQLQuery.SQL.Text := 'INSERT INTO tbl_data (StringData) VALUES (:ParamData);';
  SQLQuery.ParamByName('ParamData').AsString := RandomString(Random(20) + 1);
  SQLQuery.ExecSQL;
  SQLQuery.ParamByName('ParamData').AsString := RandomString(Random(20) + 1);
  SQLQuery.ExecSQL;
  SQLQuery.ParamByName('ParamData').AsString := RandomString(Random(20) + 1);
  SQLQuery.ExecSQL;
  Transaction.Commit;

  { Read and display all records }
  SQLQuery.SQL.Text:='SELECT * FROM tbl_data;';
  SQLQuery.Open;
  SQLQuery.First;
  WriteLn('ID':10, ' StringData':20);
  WriteLn('---------- --------------------');
  while not SQLQuery.EoF do
  begin
    Write(SQLQuery.FieldByName('ID').AsInteger:10);
    WriteLn(SQLQuery.FieldByName('StringData').AsString:20);
    SQLQuery.Next;
  end;
  WriteLn;

  { Read and display a specific record }
  SQLQuery.Close;
  SQLQuery.SQL.Text:='SELECT * FROM tbl_data WHERE ID=3;';
  SQLQuery.Open;
  Writeln(
    'StringData for ID=',SQLQuery.FieldByName('ID').AsString,': ',
    '"' + SQLQuery.FieldByName('StringData').AsString + '"'
  );
  SQLQuery.Close;

  { Clean up }
  SQLQuery.Free;
  Transaction.Free;
  SQLiteConn.Free;

  ReadLn;
end.

