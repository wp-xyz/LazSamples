unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, DBGrids, DbCtrls, db, sqldb, odbcconn;
type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
    DataSource1: TDataSource;
    Grid: TDBGrid;
    DBNavigator: TDBNavigator;
    ODBCConnection1: TODBCConnection;
    Panel1: TPanel;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, LazFileUtils;


const
//  DB_NAME = '.\northwind.mdb';            // Access 97 file format
  DB_NAME = '.\test_data.mdb';


{ Code to create a new, empty Access database file }

const
   ODBC_ADD_DSN=1;
   ODBC_CONFIG_DSN=2;
   ODBC_REMOVE_DSN=3;
   ODBC_ADD_SYS_DSN=4;
   ODBC_CONFIG_SYS_DSN=5;
   ODBC_REMOVE_SYS_DSN=6;
   ODBC_REMOVE_DEFAULT_DSN=7;

function SQLConfigDataSource(hwndParent: Integer; fRequest: Integer;
  lpszDriverString: PChar; lpszAttributes: PChar): Integer; stdcall;
  external 'odbccp32.dll';

function SQLInstallerError(iError: integer; pfErrorCode: PInteger;
  lpszErrorMsg: string; cbErrorMsgMax: integer; pcbErrorMsg: PInteger): integer; stdcall;
  external 'odbccp32.dll';

function CreateAccessDatabase(DatabaseFile: string): boolean;
var
  DBPChar: PChar;
  Driver: PChar;
  ErrorCode, ResizeErrorMessage: integer;
  ErrorMessage: PChar;
  retCode: integer;
begin
//  driver := 'Microsoft Access Driver (*.mdb)';
  driver := 'Microsoft Access Driver (*.mdb, *.accdb)';
  DBPChar:=PChar('CREATE_DBV4="'+DatabaseFile+'"');
  retCode := SQLConfigDataSource(Hwnd(nil), ODBC_ADD_DSN, Driver, DBPChar);
  if retCode<>0 then
  begin
    //try alternate driver
    Driver := 'Microsoft Access Driver (*.md, *.accdb)';
    DBPChar:=PChar('CREATE_DB="'+DatabaseFile+'"');
    retCode := SQLConfigDataSource(Hwnd(nil), ODBC_ADD_DSN, Driver, DBPChar);
  end;
  if retCode=0 then
  begin
    result:=true;
  end
  else
  begin
    result:=false;
    ErrorCode:=0;
    ResizeErrorMessage:=0;
    // todo: verify how the DLL is called - use pointers?; has not been tested.
    GetMem(ErrorMessage,512);
    try
      SQLInstallerError(1, @ErrorCode, ErrorMessage, SizeOf(ErrorMessage), @ResizeErrorMessage);
    finally
      FreeMem(ErrorMessage);
    end;
    raise Exception.CreateFmt('Error creating Access database: %s', [ErrorMessage]);
  end;
end;


{ TMainForm }

// Setting up the database connection and the datastore. Preselect a resource
// in the resource combo.
procedure TMainForm.FormCreate(Sender: TObject);
var
  needData: Boolean = false;
begin
  try
    if not FileExists(DB_NAME) then
    begin
      CreateAccessDatabase(DB_NAME);
      needData := true;
    end;

    // Connection
    ODBCConnection1.Driver := 'Microsoft Access Driver (*.mdb)';
    ODBCConnection1.Params.Clear;
    ODBCConnection1.Params.Add('DBQ=' + DB_NAME);
    ODBCConnection1.Connected := true;
    ODBCConnection1.KeepConnection := true;

    // Transaction
    SQLTransaction1.DataBase := ODBCConnection1;
    SQLTransaction1.Active := True;

    if needData then
    begin
      ODBCConnection1.ExecuteDirect(
        'CREATE TABLE People ('+
          'ID COUNTER CONSTRAINT PK_PeopleID PRIMARY KEY, ' +
          '[Last Name] TEXT(20), ' +
          '[First Name] TEXT(20) ,'+
          '[Birth date] DATE, ' +
          'Gender TEXT(1), ' +
          'Profession TEXT(20), ' +
          'Nationality TEXT(20) ' +
        ')'
      );
      ODBCConnection1.ExecuteDirect(
        'CREATE INDEX SI_PeopleName ON People([Last Name], [First Name])' );

      { Add some data }

      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Dylan'', ''Bob'', #1941-05-24#, ''Musician'', ''M'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Einstein'', ''Albert'', #1879-03-14#, ''Scientist'', ''M'', ''German'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Solzhenitsyn'', ''Aleksandr'', #1918-12-11#, ''Author'', ''M'', ''Russian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Lincoln'', ''Abraham'', #1809-02-12#, ''Politician'', ''M'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Shelley'', ''Mary'', #1851-02-1#, ''Author'', ''F'', ''British'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Steinbeck'', ''John'', #1902-02-27#, ''Author'', ''M'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Hesse'', ''Hermann'', #1877-07-02#, ''Author'', ''M'', ''Swiss'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Picasso'', ''Pablo'', #1881-10-25#, ''Artist'', ''M'', ''Spanish'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Van Gogh'', ''Vincent'', #1853-03-30#, ''Artist'', ''M'', ''Dutch'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Monroe'', ''Marilyn'', #1926-06-01#, ''Actor/actress'', ''F'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Beethoven'', ''Ludwig van'', #1770-12-17#, ''Musician'', ''M'', ''German'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Mozart'', ''Wolfgang Amadeus'', #1756-01-27#, ''Musician'', ''M'', ''Austrian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Meitner'', ''Lise'', #1878-11-07#, ''Scientist'', ''F'', ''Austrian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Tolstoi'', ''Leo'', #1828-09-09#, ''Author'', ''M'', ''Russian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Theresa'', ''Maria'', #1717-05-13#, ''Politician'', ''F'', ''Austrian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Gandhi'', ''Mahatma'', #1869-10-02#, ''Politician'', ''M'', ''Indian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Thatcher'', ''Margret'', #1925-10-13#, ''Politician'', ''F'', ''British'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Galilei'', ''Galileo'', #1564-02-14#, ''Scientist'', ''M'', ''Italian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Gates'', ''Bill'', #1955-10-28#, ''Business'', ''M'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Franklin'', ''Aretha'', #1942-03-25#, ''Musician'', ''F'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Pasteur'', ''Louis'', #1822-12-27#, ''Scientist'', ''M'', ''French'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Curie'', ''Marie'', #1867-11-07#, ''Scientist'', ''F'', ''French'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Taylor'', ''Elizabeth'', #1932-02-27#, ''Actor/Actress'', ''F'', ''French'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Merkel'', ''Angela'', #1954-07-17#, ''Politician'', ''F'', ''German'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Buonarotti'', ''Michelangelo'', #1475-03-06#, ''Artist'', ''M'', ''Italian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], [Birth date], Profession, Gender, Nationality) ' +
         ' VALUES(''Frank'', ''Anne'', #1929-06-12#, ''Author'', ''F'', ''German'')'
      );
    end;

    // Prepare sample queries. Tested all of them to be working.
    Combobox1.Items.Clear;
    Combobox1.Items.Add('SELECT * FROM People');
    Combobox1.Items.Add('SELECT [Last Name], [First Name], Profession FROM People');
    Combobox1.Items.Add('SELECT [Last Name], [First Name], Profession FROM People WHERE (Profession = ''Author'')');
    Combobox1.Items.Add('SELECT [Last Name], [First Name], Profession FROM People WHERE (Profession LIKE ''A%'')');
    Combobox1.Items.Add('SELECT [Last Name], [First Name], Profession FROM People WHERE (Profession LIKE ''[A,M]%'')');
    Combobox1.Items.Add('SELECT [Last Name], [First Name], Profession FROM People WHERE (Profession LIKE ''[!A,!M]%'')');
    Combobox1.Items.Add('SELECT [Last Name], [First Name] FROM People WHERE ([First Name] LIKE ''___'')');    // 3 arbitrary characters
    Combobox1.Items.Add('SELECT [Last Name], [First Name], [Birth date] FROM People WHERE ([Birth date] = #1941-5-24#)');
    Combobox1.Items.Add('SELECT [Last Name], [First Name], [Birth date] FROM People WHERE (([Birth date] >= #1800-01-01#) AND ([Birth date] <= #1899-12-31#))');
    Combobox1.Items.Add('SELECT [Last Name], [First Name], [Birth date] FROM People WHERE ([Birth date] BETWEEN #1700-01-01# AND #1799-12-31#)');
    Combobox1.Items.Add('SELECT [Last Name], [First Name], [Birth date] FROM People WHERE (YEAR([Birth date]) = 1770)');
    Combobox1.Items.Add('SELECT [Last Name], [First Name], [Birth date], Profession FROM People WHERE ((YEAR([Birth date]) >= 1800) AND (Profession = ''Scientist''))');
    Combobox1.Items.Add('SELECT [Last Name], [First Name], [Birth date] FROM People ORDER BY [Birth date], [Last Name], [First Name]');
    Combobox1.Items.Add('SELECT Profession, COUNT(*) AS [Number of records] FROM People GROUP BY Profession');
    Combobox1.Items.Add('SELECT Profession, COUNT(*) AS [Number of records] FROM People GROUP BY Profession HAVING (Profession LIKE ''A%'')');

    Combobox1.Text := Combobox1.Items[0];

    // Open the dataset displayed in the grid
    Button1Click(nil);
  except
    on E:Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      Close;
    end;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if ComboBox1.Text = '' then
    exit;

  SQLQuery1.Close;
  SQLQuery1.SQL.Text := ComboBox1.Text;
  SQLQuery1.Open;
end;

procedure TMainForm.ComboBox1Select(Sender: TObject);
begin
  Button1Click(nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ODBCConnection1.Connected := false;
end;

end.

