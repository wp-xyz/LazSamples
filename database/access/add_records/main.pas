unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, DBGrids, DbCtrls, db, sqldb, odbcconn;
type

  { TMainForm }

  TMainForm = class(TForm)
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    edFirstName: TDBEdit;
    edLastName: TDBEdit;
    edProfession: TDBEdit;
    edNationality: TDBEdit;
    Grid: TDBGrid;
    DBNavigator: TDBNavigator;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ODBCConnection1: TODBCConnection;
    Panel2: TPanel;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
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
  DB_NAME = 'test_data.mdb';

{ Code to create a new, empty Access database file }

const
   ODBC_ADD_DSN=1;
   { just for reference...
   ODBC_CONFIG_DSN=2;
   ODBC_REMOVE_DSN=3;
   ODBC_ADD_SYS_DSN=4;
   ODBC_CONFIG_SYS_DSN=5;
   ODBC_REMOVE_SYS_DSN=6;
   ODBC_REMOVE_DEFAULT_DSN=7; }

function SQLConfigDataSource(hwndParent: Integer; fRequest: Integer;
  lpszDriverString: PChar; lpszAttributes: PChar): Integer; stdcall;
  external 'odbccp32.dll';

function SQLInstallerError(iError: integer; pfErrorCode: PInteger;
  lpszErrorMsg: string; cbErrorMsgMax: integer; pcbErrorMsg: PInteger): integer; stdcall;
  external 'odbccp32.dll';

function CreateAccessDatabase(DatabaseFile: string): boolean;
var
  DBPChar: PChar;
  Driver: String;
  ErrorCode, ResizeErrorMessage: integer;
  ErrorMessage: PChar;
  retCode: integer;
begin
  driver := 'Microsoft Access Driver (*.mdb)';
  DBPChar:=PChar('CREATE_DBV4="'+DatabaseFile+'"');
  retCode := SQLConfigDataSource(Hwnd(nil), ODBC_ADD_DSN, PChar(driver), DBPChar);
  if retCode<>0 then
  begin
    //try alternate driver
    driver := 'Microsoft Access Driver (*.mdb, *.accdb)';
    DBPChar:=PChar('CREATE_DB="'+DatabaseFile+'"');
    retCode := SQLConfigDataSource(Hwnd(nil), ODBC_ADD_DSN, PChar(driver), DBPChar);
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
  i: Integer;
  db_filename: String;
begin
  db_filename := Application.Location  + 'test-data.mdb';
  try
    if not FileExists(db_filename) then
    begin
      CreateAccessDatabase(db_filename);
      needData := true;
    end;

    // Connection
    ODBCConnection1.Driver := 'Microsoft Access Driver (*.mdb)';
    ODBCConnection1.Params.Clear;
    ODBCConnection1.Params.Add('DBQ=' + db_filename);
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
          'Profession TEXT(15), ' +
          'Nationality TEXT(15) ' +
        ')'
      );
      ODBCConnection1.ExecuteDirect(
        'CREATE INDEX SI_PeopleName ON People([Last Name], [First Name])' );

      { Add some data }
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Dylan'', ''Bob'', ''Musician'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Einstein'', ''Albert'', ''Scientist'', ''German'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Solzhenitsyn'', ''Aleksandr'', ''Author'', ''Russian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Lincoln'', ''Abraham'', ''Politician'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Shelley'', ''Mary'', ''Author'', ''British'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Steinbeck'', ''John'', ''Author'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Hesse'', ''Hermann'', ''Author'', ''Swiss'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Picasso'', ''Pablo'',''Artist'', ''Spanish'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Van Gogh'', ''Vincent'', ''Artist'', ''Dutch'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Monroe'', ''Marilyn'', ''Actor/actress'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Beethoven'', ''Ludwig van'', ''Musician'', ''German'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Mozart'', ''Wolfgang Amadeus'', ''Musician'', ''Austrian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Meitner'', ''Lise'', ''Scientist'', ''Austrian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Tolstoi'', ''Leo'', ''Author'', ''Russian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Theresa'', ''Maria'', ''Politician'', ''Austrian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Gandhi'', ''Mahatma'', ''Politician'', ''Indian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Thatcher'', ''Margret'', ''Politician'', ''British'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Galilei'', ''Galileo'', ''Scientist'', ''Italian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Gates'', ''Bill'', ''Business'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Franklin'', ''Aretha'', ''Musician'', ''American'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Pasteur'', ''Louis'', ''Scientist'', ''French'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Curie'', ''Marie'', ''Scientist'', ''French'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Taylor'', ''Elizabeth'', ''Actor/Actress'', ''French'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Merkel'', ''Angela'', ''Politician'', ''German'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Buonarotti'', ''Michelangelo'', ''Artist'', ''Italian'')'
      );
      ODBCConnection1.ExecuteDirect(
        'INSERT INTO People ([Last Name], [First Name], Profession, Nationality) ' +
         ' VALUES(''Frank'', ''Anne'', ''Author'', ''German'')'
      );
    end;
  except
    on E:Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      Close;
    end;
  end;

  edFirstName.DataField := 'First Name';
  edLastName.DataField := 'Last Name';
  edProfession.DataField := 'Profession';
  edNationality.DataField := 'Nationality';

  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Add('SELECT');
  SQLQuery1.SQL.Add('[Last Name], [First Name], Profession, Nationality');
  SQLQuery1.SQL.Add('FROM People');
  SQLQuery1.SQL.Add('ORDER BY [Last Name], [First Name]');
  SQLQuery1.Open;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ODBCConnection1.Connected := false;
end;

end.

