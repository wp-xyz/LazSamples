unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbf, DB, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    dsMaster: TDataSource;
    dsDetail: TDataSource;
    dbfMaster: TDbf;
    dbfDetail: TDbf;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  dbfMaster.FilePathFull := Application.Location + 'data';
  dbfMaster.TableName := 'master.dbf';
  dbfMaster.Open;
  dbfMaster.IndexFieldNames := 'SYMBOL';

  dbfDetail.FilePathFull := Application.Location + 'data';
  dbfDetail.TableName := 'holdings.dbf';
  dbfDetail.Open;
  dbfDetail.MasterSource := dsMaster;
  dbfDetail.MasterFields := 'SYMBOL';
  dbfDetail.IndexFieldNames := 'SYMBOL';

  TNumericField(dbfMaster.FieldByName('CUR_PRICE')).DisplayFormat := '0.00';
  TNumericField(dbfDetail.FieldByName('SHARES')).DisplayFormat := '0';
  TNumericField(dbfDetail.FieldByName('PUR_PRICE')).DisplayFormat := '0.00';
end;

end.

