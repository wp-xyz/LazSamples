unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ListView1: TListView;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

var
  Counter: Integer = 0;
procedure TForm1.Button1Click(Sender: TObject);
const
  N = 1000*1000*100;
//  N = $8008;  // last ok
//  N = $FFFF;
begin
  inc(Counter);     // Just to distinguish the effect of subsequent clicks

  ListView1.Viewstyle := vsReport;
  ListView1.Columns.Clear;
  with ListView1.Columns.Add do begin
    Caption := 'Column 1';
    Width := 100;
  end;
  with ListView1.Columns.Add do begin
    Caption := 'Column 2';
    Width := 100;
  end;
  ListView1.OwnerData := true;
  ListView1.Items.Count := N;
  ListView1.OnData := @ListView1Data;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1Click(nil);
end;

procedure TForm1.ListView1Data(Sender: TObject; Item: TListItem);
begin
  Item.Caption := Format('Nr. %d.%d', [Counter, Item.Index]);
  Item.SubItems.Add(IntToStr(Item.Index) + '-1');
end;

end.

