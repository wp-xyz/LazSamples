unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs;

type
  TForm1 = class(TForm)
    ImageList: TImageList;
    TreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with TreeView.Items.Add(nil, 'Node 1 (no overlay)') do
  begin
    ImageIndex := 0;
    SelectedIndex := 0;
  end;

  with TreeView.Items.Add(nil, 'Node 2 (with overlay)') do
  begin
    ImageIndex := 1;
    SelectedIndex := 1;
    OverlayIndex := 6;
  end;

  with TreeView.Items.Add(nil, 'Node 3 (with overlay)') do
  begin
    ImageIndex := 3;
    SelectedIndex := 3;
    OverlayIndex := 7;
  end;

  with TreeView.Items.Add(nil, 'Node 4 (with overlay)') do
  begin
    ImageIndex := 4;
    SelectedIndex := 4;
    OverlayIndex := 8;
  end;

  with TreeView.Items.Add(nil, 'Node 5 (no overlay)') do
  begin
    ImageIndex := 5;
    SelectedIndex := 5;
  end;
end;

end.

