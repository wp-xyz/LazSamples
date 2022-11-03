unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, ExtCtrls,
  StdCtrls, Spin;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    seRed: TFloatSpinEdit;
    seGreen: TFloatSpinEdit;
    seBlue: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ShellListView1: TShellListView;
    ShellTreeView1: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure FilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShellListView1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  GraphUtil;

{ TForm1 }

procedure TForm1.FilterChange(Sender: TObject);
begin
  Image2.Picture.Assign(Image1.Picture);
  BitmapGrayscale(Image2.Picture.Bitmap, seRed.Value, seGreen.Value, seBlue.Value);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if FileExists(ParamStr(1)) then
    ShellTreeView1.Path := ParamStr(1);
end;

procedure TForm1.ShellListView1Click(Sender: TObject);
var
  filename: String;
  ext: String;
begin
  if ShellListView1.Selected = nil then
    exit;
  filename := ShellListView1.GetPathFromItem(ShellListView1.Selected);
  ext := Lowercase(ExtractFileExt(filename));
  if not ((ext = '.png') or (ext = '.bmp') or (ext = '.jpg') or (ext = '.jpeg')) then
    exit;
  Image1.Picture.LoadFromFile(filename);
  Image2.Picture.Assign(Image1.Picture);
  BitmapGrayscale(Image2.Picture.Bitmap, seRed.Value, seGreen.Value, seBlue.Value);
end;

end.

