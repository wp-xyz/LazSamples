unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs;

type
  TForm1 = class(TForm)
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ImageIndex_ScrollBar: TScrollBar;
    Overlay0_ScrollBar: TScrollBar;
    Overlay1_Scrollbar: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure ImageIndex_ScrollBarChange(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormPaint(Sender: TObject);
begin
  // Without overlay
  ImageList.Draw(Canvas, 20, 20, ImageIndex_ScrollBar.Position);

  // With overlay #0
  ImageList.Overlay(Overlay0_ScrollBar.Position, 0);
  ImageList.DrawOverlay(Canvas, 20, 60, ImageIndex_ScrollBar.Position, 0);

  // With overlay #1
  ImageList.Overlay(Overlay1_Scrollbar.Position, 1);
  ImageList.DrawOverlay(Canvas, 20, 100, ImageIndex_ScrollBar.Position, 1);
end;

procedure TForm1.ImageIndex_ScrollBarChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImageIndex_ScrollBar.Max := 5;  // Index of the last "normal" icon in ImageList
  Overlay0_ScrollBar.Max := 10;   // Index of the last overlay icon in ImageList
  Overlay0_ScrollBar.Min := 6;    // Index of the first overlay icon in ImageList
  Overlay1_Scrollbar.Max := Overlay0_ScrollBar.Max;
  Overlay1_Scrollbar.Min := Overlay0_ScrollBar.Min;
  Overlay1_Scrollbar.Position := Overlay1_Scrollbar.Max;
end;

end.

