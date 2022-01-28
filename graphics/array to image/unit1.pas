unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FRawData: Array of uint32;
    procedure GenerateRawData;
    procedure CreateImage;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  GraphType, IntfGraphics; 

const
  IMG_WIDTH = 256;
  IMG_HEIGHT = 256;
  
{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  GenerateRawData;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CreateImage;
end;

// Generate a 1D array of uint32 values (assume: left-to-right striped R/G/B gradient)
procedure TForm1.GenerateRawData;
var
  i, j, k: Integer;
  c: Byte;
begin
  SetLength(FRawData, IMG_HEIGHT * IMG_WIDTH);
  k := 0;
  for j := 0 to IMG_HEIGHT-1 do
    for i := 0 to IMG_WIDTH-1 do
    begin
      c := round(i/(IMG_Width-1) * 255);
      if j < IMG_HEIGHT div 3 then
        FRawData[k] := RGBToColor(c, 0, 0)
      else if j < 2 * IMG_HEIGHT div 3 then
        FRawData[k] := RGBToColor(0, c, 0)
      else
        FRawdata[k] := RGBToColor(0, 0, c);
      inc(k);
    end;
end;

procedure TForm1.CreateImage;
var
  rawImg: TRawImage;
  bmp: TBitmap;
begin
  { Create a rawimage with RGBA color format and link data array to it }
  
  rawImg.Init;
  //rawImg.Description.Init_BPP32_A8R8G8B8_BIO_TTB(IMG_WIDTH, IMG_HEIGHT);
  rawImg.Description.Init_BPP32_R8G8B8A8_BIO_TTB(IMG_WIDTH, IMG_HEIGHT);
  rawImg.DataSize := IMG_WIDTH * IMG_HEIGHT * SizeOf(FRawData[0]); // size of the FRawData array, in bytes
  rawImg.Data := @FRawData[0];  // rawImg.Data points to first byte of the FRawData array
  
  { Create a bitmap from the rawImg and display it in the Image component }
  bmp := TBitmap.Create;
  try
    bmp.LoadFromRawImage(rawImg, false);  // false = bmp does not "own" the image data.
    Image1.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

end.

