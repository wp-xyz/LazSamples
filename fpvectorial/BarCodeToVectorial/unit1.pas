unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ubarcodes,
  fpVectorial, wmfVectorialWriter, svgVectorialWriter;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  fpvBarCode;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
const
  W = 100;
  H = 100;
var
  msg: String;
  barcode: TBarcodeQR;
  stream: TMemoryStream;
begin
  barcode := TBarcodeQR.Create(nil);
  try
    //barcode.ForegroundColor := clRed;
    barcode.Parent := self;
    barcode.Text := Edit1.Text;
    barcode.Generate;

    stream := TMemoryStream.Create;
    try
      BarcodeToFPVectorialStream(stream, barcode, W, H, vfWindowsMetafileWMF, msg);
      if msg = '' then
        stream.SaveToFile('barcode.wmf')
      else
        MessageDlg(msg, mtError, [mbOK], 0);

      stream.Clear;
      BarcodeToFPVectorialStream(stream, barcode, W, H, vfSVG, msg);
      if msg = '' then
        stream.SaveToFile('barcode.svg')
      else
        Messagedlg(msg, mtError, [mbOK], 0);

    finally
      stream.Free;
    end;
  finally
    barcode.Free;
  end;
end;

end.

