{ This demo shows how a QRCode can be constructed from a given string and
  saved as wmf or svg file.

  Requires the packages LazBarcodes and fpvectorial.
}

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
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure Button1Click(Sender: TObject);
  private
    FBarCode: TBarCodeQR;

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
  stream: TMemoryStream;
  fn: String;
begin
  if FBarCode = nil then
  begin
    FBarCode := TBarcodeQR.Create(Self);
    FBarcode.Parent := self;
  end;
  FBarcode.Text := Edit1.Text;
  FBarcode.Generate;

  stream := TMemoryStream.Create;
  try
    if RadioButton1.Checked then  // Save as wmf
    begin
      BarcodeToFPVectorialStream(stream, FBarcode, W, H, vfWindowsMetafileWMF, msg);
      fn := 'barcode.wmf';
    end else
    if RadioButton2.Checked then  // Save as svg
    begin
      BarcodeToFPVectorialStream(stream, FBarcode, W, H, vfSVG, msg);
      fn := 'barcode.svg';
    end;

    if msg = '' then begin
      stream.SaveToFile(fn);
      MessageDlg('Barcode created and saved as ' + fn, mtInformation, [mbOK], 0);
    end else
      MessageDlg(msg, mtError, [mbOK], 0);

  finally
    stream.Free;
  end;
end;

end.

