unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ButtonPanel, Translations, PrintersDlgs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ButtonPanel1: TButtonPanel;
    ColorDialog1: TColorDialog;
    OpenDialog1: TOpenDialog;
    PrintDialog1: TPrintDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Unit2;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  MessageDlg(rsThisIsATest, mtConfirmation, [mbYes, mbNo, mbCancel], 0);   // Or insert the string directly in German...
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  OpenDialog1.Execute;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ColorDialog1.Execute;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  PrintDialog1.Execute;
end;

var
  PODir: String;

initialization
  PODir := Application.Location;
  TranslateUnitResourceStrings('lclstrconsts', PODir + 'lclstrconsts.de.po');

end.

