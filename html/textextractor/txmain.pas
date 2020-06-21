unit txMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterHTML, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnExecute: TButton;
    EdFilename: TEdit;
    MemoText: TMemo;
    PageControl1: TPageControl;
    SynEdit1: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    TabHTML: TTabSheet;
    TabText: TTabSheet;
    procedure BtnExecuteClick(Sender: TObject);
  private
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  html2text;


{ TForm1 }

procedure TForm1.BtnExecuteClick(Sender: TObject);
var
  stream: TMemoryStream;
begin
  Screen.Cursor := crHourGlass;
  stream := TMemoryStream.Create;
  try
    stream.LoadFromFile(EdFilename.Text);

    stream.Position := 0;
    SynEdit1.Lines.LoadFromStream(stream);

    MemoText.Lines.Text := ExtractTextFromHtml(SynEdit1.Lines.Text);

  finally
    stream.Free;
    Screen.Cursor := crDefault;
  end;
end;

end.

