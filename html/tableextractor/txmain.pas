unit txMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterHTML, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls, FastHtmlParser;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnExecute: TButton;
    BtnLoad: TButton;
    EdFilename: TEdit;
    MemoText: TMemo;
    PageControl1: TPageControl;
    SynEdit1: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    TabHTML: TTabSheet;
    TabText: TTabSheet;
    procedure BtnExecuteClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
  private
    FParser: THTMLParser;
    FInTable: Boolean;
    FTextInTable: String;
    function ExtractTextFromHtmlTable(AHtmlText: String): String;
    procedure TagFound(NoCaseTag, ActualTag: string);
    procedure TextFound(AText: String);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}


{ TForm1 }

procedure TForm1.BtnExecuteClick(Sender: TObject);
begin
  if SynEdit1.Lines.Text = '' then
    exit;

  Screen.Cursor := crHourGlass;
  try
    MemoText.Lines.Text := ExtractTextFromHtmlTable(SynEdit1.Lines.Text);
    PageControl1.ActivePageIndex := 1;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.BtnLoadClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    SynEdit1.Lines.LoadFromFile(EdFileName.Text);
    PageControl1.ActivePageIndex := 0;
    BtnExecute.Enabled := true;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TForm1.ExtractTextFromHtmlTable(AHtmlText: String): String;
begin
  FParser := THTMLParser.Create(AHtmlText);
  try
    FParser.OnFoundTag := @TagFound;
    FParser.OnFoundText := @TextFound;
    FTextInTable := '';
    FParser.Exec;
    Result := FTextInTable;
  finally
    FParser.Free;
  end;
end;

procedure TForm1.TagFound(NoCaseTag, ActualTag: string);

begin
  if Copy(NoCaseTag, 1, Length('<TABLE')) = '<TABLE' then
    FInTable := true;
  if NoCaseTag = '</TABLE>' then
    FInTable := false;

  if (NoCaseTag = '</TD>') or (NoCaseTag = '</TH>') then
    FTextInTable := FTextInTable + ';';

  if NoCaseTag = '</TR>' then begin
    if FTextInTable[Length(FTextInTable)] = ';' then
      Delete(FTextInTable, Length(FTextInTable), 1);
    FTextInTable := FTextInTable + LineEnding;
  end;
end;

procedure TForm1.TextFound(AText: String);
begin
  if FInTable then begin
    while (AText <> '') and (AText[1] in [' ', #10, #13]) do
      Delete(AText, 1, 1);
    FTextInTable := FTextInTable + AText;
  end;
end;

end.

