unit txMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterHTML, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls,
  fasthtmlparser, htmlutil;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnExecute: TButton;
    EdURL: TEdit;
    MemoText: TMemo;
    PageControl1: TPageControl;
    StatusBar: TStatusBar;
    SynEdit1: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    TabHTML: TTabSheet;
    TabText: TTabSheet;
    procedure BtnExecuteClick(Sender: TObject);
  private
    FMemStream: TMemoryStream;
    FInTable: Boolean;
    FInRow: Integer;
    FCellCounter: Integer;
    procedure ExtractTextFromHtml(AStream: TStream);
    procedure FoundTagHandler(NoCaseTag, ActualTag: String);
    procedure FoundTextHandler(AText: String);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  fphttpclient, regexpr;

var
  NEW_LINE: String = #10;
  FS: String = ';';

// Load internet page, avoid error 301
// http://forum.lazarus.freepascal.org/index.php/topic,17621.msg97473.html#msg97473
// also:
// http://forum.lazarus.freepascal.org/index.php/topic,37072.msg248285.html#msg248285
function DownloadHTTP(URL: String; AStream: TStream; out AErrMsg: String): Boolean;
var
  s: String;
begin
  AErrMsg := '';

  with TFPHTTPClient.Create(nil) do
    try
      AllowRedirect := true;           // that's it!
      Get(URL, AStream);
      Result := (ResponseStatusCode = 200);
      if not Result then
        AErrMsg := ResponseStatusText;
      (*
      HTTPMethod('GET', URL, AStream, [200,301]);
      case ResponseStatusCode of
        200:
          Result := True;
        301:
          begin
            // actually this is not really safe, the new location could also return 301
            s := ResponseHeaders.Text; //Values['Location'];
            Get(ResponseHeaders.Values['Location'], AStream);
            Result := true;
          end;
        else
          // ... add here other expected status code, or else to handle unexpected ones
          Result := false;
          AErrMsg := 'Return code ' + IntToStr(ResponseStatusCode) + LineEnding +
            ResponseStatusText;
      end;
      *)
    finally
      Free;
    end;
end;

{ TForm1 }

procedure TForm1.BtnExecuteClick(Sender: TObject);
var
  stream: TMemoryStream;
  err: String;
begin
  Screen.Cursor := crHourGlass;
  stream := TMemoryStream.Create;
  try
    if not DownloadHTTP(EdURL.Text, stream, err) then
    begin
      MessageDlg(err, mtError, [mbOK], 0);
      exit;
    end;

    stream.Position := 0;
    SynEdit1.Lines.LoadFromStream(stream);

    stream.Position := 0;
    MemoText.Lines.BeginUpdate;
    try
      MemoText.Lines.Clear;
      ExtractTextFromHtml(stream);
    finally
      MemoText.Lines.EndUpdate;
    end;
  finally
    stream.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.ExtractTextFromHtml(AStream: TStream);
var
  parser: THTMLParser;
  s: String;
  t: TTime;
  i: Integer;
begin
  SetLength(s, AStream.Size);
  AStream.ReadBuffer(s[1], AStream.Size);

  FMemStream := TMemoryStream.Create;
  try
    t := Now;
    parser := THtmlParser.Create(s);
    try
      FMemStream.Clear;
      parser.OnFoundTag := @FoundTagHandler;
      parser.OnFoundText := @FoundTextHandler;
      parser.Exec;
    finally
      parser.Free;
    end;

    FMemStream.Position := 0;
    MemoText.Lines.LoadFromStream(FMemStream);

  finally
    FreeAndNil(FMemStream);
  end;
end;

procedure TForm1.FoundTagHandler(NoCaseTag, ActualTag: string);
begin
  if Length(NoCaseTag) < 4 then
    exit;
  case NoCaseTag[2] of
    'T': if Pos('<TABLE', NoCaseTag) = 1 then       // <table>
           FInTable := true
         else
         if (NoCaseTag[3] in ['R']) then            // <tr>
           inc(FInRow)
         else
         if (NoCaseTag[3] in ['D','H']) and (NoCaseTag[4] in [' ', '>']) then
         begin                                      // <tc>, <th>, not <thead>
           if FCellCounter > 0 then
             FMemStream.WriteBuffer(FS[1], Length(FS));
           inc(FCellCounter);
         end;
    '/': if pos('</TABLE', NoCaseTag) = 1 then      // </table>
           FInTable := false
         else if (NoCaseTag[3] = 'T') then
         begin
           if (NoCaseTag[4] in ['R']) then begin    // </tr>
             dec(FInRow);
             FCellCounter := 0;
             FMemStream.WriteBuffer(NEW_LINE[1], Length(NEW_LINE));
           end;
         end;
  end;
end;

procedure TForm1.FoundTextHandler(AText: String);
begin
  if FInTable then
    if (FInRow > 0) and (FCellCounter > 0) then
      FMemStream.Write(AText[1], Length(AText));
end;

end.

