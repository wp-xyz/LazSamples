unit lcmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterHTML, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls,
  fasthtmlparser;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnExecute: TButton;
    EdURL: TEdit;
    MemoXRefs: TMemo;
    PageControl1: TPageControl;
    SynEdit1: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    TabHTML: TTabSheet;
    TabSheet3: TTabSheet;
    procedure BtnExecuteClick(Sender: TObject);
  private
    FMemStream: TMemoryStream;
    procedure ExtractXRefsFromHTML(AStream: TStream);
    procedure FoundTagHandler(NoCaseTag, ActualTag: String);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  fphttpclient, openssl;

// http://forum.lazarus.freepascal.org/index.php/topic,17621.msg97473.html#msg97473
// also:
// http://forum.lazarus.freepascal.org/index.php/topic,37072.msg248285.html#msg248285

function DownloadHTTP(URL: String; AStream: TStream; out AErrMsg: String): Boolean;
begin
  AErrMsg := '';
  InitSSLInterface;

  with TFPHTTPClient.Create(nil) do
    try
      AllowRedirect := true;     // avoid error 301
      try
        AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
        HTTPMethod('GET', URL, AStream, [200, 404]);
        AStream.Position := 0;
        Result := (ResponseStatusCode = 200);
        if not Result then
          AErrMsg := Format('Error code %d: %s', [ResponseStatusCode, ResponseStatusText]);
      except
        Result := false;
        AErrMsg := 'Cannot connect to server.';
      end;
    finally
      Free;
    end;
end;


{ TMainForm }

procedure TMainForm.BtnExecuteClick(Sender: TObject);
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

    SynEdit1.Lines.LoadFromStream(stream);
    ExtractXRefsFromHTML(stream);

  finally
    stream.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ExtractXRefsFromHTML(AStream: TStream);
var
  parser: THTMLParser;
  s: String;
begin
  SetLength(s, AStream.Size);
  AStream.Position := 0;
  AStream.ReadBuffer(s[1], AStream.Size);

  FMemStream := TMemoryStream.Create;
  try
    FMemStream.Clear;
    parser := THTMLParser.Create(s);
    try
      parser.OnFoundTag := @FoundTagHandler;
      parser.Exec;
    finally
      parser.Free;
    end;
    FMemStream.Position := 0;
    MemoXRefs.Lines.LoadFromStream(FMemStream);
  finally
    FreeAndNil(FMemStream);
  end;
end;

procedure TMainForm.FoundTagHandler(NoCaseTag, ActualTag: string);
const
  A_TAG = '<A HREF=';
var
  url: String;
begin
  if pos(A_TAG, NoCaseTag) = 1 then begin
    url := AnsiDequotedStr(Copy(ActualTag, Length(A_TAG)+1, MaxInt), '"');
    if pos('http', url) = 1 then begin
      FMemStream.WriteBuffer(url[1], Length(url));
      FMemStream.WriteBuffer(LineEnding[1], Length(LineEnding));
    end;
  end;
end;

end.

