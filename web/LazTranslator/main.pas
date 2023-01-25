unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnTranslate: TButton;
    Button1: TButton;
    cbSrc: TComboBox;
    cbDest: TComboBox;
    lblSrc: TLabel;
    lblDest: TLabel;
    SrcMemo: TMemo;
    DestMemo: TMemo;
    procedure btnTranslateClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function ExtractLanguage(ACombobox: TCombobox): String;
    procedure PopulateLanguageCombo(ACombobox: TComboBox);
    procedure SelectLanguage(ACombobox: TCombobox; ALang: String);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  fphttpclient, opensslsockets, fpjson, jsonparser,
  about;

function Translate(AText, SrcLang, DestLang: string): string;
var
  client: TFPHttpClient;
  request: TRawByteStringStream;
  response: TMemoryStream;
  url, r: string;
  json: TJSONData;
  j: TJSONData;
begin
  Result := '';

  url := 'https://translate.argosopentech.com/translate';
  r := Format('{"q":"%s", "source":"%s", "target":"%s", "format":"text"}', [
    AText, SrcLang, DestLang
  ]);

  client := TFPHttpClient.Create(nil);
  try
    client.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
    client.AddHeader('Content-Type', 'application/json');
    client.AddHeader('Accept', 'application/json');
    client.AllowRedirect := true;
    request := TRawByteStringStream.Create(r);
    response := TMemoryStream.Create;
    try
      client.RequestBody := request;
      try
        client.Post(url, response);
        response.Position := 0;
        json := GetJSON(response);
        try
          if json <> nil then
          begin
            j := json.FindPath('translatedText');
            if j <> nil then
              Result := j.AsString
            else
            begin
              j := json.FindPath('error');
              if j <> nil then
                Result := j.AsString;
            end;
          end;
        finally
          json.Free;
        end;
      except
        on E:Exception do
          Result := E.Message;
      end;
    finally
      response.Free;
      request.Free;
    end;
  finally
    client.Free;
  end;
end;

{ TMainForm }

procedure TMainForm.btnTranslateClick(Sender: TObject);
var
  src, dest: String;
begin
  src := ExtractLanguage(cbSrc);
  if src = '' then
    src := 'auto';
  dest := ExtractLanguage(cbDest);
  if dest = '' then
    exit;
  DestMemo.Lines.Text := Translate(SrcMemo.Lines.Text, src, dest);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  F: TAboutForm;
begin
  F := TAboutForm.Create(nil);
  try
    F.ShowModal
  finally
    F.Free;
  end;
end;

function TMainForm.ExtractLanguage(ACombobox: TCombobox): String;
var
  p: Integer;
begin
  Result := '';
  if ACombobox.Text <> '' then
  begin
    p := Pos(' (', ACombobox.Text);
    if p > 0 then
    begin
      Result := Copy(ACombobox.Text, p+2);
      Delete(Result, Length(Result), 1);
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PopulateLanguageCombo(cbSrc);
  PopulateLanguageCombo(cbDest);
  SelectLanguage(cbSrc, '');
  SelectLanguage(cbDest, 'de');
end;

procedure TMainForm.PopulateLanguageCombo(ACombobox: TComboBox);
begin
  ACombobox.Items.BeginUpdate;
  try
    ACombobox.Items.Clear;
    { languages mentioned on https://github.com/argosopentech/argos-translate:

      Arabic, Azerbaijani, Catalan, Chinese, Czech,
      Danish, Dutch, English, Esperanto, Finnish,
      French, German, Greek, Hebrew, Hindi, Hungarian, Indonesian,
      Irish, Italian, Japanese, Korean, Persian, Polish, Portuguese,
      Russian, Slovak, Spanish, Swedish, Turkish, Ukrainian
    }
    // However, commented-out languages are reported as "not supported", nevertheless
    if ACombobox = cbSrc then
      ACombobox.Items.Add('(auto)');
    // ACombobox.Items.Add('Arabic (sa)');
    // ACombobox.Items.Add('Azerbaijani (az)');
    // catalan
    // ACombobox.Items.Add('Chinese (cn)');
    // AComboBox.Items.Add('Czech (cz)');
    // ACombobox.Items.Add('Danish (dk)');
    // ACombobox.Items.Add('Dutch (nl)');
    ACombobox.Items.Add('English (en)');
    // esperanto
    // ACombobox.Items.Add('Finnish (fi)');
    ACombobox.Items.Add('French (fr)');
    ACombobox.Items.Add('German (de)');
    // ACombobox.Items.Add('Greek (gr)');
    // hebrew
    // hindi
    // ACombobox.Items.Add('Hungarian (hu)');
    ACombobox.Items.Add('Indonesian (id)');
    // ACombobox.Items.Add('Irish (ie)');
    ACombobox.Items.Add('Italian (it)');
    // ACombobox.Items.Add('Japanese (jp)');
    // ACombobox.Items.Add('Korean (kr)');
    // persian
    ACombobox.Items.Add('Polish (pl)');
    ACombobox.Items.Add('Portuguese (pt)');
    ACombobox.Items.Add('Russian (ru)');
    // slovak
    ACombobox.Items.Add('Spanish (es)');
    // ACombobox.Items.Add('Swedish (se)');
    // ACombobox.Items.Add('Turkish (tk)');
    // ACombobox.Items.Add('Ukrainian (uk)');
  finally
    ACombobox.Items.EndUpdate;
  end;
end;

procedure TMainForm.SelectLanguage(ACombobox: TCombobox; ALang: String);
var
  i: Integer;
begin
  if (ACombobox = cbSrc) and ((ALang = '') or (ALang = 'auto')) then
  begin
    ACombobox.ItemIndex := 0;
    exit;
  end;

  for i := 0 to ACombobox.Items.Count-1 do
    if pos(' (' + ALang + ')', AComboBox.Items[i]) > 0 then
    begin
      ACombobox.ItemIndex := i;
      exit;
    end;
end;

end.

