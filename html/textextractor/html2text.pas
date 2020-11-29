unit html2text;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ExtractTextFromHTML(const AHTMLText: String): String;

implementation

uses
  fasthtmlparser;

type
  THTMLTextExtractor = class
  private
    FParser: THTMLParser;
    FText: String;
    FInBody: Boolean;
  protected
    procedure FoundTagHandler(NoCaseTag, ActualTag: String);
    procedure FoundTextHandler(AText: String);
  public
    constructor Create(AHTMLText: String);
    destructor Destroy; override;
    function Execute: String;
  end;

constructor THTMLTextExtractor.Create(AHTMLText: String);
begin
  FParser := THTMLParser.Create(AHTMLText);
  FParser.OnFoundText := @FoundTextHandler;
  FParser.OnFoundTag := @FoundTagHandler;
end;

destructor THTMLTextExtractor.Destroy;
begin
  FParser.Free;
  inherited;
end;

function THTMLTextExtractor.Execute: String;
begin
  FText := '';
  FParser.Exec;
  Result := FText;
end;

procedure THTMLTextExtractor.FoundTagHandler(NoCaseTag, ActualTag: String);
begin
  if NoCaseTag = '<BODY>' then FInBody := true;
end;

procedure THTMLTextExtractor.FoundTextHandler(AText: String);
var
  s: String;
begin
  if not FInBody then  // Exclude text in the meta data
    exit;

  if AText = '' then
    exit;

  // Remove multiple line breaks from text start
  if (AText[1] in [#10, #13]) then begin
    while (AText <> '') and (AText[1] in [#10, #13]) do
      Delete(AText, 1, 1);
    s := Trim(AText);
    if s = '' then
      exit;
    AText := LineEnding + AText;
  end;

  // ... and from text end
  if (AText[Length(AText)] in [#10, #13]) then begin
    while (AText <> '') and (AText[Length(AText)] in [#10, #13]) do
      Delete(AText, Length(AText), 1);
    s := Trim(AText);
    if trim(s) = '' then
      exit;
    AText := AText + LineEnding;
  end;

  FText := FText + AText;
end;

function ExtractTextFromHTML(const AHTMLText: String): String;
var
  extractor: THTMLTextExtractor;
begin
  extractor := THTMLTextExtractor.Create(AHTMLText);
  try
    Result := extractor.Execute;
  finally
    extractor.Free;
  end;
end;


end.

