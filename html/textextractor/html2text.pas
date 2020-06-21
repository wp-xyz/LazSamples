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
  protected
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

procedure THTMLTextExtractor.FoundTextHandler(AText: String);
begin
  if AText = '' then
    exit;

  // Remove multiple line breaks from text start
  if (AText[1] in [#10, #13]) then begin
    while (AText <> '') and (AText[1] in [#10, #13]) do
      Delete(AText, 1, 1);
    AText := LineEnding + AText;
    if AText = '' then
      exit;
  end;

  // ... and from text end
  if (AText[Length(AText)] in [#10, #13]) then begin
    while (AText <> '') and (AText[Length(AText)] in [#10, #13]) do
      Delete(AText, Length(AText), 1);
    AText := AText + LineEnding;
    if AText = '' then
      exit;
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

