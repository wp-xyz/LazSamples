unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, StdCtrls, Translations,
  StrConsts;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnClickHere: TButton;
    cmbLanguages: TComboBox;
    lblTest: TLabel;
    lblLanguage: TLabel;
    procedure btnClickHereClick(Sender: TObject);
    procedure cmbLanguagesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetLangDir: String;
    procedure PopulateLanguages;
    procedure SetLanguage(ALang: String);
    procedure SetStrings;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

// Initializes the translation system: Populates the languages combobox with
// a list of all available languages, sets the initial language to the language
// of the system.
procedure TForm1.FormCreate(Sender: TObject);
begin
  PopulateLanguages;
  SetLanguage(GetLanguageID.LanguageCode);
  cmbLanguages.ItemIndex := 0;
end;

procedure TForm1.btnClickHereClick(Sender: TObject);
begin
  ShowMessage(RS_ThanksForClicking);
end;

// Fires when another item is selected in the cmbLanguages combobox.
// Translates all strings to the newly selected language.
procedure TForm1.cmbLanguagesChange(Sender: TObject);
begin
  if cmbLanguages.ItemIndex = 0 then
    SetLanguage('')
  else
    SetLanguage(cmbLanguages.Items[cmbLanguages.ItemIndex]);
end;

// Returns the name of the directory which contains the po files.
function TForm1.GetLangDir: String;
begin
  Result := Application.Location + 'languages';
end;

// Finds all po files in the languages folder and adds their country code to
// the cmbLanguages combobox for user selection of the language.
procedure TForm1.PopulateLanguages;
var
  i: Integer;
  L: TStringList;
  sa: TStringArray;
begin
  L := TStringList.Create;
  try
    FindAllFiles(L, GetLangDir, 'project1.*.po', false);
    for i := 0 to L.Count-1 do
    begin
      sa := L[i].Split('.');
      L[i] := sa[1];
    end;
    L.Sort;
    L.Insert(0, '(' + RS_Automatic + ')');
    cmbLanguages.Items.Assign(L);
  finally
    L.Free;
  end;
end;

// Switches the language of the resource strings and translates all entries
// in unit strconsts to the specified language by explictely calling
// TranslateUnitResourceStrings
procedure TForm1.SetLanguage(ALang: String);
begin
  if ALang = '' then
    ALang := GetLanguageID.LanguageCode;
  TranslateUnitResourceStrings('strconsts', Format('%s/project1.%s.po', [GetLangDir, ALang]));
  SetStrings;
end;

// Assigns the resourcestrings to the captions of the controls
procedure TForm1.SetStrings;
begin
  Caption := RS_Caption;
  btnClickHere.Caption := RS_ClickHere;
  lblLanguage.caption := RS_Language;
  lblTest.Caption := RS_Test;
  cmbLanguages.Items[0] := '(' + RS_Automatic + ')';
end;

end.

