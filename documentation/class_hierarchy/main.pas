unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LazFileUtils, IniFiles, LvlGraphCtrl,
  // Add here the units with the classes to be analyzed
  StdCtrls, ExtCtrls, ComCtrls, Buttons;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    ImageList1: TImageList;
    Label1: TLabel;
    LvlGraphControl1: TLvlGraphControl;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FRootClassName: String;
    FImages: TStrings;
    FImagesDir: String;
    FLazarusDir: String;
    function AddClassNode(AClass: TClass; ARootClassName: String=''): TLvlGraphNode;
    function GetImageIndex(AClassName: String): Integer;
    function CreateIni: TCustomIniFile;
    procedure ReadIni;
    procedure WriteIni;
  public
    procedure AddControls;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LclVersion;

{ TMainForm }

function TMainForm.AddClassNode(AClass: TClass; ARootClassName: String = ''): TLvlGraphNode;
var
  parentclass: TClass;
begin
  if ARootClassName <> '' then
    FRootClassName := ARootClassName;
  Result := LvlGraphControl1.Graph.GetNode(AClass.ClassName, true);
  Result.ImageIndex := GetImageIndex(AClass.ClassName);
  if AClass.ClassName = FRootClassName then
    exit;
  parentclass := AClass.ClassParent;
  AddClassNode(parentclass);
  LvlGraphControl1.Graph.GetEdge(parentclass.ClassName, AClass.ClassName, true);
end;

{ Add here the classes to be displayed. Make sure that their units are listed
  in the "uses" clause. }
procedure TMainForm.AddControls;
begin
  // from StdCtrls
  AddClassNode(TEdit);
  AddClassNode(TMemo);
  AddClassNode(TLabel);
  AddClassNode(TButton);

  // from ComCtrls
  AddClassNode(TTreeView);
  AddClassNode(TListView);
  AddClassNode(TPageControl);
  AddClassNode(TTabControl);
  AddClassNode(TToolbar);
  AddClassNode(TToolButton);
  AddClassNode(TStatusBar);
  AddClassNode(TImageList);

  // from ExtCtrls
  AddClassNode(TImage);

  // from Buttons
  AddClassNode(TBitBtn);
  AddClassNode(TSpeedButton);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FRootClassName := 'TComponent';
  FImages := TStringList.Create;
  ImageList1.Width := 24;
  ImageList1.Height := 24;
  ImageList1.RegisterResolutions([36, 48]);
  ImageList1.Scaled := true;

  ReadIni;

  AddControls;

  with LvlGraphControl1 do begin
    {$IF LCL_FullVersion >= 2010000}
    Options := LvlGraphControl1.Options + [lgoStraightenGraph];
    Limits.MaxLevelHeightAbs := MaxInt;
    {$IFEND}
    Images := ImageList1;
    NodeStyle.Shape := lgnsNone;
    NodeStyle.GapTop := 16;
    NodeStyle.GapBottom := 4;
    NodeStyle.GapLeft := 10;
    NodeStyle.GapRight := 10;
    NodeStyle.CaptionPosition := lgncBottom;
    NodeStyle.CaptionScale := 1.0;
    EdgeStyle.Shape := lgesStraight;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(nil) do
    try
      FileName := FLazarusDir;
      InitialDir := FLazarusDir;
      if Execute then
      begin
        FLazarusDir := FileName;
        FImagesDir := AppendPathDelim(FLazarusDir) + 'images/components/';
        WriteIni;
        ShowMessage('Please restart the application.');
      end;
    finally
      Free;
    end;
end;

function TMainForm.GetImageIndex(AClassName: String): Integer;
var
  png, png_150, png_200: TCustomBitmap;
  fn: String;
begin
  result := -1;
  fn := FImagesDir + lowercase(AClassName);
  if not FileExists(fn + '.png') then
    fn := FImagesDir + 'default';
  if FileExists(fn + '.png') then
  begin
    Result := FImages.IndexOf(fn);
    if Result = - 1 then
    begin
      png := TPortableNetworkgraphic.Create;
      png_150 := TPortableNetworkgraphic.Create;
      png_200 := TPortableNetworkgraphic.Create;
      try
        png.LoadFromFile(fn + '.png');
        if FileExists(fn + '_150.png') then
          png_150.LoadFromFile(fn + '_150.png')
        else
          FreeAndNil(png_150);
        if FileExists(fn + '_200.png') then
          png_200.LoadFromFile(fn + '_200.png')
        else
          FreeAndNil(png_200);
        Result := ImageList1.AddMultipleResolutions([png, png_150, png_200]);
        FImages.Add(fn);
      finally
        png.Free;
      end;
    end;
  end;
end;

function TMainForm.CreateIni: TCustomIniFile;
var
  fn: String;
begin
  fn := Application.Location + ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  Result := TIniFile.Create(fn);
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
begin
  ini := CreateIni;
  try
    FLazarusDir := ini.ReadString('Settings', 'LazarusDir', '');
    FImagesDir := AppendPathDelim(FLazarusDir) + 'images/components/';
  finally
    ini.Free;
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
begin
  ini := CreateIni;
  try
    ini.WriteString('Settings', 'LazarusDir', FLazarusDir);
  finally
    ini.Free;
  end;
end;

end.

