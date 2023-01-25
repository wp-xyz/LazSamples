unit about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses
  Types;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Image1.Picture.Assign(Application.Icon);
  with Image1.Picture.Icon do
    Current := GetBestIndexForSize(Size(Image1.Width, Image1.Height));
end;

end.

