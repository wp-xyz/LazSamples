program DBGridDragDrop;

uses
  Interfaces,
  Forms,
  mainformu in 'mainformu.pas' {Form1};

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
