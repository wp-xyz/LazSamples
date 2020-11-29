{ Demonstrates usage of a TFlowPanel }

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FlowPanel1: TFlowPanel;
    ScrollBox1: TScrollBox;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if sender = Button1 then
    with TButton.Create(self) do
    begin
      Caption := 'Control #' + IntToStr(FlowPanel1.ControlList.Count);
      Parent := FlowPanel1;
    end
  else
  if Sender = Button2 then
    with TEdit.Create(self) do
    begin
      Text := 'Control #' + IntToStr(FlowPanel1.ControlList.Count);
      Parent := FlowPanel1;
    end
  else
    raise Exception.Create('Not supported.');

  Caption := IntToStr(FlowPanel1.ControlList.Count) + ' controls in FlowPanel';
end;

end.

