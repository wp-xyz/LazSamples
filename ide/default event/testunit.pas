unit TestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComponentEditors;

type
  TMyNotebookEditor = class(TUntabbedNotebookComponentEditor)
  public
    constructor Create(AComponent: TComponent;
      ADesigner: TComponentEditorDesigner); override;
  end;

procedure Register;

implementation

uses
  ExtCtrls;

constructor TMyNotebookEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited;
  BestEditEvent := 'OnResize';
end;

procedure Register;
begin
  RegisterComponentEditor(TNotebook, TMyNotebookEditor);
end;

end.

