unit GridPrnPreviewDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GridPrn, GridPrnPreviewForm;

type
  TGridPrintPreviewDialog = class(TComponent)
  private
    FGridPrinter: TGridPrinter;
    FToolBarButtons: TGridPrintPreviewToolBarButtons;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute;
  published
    property GridPrinter: TGridPrinter read FGridPrinter write FGridPrinter;
    property ToolBarButtons: TGridPrintPreviewToolBarButtons
      read FToolBarButtons write FToolBarButtons default DEFAULT_GRIDPRN_TOOLBARBUTTONS;
  end;

implementation

uses
  Controls;

constructor TGridPrintPreviewDialog.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TGridPrintPreviewDialog.Execute;
var
  F: TGridPrintPreviewForm;
begin
  if FGridPrinter = nil then
    exit;

  F := TGridPrintPreviewForm.Create(nil);
  try
    F.GridPrinter := FGridPrinter;
    F.ToolBarButtons := FToolBarButtons;
    if (F.ShowModal = mrOK) then
      FGridPrinter.Print;
  finally
    F.Free;
  end;
end;

procedure TGridPrintPreviewDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FGridPrinter then
      FGridPrinter := nil;
  end;
end;

end.

