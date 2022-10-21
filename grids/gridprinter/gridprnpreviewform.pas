unit GridPrnPreviewForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Types,
  StdCtrls, ExtCtrls, ComCtrls, Dialogs, Menus, ActnList,
  GridPrn;

type

  { TGridPrintPreviewForm }

  TGridPrintPreviewForm = class(TForm)
    acPrint: TAction;
    acClose: TAction;
    acFirstPage: TAction;
    acPrevPage: TAction;
    acNextPage: TAction;
    acLastPage: TAction;
    acZoom100: TAction;
    acZoomToFitWidth: TAction;
    acZoomToFitHeight: TAction;
    acZoomOut: TAction;
    acZoomIn: TAction;
    ActionList: TActionList;
    edPageNo: TEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    InfoPanel: TPanel;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    PreviewPopupMenu: TPopupMenu;
    PreviewImage: TImage;
    ScrollBox: TScrollBox;
    ToolbarImages: TImageList;
    ToolBar: TToolBar;
    tbPrint: TToolButton;
    tbClose: TToolButton;
    tbFirst: TToolButton;
    tbPrev: TToolButton;
    tbNext: TToolButton;
    tbLast: TToolButton;
    tbDivider1: TToolButton;
    tbDivider2: TToolButton;
    tbDivider3: TToolButton;
    tbZoomIn: TToolButton;
    tbZoomOut: TToolButton;
    tbZoomWidth: TToolButton;
    tbZoomHeight: TToolButton;
    tbZoom100: TToolButton;
    procedure acCloseExecute(Sender: TObject);
    procedure acFirstPageExecute(Sender: TObject);
    procedure acLastPageExecute(Sender: TObject);
    procedure acNextPageExecute(Sender: TObject);
    procedure acPrevPageExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure ActionListUpdate({%H-}AAction: TBasicAction; var {%H-}Handled: Boolean);
    procedure acZoom100Execute(Sender: TObject);
    procedure acZoomInExecute(Sender: TObject);
    procedure acZoomOutExecute(Sender: TObject);
    procedure acZoomToFitHeightExecute(Sender: TObject);
    procedure acZoomToFitWidthExecute(Sender: TObject);
    procedure edPageNoEditingDone(Sender: TObject);
    procedure edPageNoMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PreviewImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PreviewImageMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure ScrollBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolBarResize(Sender: TObject);
  private
    FGridPrinter: TGridPrinter;
    FPageCount: Integer;
    FPageNumber: Integer;
    FZoom: Integer;
    procedure SetGridPrinter(AValue: TGridPrinter);
    procedure SetPageNumber(AValue: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ShowPage(APageNo, AZoom: Integer);
    procedure UpdateInfoPanel;

  public
    procedure ZoomToFitHeight;
    procedure ZoomToFitWidth;
    property PageNumber: Integer read FPageNumber write SetPageNumber;

  published
    property GridPrinter: TGridPrinter read FGridPrinter write FGridPrinter;

  end;

var
  GridPrintPreviewForm: TGridPrintPreviewForm;

implementation

{$R *.lfm}

uses
  Printers;

const
  ZOOM_MULTIPLIER = 1.05;

{ TGridPrintPreviewForm }

procedure TGridPrintPreviewForm.FormShow(Sender: TObject);
begin
  if FGridPrinter <> nil then
    ShowPage(1, FZoom);
end;

procedure TGridPrintPreviewForm.PreviewImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Scrollbox.SetFocus;
end;

procedure TGridPrintPreviewForm.FormCreate(Sender: TObject);
begin
  InfoPanel.ParentColor := true;
  FPageNumber := 1;
  FZoom := 100;
end;

procedure TGridPrintPreviewForm.acCloseExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TGridPrintPreviewForm.acFirstPageExecute(Sender: TObject);
begin
  ShowPage(1, FZoom);
end;

procedure TGridPrintPreviewForm.acLastPageExecute(Sender: TObject);
begin
  ShowPage(FPageCount, FZoom);
end;

procedure TGridPrintPreviewForm.acNextPageExecute(Sender: TObject);
begin
  if FPageNumber < FPageCount then
    ShowPage(FPageNumber+1, FZoom);
end;

procedure TGridPrintPreviewForm.acPrevPageExecute(Sender: TObject);
begin
  if FPageNumber > 1 then
    ShowPage(FPageNumber-1, FZoom);
end;

procedure TGridPrintPreviewForm.acPrintExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TGridPrintPreviewForm.acZoom100Execute(Sender: TObject);
begin
  ShowPage(FPageNumber, 100);
end;

procedure TGridPrintPreviewForm.acZoomToFitHeightExecute(Sender: TObject);
begin
  ZoomToFitHeight;
end;

procedure TGridPrintPreviewForm.ActionListUpdate(AAction: TBasicAction;
  var Handled: Boolean);
begin
  acPrint.Enabled := (FGridPrinter <> nil) and (FPageCount > 0);
  acFirstPage.Enabled := (FGridPrinter <> nil) and (FPageCount > 0) and (FPageNumber > 1);
  acPrevPage.Enabled := acFirstPage.Enabled;
  acNextPage.Enabled := (FGridPrinter <> nil) and (FPageCount > 0) and (FPageNumber < FPageCount);
  acLastPage.Enabled := acNextPage.Enabled;
end;

procedure TGridPrintPreviewForm.acZoomInExecute(Sender: TObject);
begin
  ShowPage(FPageNumber, round(FZoom * ZOOM_MULTIPLIER));
end;

procedure TGridPrintPreviewForm.acZoomOutExecute(Sender: TObject);
var
  newZoom: Integer;
  nextNewZoom: Integer;
begin
  newZoom := round(FZoom / ZOOM_MULTIPLIER);
  nextNewZoom := round(newZoom / ZOOM_MULTIPLIER);
  // Prevent reaching a state in which the rounded zoom factor is so small that
  // it does not change any more.
  if nextNewZoom <> newZoom then
    ShowPage(FPageNumber, round(FZoom / ZOOM_MULTIPLIER));
end;

procedure TGridPrintPreviewForm.acZoomToFitWidthExecute(Sender: TObject);
begin
  ZoomToFitWidth;
end;

procedure TGridPrintPreviewForm.edPageNoEditingDone(Sender: TObject);
begin
  if TryStrToInt(edPageNo.Text, FPageNumber) then
  begin
    if FPageNumber < 1 then FPageNumber := 1;
    if FPageNumber > FPageCount then FPageNumber := FPageCount;
    ShowPage(FPageNumber, FZoom);
  end;
end;

procedure TGridPrintPreviewForm.edPageNoMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if WheelDelta < 0 then
  begin
    if FPageNumber < FPageCount then FPageNumber := FPageNumber + 1 else exit;
  end else
    if FPageNumber > 1 then FPageNumber := FPageNumber - 1 else exit;
  ShowPage(FPageNumber, FZoom);
end;

procedure TGridPrintPreviewForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FGridPrinter then
      FGridPrinter := nil;
  end;
end;

procedure TGridPrintPreviewForm.PreviewImageMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  newZoom: Integer;
  nextNewZoom: Integer;
begin
  if (ssCtrl in Shift) then
  begin
    if WheelDelta > 0 then
      ShowPage(FPageNumber, round(FZoom * ZOOM_MULTIPLIER))
    else
    begin
      // Prevent reaching a state in which the zoom factor is so small that
      // it does not change any more due to rounding.
      newZoom := round(FZoom / ZOOM_MULTIPLIER);
      nextNewZoom := round(newZoom / ZOOM_MULTIPLIER);
      if nextNewZoom <> newZoom then
        ShowPage(FPageNumber, newZoom);
    end;
  end;
end;

procedure TGridPrintPreviewForm.ScrollBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Scrollbox.SetFocus;
end;

procedure TGridPrintPreviewForm.ToolBarResize(Sender: TObject);
begin
  UpdateInfoPanel;
end;

procedure TGridPrintPreviewForm.SetGridPrinter(AValue: TGridPrinter);
begin
  if FGridPrinter = AValue then
    exit;
  FGridPrinter := AValue;
  ShowPage(1, FZoom);
end;

procedure TGridPrintPreviewForm.SetPageNumber(AValue: Integer);
begin
  if AValue <> FPageNumber then
    ShowPage(AValue, FZoom);
end;

procedure TGridPrintPreviewForm.ShowPage(APageNo, AZoom: Integer);
var
  bmp: TBitmap;
begin
  if FGridPrinter = nil then
  begin
    FPageCount := 0;
    FPageNumber := 0;
    PreviewImage.Picture.Clear;
    exit;
  end;

  FPageNumber := APageNo;
  FZoom := AZoom;

  // Instruct the GridPrinter to create the preview bitmap of the selected page
  bmp := FGridPrinter.CreatePreviewBitmap(FPageNumber, FZoom);
  try
    // Load the bitmap into the PreviewImage component
    PreviewImage.Width := bmp.Width;
    PreviewImage.Height := bmp.Height;
    PreviewImage.Picture.Bitmap.Assign(bmp);
    FPageCount := FGridPrinter.PageCount;
    UpdateInfoPanel;
  finally
    bmp.Free;
  end;
end;

procedure TGridPrintPreviewForm.UpdateInfoPanel;
begin
  InfoPanel.Caption := Format('Page %d of %d, Zoom %d %%', [FPageNumber, FPageCount, FZoom]);
  InfoPanel.Width := InfoPanel.Canvas.TextWidth(InfoPanel.Caption);
  InfoPanel.Left := Toolbar.ClientWidth - InfoPanel.Width - 8;
  edPageNo.Text := IntToStr(FPageNumber);
end;

procedure TGridPrintPreviewForm.ZoomToFitWidth;
var
  w: Integer;
begin
  if Printer = nil then
    exit;
  w := Scrollbox.ClientWidth - 2*PreviewImage.Left;
  FZoom := round(w / Printer.PageWidth * Printer.XDPI/ ScreenInfo.PixelsPerInchX * 100);
  ShowPage(FPageNumber, FZoom);
end;

procedure TGridPrintPreviewForm.ZoomToFitHeight;
var
  h: Integer;
begin
  if Printer = nil then
    exit;
  h := Scrollbox.ClientHeight - 2*PreviewImage.Top;
  FZoom := round(h / Printer.PageHeight * Printer.YDPI / ScreenInfo.PixelsPerInchY * 100);
  ShowPage(FPageNumber, FZoom);
end;

end.

