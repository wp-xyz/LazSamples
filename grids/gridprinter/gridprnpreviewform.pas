unit GridPrnPreviewForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Types, LazLoggerBase,
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
    acPageMargins: TAction;
    acHeaderFooter: TAction;
    acZoom100: TAction;
    acZoomToFitWidth: TAction;
    acZoomToFitHeight: TAction;
    acZoomOut: TAction;
    acZoomIn: TAction;
    ActionList: TActionList;
    edPageNumber: TEdit;
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
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure acCloseExecute(Sender: TObject);
    procedure acFirstPageExecute(Sender: TObject);
    procedure acHeaderFooterExecute(Sender: TObject);
    procedure acLastPageExecute(Sender: TObject);
    procedure acNextPageExecute(Sender: TObject);
    procedure acPageMarginsExecute(Sender: TObject);
    procedure acPrevPageExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure ActionListUpdate({%H-}AAction: TBasicAction; var {%H-}Handled: Boolean);
    procedure acZoom100Execute(Sender: TObject);
    procedure acZoomInZoomOutExecute(Sender: TObject);
    procedure acZoomToFitHeightExecute(Sender: TObject);
    procedure acZoomToFitWidthExecute(Sender: TObject);
    procedure edPageNumberEditingDone(Sender: TObject);
    procedure edPageNumberKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure edPageNumberMouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure PreviewImageMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PreviewImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PreviewImageMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PreviewImageMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure PreviewImagePaint(Sender: TObject);
    procedure ScrollBoxKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure ScrollBoxMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure ToolBarResize(Sender: TObject);
  private
    FActivated: Boolean;
    FDraggedMargin: Integer;  // 0=left margin, 1=top, 2=right, 3=bottom 4=header 5=footer
    FDraggedPos: Integer;
    FGridPrinter: TGridPrinter;
    FPageCount: Integer;
    FPageNumber: Integer;
    FUpdatePreviewHandler: TNotifyEvent;
    FZoom: Integer;
    FZoomMax: Integer;
    FZoomMin: Integer;
    procedure SetPageNumber(AValue: Integer);
  protected
    function CalcDraggedMargin(AMargin: Integer; APosition: Integer): Double;
    procedure HideDraggedMarginHint;
    function MouseOverMarginLine(X, Y: Integer): Integer;
    function NextZoomFactor(AZoomIn: Boolean): Integer;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ShowDraggedMarginHint(AMarginIndex, ADraggedPos: Integer; AMarginName: String);
    procedure ShowPage(APageNo: Integer; AZoom: Integer = 0);
    procedure UpdateInfoPanel;
    procedure VerifyZoomMin;

  public
    constructor Create(AOwner: TComponent); override;
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
  LCLIntf, LCLType, Printers, GridPrnHeaderFooterForm;

const
  ZOOM_MULTIPLIER = 1.05;

{ Returns true when X1 is in range between X2-Delta and X2+Delta. }
function InRange(X1, X2, Delta: Integer): Boolean; inline;
begin
  Result := (X1 >= X2-Delta) and (X1 <= X2+Delta);
end;

{ Returns X if it is in the range between X1 and X2, otherwise either X1 or X2,
  depending on wheter X is <X1 or >X2. }
function EnsureRange(X, X1, X2: Integer): Integer;
begin
  if X < X1 then
    Result := X1
  else
  if X > X2 then
    Result := X2
  else
    Result := X;
end;

{ TGridPrintPreviewForm }

constructor TGridPrintPreviewForm.Create(AOwner: TComponent);
begin
  inherited;
  Scrollbox.OnKeyDown := @ScrollBoxKeyDown;
  InfoPanel.ParentColor := true;
  FPageNumber := 0;
  FZoom := 100;
  FZoomMax := 1000;  // To avoid too-large bitmaps
  FZoomMin := 10;
  FDraggedMargin := -1;
  VerifyZoomMin;
  ActiveControl := Scrollbox;
end;

procedure TGridPrintPreviewForm.acCloseExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TGridPrintPreviewForm.acFirstPageExecute(Sender: TObject);
begin
  ShowPage(1);
end;

procedure TGridPrintPreviewForm.acHeaderFooterExecute(Sender: TObject);
var
  F: TGridPrintHeaderFooterForm;
begin
  F := TGridPrintHeaderFooterForm.Create(nil);
  try
    F.GridPrinter := FGridPrinter;
    if F.ShowModal = mrOK then
      ShowPage(FPageNumber, FZoom);
  finally
    F.Free;
  end;
end;

procedure TGridPrintPreviewForm.acLastPageExecute(Sender: TObject);
begin
  ShowPage(FPageCount);
end;

procedure TGridPrintPreviewForm.acNextPageExecute(Sender: TObject);
begin
  if FPageNumber < FPageCount then
    ShowPage(FPageNumber+1);
end;

procedure TGridPrintPreviewForm.acPageMarginsExecute(Sender: TObject);
begin
  acPageMargins.Checked := not acPageMargins.Checked;
  PreviewImage.Invalidate;
end;

procedure TGridPrintPreviewForm.acPrevPageExecute(Sender: TObject);
begin
  if FPageNumber > 1 then
    ShowPage(FPageNumber-1);
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

procedure TGridPrintPreviewForm.acZoomInZoomOutExecute(Sender: TObject);
var
  newZoom: Integer;
begin
  newZoom := NextZoomFactor(Sender = acZoomIn);
  ShowPage(FPageNumber, newZoom);
end;

{ Selects a zoom factor such that the preview of the page fills the form. }
procedure TGridPrintPreviewForm.acZoomToFitWidthExecute(Sender: TObject);
begin
  ZoomToFitWidth;
end;

{ Converts the position of the dragged margin to millimeters. }
function TGridPrintPreviewForm.CalcDraggedMargin(AMargin: Integer;
  APosition: Integer): Double;
begin
  case AMargin of
    0: Result := px2mm(APosition, FGridPrinter.PixelsPerInchX);
    1: Result := px2mm(APosition, FGridPrinter.PixelsPerInchY);
    2: Result := px2mm(FGridPrinter.PageWidth - APosition, FGridPrinter.PixelsPerInchX);
    3: Result := px2mm(FGridPrinter.PageHeight - APosition, FGridPrinter.PixelsPerInchY);
    4: Result := px2mm(APosition, FGridPrinter.PixelsPerInchY);
    5: Result := px2mm(FGridPrinter.PageHeight - APosition, FGridPrinter.PixelsPerInchY);
  end;
end;

{ Allows to select a page by entering its number in the PageNo edit and
  pressing ENTER: }
procedure TGridPrintPreviewForm.edPageNumberEditingDone(Sender: TObject);
begin
  if TryStrToInt(edPageNumber.Text, FPageNumber) then
  begin
    if FPageNumber < 1 then FPageNumber := 1;
    if FPageNumber > FPageCount then FPageNumber := FPageCount;
    ShowPage(FPageNumber);
  end;
end;

procedure TGridPrintPreviewForm.edPageNumberKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_LEFT:
      if FPageNumber > 1 then ShowPage(FPageNumber-1);
    VK_RIGHT:
      if FPageNumber < FPageCount then ShowPage(FPageNumber+1);
    VK_HOME:
      ShowPage(1);
    VK_END:
      ShowPage(FPageCount);
  end;
end;

{ Activates scrolling of pages by means of rotating mouse wheel over the
  PageNo edit. }
procedure TGridPrintPreviewForm.edPageNumberMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if WheelDelta < 0 then
  begin
    if FPageNumber < FPageCount then FPageNumber := FPageNumber + 1 else exit;
  end else
    if FPageNumber > 1 then FPageNumber := FPageNumber - 1 else exit;
  ShowPage(FPageNumber);
end;

procedure TGridPrintPreviewForm.FormActivate(Sender: TObject);
begin
  if FActivated then
    exit;
  FUpdatePreviewHandler := FGridPrinter.OnUpdatePreview;
  ShowPage(1, 100);
  FActivated := true;
end;

procedure TGridPrintPreviewForm.HideDraggedMarginHint;
begin
  Application.CancelHint;
end;

// Result 0=left margin, 1=top margin, 2=right margin, 3=bottom margin, 4=header, 5=footer
function TGridPrintPreviewForm.MouseOverMarginLine(X, Y: Integer): Integer;
CONST
  DELTA = 4;
var
  coord: Integer;
begin
  if (FGridPrinter = nil) or (not acPageMargins.Checked) then
    exit(-1);

  if InRange(X, FGridPrinter.PageRect.Left, DELTA) then
    exit(0);

  if InRange(Y, FGridPrinter.PageRect.Top, DELTA) then
    exit(1);

  if InRange(X, FGridPrinter.PageRect.Right, DELTA) then
    exit(2);

  if InRange(Y, FGridPrinter.PageRect.Bottom, DELTA) then
    exit(3);

  if FGridPrinter.Header.IsShown then
  begin
    coord := mm2px(FGridPrinter.Margins.Header, FGridPrinter.PixelsPerInchY);
    if InRange(y, coord, DELTA) then
      exit(4);
  end;

  if FGridPrinter.Footer.IsShown then
  begin
    coord := mm2px(FGridPrinter.Margins.Footer, FGridPrinter.PixelsPerInchY);
    if InRange(y, FGridPrinter.PageHeight - coord, DELTA) then
      exit(5);
  end;

  Result := -1;
end;

function TGridPrintPreviewForm.NextZoomFactor(AZoomIn: Boolean): Integer;
begin
  if AZoomIn then
    Result := round(FZoom * ZOOM_MULTIPLIER)
  else
    Result := round(FZoom / ZOOM_MULTIPLIER);
  Result := EnsureRange(Result, FZoomMin, FZoomMax);
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

procedure TGridPrintPreviewForm.PreviewImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Scrollbox.SetFocus;
  if (ssLeft in Shift) then
    FDraggedMargin := MouseOverMarginLine(X, Y);
end;

procedure TGridPrintPreviewForm.PreviewImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  minWidth: Integer;
  minHeight: Integer;
  y0: Integer;
  one_mm: Integer;
  marginName: String;
begin
  if (FGridPrinter = nil) or not (acPageMargins.Checked) then
    exit;

  if not (ssLeft in Shift) then
  begin
    FDraggedMargin := MouseOverMarginLine(X, Y);
    case FDraggedMargin of
      -1: Screen.Cursor := crDefault;
      0,2: Screen.Cursor := crHSplit;
      1,3,4,5: Screen.Cursor := crVSplit;
    end;
  end;

  if (ssLeft in Shift) then
  begin
    minWidth := FGridPrinter.PageWidth div 4;
    minHeight := FGridPrinter.PageHeight div 4;
    one_mm := mm2px(1.0, FGridPrinter.PixelsPerInchY);
    case FDraggedMargin of
      0: begin
           // Left margin
           marginName := 'Left margin';
           FDraggedPos := X;
           if (FDraggedPos < 0) then
             FDraggedPos := 0;
           if FGridPrinter.PageRect.Right - FDraggedPos < minWidth then
             FDraggedPos := FGridPrinter.PageRect.Right - minWidth
         end;
      1: begin
           // Top margin
           marginName := 'Top margin';
           FDraggedPos := Y;
           if FGridPrinter.Header.IsShown then
           begin
             y0 := FGridPrinter.HeaderMargin + one_mm;
             if (FDraggedPos < y0) then
               FDraggedPos := y0;
           end;
           if (FDraggedPos < 0) then
             FDraggedPos := 0;
           if FGridPrinter.PageRect.Bottom - FDraggedPos < minHeight then
             FDraggedPos := FGridPrinter.PageRect.Bottom - minWidth;
         end;
      2: begin
           // Right margin
           marginName := 'Right margin';
           FDraggedPos := X;
           if FDraggedPos > FGridPrinter.PageWidth then
             FDraggedPos := FGridPrinter.PageWidth;
           if FDraggedPos - FGridPrinter.PageRect.Left < minWidth then
             FDraggedPos := FGridPrinter.PageRect.Left + minWidth;
         end;
      3: begin
           // Bottom margin
           marginName := 'Bottom margin';
           FDraggedPos := Y;
           if FGridPrinter.Footer.IsShown then
           begin
             y0 := FGridPrinter.PageHeight - FGridPrinter.FooterMargin - one_mm;
             if FDraggedPos > y0 then
               FDraggedPos := y0;
           end;
           if FDraggedPos > FGridPrinter.PageHeight then
             FDraggedPos := FGridPrinter.PageHeight;
           if FDraggedPos - FGridPrinter.PageRect.Top < minHeight then
             FDraggedPos := FGridPrinter.PageRect.Top + minHeight;
         end;
      4: begin
           // Header
           marginName := 'Header margin';
           FDraggedPos := Y;
           if FDraggedPos < 0 then
             FDraggedPos := 0;
           if FDraggedPos > FGridPrinter.PageRect.Top - one_mm then
             FDraggedPos := FGridPrinter.PageRect.Top - one_mm;
         end;
      5: begin
           // Footer
           marginName := 'Footer margin';
           FDraggedPos := Y;
           if FDraggedPos > FGridPrinter.PageHeight then
             FDraggedPos := FGridPrinter.PageHeight;
           if FDraggedPos < FGridPrinter.PageRect.Bottom + one_mm then
             FDraggedPos := FGridPrinter.PageRect.Bottom + one_mm;
         end;
      else
        raise Exception.Create('[PreviewImageMouseMove] Unexpected value of FDraggedMargin');
    end;


    // Redraw the preview to update the dragged red margin line
    PreviewImage.Repaint;

    ShowDraggedMarginHint(FDraggedMargin, FDraggedPos, marginName);
  end;
end;

procedure TGridPrintPreviewForm.PreviewImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  dragged: Integer;
  newMargin: Double;
begin
  if (FDraggedMargin > -1) then
  begin
    newMargin := CalcDraggedMargin(FDraggedMargin, FDraggedPos);
    dragged := FDraggedMargin;
    FDraggedMargin := -1;
    case dragged of
      0: FGridPrinter.Margins.Left := newMargin;
      1: FGridPrinter.Margins.Top := newMargin;
      2: FGridPrinter.Margins.Right := newMargin;
      3: FGridPrinter.Margins.Bottom := newMargin;
      4: FGridPrinter.Margins.Header := newMargin;
      5: FGridPrinter.Margins.Footer := newMargin;
    end;
    HideDraggedMarginHint;
    Screen.Cursor := crDefault;
    ShowPage(FPageNumber);
  end;
end;

procedure TGridPrintPreviewForm.PreviewImageMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  newZoom: Integer;
begin
  if (ssCtrl in Shift) then
  begin
    newZoom := NextZoomFactor(WheelDelta > 0);
    ShowPage(FPageNumber, newZoom);
  end;
end;

procedure TGridPrintPreviewForm.PreviewImagePaint(Sender: TObject);
var
  x, y: Integer;

begin
  if FGridPrinter = nil then
    exit;

  if acPageMargins.Checked then
  begin
    PreviewImage.Canvas.Pen.Color := clRed;
    PreviewImage.Canvas.Pen.Style := psDash;

    // Left margin line
    if FDraggedMargin = 0 then
      x := FDraggedPos
    else
      x := FGridPrinter.PageRect.Left;
    PreviewImage.Canvas.Line(x, 0, x, PreviewImage.Height);

    // Top margin line
    if FDraggedMargin = 1 then
      y := FDraggedPos
    else
      y := FGridPrinter.PageRect.Top;
    PreviewImage.Canvas.Line(0, y, PreviewImage.Width, y);

    // Right margin line
    if FDraggedMargin = 2 then
      x := FDraggedPos
    else
      x := FGridPrinter.PageRect.Right;
    PreviewImage.Canvas.Line(x, 0, x, PreviewImage.Height);

    // Bottom margin line
    if FDraggedMargin = 3 then
      y := FDraggedPos
    else
      y := FGridPrinter.PageRect.Bottom;
    PreviewImage.Canvas.Line(0, y, PreviewImage.Width, y);

    // Header line
    if FGridPrinter.Header.IsShown then
    begin
      if FDraggedMargin = 4 then
        y := FDraggedPos
      else
        y := mm2px(FGridPrinter.Margins.Header, FGridPrinter.PixelsPerInchY);
      PreviewImage.Canvas.Line(0, y, PreviewImage.Width, y);
    end;

    // Footer line
    if FGridPrinter.Footer.IsShown then
    begin
      if FDraggedMargin = 5 then
        y := FDraggedPos
      else
        y := FGridPrinter.PageHeight - mm2px(FGridPrinter.Margins.Footer, FGridPrinter.PixelsPerInchY);
      PreviewImage.Canvas.Line(0, y, PreviewImage.Width, y);
    end;
  end;
end;

procedure TGridPrintPreviewForm.ScrollBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DOWN, VK_Next:
      with Scrollbox.VertScrollbar do
      begin
        if (Position = Range-Page) and (FPageNumber < FPageCount) then
        begin
          ShowPage(FPageNumber+1);
          Position := 0;
        end
        else
          case Key of
            VK_DOWN: Position := Position + Increment;
            VK_NEXT: Position := Position + Page;
          end;
      end;
    VK_UP, VK_PRIOR:
      with Scrollbox.VertScrollbar do
      begin
        if (Position = 0) and (FPageNumber > 1) then
        begin
          ShowPage(FPageNumber-1);
          Position := Range-Page;
        end
        else
          case Key of
            VK_UP: Position := Position - Increment;
            VK_PRIOR: Position := Position - Page;
          end;
      end;
    VK_LEFT:
      with Scrollbox.HorzScrollbar do
        Position := Position - Increment;
    VK_RIGHT:
      with Scrollbox.HorzScrollbar do
        Position := Position + Increment;
    VK_HOME:
      with Scrollbox.HorzScrollbar do
        Position := Position - Page;
    VK_END:
      with Scrollbox.HorzScrollbar do
        Position := Position + Page;
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

procedure TGridPrintPreviewForm.SetPageNumber(AValue: Integer);
begin
  if AValue <> FPageNumber then
    ShowPage(AValue);
end;

procedure TGridPrintPreviewForm.ShowDraggedMarginHint(
  AMarginIndex, ADraggedPos: Integer; AMarginName: String);
var
  hintStr: String;
  P: TPoint;
begin
  P := Mouse.CursorPos;
  hintStr := Format('%s: %.1f mm', [AMarginName, CalcDraggedMargin(AMarginIndex, ADraggedPos)]);
  PreviewImage.Hint := hintStr;
  Application.HintPause := 0;
  Application.CancelHint;
  Application.ActivateHint(P, true);
end;

procedure TGridPrintPreviewForm.ShowPage(APageNo: Integer; AZoom: Integer = 0);
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
  if AZoom > 0 then
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
  edPageNumber.Text := IntToStr(FPageNumber);
end;

{ Adjusts FZoomMin to avoid the situation that, due to integer rounding,
  the zoom factor cannot be changed any more by clicking a zoom button or
  by mousewheel. }
procedure TGridPrintPreviewForm.VerifyZoomMin;
var
  nextHigherZoom: Integer;
begin
  nextHigherZoom := round(FZoomMin * ZOOM_MULTIPLIER);
  while nextHigherZoom = FZoomMin do
  begin
    FZoomMin := nextHigherZoom + 1;
    nextHigherZoom := round(FZoomMin * ZOOM_MULTIPLIER);
  end;
end;

procedure TGridPrintPreviewForm.ZoomToFitWidth;
var
  w: Integer;
begin
  if Printer = nil then
    exit;

  // Correct for scrollbar width when the vert scrollbar is currently hidden,
  // but will be shown after displaying the preview page.
  if (not Scrollbox.VertScrollbar.IsScrollbarVisible) and
     (Printer.PageHeight/Printer.PageWidth > Scrollbox.ClientHeight/Scrollbox.ClientWidth)
  then
    w := Scrollbox.VertScrollbar.ClientSizeWithBar
  else
    w := Scrollbox.ClientWidth;
  w := w - 2*PreviewImage.Left;
  FZoom := round(w / Printer.PageWidth * Printer.XDPI/ ScreenInfo.PixelsPerInchX * 100);
  ShowPage(FPageNumber, FZoom);
end;

procedure TGridPrintPreviewForm.ZoomToFitHeight;
var
  h: Integer;
begin
  if Printer = nil then
    exit;

  // Correct for scrollbar height when the horizontal scrollbar is currently hidden,
  // but will be shown after displaying the preview page.
  if (not Scrollbox.HorzScrollbar.IsScrollbarVisible) and
     (Printer.PageHeight/Printer.PageWidth < Scrollbox.ClientHeight/Scrollbox.ClientWidth)
  then
    h := Scrollbox.HorzScrollbar.ClientSizeWithBar
  else
    h := Scrollbox.ClientHeight;
  h := h - 2*PreviewImage.Top;
  FZoom := round(h / Printer.PageHeight * Printer.YDPI / ScreenInfo.PixelsPerInchY * 100);
  ShowPage(FPageNumber, FZoom);
end;

end.

