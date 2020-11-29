{ Shows how a menu can be custom-drawn }

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, LCLType,
  StdCtrls, ActnList, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    N1: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure CheckBox1Change(Sender: TObject);
    procedure MenuItemDrawHandler(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Types, ImgList;

{ TForm1 }

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if Checkbox1.Checked then
    PopupMenu1.Images := ImageList1
  else
    PopupMenu1.Images := nil;
end;

procedure TForm1.MenuItemDrawHandler(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
const
  checkmark: String = '✓';
var
  x, y: Integer;
  imgRes: TScaledImageListResolution;
  margin: Integer;
  w, h: Integer;
  lMenu: TMenu;
  menuItem: TMenuItem;
  gutter: Integer;
  sz: TSize;
begin
  // Preparations
  if not (Sender is TMenuItem) then
    exit;  // The following code only works when the Sender is a MenuItem

  menuItem := TMenuItem(Sender);
  lMenu := menuItem.GetParentMenu;
  if lMenu = nil then
    exit;

  ACanvas.Font.Assign(Screen.MenuFont);
  margin := Scale96ToFont(3);

  // Calculate gutter width
  if Assigned(lMenu.Images) then
  begin
    // get the image list for the current resolution. Note: ImageList1.Scaled must be true.
    imgRes := lMenu.Images.ResolutionForPPI[lMenu.ImagesWidth, ACanvas.Font.PixelsPerInch, GetCanvasScaleFactor];
    w := imgRes.Width;   // these are the real image dimensions after scaling
    h := imgRes.Height;
    gutter := w + 2*margin;
  end else
  begin
    // No image list --> use the width of the checkmark character plus some margin
    gutter := 2*ACanvas.TextWidth(checkmark) + 2*margin;
  end;

  if not menuItem.Enabled then
    ACanvas.Font.Color := clGray;

  // Draw packground
  if AState * [odSelected, odFocused] <> [] then begin
    ACanvas.Brush.Color := ColorButton2.ButtonColor;
    ACanvas.Pen.Color := ColorButton3.ButtonColor;
    ACanvas.Rectangle(ARect);
    if menuItem.Enabled then
      ACanvas.Font.Style := [fsBold];
  end
  else begin
    ACanvas.GradientFill(ARect, clWhite, ColorButton1.ButtonColor, gdHorizontal);
    ACanvas.Brush.Color := clWhite;
    ACanvas.FillRect(ARect.Left, ARect.Top, gutter, ARect.Bottom);
    // Draw gutter line
    ACanvas.Pen.Color := clGray;
    ACanvas.Line(gutter, ARect.Top, gutter, ARect.Bottom);
  end;

  // Draw images in the gutter
  x := margin;
  if Assigned(lMenu.Images) and (menuItem.ImageIndex > -1) and (menuItem.Checked or not menuItem.AutoCheck) then
  begin
    y := (ARect.Top + ARect.Bottom - h) div 2;
    imgRes.Draw(ACanvas, x, y, menuItem.ImageIndex, menuItem.Enabled);
  end else
  begin
    // no images --> draw check marks or radio buttons
    y := (ARect.Top + ARect.Bottom - ACanvas.TextHeight('Tg')) div 2;
    if menuItem.Checked then begin
      x := (gutter - ACanvas.TextWidth(checkmark)) div 2;
      ACanvas.TextOut(x, y, checkmark)
    end;
  end;

  // Draw text
  x := gutter + margin*2;
  if (menuItem.Caption = '-') then
  begin
    ACanvas.Pen.Color := clGray;
    y := (ARect.Top + ARect.Bottom) div 2;
    ACanvas.Line(gutter + margin, y, ARect.Right, Y);
  end else
  begin
    y := (ARect.Top + ARect.Bottom - ACanvas.TextHeight('Tg')) div 2;
    ACanvas.Brush.Style := bsClear;
    if not menuItem.Enabled then
      ACanvas.Font.Color := clGrayText;
    ACanvas.TextOut(x, y,  menuItem.Caption);
  end;
end;

end.

