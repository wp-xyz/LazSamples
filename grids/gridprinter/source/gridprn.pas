unit GridPrn;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, StdCtrls, Grids, Printers;

type
  TGridPrinter = class;  // forward declaration

  TGridPrnGetCellTextEvent = procedure (Sender: TObject; AGrid: TCustomGrid;
    ACol, ARow: Integer; var AText: String) of object;

  TGridPrnOrder = (poRowsFirst, poColsFirst);

  TGridPrnMargins = class(TPersistent)
  private
    FMargins: array[0..5] of Double;
    FOwner: TGridPrinter;
    function GetMargin(AIndex: Integer): Double;
    function IsStoredMargin(AIndex: Integer): Boolean;
    procedure SetMargin(AIndex: Integer; AValue: Double);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TGridPrinter);
  published
    property Left: Double index 0 read GetMargin write SetMargin stored IsStoredMargin;
    property Top: Double index 1 read GetMargin write SetMargin stored IsStoredMargin;
    property Right: Double index 2 read GetMargin write SetMargin stored IsStoredMargin;
    property Bottom: Double index 3 read GetMargin write SetMargin stored IsStoredMargin;
    property Header: Double index 4 read GetMargin write SetMargin stored IsStoredMargin;
    property Footer: Double index 5 read GetMargin write SetMargin stored IsStoredMargin;
  end;

  TGridPrnHeaderFooterSection = (hfsLeft, hfsCenter, hfsRight);

  TGridPrnHeaderFooter = class(TPersistent)
  private
    FFont: TFont;
    FLineColor: TColor;
    FLineWidth: Double;
    FShowLine: Boolean;
    FOwner: TGridPrinter;
    FSectionSeparator: String;
    FSectionText: array[TGridPrnHeaderFooterSection] of string;
    FVisible: Boolean;
    function GetProcessedText(AIndex: TGridPrnHeaderFooterSection): String;
    function GetSectionText(AIndex: TGridPrnHeaderFooterSection): String;
    function GetText: String;
    function IsLineWidthStored: Boolean;
    procedure SetFont(AValue: TFont);
    procedure SetLineColor(AValue: TColor);
    procedure SetLineWidth(AValue: Double);
    procedure SetSectionText(AIndex: TGridPrnHeaderFooterSection; AValue: String);
    procedure SetShowLine(AValue: Boolean);
    procedure SetText(AValue: String);
    procedure SetVisible(AValue: Boolean);
  protected
    procedure Changed(Sender: TObject);
  public
    constructor Create(AOwner: TGridPrinter);
    destructor Destroy; override;
    function IsShown: Boolean;
    function RealLineColor: TColor;
    function RealLineWidth: Integer;
    property ProcessedText[AIndex: TGridPrnHeaderFooterSection]: String read GetProcessedText;
    property SectionText[AIndex: TGridPrnHeaderFooterSection]: String read GetSectionText;
  published
    property Font: TFont read FFont write SetFont;
    property LineColor: TColor read FLineColor write SetLineColor default clDefault;
    property LineWidth: Double read FLineWidth write SetLineWidth stored IsLineWidthStored;
    property SectionSeparator: String read FSectionSeparator write FSectionSeparator;
    property ShowLine: Boolean read FShowLine write SetShowLine default true;
    property Text: String read GetText write SetText;
    property Visible: Boolean read FVisible write SetVisible default true;
  end;


  { TGridPrinter }

  TGridPrnOutputDevice = (odPrinter, odPreview);

  TGridPrinter = class(TComponent)
  private
    FBorderLineColor: Integer;
    FBorderLineWidth: Double;
    FFixedLineColor: TColor;
    FFixedLineWidth: Double;
    FGrid: TCustomGrid;
    FGridLineColor: TColor;
    FGridLineWidth: Double;
    FHeader: TGridPrnHeaderFooter;
    FFileName: String;      // to be used by header/footer
    FFooter: TGridPrnHeaderFooter;
    FMargins: TGridPrnMargins;
    FMonochrome: Boolean;
    FPadding: Integer;
    FPageHeight: Integer;
    FPageWidth: Integer;
    FPrintOrder: TGridPrnOrder;
    FOnGetCellText: TGridPrnGetCellTextEvent;
    FOnPrepareCanvas: TOnPrepareCanvasEvent;
    FOnUpdatePreview: TNotifyEvent;
    function GetBorderLineWidthHor: Integer;
    function GetBorderLineWidthVert: Integer;
    function GetCanvas: TCanvas;
    function GetFixedLineWidthHor: Integer;
    function GetFixedLineWidthVert: Integer;
    function GetGridLineWidthHor: Integer;
    function GetGridLineWidthVert: Integer;
    function GetOrientation: TPrinterOrientation;
    function GetPageCount: Integer;
    function GetPageNumber: Integer;
    function IsBorderLineWidthStored: Boolean;
    function IsFixedLineWidthStored: Boolean;
    function IsGridLineWidthStored: Boolean;
    procedure SetBorderLineColor(AValue: TColor);
    procedure SetBorderLineWidth(AValue: Double);
    procedure SetFileName(AValue: String);
    procedure SetFixedLineColor(AValue: TColor);
    procedure SetFixedLineWidth(AValue: Double);
    procedure SetGrid(AValue: TCustomGrid);
    procedure SetGridLineColor(AValue: TColor);
    procedure SetGridLineWidth(AValue: Double);
    procedure SetOrientation(AValue: TPrinterOrientation);
  protected
    FFactorX: Double;              // Multiply to convert screen to printer/preview pixels
    FFactorY: Double;
    FLeftMargin: Integer;         // Scaled page margins
    FTopMargin: Integer;
    FRightMargin: Integer;
    FBottomMargin: Integer;
    FHeaderMargin: Integer;
    FFooterMargin: Integer;
    FColWidths: array of Double;      // Array of scaled grid column widts
    FRowHeights: array of Double;     // Array of scaled grid row heights
    FFixedColPos: Integer;            // Scaled right end of the fixed cols
    FFixedRowPos: Integer;            // Scaled bottom end of the fixed rows
    FOutputDevice: TGridPrnOutputDevice;
    FPageBreakRows: array of Integer;  // Indices of first row on new page
    FPageBreakCols: array of Integer;  // Indices of first columns on new page
    FPageNumber: Integer;
    FPageCount: Integer;
    FPageRect: TRect;                  // Bounds of printable rectangle
    FPixelsPerInchX: Integer;
    FPixelsPerInchY: Integer;
    FPreviewBitmap: TBitmap;           // Bitmap to which the preview image is printed
    FPreviewPage: Integer;             // Page request for the preview bitmap
    FPreviewPercent: Integer;          // Scaling factor for preview bitmap
    FColCount: Integer;
    FRowCount: Integer;
    FFixedCols: Integer;
    FFixedRows: Integer;
    FPrinting: Boolean;
    procedure DoPrepareCanvas(ACol, ARow: Integer); virtual;
    procedure DoUpdatePreview; virtual;
    procedure Execute(ACanvas: TCanvas);
    procedure LayoutPagebreaks;
    procedure Measure(APageWidth, APageHeight, XDpi, YDpi: Integer);
    procedure NewPage;
    procedure Prepare;
    procedure PrepareCanvas(ACanvas: TCanvas; ACol, ARow: Integer); virtual;
    procedure PrintByCols(ACanvas: TCanvas);
    procedure PrintByRows(ACanvas: TCanvas);
    procedure PrintCell(ACanvas: TCanvas; ACol, ARow: Integer; ARect: TRect); virtual;
    procedure PrintCheckbox(ACanvas: TCanvas; ACol, ARow: Integer; ARect: TRect;
      ACheckState: TCheckboxstate); virtual;
    procedure PrintColHeaders(ACanvas: TCanvas; ACol1, ACol2: Integer);
    procedure PrintFooter(ACanvas: TCanvas);
    procedure PrintHeader(ACanvas: TCanvas);
    procedure PrintGridLines(ACanvas: TCanvas; AFirstCol, AFirstRow, XEnd, YEnd: Integer);
    procedure PrintPage(ACanvas: TCanvas; AStartCol, AStartRow, AEndCol, AEndRow: Integer);
    procedure PrintRowHeader(ACanvas: TCanvas; ARow: Integer; Y: Double);
    procedure ScaleColWidths(AFactor: Double);
    procedure ScaleRowHeights(AFactor: Double);
    procedure SelectFont(ACanvas: TCanvas; AFont: TFont);
    property OutputDevice: TGridPrnOutputDevice read FOutputDevice;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreatePreviewBitmap(APageNo, APercentage: Integer): TBitmap;
    function GetCellText(ACol, ARow: Integer): String; virtual;
    procedure Print;
    function ScaleX(AValue: Integer): Integer; inline;
    function ScaleY(AValue: Integer): Integer; inline;
    procedure UpdatePreview;
    property Canvas: TCanvas read GetCanvas;
    property FooterMargin: Integer read FFooterMargin;
    property HeaderMargin: Integer read FHeaderMargin;
    property PageHeight: Integer read FPageHeight;
    property PageWidth: Integer read FPageWidth;
    property PageRect: TRect read FPageRect;
    property PixelsPerInchX: Integer read FPixelsPerInchX;
    property PixelsPerInchY: Integer read FPixelsPerInchY;
    property PageCount: Integer read GetPageCount;
    property PageNumber: Integer read FPageNumber;
  published
    property Grid: TCustomGrid read FGrid write SetGrid;
    property BorderLineColor: TColor read FBorderLineColor write SetBorderLineColor default clDefault;
    property BorderLineWidth: Double read FBorderLineWidth write SetBorderLineWidth stored IsBorderLineWidthStored;
    property FileName: String read FFileName write SetFileName;
    property FixedLineColor: TColor read FFixedLineColor write SetFixedLineColor default clDefault;
    property FixedLineWidth: Double read FFixedLineWidth write SetFixedLineWidth stored IsFixedLineWidthStored;
    property Footer: TGridPrnHeaderFooter read FFooter write FFooter;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clDefault;
    property GridLineWidth: Double read FGridLineWidth write SetGridLineWidth stored IsGridLineWidthStored;
    property Header: TGridPrnHeaderFooter read FHeader write FHeader;
    property Margins: TGridPrnMargins read FMargins write FMargins;
    property Monochrome: Boolean read FMonochrome write FMonochrome default false;
    property Orientation: TPrinterOrientation read GetOrientation write SetOrientation default poPortrait;
    property PrintOrder: TGridPrnOrder read FPrintOrder write FPrintOrder default poRowsFirst;
    property OnGetCellText: TGridPrnGetCellTextEvent read FOnGetCellText write FOnGetCellText;
    property OnPrepareCanvas: TOnPrepareCanvasEvent read FOnPrepareCanvas write FOnPrepareCanvas;
    property OnUpdatePreview: TNotifyEvent read FOnUpdatePreview write FOnUpdatePreview;
  end;

function mm2px(mm: Double; dpi: Integer): Integer;
function px2mm(px: Integer; dpi: Integer): Double;

implementation

uses
  LCLIntf, LCLType, OSPrinters, Themes;

type
  TGridAccess = class(TCustomGrid);

const
  INCH = 25.4;    // 1" = 25.4 mm

  DefaultTextStyle: TTextStyle = (
    Alignment: taLeftJustify;
    Layout: tlCenter;
    SingleLine: true;
    Clipping: true;
    ExpandTabs: false;
    ShowPrefix: false;
    WordBreak: false;
    Opaque: false;
    SystemFont: false;
    RightToLeft: false;
    EndEllipsis: false
  );

function IfThen(cond: Boolean; a, b: Integer): Integer;
begin
  if cond then Result := a else Result := b;
end;

function IfThen(cond: Boolean; a, b: TColor): TColor;
begin
  if cond then Result := a else Result := b;
end;

function DefaultFontSize(AFont: TFont): Integer;
var
  fontData: TFontData;
begin
  fontData := GetFontData(AFont.Handle);
  Result := abs(fontData.Height) * 72 div ScreenInfo.PixelsPerInchY;
end;

procedure FixFontSize(AFont: TFont);
begin
  if AFont.Size = 0 then
    AFont.Size := DefaultFontSize(AFont);
end;

function mm2px(mm: Double; dpi: Integer): Integer;
begin
  Result := round(mm/INCH * dpi);
end;

function px2mm(px: Integer; dpi: Integer): Double;
begin
  Result := px * INCH / dpi;
end;


{ TGridPrnMargins }

constructor TGridPrnMargins.Create(AOwner: TGridPrinter);
var
  i: Integer;
begin
  inherited Create;
  FOwner := AOwner;
  for i := 0 to 3 do FMargins[i] := 20.0;
  for i := 4 to 5 do FMargins[i] := 10.0;
end;

procedure TGridPrnMargins.Changed;
begin
  if (FOwner <> nil) then
    FOwner.UpdatePreview;
end;

function TGridPrnMargins.GetMargin(AIndex: Integer): Double;
begin
  Result := FMargins[AIndex];
end;

function TGridPrnMargins.IsStoredMargin(AIndex: Integer): Boolean;
begin
  case AIndex of
    0..3: Result := FMargins[AIndex] <> 20.0;
    4..5: Result := FMargins[AIndex] <> 10.0;
  end;
end;

procedure TGridPrnMargins.SetMargin(AIndex: Integer; AValue: Double);
begin
  if FMargins[AIndex] <> AValue then
  begin
    FMargins[AIndex] := AValue;
    Changed;
  end;
end;


{ TGridPrnHeaderFooter }

constructor TGridPrnHeaderFooter.Create(AOwner: TGridPrinter);
begin
  inherited Create;
  FOwner := AOwner;

  FSectionSeparator := '|';

  FFont := TFont.Create;
  FixFontSize(FFont);
  FFont.Size := FFont.Size - 1;
  FFont.OnChange := @Changed;

  FLineColor := clDefault;
  FLineWidth := 0;
  FShowLine := true;
  FVisible := true;
end;

destructor TGridPrnHeaderFooter.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TGridPrnHeaderFooter.Changed(Sender: TObject);
begin
  if (FOwner <> nil) then
    FOwner.UpdatePreview;
end;

function TGridPrnHeaderFooter.GetProcessedText(AIndex: TGridPrnHeaderFooterSection): String;
const
  UNKNOWN = '<unknown>';

  procedure Replace(AParam: String);
  var
    s: String;
  begin
    if FOwner <> nil then
      case AParam of
        '$PAGECOUNT': s := IntToStr(FOwner.PageCount);
        '$PAGE': s := IntToStr(FOwner.PageNumber);
        '$FULL_FILENAME': s := ExpandFileName(FOwner.FileName);
        '$FILENAME': s := ExtractFileName(FOwner.FileName);
        '$PATH': s := ExtractFilePath(ExpandFileName(FOwner.FileName));
      end
    else
      s := UNKNOWN;
    Result := StringReplace(Result, AParam, s, [rfReplaceAll, rfIgnoreCase]);
  end;

  begin
  Result := FSectionText[AIndex];
  Result := StringReplace(Result, '$DATE', DateToStr(Now), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$TIME', TimeToStr(Now), [rfReplaceAll, rfIgnoreCase]);
  Replace('$PAGECOUNT');
  Replace('$PAGE');
  Replace('$FULL_FILENAME');
  Replace('$FILENAME');
  Replace('$PATH');
end;

function TGridPrnHeaderFooter.GetSectionText(AIndex: TGridPrnHeaderFooterSection): String;
begin
  Result := FSectionText[AIndex];
end;

function TGridPrnHeaderFooter.GetText: String;
begin
  Result :=
    FSectionText[hfsLeft] + FSectionSeparator +
    FSectionText[hfsCenter] + FSectionSeparator +
    FSectionText[hfsRight];
end;

function TGridPrnHeaderFooter.IsLineWidthStored: Boolean;
begin
  Result := FLineWidth > 0;
end;

function TGridPrnHeaderFooter.IsShown: Boolean;
begin
  Result := FVisible and (
    (FSectionText[hfsLeft] <> '') or
    (FSectionText[hfsCenter] <> '') or
    (FSectionText[hfsRight] <> '')
  );
end;

function TGridPrnHeaderFooter.RealLineColor: TColor;
begin
  if ((FOwner <> nil) and FOwner.Monochrome) or (FLineColor = clDefault) then
    Result := clBlack
  else
    Result := FLineColor;
end;

function TGridPrnHeaderFooter.RealLineWidth: Integer;
begin
  if FLineWidth = 0 then
    Result := FOwner.ScaleY(1)
  else
    Result := mm2px(FLineWidth, FOwner.PixelsPerInchY);
end;

procedure TGridPrnHeaderFooter.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  Changed(nil);
end;

procedure TGridPrnHeaderFooter.SetLineColor(AValue: TColor);
begin
  if FLineColor <> AValue then
  begin
    FLineColor := AValue;
    Changed(nil);
  end;
end;

procedure TGridPrnHeaderFooter.SetLineWidth(AValue: Double);
begin
  if FLineWidth <> AValue then
  begin
    FLineWidth := AValue;
    Changed(nil);
  end;
end;

procedure TGridPrnHeaderFooter.SetSectionText(AIndex: TGridPrnHeaderFooterSection;
  AValue: String);
begin
  if FSectionText[AIndex] <> AValue then
  begin
    FSectionText[AIndex] := AValue;
    Changed(nil);
  end;
end;

procedure TGridPrnHeaderFooter.SetShowLine(AValue: Boolean);
begin
  if FShowLine <> AValue then
  begin
    FShowLine := AValue;
    Changed(nil);
  end;
end;

procedure TGridPrnHeaderFooter.SetText(AValue: String);
var
  sa: TStringArray;
begin
  if GetText = AValue then
    exit;
  sa := AValue.Split([FSectionSeparator]);
  if Length(sa) > 0 then FSectionText[hfsLeft] := sa[0] else FSectionText[hfsLeft] := '';
  if Length(sa) > 1 then FSectionText[hfsCenter] := sa[1] else FSectionText[hfsCenter] := '';
  if Length(sa) > 2 then FSectionText[hfsRight] := sa[2] else FSectionText[hfsRight] := '';
  Changed(self);
end;

procedure TGridPrnHeaderFooter.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed(self);
  end;
end;


{ TGridPrinter }

constructor TGridPrinter.Create(AOwner: TComponent);
begin
  inherited;

  FMargins := TGridPrnMargins.Create(Self);
  FHeader := TGridPrnHeaderFooter.Create(Self);
  FFooter := TGridPrnHeaderFooter.Create(Self);

  FPrintOrder := poRowsFirst;
  FBorderLineColor := clDefault;
  FFixedLineColor := clDefault;
  FGridLineColor := clDefault;
end;

destructor TGridPrinter.Destroy;
begin
  FHeader.Free;
  FFooter.Free;
  FMargins.Free;
  inherited;
end;

function TGridPrinter.CreatePreviewBitmap(APageNo, APercentage: Integer): TBitmap;
begin
  if FGrid = nil then
  begin
    Result := nil;
    exit;
  end;

  FOutputDevice := odPreview;

  FPreviewPercent := APercentage;
  FPreviewPage := APageNo;  // out-of-range values are handled by Prepare
  Prepare;

  FPreviewBitmap := TBitmap.Create;
  FPreviewBitmap.SetSize(FPageWidth, FPageHeight);
  FPreviewBitmap.Canvas.Brush.Color := clWhite;
  FPreviewBitmap.Canvas.FillRect(0, 0, FPageWidth, FPageHeight);

  Execute(FPreviewBitmap.Canvas);

  Result := FPreviewBitmap;
end;

procedure TGridPrinter.DoPrepareCanvas(ACol, ARow: Integer);
begin
  if Assigned(FOnPrepareCanvas) then
    FOnPrepareCanvas(Self, ACol, ARow, []);
end;

procedure TGridPrinter.DoUpdatePreview;
begin
  if Assigned(FOnUpdatePreview) and (FOutputDevice = odPreview) then
    FOnUpdatePreview(Self);
end;

procedure TGridPrinter.Execute(ACanvas: TCanvas);
begin
  FPrinting := true;
  case FPrintOrder of
    poRowsFirst: PrintByRows(ACanvas);
    poColsFirst: PrintByCols(ACanvas);
  end;
  FPrinting := false;
end;

function TGridPrinter.GetBorderLineWidthHor: Integer;
begin
  if FBorderLineWidth = 0 then
    Result := ScaleY(2)
  else
    Result := mm2px(FBorderLineWidth, FPixelsPerInchY);
end;

function TGridPrinter.GetBorderLineWidthVert: Integer;
begin
  if FBorderLineWidth = 0 then
    Result := ScaleX(2)
  else
    Result := mm2px(FBorderLineWidth, FPixelsPerInchX);
end;


function TGridPrinter.GetCanvas: TCanvas;
begin
  if FPrinting then
    case FOutputDevice of
      odPrinter: Result := Printer.Canvas;
      odPreview: Result := FPreviewBitmap.Canvas;
    end
  else
    Result := nil;
end;

function TGridPrinter.GetCellText(ACol, ARow: Integer): String;
var
  col: TGridColumn;
  lGrid: TGridAccess;
begin
  Result := '';
  if FGrid = nil then
    exit;

  lGrid := TGridAccess(FGrid);
  if lGrid.Columns.Enabled and (ACol >= FFixedCols) and (ARow = 0) then
  begin
    col := lGrid.Columns[ACol - FFixedCols];
    Result := col.Title.Caption;
    exit;
  end;

  if Assigned(FOnGetCellText) then
    FOnGetCellText(self, FGrid, ACol, ARow, Result)
  else
    Result := lGrid.GetCells(Acol, ARow);
end;

function TGridPrinter.GetFixedLineWidthHor: Integer;
begin
  if FFixedLineWidth = 0 then
    Result := ScaleY(TGridAccess(FGrid).GridLineWidth)
  else
    Result := mm2px(FFixedLineWidth, FPixelsPerInchY);
end;

function TGridPrinter.GetFixedLineWidthVert: Integer;
begin
  if FFixedLineWidth = 0 then
    Result := ScaleX(TGridAccess(FGrid).GridLineWidth)
  else
    Result := mm2px(FFixedLineWidth, FPixelsPerInchX);
end;

function TGridPrinter.GetGridLineWidthHor: Integer;
begin
  if FGridLineWidth = 0 then
    Result := ScaleY(TGridAccess(FGrid).GridLineWidth)
  else
    Result := mm2px(FGridLineWidth, FPixelsPerInchY);
end;

function TGridPrinter.GetGridLineWidthVert: Integer;
begin
  if FGridLineWidth = 0 then
    Result := ScaleX(TGridAccess(FGrid).GridLineWidth)
  else
    Result := mm2px(FGridLineWidth, FPixelsPerInchX);
end;

function TGridPrinter.GetOrientation: TPrinterOrientation;
begin
  Result := Printer.Orientation;
end;

function TGridPrinter.GetPageCount: Integer;
begin
  if FPageCount = 0 then
    Prepare;
  Result := FPageCount;
end;

function TGridPrinter.GetPageNumber: Integer;
begin
  if FPageNumber <= 0 then
    Prepare;
  Result := FPageNumber;
end;

function TGridPrinter.IsBorderLineWidthStored: Boolean;
begin
  Result := FBorderLineWidth <> 0;
end;

function TGridPrinter.IsFixedLineWidthStored: Boolean;
begin
  Result := FFixedLineWidth <> 0.0;
end;

function TGridPrinter.IsGridLineWidthStored: Boolean;
begin
  Result := FGridLineWidth <> 0.0;
end;

{ Find the column and row indices before which page breaks are occuring.
  Store them in the arrays FPageBreakCols and FPageBreakRows.
  Note that the indices do not contain the fixed columns/rows. }
procedure TGridPrinter.LayoutPageBreaks;
var
  col, row: Integer;
  n: Integer;
  totalWidth, totalHeight: Double;
begin
  // Scanning horizontally --> get page break column indices
  SetLength(FPageBreakCols, FColCount);
  n := 0;
  totalWidth := FFixedColPos;
  FPageBreakCols[0] := FFixedCols;
  for col := FFixedCols to FColCount-1 do
  begin
    totalWidth := totalWidth + FColWidths[col];
    if totalWidth >= FPageRect.Right then
    begin
      inc(n);
      FPageBreakCols[n] := col;
      totalWidth := FFixedColPos + FColWidths[col];
    end;
  end;
  SetLength(FPageBreakCols, n+1);

  // Scanning vertically --> get page break row indices
  SetLength(FPageBreakRows, FRowCount);
  n := 0;
  totalHeight := FFixedRowPos;
  FPageBreakRows[0] := FFixedRows;
  for row := FFixedRows to FRowCount-1 do
  begin
    totalHeight := totalHeight + FRowHeights[row];
    if totalHeight > FPageRect.Bottom then
    begin
      inc(n);
      FPageBreakRows[n] := row;
      totalHeight := FFixedRowPos + FRowHeights[row];
    end;
  end;
  SetLength(FPageBreakRows, n+1);

  FPageCount := Length(FPageBreakCols) * Length(FPageBreakRows);
end;

{ Converts length properties to the specified pixel density. }
procedure TGridPrinter.Measure(APageWidth, APageHeight, XDpi, YDpi: Integer);
begin
  // Multiplication factor needed by ScaleX and ScaleY
  FFactorX := XDpi / ScreenInfo.PixelsPerInchX;
  FFactorY := YDpi / ScreenInfo.PixelsPerInchY;

  // Margins in the new pixeld density units.
  FLeftMargin := mm2px(FMargins.Left, XDpi);
  FTopMargin := mm2px(FMargins.Top, YDpi);
  FRightMargin := mm2px(FMargins.Right, XDpi);
  FBottomMargin := mm2px(FMargins.Bottom, YDpi);
  FHeaderMargin := mm2px(FMargins.Header, YDpi);
  FFooterMargin := mm2px(FMargins.Footer, YDpi);
  FPageRect := Rect(FLeftMargin, FTopMargin, APageWidth - FRightMargin, APageHeight - FBottomMargin);
  FPadding := ScaleX(varCellPadding);

  // Calculates column widths and row heights in the new pixel density units
  ScaleColWidths(FFactorX);
  ScaleRowHeights(FFactorY);
end;

procedure TGridPrinter.NewPage;
begin
  if FOutputDevice = odPrinter then
    Printer.NewPage;
end;

procedure TGridPrinter.Prepare;
begin
  // Calculate grid indices at which page breaks occur. Since the font size is
  // an integer, the zoomed preview may have slightly different values - which
  // is not desired. Therefore, we calculate this for the printer resolution.
  Measure(Printer.PageWidth, Printer.PageHeight, Printer.XDPI, Printer.YDPI);
  LayoutPagebreaks;

  case FOutputDevice of
    odPrinter:
      begin
        FPixelsPerInchX := Printer.XDPI;
        FPixelsPerInchY := Printer.YDPI;
        FPageWidth := Printer.PageWidth;
        FPageHeight := Printer.PageHeight;
      end;
    odPreview:
      begin
        if FPreviewPercent = 0 then
          exit;
        FPixelsPerInchX := ScreenInfo.PixelsPerInchX * FPreviewPercent div 100;
        FPixelsPerInchY := ScreenInfo.PixelsPerInchY * FPreviewPercent div 100;
        FPageWidth := round(Printer.PageWidth * FPixelsPerInchX / Printer.XDPI);
        FPageHeight := round(Printer.PageHeight * FPixelsPerInchY / Printer.YDPI);
        // Recalculates page dimensions and col/row sizes, now based on
        // the "real" ppi of the preview.
        Measure(FPageWidth, FPageHeight, FPixelsPerInchX, FPixelsPerInchY);
      end;
  end;
end;

procedure TGridPrinter.PrepareCanvas(ACanvas: TCanvas; ACol, ARow: Integer);
var
  lGrid: TGridAccess;
  color, alternateColor: TColor;
  textStyle: TTextStyle;
begin
  lGrid := TGridAccess(FGrid);

  // Background color
  ACanvas.Brush.Style := bsSolid;
  if (ACol < FFixedCols) or (ARow < FFixedRows) then
    ACanvas.Brush.Color := ColorToRGB(lGrid.FixedColor)
  else
  begin
    color := ColorToRGB(lGrid.Color);
    alternateColor := ColorToRGB(lGrid.AlternateColor);
    if (color <> alternateColor) and Odd(ARow) then
      ACanvas.Brush.Color := alternateColor
    else
      ACanvas.Brush.Color := color;
  end;
  // Font
  SelectFont(ACanvas, lGrid.Font);
  // Text style
  textStyle := DefaultTextStyle;
  if (goCellEllipsis in lGrid.Options) then
    textStyle.EndEllipsis := true;
  ACanvas.TextStyle := textStyle;

  // Fire the event OnPrepareCanvas
  DoPrepareCanvas(ACol, ARow);

  // Fix zero font size and monochrome text color
  FixFontSize(ACanvas.Font);
  if FMonochrome then
    ACanvas.Font.Color := clBlack;
end;

procedure TGridPrinter.Print;
begin
  if FGrid = nil then
    exit;

  FOutputDevice := odPrinter;
  Prepare;
  Printer.BeginDoc;
  try
    Execute(Printer.Canvas);
  finally
    Printer.EndDoc;
  end;
end;

{ Advances first along rows when handling page-breaks. }
procedure TGridPrinter.PrintByCols(ACanvas: TCanvas);
var
  vertPage, horPage: Integer;
  col1, col2: Integer;
  row1, row2: Integer;
begin
  SelectFont(ACanvas, FGrid.Font);
  FPageNumber := 1;

  for horPage := 0 to High(FPageBreakCols) do
  begin
    col1 := FPageBreakCols[horPage];
    if horPage < High(FPageBreakCols) then
      col2 := FPageBreakCols[horPage+1] - 1
    else
      col2 := FColCount-1;

    for vertPage := 0 to High(FPageBreakRows) do
    begin
      row1 := FPageBreakRows[vertPage];
      if vertPage < High(FPageBreakRows) then
        row2 := FPageBreakRows[vertPage+1] - 1
      else
        row2 := FRowCount-1;
      // Print page beginning at col1/row1
      if (FOutputDevice = odPrinter) or  // Printer renders all pages
         (FPageNumber = FPreviewPage)    // Preview can render only a single page
      then
        PrintPage(ACanvas, col1, row1, col2, row2);
      inc(FPageNumber);
    end;
  end;
end;

{ Advances first along columns when handling page-breaks. }
procedure TGridPrinter.PrintByRows(ACanvas: TCanvas);
var
  vertPage, horPage: Integer;
  col1, col2: Integer;
  row1, row2: Integer;
begin
  SelectFont(ACanvas, FGrid.Font);
  FPageNumber := 1;

  for vertPage := 0 to High(FPageBreakRows) do
  begin
    row1 := FPageBreakRows[vertPage];
    if vertPage < High(FPageBreakRows) then
      row2 := FPageBreakRows[vertPage+1] - 1
    else
      row2 := FRowCount-1;

    for horPage := 0 to High(FPageBreakCols) do
    begin
      col1 := FPageBreakCols[horPage];
      if horPage < High(FPageBreakCols) then
        col2 := FPageBreakCols[horPage+1] - 1
      else
        col2 := FColCount-1;
      // Print the page beginning at col1/row1
      if (FOutputDevice = odPrinter) or  // Printer renders all pages
         (FPageNumber = FPreviewPage)    // Preview can render only a single page
      then
        PrintPage(ACanvas, col1, row1, col2, row2);
      inc(FPageNumber);
    end;
  end;
end;

{ Prints the cell at ACol/ARow. The cell will appear in the given rectangle. }
procedure TGridPrinter.PrintCell(ACanvas: TCanvas; ACol, ARow: Integer;
  ARect: TRect);
var
  s: String;
  col: TGridColumn;
  lGrid: TGridAccess;
  checkedState: TCheckboxState;
begin
  lGrid := TGridAccess(FGrid);

  PrepareCanvas(ACanvas, ACol, ARow);
  if not FMonochrome then
    ACanvas.FillRect(ARect);

  s := GetCellText(ACol, ARow);
  InflateRect(ARect, -FPadding, 0);

  // Handle checkbox columns
  if lGrid.Columns.Enabled and (ACol >= FFixedCols) and (ARow >= FFixedRows) then
  begin
    col := lGrid.Columns[ACol - FFixedCols];
    if col.Buttonstyle = cbsCheckboxColumn
    then begin
      if s = col.ValueChecked then
        checkedState := cbChecked
      else
      if s = col.ValueUnChecked then
        checkedState := cbUnchecked
      else
        checkedState := cbGrayed;
      PrintCheckbox(ACanvas, ACol, ARow, ARect, checkedState);
      exit;
    end;
  end;

  // Normal text output
  ACanvas.TextRect(ARect, ARect.Left, ARect.Top, s);
end;

procedure TGridPrinter.PrintCheckbox(ACanvas: TCanvas; ACol, ARow: Integer;
  ARect: TRect; ACheckState: TCheckboxstate);
const
  arrtb:array[TCheckboxState] of TThemedButton =
    (tbCheckBoxUncheckedNormal, tbCheckBoxCheckedNormal, tbCheckBoxMixedNormal);
var
  details: TThemedElementDetails;
  cSize: TSize;
  R: TRect;
  P: Array[0..2] of TPoint;
begin
  // Determine size of checkbox
  details := ThemeServices.GetElementDetails(arrtb[ACheckState]);
  cSize := ThemeServices.GetDetailSize(Details);
  cSize.cx := ScaleX(cSize.cx);
  cSize.cy := ScaleY(cSize.cy);
  // Position the checkbox within the given rectangle, ARect.
  R.Left := (ARect.Left + ARect.Right - cSize.cx) div 2;
  R.Top := (ARect.Top + ARect.Bottom - cSize.cy) div 2;
  R.BottomRight := Point(R.Left + cSize.cx, R.Top + cSize.cy);
  // Prepare pen and brush
  ACanvas.Pen.Width := ScaleX(1);
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Style := psSolid;
  if ACheckState = cbGrayed then
    ACanvas.Brush.Color := clSilver
  else
    ACanvas.Brush.Color := clWhite;
  ACanvas.Brush.Style := bsSolid;
  // Draw checkbox border (= unchecked state)
  InflateRect(R, -ACanvas.Pen.Width div 2, -ACanvas.Pen.Width div 2);
  ACanvas.Rectangle(R);
  InflateRect(R, -ACanvas.Pen.Width div 2, -ACanvas.Pen.Width div 2);
  // Draw checkmark if checked or grayed
  if ACheckState in [cbChecked, cbGrayed] then
  begin
    if ACheckState = cbGrayed then ACanvas.Pen.Color := clGray;
    ACanvas.Pen.Width := ScaleX(2);
    P[0] := Point(R.Left + cSize.cx div 6, R.Top + cSize.cy div 2);
    P[1] := Point(R.Left + cSize.cx div 3, R.Bottom - cSize.cy div 6);
    P[2] := Point(R.Right - cSize.cx div 6, R.Top + cSize.cy div 6);
    ACanvas.PolyLine(P);
  end;
end;

{ Prints the column headers: at first the fixed column headers, then the
  headers between ACol1 and ACol2. }
procedure TGridPrinter.PrintColHeaders(ACanvas: TCanvas; ACol1, ACol2: Integer);
var
  R: TRect;
  col, row: Integer;
  x, y, x2, y2: Double;
begin
  x := FLeftMargin;
  y := FTopMargin;
  for row := 0 to FFixedRows-1 do
  begin
    y2 := FTopMargin + FRowHeights[row];
    for col := 0 to FFixedCols-1 do
    begin
      x2 := x + FColWidths[col];
      R := Rect(round(x), round(y), round(x2), round(y2));
      PrintCell(ACanvas, col, row, R);
      x := x2;
    end;
    for col := ACol1 to ACol2 do
    begin
      x2 := x + FColWidths[col];
      R := Rect(round(x), round(y), round(x2), round(y2));
      PrintCell(ACanvas, col, row, R);
      x := x2;
    end;
    y := y2;
  end;
end;

procedure TGridPrinter.PrintFooter(ACanvas: TCanvas);
var
  Width: array[TGridPrnHeaderFooterSection] of Integer = (0, 0, 0);
  printableWidth, lineHeight: Integer;
  x, y: Integer;
  s: String;
  R: TRect;
  textStyle: TTextStyle;
begin
  if not FFooter.IsShown then
    exit;

  SelectFont(ACanvas, FFooter.Font);
  printableWidth := FPageRect.Width;
  if (FFooter.SectionText[hfsLeft] <> '') and (FFooter.SectionText[hfsCenter] = '') and (FFooter.SectionText[hfsRight] = '') then
    Width[hfsLeft] := printableWidth
  else
  if (FFooter.SectionText[hfsLeft] = '') and (FFooter.SectionText[hfsCenter] <> '') and (FFooter.SectionText[hfsRight] = '') then
    Width[hfsCenter] := printableWidth
  else
  if (FFooter.SectionText[hfsLeft] = '') and (FFooter.SectionText[hfsCenter] = '') and (FFooter.SectionText[hfsRight] <> '') then
    Width[hfsRight] := printableWidth
  else begin
    Width[hfsLeft] := printableWidth div 3;
    Width[hfsCenter] := printableWidth div 3;
    Width[hfsRight] := printableWidth div 3;
  end;

  lineHeight := ACanvas.TextHeight('Rg');
  textStyle := DefaultTextStyle;

  y := FPageHeight - FFooterMargin - lineHeight;
  if FFooter.SectionText[hfsLeft] <> '' then
  begin
    s := FFooter.ProcessedText[hfsLeft];
    x := FLeftMargin;
    R := Rect(x, y, x + Width[hfsLeft], y + lineHeight);
    ACanvas.TextRect(R, R.Left, R.Top, s);
  end;
  if FFooter.SectionText[hfsCenter] <> '' then
  begin
    s := FFooter.ProcessedText[hfsCenter];
    x := (FPageRect.Left + FPageRect.Right - Width[hfsCenter]) div 2;
    R := Rect(x, y, x + Width[hfsCenter], y + lineHeight);
    textStyle.Alignment := taCenter;
    ACanvas.TextRect(R, R.Left, R.Top, s, textStyle);
  end;
  if FFooter.SectionText[hfsRight] <> '' then
  begin
    s := Footer.ProcessedText[hfsRight];
    x := FPageRect.Right;
    R := Rect(x, y, x + Width[hfsRight], y + lineHeight);
    textStyle.Alignment := taRightJustify;
    ACanvas.TextRect(R, R.Left, R.Top, s, textStyle);
  end;

  if FFooter.ShowLine then
  begin
    ACanvas.Pen.Color := FFooter.RealLineColor;
    ACanvas.Pen.Width := FFooter.RealLineWidth;
    ACanvas.Pen.Style := psSolid;
    dec(y, (ACanvas.Pen.Width+1) div 2);
    ACanvas.Line(FPageRect.Left, y, FPageRect.Right, y);
  end;
end;

procedure TGridPrinter.PrintGridLines(ACanvas: TCanvas;
  AFirstCol, AFirstRow, XEnd, YEnd: Integer);
var
  x, y: Double;
  xr, yr: Integer;  // x, y rounded to integer
  col, row: Integer;
  lGrid: TGridAccess;
begin
  lGrid := TGridAccess(FGrid);

  // Print inner grid lines
  ACanvas.Pen.Style := lGrid.GridLineStyle;
  ACanvas.Pen.Color := IfThen(FMonoChrome, clBlack,
    IfThen(FGridLineColor = clDefault, lGrid.GridLineColor, FGridLineColor));
  // ... vertical fixed cell lines
  if (goFixedVertLine in lGrid.Options) then
  begin
    ACanvas.Pen.Width := GetGridLineWidthVert;
    col := 1;
    x := FLeftMargin;
    while col < lGrid.FixedCols do
    begin
      x := x + FColWidths[col-1];
      xr := round(x);
      ACanvas.Line(xr, FTopMargin, xr, YEnd);
      inc(col);
    end;
    col := AFirstCol;
    x := FFixedColPos;
    xr := round(x);
    while (xr < XEnd) and (col < lGrid.ColCount) do
    begin
      x := x + FColWidths[col];
      xr := round(x);
      ACanvas.Line(xr, FTopMargin, xr, FFixedRowPos);
      inc(col);
    end;
  end;
  // ... vertical grid lines
  if (goVertLine in lGrid.Options) then
  begin
    ACanvas.Pen.Width := GetGridLineWidthVert;
    col := AFirstCol;
    x := FFixedColPos;
    xr := round(x);
    while (xr < XEnd) and (col < lGrid.ColCount) do
    begin
      x := x + FColWidths[col];
      xr := round(x);
      ACanvas.Line(xr, FFixedRowPos, xr, YEnd);
      inc(col);
    end;
  end;
  // ... horizontal fixed cell lines
  if (goFixedHorzLine in lGrid.Options) then
  begin
    ACanvas.Pen.Width := GetGridLineWidthHor;
    row := 1;
    y := FTopMargin;
    yr := round(y);
    while row < lGrid.FixedRows do
    begin
      y := y + FRowHeights[row];
      yr := round(y);
      ACanvas.Line(FLeftMargin, yr, XEnd, yr);
      inc(row);
    end;
    row := AFirstRow;
    y := FFixedRowPos;
    yr := round(y);
    while (yr < YEnd) and (row < lGrid.RowCount) do
    begin
      y := y + FRowHeights[row];
      yr := round(y);
      ACanvas.Line(FLeftMargin, yr, FFixedColPos, yr);
      inc(row);
    end;
  end;
  // ... horizontal grid lines
  if (goHorzLine in lGrid.Options) then
  begin
    ACanvas.Pen.Width := GetGridLineWidthHor;
    row := AFirstRow;
    y := FFixedRowPos;
    yr := round(y);
    while (yr < YEnd) and (row < lGrid.RowCount) do
    begin
      y := y + FRowHeights[row];
      yr := round(y);
      ACanvas.Line(FFixedColPos, yr, XEnd, yr);
      inc(row);
    end;
  end;

  // Print header border lines between fixed and normal cells
  // ... horizontal
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := IfThen(FMonochrome or (FFixedLineColor = clDefault), clBlack, FFixedLineColor);
  ACanvas.Pen.Width := GetFixedLineWidthHor;
  ACanvas.Line(FLeftMargin, FFixedRowPos, XEnd, FFixedRowPos);
  // ... vertical
  ACanvas.Pen.Width := GetFixedLineWidthVert;
  ACanvas.Line(FFixedColPos, FTopMargin, FFixedColPos, YEnd);

  // Print outer border lines
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := IfThen(FMonochrome, clBlack,
    IfThen(FBorderLineColor = clDefault, clBlack, ColorToRGB(FBorderLineColor)));
  // ... horizontal
  ACanvas.Pen.Width := GetBorderLineWidthHor;
  ACanvas.Line(FLeftMargin, FTopMargin, XEnd, FTopMargin);
  ACanvas.Line(FLeftMargin, YEnd, XEnd, YEnd);
  // ... vertical
  ACanvas.Pen.Width := GetBorderLineWidthVert;
  ACanvas.Line(FLeftMargin, FTopMargin, FLeftMargin, YEnd);
  ACanvas.Line(XEnd, FTopMargin, XEnd, YEnd);
end;

procedure TGridPrinter.PrintHeader(ACanvas: TCanvas);
var
  Width: array[TGridPrnHeaderFooterSection] of Integer = (0, 0, 0);
  printableWidth, lineHeight: Integer;
  x, y: Integer;
  s: String;
  R: TRect;
  textStyle: TTextStyle;
begin
  if not FHeader.IsShown then
    exit;

  SelectFont(ACanvas, FHeader.Font);
  printableWidth := FPageRect.Width;
  if (FHeader.SectionText[hfsLeft] <> '') and (FHeader.SectionText[hfsCenter] = '') and (FHeader.SectionText[hfsRight] = '') then
    Width[hfsLeft] := printableWidth
  else
  if (FHeader.SectionText[hfsLeft] = '') and (FHeader.SectionText[hfsCenter] <> '') and (FHeader.SectionText[hfsRight] = '') then
    Width[hfsCenter] := printableWidth
  else
  if (FHeader.SectionText[hfsLeft] = '') and (FHeader.SectionText[hfsCenter] = '') and (FHeader.SectionText[hfsRight] <> '') then
    Width[hfsRight] := printableWidth
  else begin
    Width[hfsLeft] := printableWidth div 3;
    Width[hfsCenter] := printableWidth div 3;
    Width[hfsRight] := printableWidth div 3;
  end;

  lineHeight := ACanvas.TextHeight('Rg');
  textStyle := DefaultTextStyle;

  y := FHeaderMargin;
  if FHeader.SectionText[hfsLeft] <> '' then
  begin
    s := FHeader.ProcessedText[hfsLeft];
    x := FLeftMargin;
    R := Rect(x, y, x + Width[hfsLeft], y + lineHeight);
    ACanvas.TextRect(R, R.Left, R.Top, s);
  end;
  if FHeader.SectionText[hfsCenter] <> '' then
  begin
    s := FHeader.ProcessedText[hfsCenter];
    x := (FPageRect.Left + FPageRect.Right - Width[hfsCenter]) div 2;
    R := Rect(x, y, x + Width[hfsCenter], y + lineHeight);
    textStyle.Alignment := taCenter;
    ACanvas.TextRect(R, R.Left, R.Top, s, textStyle);
  end;
  if FHeader.SectionText[hfsRight] <> '' then
  begin
    s := FHeader.ProcessedText[hfsRight];
    x := FPageRect.Right - Width[hfsRight];
    R := Rect(x, y, x + Width[hfsRight], y + lineHeight);
    textStyle.Alignment := taRightJustify;
    ACanvas.TextRect(R, R.Left, R.Top, s, textStyle);
  end;

  if FHeader.ShowLine then
  begin
    ACanvas.Pen.Color := FHeader.RealLineColor;
    ACanvas.Pen.Width := FHeader.RealLineWidth;
    ACanvas.Pen.Style := psSolid;
    inc(y, lineHeight + (ACanvas.Pen.Width+1) div 2);
    ACanvas.Line(FPageRect.Left, y, FPageRect.Right, y);
  end;
end;

procedure TGridPrinter.PrintPage(ACanvas: TCanvas;
  AStartCol, AStartRow, AEndCol, AEndRow: Integer);
var
  x, y: Double;
  x2, y2: Double;
  row, col: Integer;
  lastPagePrinted: Boolean;
  R: TRect;
begin
  // Print column headers
  PrintColHeaders(ACanvas, AStartCol, AEndCol);

  // Print grid cells
  y := FFixedRowPos;
  for row := AStartRow to AEndRow do
  begin
    y2 := y + FRowHeights[row];
    PrintRowHeader(ACanvas, row, y);
    x := FFixedColPos;
    for col := AStartCol to AEndCol do
    begin
      x2 := x + FColWidths[col];
      R := Rect(round(x), round(y), round(x2), round(y2));
      PrintCell(ACanvas, col, row, R);
      x := x2;
    end;
    y := y2;
  end;

  // Print cell grid lines
  PrintGridLines(ACanvas, AStartCol, AStartRow, round(x2), round(y2));

  // Print header and footer
  PrintHeader(ACanvas);
  PrintFooter(ACanvas);

  // Unless we printed the last cell we must send a pagebreak to the printer.
  lastPagePrinted := (AEndCol = FColCount-1) and (AEndRow = FRowCount-1);
  if not lastPagePrinted then
    NewPage;
end;

{ Prints the row headers of the specified row. Row headers are the cells in the
  FixedCols of that row. The row is positioned at the given y coordinate on
  the canvas. }
procedure TGridPrinter.PrintRowHeader(ACanvas: TCanvas; ARow: Integer;
  Y: Double);
var
  R: TRect;
  col: Integer;
  y1, y2: Integer;
  x, x2: Double;
begin
  x := FLeftMargin;                    // left side of the row
  y1 := round(Y);                      // upper end of the row
  y2 := round(Y + FRowHeights[ARow]);  // lower end of the row
  for col := 0 to FFixedCols-1 do
  begin
    x2 := x + FColWidths[col];
    R := Rect(round(x), y1, round(x2), y2);
    PrintCell(ACanvas, col, ARow, R);
    x := x2;
  end;
end;

procedure TGridPrinter.ScaleColWidths(AFactor: Double);
var
  i: Integer;
  w: Double;
  fixed: Double;
begin
  fixed := FLeftMargin;
  SetLength(FColWidths, FColCount);
  for i := 0 to FColCount-1 do
  begin
    w := AFactor * TGridAccess(FGrid).ColWidths[i];
    FColWidths[i] := w;
    if i < FFixedCols then
      fixed := fixed + w;
  end;
  FFixedColPos := round(fixed);
end;

procedure TGridPrinter.ScaleRowHeights(AFactor: Double);
var
  i: Integer;
  h: Double;
  fixed: Double;
begin
  fixed := FTopMargin;
  SetLength(FRowHeights, FRowCount);
  for i := 0 to FRowCount-1 do
  begin
    h := AFactor * TGridAccess(FGrid).RowHeights[i];
    FRowHeights[i] := h;
    if i < FFixedRows then
      fixed := fixed + h;
  end;
  FFixedRowPos := round(fixed);
end;

function TGridPrinter.ScaleX(AValue: Integer): Integer;
begin
  Result := Round(FFactorX * AValue);
end;

function TGridPrinter.ScaleY(AValue: Integer): Integer;
begin
  Result := Round(FFactorY * AValue);
end;

procedure TGridPrinter.SelectFont(ACanvas: TCanvas; AFont: TFont);
var
  fd: TFontData;
begin
  ACanvas.Font.Assign(AFont);
  ACanvas.Font.PixelsPerInch := FPixelsPerInchY;
  if AFont.Size = 0 then
  begin
    fd := GetFontData(AFont.Handle);
    ACanvas.Font.Size := abs(fd.Height) * 72 div ScreenInfo.PixelsPerInchY;
  end;
end;

procedure TGridPrinter.SetBorderLineColor(AValue: TColor);
begin
  if FBorderLineColor <> AValue then
  begin
    FBorderLineColor := AValue;
    UpdatePreview;
  end;
end;

procedure TGridPrinter.SetBorderLineWidth(AValue: Double);
begin
  if FBorderLineWidth <> AValue then
  begin
    FBorderLineWidth := AValue;
    UpdatePreview;
  end;
end;

procedure TGridPrinter.SetFileName(AValue: String);
begin
  if FFileName <> AValue then
  begin
    FFileName := AValue;
    UpdatePreview;
  end;
end;

procedure TGridPrinter.SetFixedLineColor(AValue: TColor);
begin
  if FFixedLineColor <> AValue then
  begin
    FFixedLineColor := AValue;
    UpdatePreview;
  end;
end;

procedure TGridPrinter.SetFixedLineWidth(AValue: Double);
begin
  if FFixedLineWidth <> AValue then
  begin
    FFixedLineWidth := AValue;
    UpdatePreview;
  end;
end;

procedure TGridPrinter.SetGrid(AValue: TCustomGrid);
begin
  FGrid := AValue;
  FColCount := TGridAccess(FGrid).ColCount;
  FRowCount := TGridAccess(FGrid).RowCount;
  FFixedCols := TGridAccess(FGrid).FixedCols;
  FFixedRows := TGridAccess(Fgrid).FixedRows;
  FPageNumber := 0;
  FPageCount := 0;
end;

procedure TGridPrinter.SetGridLineColor(AValue: TColor);
begin
  if FGridLineColor <> AValue then
  begin
    FGridLineColor := AValue;
    UpdatePreview;
  end;
end;

procedure TGridPrinter.SetGridLineWidth(AValue: Double);
begin
  if FGridLineWidth <> AValue then
  begin
    FGridLineWidth := AValue;
    UpdatePreview;
  end;
end;

procedure TGridPrinter.SetOrientation(AValue: TPrinterOrientation);
begin
  if GetOrientation <> AValue then
  begin
    Printer.Orientation := AValue;
    UpdatePreview;
  end;
end;

procedure TGridPrinter.UpdatePreview;
begin
  if FOutputDevice = odPreview then
    DoUpdatePreview;
end;

end.

