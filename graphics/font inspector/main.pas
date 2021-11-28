unit main;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Types,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ValEdit,
  ExtCtrls, Grids, ComCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnSampleText: TButton;
    cbFontName: TComboBox;
    cbFontSize: TComboBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbUnderline: TCheckBox;
    cbStrikeout: TCheckBox;
    gbFont: TGroupBox;
    gbSample: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    PaintBox: TPaintBox;
    Panel1: TPanel;
    TabControl: TTabControl;
    ValueListEditor1: TValueListEditor;
    procedure btnSampleTextClick(Sender: TObject);
    procedure cbBoldChange(Sender: TObject);
    procedure cbFontNameChange(Sender: TObject);
    procedure cbFontSizeChange(Sender: TObject);
    procedure cbItalicChange(Sender: TObject);
    procedure cbStrikeoutChange(Sender: TObject);
    procedure cbUnderlineChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure ValueListEditor1PrepareCanvas(sender: TObject; aCol,
      aRow: Integer; aState: TGridDrawState);
  private
    FSampleText: String;
    FUpdateNeeded: Boolean;
    procedure ListFont(AFontData: TFontData; ATextMetric: TTextMetric);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math, TypInfo, GraphUtil;

{ TMainForm }

procedure TMainForm.btnSampleTextClick(Sender: TObject);
begin
  FSampleText := InputBox('Enter sample text', 'Text:', FSampleText);
  Paintbox.Invalidate;
end;

procedure TMainForm.cbBoldChange(Sender: TObject);
begin
  if cbBold.Checked then
    Paintbox.Font.Style := Paintbox.Font.Style + [fsBold]
  else
    Paintbox.Font.Style := Paintbox.Font.Style - [fsBold]
end;

procedure TMainForm.cbFontNameChange(Sender: TObject);
begin
  PaintBox.Font.Name := cbFontName.Items[cbFontName.ItemIndex];
  FUpdateNeeded := true;
  Paintbox.Invalidate;
end;

procedure TMainForm.cbFontSizeChange(Sender: TObject);
var
  h: Integer;
begin
  if TryStrToInt(cbFontSize.Items[cbFontSize.ItemIndex], h) then
  begin
    Paintbox.Font.Size := h;
    FUpdateNeeded := true;
    Paintbox.Invalidate;
  end;
end;

procedure TMainForm.cbItalicChange(Sender: TObject);
begin
  if cbItalic.Checked then
    Paintbox.Font.Style := Paintbox.Font.Style + [fsItalic]
  else
    Paintbox.Font.Style := Paintbox.Font.Style - [fsItalic]
end;

procedure TMainForm.cbUnderlineChange(Sender: TObject);
begin
  if cbUnderline.Checked then
    Paintbox.Font.Style := Paintbox.Font.Style + [fsUnderline]
  else
    Paintbox.Font.Style := Paintbox.Font.Style - [fsUnderline]
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
end;

procedure TMainForm.cbStrikeoutChange(Sender: TObject);
begin
  if cbStrikeout.Checked then
    Paintbox.Font.Style := Paintbox.Font.Style + [fsStrikeout]
  else
    Paintbox.Font.Style := Paintbox.Font.Style - [fsStrikeout];
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cInputQueryEditSizePercents := 0;  // Avoid Inputbox becoming too wide. 
  
  ValueListEditor1.DefaultRowHeight := abs(ValueListEditor1.Font.Height)+2*varCellPadding;
  ValueListEditor1.Constraints.MinHeight := 24*ValueListEditor1.DefaultRowHeight;
  
  FSampleText := 'AghjyÄŠŢ';
  cbFontName.Items.Assign(Screen.Fonts);
  cbFontName.Items.Insert(0, 'default');
  cbFontName.ItemIndex := 0;
  ListFont(GetFontData(Font.Handle), Default(TTextMetric));
  FUpdateNeeded := true;
end;

procedure TMainForm.ListFont(AFontData: TFontData; ATextMetric: TTextMetric);
var
  fd: TFontData absolute AFontData;
  tm: TTextMetric;
begin
  tm := ATextMetric;
  with ValueListEditor1 do
  begin
    Clear;
    if TabControl.TabIndex = 0 then // Font Data
    begin
      InsertRow('Handle', IntToStr(fd.Handle), true);
      InsertRow('Height (Size)', Format('%d (%d px, %d pt)', [fd.Height, -fd.Height, round(-fd.Height/Screen.PixelsPerInch*72)]), true);
      InsertRow('Pitch', GetEnumName(TypeInfo(TFontPitch), Integer(fd.Pitch)), true);
      InsertRow('Style', SetToString(PTypeInfo(TypeInfo(TFontStylesBase)), integer(fd.Style), true), true);
      InsertRow('CharSet', IntToStr(fd.CharSet), true);
      InsertRow('Quality', GetEnumName(TypeInfo(TFontQuality), Integer(fd.Quality)), true);
      InsertRow('Name', fd.Name, true);
      InsertRow('Orientation', IntToStr(fd.Orientation), true);
    end else
    begin   // Text metic as determined by LCLIntf.GetTextMetrics
      InsertRow('Height (H)', IntToStr(tm.tmHeight), true);
      InsertRow('Ascender (A)', IntToStr(tm.tmAscent), true);
      InsertRow('Descender (D)', IntToStr(tm.tmDescent), true);
      InsertRow('Internal leading (IL)', IntToStr(tm.tmInternalLeading), true);
      InsertRow('External leading (EL)', IntToStr(tm.tmExternalLeading), true);
      InsertRow('Char height (CH = A - IL)', IntToStr(tm.tmAscent - tm.tmInternalLeading), true);
      InsertRow('Average char width', IntToStr(tm.tmAveCharWidth), true);
      InsertRow('Max char width', IntToStr(tm.tmMaxCharWidth), true);
      InsertRow('Weight', IntToStr(tm.tmWeight), true);
      InsertRow('Overhang', IntToStr(tm.tmOverhang), true);
      InsertRow('Digitized Aspect X', IntToStr(tm.tmDigitizedAspectX), true);
      InsertRow('Digitized Aspect Y', IntToStr(tm.tmDigitizedAspectY), true);
      InsertRow('First char', UTF8Encode(''+tm.tmFirstChar), true);
      InsertRow('Last char', UTF8Encode(''+tm.tmLastChar), true);
      InsertRow('Default char', UTF8Encode(''+tm.tmDefaultChar), true);
      InsertRow('Break char', UTF8Encode(''+tm.tmBreakChar), true);
      InsertRow('Italic', IntToStr(tm.tmItalic), true);
      InsertRow('Underlined', IntToStr(tm.tmUnderlined), true);
      InsertRow('Struck out', IntToStr(tm.tmStruckOut), true);
      InsertRow('Pitch and Family', IntToStr(tm.tmPitchAndFamily), true);
      InsertRow('CharSet', IntToStr(tm.tmCharSet), true);
    end;
  end;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  x, y, yt, h: Integer;
  tm: TTextMetric;
begin
  x := 0;
  y := 0;
  with PaintBox do
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Brush.Color := clWindow; //GetHighlightColor(ColorToRgb(clSilver), 40);
    Canvas.FillRect(0, 0, Width, Height);
    
    Canvas.Font.Assign(Font);
    GetTextMetrics(Canvas.Handle, tm);
    
    y := (Height - tm.tmHeight) div 2;
//    Canvas.Brush.Color := clWhite;
//    Canvas.FillRect(10, y, Width, y+tm.tmHeight);
    
    Canvas.TextOut(x, y, FSampleText);

    Canvas.Pen.Color := clMedGray;
    Canvas.Line(0, y, Width, y);
    Canvas.Line(0, y + tm.tmHeight, Width, y + tm.tmHeight);
    Canvas.Pen.Color := clMedGray;
    Canvas.Line(0, y + tm.tmAscent, Width, y + tm.tmAscent);
    Canvas.Line(0, y + tm.tmInternalLeading, Width, y + tm.tmInternalLeading);
        
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := 'DejaVu Sans';
    Canvas.Font.Size := 9;
    h := Canvas.TextHeight('Tg');
    
    x := Width-10;
    Canvas.Pen.Color := clBlack;
    Canvas.Line(x, y, x, y+tm.tmHeight);
    Canvas.Font.Color := Canvas.Pen.Color;
    yt := Max(0, y + (tm.tmHeight - h) div 2);
    Canvas.TextOut(x - Canvas.TextWidth('H')-4, yt, 'H');
    
    dec(x, 30);
    Canvas.Pen.Color := clRed;
    Canvas.Line(x, y, x, y+tm.tmAscent);
    Canvas.Font.Color := clRed;
    yt := Max(0, y + (tm.tmAscent - h) div 2);
    Canvas.TextOut(x - Canvas.TextWidth('A')-4, yt, 'A');
    
    dec(x, 30);
    Canvas.Pen.Color := clBlue;
    Canvas.Line(x, y+tm.tmAscent, x, y+tm.tmHeight);
    Canvas.Font.Color := clBlue;
    yt := Max( 0, y + tm.tmAscent + (tm.tmDescent - h) div 2);
    Canvas.TextOut(x - Canvas.TextWidth('D')-4, yt, 'D');
    
    Canvas.Pen.Color := clGreen;
    Canvas.Line(x, y, x, y+tm.tmInternalLeading);
    Canvas.Font.Color := clGreen;
    yt := Max(0, y + (tm.tmInternalLeading - h) div 2);
    Canvas.TextOut(x - Canvas.TextWidth('IL')-4, yt, 'IL');

    dec(x, 30);
    Canvas.Pen.Color := clFuchsia;
    Canvas.Line(x, y + tm.tmInternalLeading, x, y + tm.tmAscent);
    Canvas.Font.Color := clFuchsia;
    yt := Max(0, y + tm.tmInternalLeading + (tm.tmAscent - tm.tmInternalLeading - h) div 2);
    Canvas.TextOut(x - Canvas.TextWidth('CH')-4, yt, 'CH');
  end;
  
  if FUpdateNeeded then
  begin
    ListFont(GetFontData(Paintbox.Font.Handle), tm);
    FUpdateNeeded := false;
  end;
end;

procedure TMainForm.TabControlChange(Sender: TObject);
begin
  FUpdateNeeded := true;
  Paintbox.Invalidate;
end;

procedure TMainForm.ValueListEditor1PrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if ValueListEditor1.Cells[1, ARow] = '' then
    ValueListEditor1.Canvas.Font.Style := [fsBold];
  if (ACol = 1) then 
  begin
    if pos('Ascender (A)', ValueListEditor1.Cells[0, ARow]) <> 0 then
      ValueListEditor1.Canvas.Font.Color := clRed;
    if pos('Descender (D)', ValueListEditor1.Cells[0, ARow]) <> 0 then
      ValueListEditor1.Canvas.Font.Color := clBlue;
    if pos('Internal leading (IL)', ValueListEditor1.Cells[0, ARow]) <> 0 then
      ValueListEditor1.Canvas.Font.Color := clGreen;
    if pos('Glyph', ValueListEditor1.Cells[0, ARow]) <> 0 then
      ValueListEditor1.Canvas.Font.Color := clFuchsia;
  end;
end;


end.

