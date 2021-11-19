unit main;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Types,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ValEdit,
  ExtCtrls, Grids;

type

  { TMainForm }

  TMainForm = class(TForm)
    cbFontName: TComboBox;
    cbFontSize: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    PaintBox: TPaintBox;
    TestPanel: TPanel;
    ValueListEditor1: TValueListEditor;
    procedure cbFontNameChange(Sender: TObject);
    procedure cbFontSizeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ValueListEditor1PrepareCanvas(sender: TObject; aCol,
      aRow: Integer; aState: TGridDrawState);
  private
    FUpdateNeeded: Boolean;
    procedure ListFont(AFontData: TFontData; ATextMetric: TTextMetric);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  typinfo;

{ TMainForm }

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

procedure TMainForm.FormActivate(Sender: TObject);
begin
  with ValueListEditor1 do
    ClientHeight := RowCount * DefaultRowHeight;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cbFontName.Items.Assign(Screen.Fonts);
  cbFontName.Items.Insert(0, 'default');
  cbFontName.ItemIndex := 0;
  ListFont(GetFontData(Font.Handle), Default(TTextMetric));
  FUpdateNeeded := true;
end;

procedure TMainForm.ListFont(AFontData: TFontData; ATextMetric: TTextMetric);
var
  h: Integer;
  fd: TFontData absolute AFontData;
  tm: TTextMetric;
begin
  tm := ATextMetric;
  with ValueListEditor1 do
  begin
    Clear;
    InsertRow('GetFontData', '', true);
    InsertRow('  Handle', IntToStr(fd.Handle), true);
    InsertRow('  Height (Size)', Format('%d (%d px, %d pt)', [fd.Height, -fd.Height, round(-fd.Height/Screen.PixelsPerInch*72)]), true);
    InsertRow('  Pitch', GetEnumName(TypeInfo(TFontPitch), Integer(fd.Pitch)), true);
    InsertRow('  Style', SetToString(PTypeInfo(TypeInfo(TFontStylesBase)), integer(fd.Style), true), true);
    InsertRow('  CharSet', IntToStr(fd.CharSet), true);
    InsertRow('  Quality', GetEnumName(TypeInfo(TFontQuality), Integer(fd.Quality)), true);
    InsertRow('  Name', fd.Name, true);
    InsertRow('  Orientation', IntToStr(fd.Orientation), true);
    InsertRow(' ', '', true);
    InsertRow('GetTextMetrics', '', true);
    InsertRow('  Height (H)', IntToStr(tm.tmHeight), true);
    InsertRow('  Ascender (A)', IntToStr(tm.tmAscent), true);
    InsertRow('  Descender (D)', IntToStr(tm.tmDescent), true);
    InsertRow('  Internal leading (IL)', IntToStr(tm.tmInternalLeading), true);
    InsertRow('  External leading (EL)', IntToStr(tm.tmExternalLeading), true);
    InsertRow('  Glyph "height" (GH)', IntToStr(tm.tmAscent - tm.tmInternalLeading), true);
  end;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
const
  TXT = 'AghjyÄŠŢ';
var
  x, y, yt, h: Integer;
  tm: TTextMetric;
begin
  x := 0;
  y := 0;
  with PaintBox do
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Brush.Color := clDefault;
    Canvas.Pen.Color := clSilver;
    Canvas.FillRect(0, 0, Width, Height);
    
    Canvas.Font.Assign(Font);
    GetTextMetrics(Canvas.Handle, tm);
    
    y := (Height - tm.tmHeight) div 2;
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(0, y, Width, y+tm.tmHeight);
    
    Canvas.TextOut(x, y, TXT);

    Canvas.Line(0, y + tm.tmAscent, Width, y + tm.tmAscent);
    Canvas.Line(0, y + tm.tmInternalLeading, Width, y + tm.tmInternalLeading);
    Canvas.Line(0, y + tm.tmHeight, Width, y + tm.tmHeight);
        
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := 'DejaVu Sans';
    Canvas.Font.Size := 9;
    h := Canvas.TextHeight('Tg');
    
    x := Width-10;
    Canvas.Pen.Color := clBlack;
    Canvas.Line(x, y, x, y+tm.tmHeight);
    Canvas.Font.Color := Canvas.Pen.Color;
    yt := y + (tm.tmHeight - h) div 2;
    if yt < 0 then yt := 0;
    Canvas.TextOut(x - Canvas.TextWidth('H')-4, yt, 'H');
    
    dec(x, 30);
    Canvas.Pen.Color := clRed;
    Canvas.Line(x, y, x, y+tm.tmAscent);
    Canvas.Font.Color := clRed;
    yt := y + (tm.tmAscent - h) div 2;
    if yt < 0 then yt := 0;
    Canvas.TextOut(x - Canvas.TextWidth('A')-4, yt, 'A');
    
    dec(x, 30);
    Canvas.Pen.Color := clBlue;
    Canvas.Line(x, y+tm.tmAscent, x, y+tm.tmHeight);
    Canvas.Font.Color := clBlue;
    yt := y + tm.tmAscent + (tm.tmDescent - h) div 2;
    if yt < 0 then yt := 0;
    Canvas.TextOut(x - Canvas.TextWidth('D')-4, yt, 'D');
    
    Canvas.Pen.Color := clGreen;
    Canvas.Line(x, y, x, y+tm.tmInternalLeading);
    Canvas.Font.Color := clGreen;
    yt := y + (tm.tmInternalLeading - h) div 2;
    if yt < 0 then yt := 0;
    Canvas.TextOut(x - Canvas.TextWidth('IL')-4, yt, 'IL');

    dec(x, 30);
    Canvas.Pen.Color := clFuchsia;
    Canvas.Line(x, y + tm.tmInternalLeading, x, y + tm.tmAscent);
    Canvas.Font.Color := clFuchsia;
    yt := y + tm.tmInternalLeading + (tm.tmAscent - tm.tmInternalLeading - h) div 2;
    if yt < 0 then yt := 0;
    Canvas.TextOut(x - Canvas.TextWidth('GH')-4, yt, 'GH');
  end;
  
  if FUpdateNeeded then
  begin
    ListFont(GetFontData(Paintbox.Font.Handle), tm);
    FUpdateNeeded := false;
  end;
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

