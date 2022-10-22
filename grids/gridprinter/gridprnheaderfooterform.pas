unit GridPrnHeaderFooterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  ComCtrls, ExtCtrls, Spin, GridPrn;

type

  { TGridPrintHeaderFooterForm }

  TGridPrintHeaderFooterForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    btnFont: TButton;
    ButtonPanel1: TButtonPanel;
    cbShow: TCheckBox;
    cbShowLine: TCheckBox;
    clbLineColor: TColorButton;
    edTextLeft: TEdit;
    edTextCenter: TEdit;
    edTextRight: TEdit;
    FontDialog: TFontDialog;
    lblTextInfo: TLabel;
    lblLineWidth: TLabel;
    seLineWidth: TFloatSpinEdit;
    TabControl: TTabControl;
    procedure btnFontClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure TabControlChanging(Sender: TObject; var AllowChange: Boolean);
  private
    type
      THFParams = record
        Visible: Boolean;
        LeftText: String;
        CenterText: String;
        RightText: String;
        Font: TFont;
        ShowLine: Boolean;
        LineWidth: Double;
        LineColor: TColor;
      end;
  private
    FGridPrinter: TGridPrinter;
    FParams: array[0..1] of THFParams;   // 0=Header, 1=Footer
    procedure SetGridPrinter(AValue: TGridPrinter);
  protected
    procedure ControlsToParams(AIndex: Integer);
    procedure ParamsToControls(AIndex: Integer);
    procedure ParamsToPrinter(AIndex: Integer);
    procedure PrinterToParams(AIndex: Integer);
  public
    property GridPrinter: TGridPrinter read FGridPrinter write SetGridPrinter;
  end;

var
  GridPrintHeaderFooterForm: TGridPrintHeaderFooterForm;

implementation

{$R *.lfm}

{ TGridPrintHeaderFooterForm }

procedure TGridPrintHeaderFooterForm.FormActivate(Sender: TObject);
var
  delta: Integer;
begin
  delta := TabControl.Height - TabControl.ClientHeight;
  Constraints.MinHeight := delta + clbLineColor.Top + clbLineColor.Height +
    clbLinecolor.BorderSpacing.Bottom + ButtonPanel1.Height +
    TabControl.BorderSpacing.Around * 2;
  Constraints.MinWidth := edTextLeft.Left + edTextLeft.Width + Bevel1.Width +
    edTextCenter.Width + Bevel2.Width + edTextRight.Width + edTextRight.BorderSpacing.Right +
    TabControl.BorderSpacing.Around * 2;
  Width := 0;
  Height := 0;
end;

procedure TGridPrintHeaderFooterForm.OKClick(Sender: TObject);
begin
  ControlsToParams(TabControl.TabIndex);
  ParamsToPrinter(0);
  ParamsToPrinter(1);
end;

procedure TGridPrintHeaderFooterForm.btnFontClick(Sender: TObject);
begin
  FontDialog.Execute;
end;

procedure TGridPrintHeaderFooterForm.FormCreate(Sender: TObject);
begin
  FParams[0].Font := TFont.Create;
  FParams[1].Font := TFont.Create;
end;

procedure TGridPrintHeaderFooterForm.FormDestroy(Sender: TObject);
begin
  FParams[0].Font.Free;
  FParams[1].Font.Free;
end;

procedure TGridPrintHeaderFooterForm.TabControlChange(Sender: TObject);
begin
  ParamsToControls(TabControl.TabIndex);
end;

procedure TGridPrintHeaderFooterForm.TabControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  ControlsToParams(TabControl.TabIndex);
end;

procedure TGridPrintHeaderFooterForm.ControlsToParams(AIndex: Integer);
begin
  FParams[AIndex].Visible := cbShow.Checked;
  FParams[AIndex].LeftText := edTextLeft.Text;
  FParams[AIndex].CenterText:= edTextCenter.Text;
  FParams[AIndex].RightText := edTextRight.Text;
  FParams[AIndex].Font.Assign(FontDialog.Font);

  FParams[AIndex].ShowLine := cbShowLine.Checked;
  FParams[AIndex].LineWidth := seLineWidth.Value;
  FParams[AIndex].LineColor := clbLinecolor.ButtonColor;
end;

procedure TGridPrintHeaderFooterForm.ParamsToControls(AIndex: Integer);
begin
  cbShow.Checked := FParams[AIndex].Visible;
  edTextLeft.Text := FParams[AIndex].LeftText;
  edTextCenter.Text := FParams[AIndex].CenterText;
  edTextRight.Text := FParams[AIndex].RightText;
  FontDialog.Font.Assign(FParams[AIndex].Font);

  cbShowLine.Checked := FParams[AIndex].ShowLine;
  seLineWidth.Value := FParams[AIndex].LineWidth;
  clbLinecolor.ButtonColor := FParams[AIndex].LineColor;
end;

procedure TGridPrintHeaderFooterForm.ParamsToPrinter(AIndex: Integer);
var
  HF: TGridPrnHeaderFooter;
begin
  case AIndex of
    0: HF := FGridPrinter.Header;
    1: HF := FGridPrinter.Footer;
  end;

  with FParams[AIndex] do
  begin
    HF.Visible := Visible;
    HF.Text := LeftText + HF.SectionSeparator + CenterText + HF.SectionSeparator + RightText;
    HF.Font.Assign(Font);
    HF.ShowLine := ShowLine;
    HF.Linecolor := LineColor;
    HF.LineWidth := LineWidth;
  end;
end;

procedure TGridPrintHeaderFooterForm.PrinterToParams(AIndex: Integer);
var
  HF: TGridPrnHeaderFooter;
begin
  case AIndex of
    0: HF := FGridPrinter.Header;
    1: HF := FGridPrinter.Footer;
  end;

  with FParams[AIndex] do
  begin
    Visible := HF.Visible;
    LeftText := HF.SectionText[hfsLeft];
    CenterText := HF.SectionText[hfsCenter];
    RightText := HF.SectionText[hfsRight];
    Font.Assign(HF.Font);
    ShowLine := HF.ShowLine;
    LineColor := HF.RealLineColor;
    LineWidth := HF.LineWidth;
  end;
end;

procedure TGridPrintHeaderFooterForm.SetGridPrinter(AValue: TGridPrinter);
begin
  if AValue = nil then
    raise Exception.Create('GridPrinter is nil.');

  if FGridPrinter <> AValue then
  begin
    FGridPrinter := AValue;
    PrinterToParams(0);
    PrinterToParams(1);
    ParamsToControls(TabControl.TabIndex);
  end;
end;

end.

