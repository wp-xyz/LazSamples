{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GridPrinterPkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  GridPrnHeaderFooterForm, GridPrn, GridPrnPreviewForm, GridPrnReg, 
  GridPrnPreviewDlg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GridPrnReg', @GridPrnReg.Register);
end;

initialization
  RegisterPackage('GridPrinterPkg', @Register);
end.
