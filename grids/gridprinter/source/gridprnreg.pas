unit GridPrnReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation

uses
  GridPrn;

{$R gridprinter_icons.res}

procedure Register;
begin
  RegisterComponents('Misc', [TGridPrinter]);
end;

end.

