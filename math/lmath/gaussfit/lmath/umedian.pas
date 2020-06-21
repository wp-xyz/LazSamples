{ ******************************************************************
  Median
  ****************************************************************** }

unit umedian;

interface

uses
  utypes;

{Returns median for vector X }
function Median(X : TVector; Lb, Ub : Integer) : Float;

implementation

function Median(X: TVector; Lb, Ub: integer): float;
var
   I,J,M:integer;
   A,Buf:float;
begin
   M := (Ub - Lb) div 2 + 1;
   while Lb < Ub - 1 do
   begin
     A := X[M];
     I := Lb; J := Ub;
     repeat
       while X[I] < A do inc(I);
       while X[J] > A do dec(J);
       if I <= J then
       begin
         Buf := X[I];
         X[I] := X[J];
         X[J] := Buf;
         inc(I); dec(J);
       end;
     until I > J;
     if J < M then Lb := I;
     if I > M then Ub := J;
   end;
   Result := X[M];
end;

end.

