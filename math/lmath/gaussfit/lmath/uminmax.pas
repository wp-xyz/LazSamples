{ ******************************************************************
  Minimum, maximum, sign and exchange
  ****************************************************************** }

unit uminmax;

interface

uses
  utypes;

{ Minimum of 2 reals }
function Min(X, Y : Float) : Float; overload;

{ Maximum of 2 reals }
function Max(X, Y : Float) : Float; overload;

 { Minimum of 2 integers }
function Min(X, Y : Integer) : Integer; overload;

{ Maximum of 2 integers }
function Max(X, Y : Integer) : Integer; overload;

{ Sign (returns 1 if X = 0) }
function Sgn(X : Float) : Integer; overload;

{ Sign (returns 0 if X = 0) }
function Sgn0(X : Float) : Integer;

{ Sgn(B) * |A| }
function DSgn(A, B : Float) : Float;

// compatibility with Math unit
function Sign(X: Float):integer; inline;

{ Exchange 2 reals }
procedure Swap(var X, Y : Float);   overload;

{ Exchange 2 integers }
procedure Swap(var X, Y : Integer); overload;

implementation

  function Min(X, Y : Float) : Float;
  begin
    if X <= Y then Min := X else Min := Y;
  end;

  function Max(X, Y : Float) : Float;
  begin
    if X >= Y then Max := X else Max := Y;
  end;

  function Min(X, Y : Integer) : Integer;
  begin
    if X <= Y then Min := X else Min := Y;
  end;

  function Max(X, Y : Integer) : Integer;
  begin
    if X >= Y then Max := X else Max := Y;
  end;

  function Sgn(X : Float) : Integer;
  begin
    if X >= 0.0 then Sgn := 1 else Sgn := - 1;
  end;

  function Sgn0(X : Float) : Integer;
  begin
    if X > 0.0 then
      Sgn0 := 1
    else if IsZero(X) then
      Sgn0 := 0
    else
      Sgn0 := - 1;
  end;

  function DSgn(A, B : Float) : Float;
  begin
    if B < 0.0 then DSgn := - Abs(A) else DSgn := Abs(A)
  end;

  function Sign(X: Float): integer;
  begin
    Result := Sgn0(X);
  end;

  procedure Swap(var X, Y : Float);
  var
    Temp : Float;
  begin
    Temp := X;
    X := Y;
    Y := Temp;
  end;

  procedure Swap(var X, Y : Integer);
  var
    Temp : Integer;
  begin
    Temp := X;
    X := Y;
    Y := Temp;
  end;

end.
