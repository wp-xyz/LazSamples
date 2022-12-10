{-------------------------------------------------------------------------------
                         EIS5 (DPT 9) conversion library
--------------------------------------------------------------------------------
  Reference: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjc74-m2e37AhUtXfEDHTFGAXIQFnoECD0QAQ&url=https%3A%2F%2Fwww.knx.org%2FwAssets%2Fdocs%2Fdownloads%2FCertification%2FInterworking-Datapoint-types%2F03_07_02-Datapoint-Types-v02.02.01-AS.pdf&usg=AOvVaw1V_kL8xwe95wedZWlBN4bN

  EIS5 format: 2-byte float
  Structure:     MSB       LSB
              SEEEEMMM  MMMMMMMM
              S = sign (0 = positive, 1 = negative)
              E = exponent  0..15
              M = mantissa -2048 ... 2047 (two's complement)

  Float value = (0.01*M)*2^E

  $7FFF = value for invalid data (NaN)

  Range: [-671 088.64 ... 670 433.28]
-------------------------------------------------------------------------------}

unit eis5lib;

{$mode ObjFPC}{$H+}

interface

type
  TEIS5 = type word;

const
  EIS5_NaN: TEIS5 = $7FFF;

function EIS5ToFloat(AValue: TEIS5): Single;
function FloatToEIS5(AValue: Single): TEIS5;

implementation

uses
  Math;

const
  SIGN_MASK     = Word(%1000000000000000);
  EXPONENT_MASK = Word(%0111100000000000);
  MANTISSA_MASK = Word(%0000011111111111);

function EIS5ToFloat(AValue: TEIS5): Single;
var
  isNeg: Boolean;
  exponent: Integer;
  Mantissa: Integer;
begin
  if AValue = EIS5_NaN then
  begin
    Result := NaN;
    exit;
  end;

  isNeg    := (AValue and SIGN_MASK) <> 0;
  exponent := (AValue and EXPONENT_MASK) shr 11;
  mantissa := (AValue and MANTISSA_MASK);
  if isNeg then
    mantissa := - (MANTISSA_MASK - mantissa + 1);
  Result := 0.01 * (mantissa shl exponent);
end;

function FloatToEIS5(AValue: Single): TEIS5;
var
  sign: Word;
  mantissa: Integer;
  exponent: Integer;
begin
  if IsNaN(AValue) then
  begin
    Result := EIS5_NaN;
    exit;
  end;

  if AValue < 0 then
    sign := SIGN_MASK
  else
    sign := 0;

  mantissa := round(abs(AValue) * 100.0);
  exponent := 0;
  while (mantissa > 2047) do
  begin
    mantissa := mantissa shr 1;
    inc(exponent);
  end;

  if exponent > 15 then
  begin
    Result := EIS5_NaN;
    exit;
  end;

  if AValue < 0 then
    mantissa := (MANTISSA_MASK - mantissa) + 1;

  Result := sign or (exponent shl 11) or (mantissa and MANTISSA_MASK);
end;

end.

