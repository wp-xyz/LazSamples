unit uErrors;
{ ------------------------------------------------------------------
  Error handling
   ------------------------------------------------------------------ }
{$mode objfpc}{$H+}

interface
uses uTypes;
const
  MathOK = 0; {< No error }
  //  Error codes for mathematical functions
  FOk        = 0;  {< No error }
  FDomain    = 1;  {< Argument domain error }
  FSing      = 2;  {< Function singularity }
  FOverflow  = 3;  {< Overflow range error }
  FUnderflow = 4;  {< Underflow range error }
  FTLoss     = 5;  {< Total loss of precision }
  FPLoss     = 6;  {< Partial loss of precision }
//  Error codes for matrix computations
  MatOk      = 0;  {< No error }
  MatNonConv = 7;  {< Non-convergence }
  MatSing    = 8;  {< Quasi-singular matrix }
  MatErrDim  = 9;  {< Non-compatible dimensions }
  MatNotPD   = 10; {< Matrix not positive definite }
//  Error codes for optimization and nonlinear equations
  OptOk        = 0;  {< No error }
  OptNonConv   = 11;  {< Non-convergence }
  OptSing      = 12;  {< Quasi-singular hessian matrix }
  OptBigLambda = 13;  {< Too high Marquardt parameter }
//  Error codes for nonlinear regression
  NLMaxPar  = 14;  {< Max. number of parameters exceeded }
  NLNullPar = 15;  {< Initial parameter equal to zero }

  ErrorMessage : array[0..15] of String =
    ('No error',
     'Argument domain error',
     'Function singularity',
     'Overflow range error',
     'Underflow range error',
     'Total loss of precision',
     'Partial loss of precision',
     'Non-convergence',
     'Quasi-singular matrix',
     'Non-compatible dimensions',
     'Matrix not positive definite',
     'Non-convergence',
     'Quasi-singular hessian matrix',
     'Too high Marquardt parameter',
     'Max. number of parameters exceeded',
     'Initial parameter equal to zero'
    );
  { Sets the error code. Optional argument ErrAddr allows to set user-supplied variable to store it.
    This *must* be used in multi-threaded application. The same address must be used in MathErr}
procedure SetErrCode(ErrCode : Integer; EMessage:string = '');

{ Sets error code and default function value }
function DefaultVal(ErrCode : Integer; DefVal : Float; EMessage:string = '') : Float;

{ Returns the error code. If user supplied ErrAddr for SetErrCode, it must be used here as well }
function MathErr : Integer;

// returns error message
function MathErrMessage:string;

implementation
var
  gErrCode  : Integer = 0;
  gEMessage  : string = '';

procedure SetErrCode(ErrCode : Integer; EMessage:string = '');
begin
   gErrCode := ErrCode;
   if (EMessage = '') and (ErrCode in [0..15]) then
     gEMessage := ErrorMessage[ErrCode]
   else
     gEMessage := EMessage;
end;

function DefaultVal(ErrCode : Integer; DefVal : Float; EMessage:string = '') : Float;
begin
  SetErrCode(ErrCode,EMessage);
  DefaultVal := DefVal;
end;

function MathErr : Integer;
begin
    MathErr := gErrCode
end;

function MathErrMessage:string;
begin
  Result := gEMessage;
end;

end.

