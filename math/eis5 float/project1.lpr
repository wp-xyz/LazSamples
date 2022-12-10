program project1;

uses
  SysUtils, StrUtils, eis5lib, Math;

procedure Test(a: Word; Expected: String);
const
  D = 10;
var
  x, y: Single;
  b: Word;
begin
  x := EIS5ToFloat(a);
  b := FloatToEIS5(x);
  y := EIS5ToFloat(b);
  Write(Format(
    '$%.4x = %%%s %s', [a, IntToBin(Hi(a), 8), IntToBin(Lo(a), 8)]),
    '  -->  ', x:D:2,
    '  -->  ', Format('$%.4x = %%%s %s', [b, IntToBin(Hi(b), 8), IntToBin(Lo(b), 8)]),
    '  -->  ', y:D:2);
  if Expected <> '' then
    Write('   expected: ', Expected);
  WriteLn;
end;


begin
  WriteLn('Positive values...');
  Test($3E90, '2150.40'); // https://knx-user-forum.de/forum/%C3%B6ffentlicher-bereich/knx-eib-forum/9663-%E2%88%9A-daten-telegramm
  Test($1901, '20.56');   // https://knx-professionals-forum.de/forum/archive/index.php/t-11821.html
  Test($18D2, '16.80');   // https://knx-professionals-forum.de/forum/archive/index.php/t-11821.html
  Test($0C29, '21.30');   // https://knx-professionals-forum.de/forum/archive/index.php/t-9968.html
  Test($191D, '');
  Test($1423, '');
  Test($1927, '23.60');   // https://crestroninfo.bboard.de/board/ftopic-65640542nx15896-260.html
  Test($190F, '21.68');   // https://knx-professionals-forum.de/forum/archive/index.php/t-9968.html
  Test(%0111111111111110, '670 433.28');  // Max value
  // ---------------------------------------------------------------------------
  WriteLn;
  WriteLn('Negative values...');
  Test($879C, '-1.0');    // https://crestroninfo.bboard.de/board/ftopic-65640542nx15896-260.html
  Test($9FF5, '-0.90');
  Test(%1111100000000000, '-671 088.64');  // Min value
  // ---------------------------------------------------------------------------
  WriteLn;
  WriteLn('Special values...');
  Test($7FFF, 'NaN');  // expected: NaN

  WriteLn;
  Write('Press ENTER to close...');
  ReadLn;
end.

