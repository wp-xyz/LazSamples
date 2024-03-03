program utf8_console;
{$mode objfpc}{$H+}
{$R *.res}   // Do not remove. Check "Use manifest resouce" and "ANSI codepage is UTF8" in project options
uses
  windows;
var
  s1, s2, s3: String;
begin
  SetConsoleOutputCP(CP_utf8);          // important
  SetTextCodepage(Output, cp_utf8);     // important

  s1 := '春夏';
  s2 :='秋冬';
  s3 := s1 + s2;
  WriteLn(s1, ' ', s2, ' ' , s3);

  Readln;
end.

