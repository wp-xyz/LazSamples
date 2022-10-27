dir /b *.png > imagelist.txt
lazres ..\source\gridprinter_icons.res @imagelist.txt
del /q imagelist.txt
