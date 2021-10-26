:: ----------------------------------------------------------------------------
:: Batch file to create TAChart chm help files
::
:: Adapt path to fpdoc.exe to your fpc configuration.
:: ----------------------------------------------------------------------------

set fpdoc=C:\Lazarus\fpc-3.2.2\bin\i386-win32\fpdoc.exe
%fpdoc% --project=tachart-help.hpr
