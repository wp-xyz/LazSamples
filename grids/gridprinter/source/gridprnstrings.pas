unit GridPrnStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  // Print Preview
  RSPrintPreview = 'Print Preview';
  RSPrint = 'Print';
  RSClose = 'Close';

  RSShowFirstPage = 'Show first page';
  RSShowPrevPage = 'Show previous page';
  RSShowNextPage = 'Show next page';
  RSShowLastPage = 'Show last page';
  RSZoomIn = 'Zoom in';
  RSZoomOut = 'Zoom out';
  RSZoomToFitPageWidth = 'Zoom to fit page width';
  RSZoomToFitPageHeight = 'Zoom to fit page height';
  RSOriginalSize = 'Original size (100%)';
  RSPageMarginsConfig = 'Page margins configuration';
  RSHeaderFooterConfig = 'Header/footer configuration';
  RSPortraitPageOrientation = 'Portrait page orientation';
  RSLandscapePageOrientation = 'Landscape page orientation';
  RSPrintColsFirst = 'First print columns from top to bottom,' + LineEnding +
    'then print from left to right';
  RSPrintRowsFirst = 'First print rows from left to right,' + LineEnding +
    'then print from top to bottom';

  RSLeftMargin = 'Left margin';
  RSTopMargin = 'Top margin';
  RSRightMargin = 'Right margin';
  RSBottomMargin = 'Bottom margin';
  RSHeaderMargin = 'Header margin';
  RSFooterMargin = 'Footer margin';

  RSPageAndZoomInfo = 'Page %d of %d, Zoom %d %%';

  // Header / footer
  RSHeader = 'Header';
  RSFooter = 'Footer';
  RSShow = 'Show';
  RSFont = 'Font';
  RSHeaderFooterSectionParameterInfo =
    'Each section can contain the following parameters:' + LineEnding +
    '  $DATE - Current date' + LineEnding +
    '  $TIME - Current time' + LineEnding +
    '  $PAGE - Page number' + LineEnding +
    '  $PAGECOUNT - Number of pages' + LineEnding +
    '  $FULL_FILENAME - Full name of the printed file' + LineEnding +
    '  $FILENAME - Name of the printed file, without path' + LineEnding +
    ' $PATH - Path of the printed file';
  RSShowDividingLine = 'Show dividing line';
  RSLineWidthMM = 'Line width (mm)';
  RSLineColor = 'Line color';
  RSTextInLeftAlignedSection = 'Text in left-aligned section';
  RSTextInCenteredSection = 'Text in centered section';
  RSTextInRightAlignedSection = 'Text in right-aligned section';

implementation

end.

