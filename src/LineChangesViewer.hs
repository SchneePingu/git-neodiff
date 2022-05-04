module LineChangesViewer
( viewLineChanges
) where


import Data.List (intercalate, unzip)
import Data.Bifunctor (bimap)
import Text
import LineChangeData


type ColumnWidths = (ColumnWidth, ColumnWidth)
type ColumnWidth = Int


viewLineChanges :: [[LineChange]] -> Text
viewLineChanges lineChanges =
  let columnWidths = getColumnWidths lineChanges
  in  intercalate newline $
      map (concatMap (viewLineChange columnWidths)) lineChanges


getColumnWidths :: [[LineChange]] -> ColumnWidths
getColumnWidths lineChanges =
  bimap maximum maximum $
  unzip $
  map getColumnWidthsForSingleLine $
  concat lineChanges


getColumnWidthsForSingleLine :: LineChange -> ColumnWidths
getColumnWidthsForSingleLine (LineChange typeOfLineChange (oldLine, newLine) _) =
  let oldLineNumberWidth = getWidth oldLine
      newLineNumberWidth = getWidth newLine
  in  case typeOfLineChange of
    AddedLine -> (0, newLineNumberWidth)
    DeletedLine -> (oldLineNumberWidth, 0)
    UnchangedLine -> (oldLineNumberWidth, newLineNumberWidth)
  where
    getWidth = length . show


viewLineChange :: ColumnWidths -> LineChange -> Text
viewLineChange columnWidths (LineChange typeOfLineChange lineNumbers content) =
  viewTypeOfLineChange typeOfLineChange ++
  space ++
  viewLineNumbers typeOfLineChange columnWidths lineNumbers ++
  space ++
  viewContent typeOfLineChange content ++
  newline


viewTypeOfLineChange :: TypeOfLineChange -> Text
viewTypeOfLineChange AddedLine = color green "+"
viewTypeOfLineChange DeletedLine = color red "-"
viewTypeOfLineChange UnchangedLine = view " "


viewLineNumbers :: TypeOfLineChange -> ColumnWidths -> LineNumbers -> Text
viewLineNumbers typeOfLineChange widths@(leftWidth, rightWidth) (oldNumber, newNumber) =
  let leftLineNumber = showLeftLineNumber typeOfLineChange leftWidth oldNumber
      rightLineNumber = showRightLineNumber typeOfLineChange rightWidth newNumber
      columnSeparator = getColumnSeparator widths
  in  view leftLineNumber ++
      view columnSeparator ++
      color yellow rightLineNumber


showLeftLineNumber :: TypeOfLineChange -> ColumnWidth -> LineNumber -> String
showLeftLineNumber AddedLine width _ = showLineNumberPlaceholder width
showLeftLineNumber _ width lineNumber = showLineNumber width lineNumber


showRightLineNumber :: TypeOfLineChange -> ColumnWidth -> LineNumber -> String
showRightLineNumber DeletedLine width _ = showLineNumberPlaceholder width
showRightLineNumber _ width lineNumber = showLineNumber width lineNumber


showLineNumberPlaceholder :: ColumnWidth -> String
showLineNumberPlaceholder 0 = ""
showLineNumberPlaceholder columnWidth = replicate (columnWidth + 1) ' '


showLineNumber :: ColumnWidth -> LineNumber -> String
showLineNumber 0 _ = ""
showLineNumber columnWidth lineNumber =
  let lineNumberAsString = show lineNumber
      lineNumberWidth = length lineNumberAsString
      numberOfSpaces = max 0 (columnWidth - lineNumberWidth)
      leadingSpaces = replicate numberOfSpaces ' '
  in  take columnWidth ( leadingSpaces ++ lineNumberAsString) ++ ":"


getColumnSeparator :: ColumnWidths -> String
getColumnSeparator (leftColumnWidth, rightColumnWidth) =
  if   min leftColumnWidth rightColumnWidth == 0
  then ""
  else " "


viewContent :: TypeOfLineChange -> Content -> Text
viewContent AddedLine = color green 
viewContent DeletedLine = color red
viewContent UnchangedLine = view 
