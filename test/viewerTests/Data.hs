module Data where

import LineChangeData
import Text

addedLine = LineChange AddedLine (0, 1) "added"

textForAddedLine =
  color green "+" ++
  space ++
  view "" ++
  view "" ++
  color yellow "1:" ++
  space ++
  color green "added" ++
  newline

deletedLine = LineChange DeletedLine (2, 0) "deleted"

textForDeletedLine =
  color red "-" ++
  space ++
  view "2:" ++
  view "" ++
  color yellow "" ++
  space ++
  color red "deleted" ++
  newline

unchangedLine = LineChange UnchangedLine (3, 4) "unchanged"

textForUnchangedLine =
  view " " ++
  space ++
  view "3:" ++
  view " " ++
  color yellow "4:" ++
  space ++
  view "unchanged" ++
  newline

sectionOfLineChanges = [addedLine, deletedLine, unchangedLine]

textForSectionOfLineChanges =
  color green "+" ++
  space ++
  view "  " ++
  view " " ++
  color yellow "1:" ++
  space ++
  color green "added" ++
  newline ++
  color red "-" ++
  space ++
  view "2:" ++
  view " " ++
  color yellow "  " ++
  space ++
  color red "deleted" ++
  newline ++
  view " " ++
  space ++
  view "3:" ++
  view " " ++
  color yellow "4:" ++
  space ++
  view "unchanged" ++
  newline


unchangedLineWithLineNumbers :: LineNumbers -> LineChange
unchangedLineWithLineNumbers lineNumbers =
  LineChange UnchangedLine lineNumbers "unchanged"
