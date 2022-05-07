module Data where

import LineChangeData
import FileChangeData
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

oldFileName = "Old.txt"
newFileName = "New.txt"

getTextForFileChange :: String -> (Int, Int) -> Text
getTextForFileChange typeOfFileChange (numberOfAddedLines, numberOfDeletedLines) =
  color cyan "@ "  ++
  color blue newFileName ++
  newline ++
  view "(" ++
  view typeOfFileChange ++
  view ": " ++
  view oldFileName ++
  view ")" ++
  newline ++
  view "[" ++
  color green ("+" ++ show numberOfAddedLines) ++
  view " | " ++
  color red ("-" ++ show numberOfDeletedLines) ++
  view "]" ++
  newline

addedFile = FileChange AddedFile (oldFileName, newFileName) [[addedLine]]

textForAddedFile =
  getTextForFileChange "added" (1, 0) ++
  textForAddedLine

deletedFile = FileChange DeletedFile (oldFileName, newFileName) [[deletedLine]]

textForDeletedFile =
  getTextForFileChange "deleted" (0, 1) ++
  textForDeletedLine

renamedFile = FileChange RenamedFile (oldFileName, newFileName) [[unchangedLine]]

textForRenamedFile =
  getTextForFileChange "renamed" (0, 0) ++
  textForUnchangedLine

editedFile = FileChange EditedFile (oldFileName, newFileName) [sectionOfLineChanges]

textForEditedFile =
  getTextForFileChange "edited" (1, 1) ++
  textForSectionOfLineChanges

multipleChangedFiles = [addedFile, deletedFile, renamedFile, editedFile]

textForMultipleChangedFiles =
  textForAddedFile ++
  newline ++
  textForDeletedFile ++
  newline ++
  textForRenamedFile ++
  newline ++
  textForEditedFile
