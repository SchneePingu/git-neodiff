module FileChangesViewer
( viewFileChange
) where


import LineChangeData
import FileChangeData
import LineChangesViewer
import Text


viewFileChange :: FileChange -> Text
viewFileChange (FileChange typeOfFileChange (oldFileName, newFileName) lineChanges) =
  color cyan "@ " ++
  color blue newFileName ++
  newline ++
  view "(" ++
  viewTypeOfFileChange typeOfFileChange ++
  view ": " ++
  view oldFileName ++
  view ")" ++
  newline ++
  viewStatistics lineChanges ++
  newline ++
  viewLineChanges lineChanges


viewTypeOfFileChange :: TypeOfFileChange -> Text
viewTypeOfFileChange = view . showTypeOfFileChange


showTypeOfFileChange :: TypeOfFileChange -> String
showTypeOfFileChange AddedFile = "added"
showTypeOfFileChange DeletedFile = "deleted"
showTypeOfFileChange RenamedFile = "renamed"
showTypeOfFileChange EditedFile = "edited"


viewStatistics :: [[LineChange]] -> Text
viewStatistics lineChanges =
  let allChanges = concat lineChanges
      numberOfAdditions = getNumberOfChangesOfType AddedLine allChanges
      numberOfDeletions = getNumberOfChangesOfType DeletedLine allChanges
  in  view "[" ++
      viewNumberOfChangesOfType AddedLine numberOfAdditions ++
      view " | " ++
      viewNumberOfChangesOfType DeletedLine numberOfDeletions ++
      view "]"


getNumberOfChangesOfType :: TypeOfLineChange -> [LineChange] -> Int
getNumberOfChangesOfType typeOfChange lineChanges =
  length $ filter (hasTypeOfLineChange typeOfChange) lineChanges


hasTypeOfLineChange :: TypeOfLineChange -> LineChange -> Bool
hasTypeOfLineChange typeOfChange = (typeOfChange ==) . typeOfLineChange 


viewNumberOfChangesOfType :: TypeOfLineChange -> Int -> Text
viewNumberOfChangesOfType typeOfLineChange numberOfChanges =
  let numberOfChangesString = show numberOfChanges
  in  case typeOfLineChange of
    AddedLine -> color green $ "+" ++ numberOfChangesString
    DeletedLine -> color red $ "-" ++ numberOfChangesString
    UnchangedLine -> view numberOfChangesString

