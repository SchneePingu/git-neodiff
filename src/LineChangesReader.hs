module LineChangesReader
( readLineChanges
, isBeginningOfLineChanges
) where


import LineData
import LineChangeData
import RegexUtil
import Data.List (isPrefixOf)


readLineChanges :: [Line] -> [[LineChange]]
readLineChanges [] = []
readLineChanges (lineWithLineNumbers : followingLines) =
  let lineNumbers = readLineNumbers lineWithLineNumbers
      (blockOfLineChanges, followingBlocksOfLineChanges) =
        break isBeginningOfLineChanges followingLines
  in  readBlockOfLineChanges lineNumbers blockOfLineChanges
      : readLineChanges followingBlocksOfLineChanges


readBlockOfLineChanges :: LineNumbers -> [Line] -> [LineChange]
readBlockOfLineChanges _ [] = []
readBlockOfLineChanges lineNumbers (line : followingLines) =
  let (lineChange, updatedLineNumbers) = readSingleLineChange lineNumbers line
  in  lineChange : readBlockOfLineChanges updatedLineNumbers followingLines 


readSingleLineChange :: LineNumbers -> Line -> (LineChange, LineNumbers)
readSingleLineChange lineNumbers line =
  let typeOfLineChange = readTypeOfLineChange line
      content = readContent line
      updatedLineNumbers = updateLineNumbers typeOfLineChange lineNumbers
  in  (LineChange typeOfLineChange lineNumbers content, updatedLineNumbers)


readTypeOfLineChange :: Line -> TypeOfLineChange
readTypeOfLineChange line
  | "+" `isPrefixOf` line = AddedLine
  | "-" `isPrefixOf` line = DeletedLine
  | otherwise = UnchangedLine


readContent :: Line -> Content
readContent = tail


updateLineNumbers :: TypeOfLineChange -> LineNumbers -> LineNumbers
updateLineNumbers typeOfLineChange (oldLineNumber, newLineNumber) =
  case typeOfLineChange of
    AddedLine -> (oldLineNumber, newLineNumber + 1)
    DeletedLine -> (oldLineNumber + 1, newLineNumber)
    UnchangedLine -> (oldLineNumber + 1, newLineNumber + 1)


-- e. g. "@@ -1,2 +3,4 @@ ..."
regexForBeginningOfLineChanges = "^@@ [-]([0-9]+),[0-9]+ [+]([0-9]+),[0-9]+ @@"


isBeginningOfLineChanges :: Line -> Bool
isBeginningOfLineChanges line =
  line `matches` regexForBeginningOfLineChanges


readLineNumbers :: Line -> LineNumbers
readLineNumbers line =
  let [oldLineNumberAsString, newLineNumberAsString] =
        getMatchingGroups line regexForBeginningOfLineChanges
  in  (read oldLineNumberAsString, read newLineNumberAsString)
