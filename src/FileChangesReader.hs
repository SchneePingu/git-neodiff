module FileChangesReader
( readFileChanges
, isBeginningOfFileChanges
) where


import LineData
import FileChangeData
import LineChangesReader
import RegexUtil
import Data.List (isPrefixOf)


readFileChanges :: [Line] -> [FileChange]
readFileChanges [] = []
readFileChanges (lineWithFileNames : followingLines) =
  let fileChange = FileChange typeOfFileChange fileNames lineChanges
      typeOfFileChange = readTypeOfFileChange linesWithTypeOfFileChange
      fileNames = readFileNames lineWithFileNames
      lineChanges = readLineChanges linesWithLineChanges
  in  fileChange : readFileChanges followingBlocksWithFileChanges
  where
    (blockWithFileChange, followingBlocksWithFileChanges) =
      break isBeginningOfFileChanges followingLines
    (linesWithTypeOfFileChange, linesWithLineChanges) =
      break isBeginningOfLineChanges blockWithFileChange
    


readTypeOfFileChange :: [Line] -> TypeOfFileChange
readTypeOfFileChange [] = EditedFile
readTypeOfFileChange (line : otherLines)
  | "new " `isPrefixOf` line = AddedFile
  | "deleted " `isPrefixOf` line = DeletedFile
  | "rename " `isPrefixOf` line = RenamedFile
  | otherwise = readTypeOfFileChange otherLines


-- e. g. "diff --git a/old.log b/new.log"
regexForBeginningOfFileChanges = "^diff --git a/(.+) b/(.+)"


isBeginningOfFileChanges :: Line -> Bool
isBeginningOfFileChanges line =
  line `matches` regexForBeginningOfFileChanges


readFileNames :: Line -> FileNames
readFileNames line =
  let (oldFileName : newFileName : []) =
        getMatchingGroups line regexForBeginningOfFileChanges
  in  (oldFileName, newFileName)
