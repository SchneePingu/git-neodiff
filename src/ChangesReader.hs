module ChangesReader
( readChanges
) where


import LineData
import LineReader
import FileChangeData
import FileChangesReader


readChanges :: IO [FileChange]
readChanges = do
    allLines <- readLines
    return $ readFileChanges $ discardHeader allLines


discardHeader :: [Line] -> [Line]
discardHeader = dropWhile (not . isBeginningOfFileChanges)
