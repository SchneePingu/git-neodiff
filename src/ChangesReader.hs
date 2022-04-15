module ChangesReader
( readChanges
) where


import LineData
import LineReader
import FileChangeData
import FileChangesReader
import ChangesData


readChanges :: IO Changes
readChanges = do
    allLines <- readLines
    let (header, body) = break isBeginningOfFileChanges allLines
        fileChanges = readFileChanges body
    return $ Changes header fileChanges
