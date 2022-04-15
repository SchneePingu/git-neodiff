module ChangesViewer
( viewChanges
) where


import Data.List (intersperse)
import Text
import FileChangeData
import FileChangesViewer


viewChanges :: [FileChange] -> IO ()
viewChanges fileChanges =
    printText $
    concat $ 
    intersperse newline $
    map viewFileChange fileChanges
