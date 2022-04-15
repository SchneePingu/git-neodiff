module ChangesViewer
( viewChanges
) where


import Data.List (intersperse)
import Text
import FileChangeData
import FileChangesViewer
import ChangesData


viewChanges :: Changes -> IO ()
viewChanges (Changes header fileChanges) = do
    let separator =
          if   header == []
          then view ""
          else newline
        changes =
          viewHeader header ++
          separator ++
          viewFileChanges fileChanges
    printText changes


viewHeader :: Header -> Text
viewHeader = concat . intersperse newline . map view
