module ChangesViewer
( viewChanges
) where


import Data.List (intercalate)
import Text
import FileChangeData
import FileChangesViewer
import ChangesData


viewChanges :: Changes -> IO ()
viewChanges (Changes header fileChanges) = do
    let separator =
          if   null header
          then view ""
          else newline
        changes =
          viewHeader header ++
          separator ++
          viewFileChanges fileChanges
    printText changes


viewHeader :: Header -> Text
viewHeader = intercalate newline . map view
