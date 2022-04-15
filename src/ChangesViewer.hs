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
    let changes =
          viewHeader header ++
          newline ++
          viewFileChanges fileChanges
    printText changes


viewHeader :: Header -> Text
viewHeader = concat . intersperse newline . map view
