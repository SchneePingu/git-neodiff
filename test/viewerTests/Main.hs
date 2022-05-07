import Test.Framework (defaultMain)

import qualified TestLineChangesViewer
import qualified TestFileChangesViewer


main :: IO ()
main = defaultMain tests

tests =
    [ TestLineChangesViewer.unitTests
    , TestFileChangesViewer.unitTests
    ]
