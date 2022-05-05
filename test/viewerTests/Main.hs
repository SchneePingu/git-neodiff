import Test.Framework (defaultMain)

import qualified TestLineChangesViewer


main :: IO ()
main = defaultMain tests

tests =
    [ TestLineChangesViewer.unitTests
    ]
