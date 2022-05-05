module TestLineChangesViewer where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import LineChangesViewer
import Text


unitTests = testGroup "TestLineChangesViewer" testCases

testCases =
    [ testNoTextForNoLineChangesEmpty
    ]

testNoTextForNoLineChangesEmpty =
    testCase "No text is shown for no line changes."
    $ viewLineChanges [[]]
    @?= []
