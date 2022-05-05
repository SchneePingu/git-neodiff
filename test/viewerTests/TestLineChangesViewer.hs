module TestLineChangesViewer where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import LineChangesViewer
import LineChangeData
import Text


unitTests = testGroup "TestLineChangesViewer" testCases

testCases =
    [ testNoTextForNoLineChangesEmpty
    , testTextForAddedLine
    , testTextForDeletedLine
    , testTextForUnchangedLine
    ]

testNoTextForNoLineChangesEmpty =
    testCase "No text is shown for no line changes."
    $ viewLineChanges [[]]
    @?= []


testTextForAddedLine =
    testCase "Text is shown for added line."
    $ viewLineChanges [[LineChange AddedLine (0, 1) "added"]]
    @?= expectedText
    where
      expectedText =
        color green "+" ++
        space ++
        view "" ++
        view "" ++
        color yellow "1:" ++
        space ++
        color green "added" ++
        newline

testTextForDeletedLine =
    testCase "Text is shown for deleted line."
    $ viewLineChanges [[LineChange DeletedLine (1, 0) "deleted"]]
    @?= expectedText
    where
      expectedText =
        color red "-" ++
        space ++
        view "1:" ++
        view "" ++
        color yellow "" ++
        space ++
        color red "deleted" ++
        newline


testTextForUnchangedLine =
    testCase "Text is shown for unchanged line."
    $ viewLineChanges [[LineChange UnchangedLine (1, 2) "unchanged"]]
    @?= expectedText
    where
      expectedText =
        view " " ++
        space ++
        view "1:" ++
        view " " ++
        color yellow "2:" ++
        space ++
        view "unchanged" ++
        newline
