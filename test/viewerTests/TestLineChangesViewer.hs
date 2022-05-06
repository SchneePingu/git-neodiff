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
    , testTextForSectionOfLineChanges
    , testTextForMultipleSectionsOfLineChanges
    , testTextForPaddingOfLineNumbers
    ]



testNoTextForNoLineChangesEmpty =
    testCase "No text is shown for no line changes."
    $ viewLineChanges [[]]
    @?= []


testTextForAddedLine =
    testCase "Text is shown for added line."
    $ viewLineChanges [[addedLine]]
    @?= textForAddedLine

testTextForDeletedLine =
    testCase "Text is shown for deleted line."
    $ viewLineChanges [[deletedLine]]
    @?= textForDeletedLine


testTextForUnchangedLine =
    testCase "Text is shown for unchanged line."
    $ viewLineChanges [[unchangedLine]]
    @?= textForUnchangedLine

testTextForSectionOfLineChanges =
    testCase "Text is shown for section of line changes."
    $ viewLineChanges [sectionOfLineChanges]
    @?= textForSectionOfLineChanges

testTextForMultipleSectionsOfLineChanges =
    testCase "Text is shown for multiple sections of line changes."
    $ viewLineChanges [sectionOfLineChanges, sectionOfLineChanges]
    @?= expectedText
    where
      expectedText =
        textForSectionOfLineChanges ++
        newline ++
        textForSectionOfLineChanges

testTextForPaddingOfLineNumbers =
    testCase "Text is shown with padding for line numbers."
    $ viewLineChanges [[ unchangedLineWithLineNumbers (3, 4)
                       , unchangedLineWithLineNumbers (13, 14) ]]
    @?= expectedText
    where
      expectedText =
        view " " ++
        space ++
        view " 3:" ++
        view " " ++
        color yellow " 4:" ++
        space ++
        view "unchanged" ++
        newline ++
        view " " ++
        space ++
        view "13:" ++
        view " " ++
        color yellow "14:" ++
        space ++
        view "unchanged" ++
        newline

addedLine = LineChange AddedLine (0, 1) "added"

textForAddedLine =
  color green "+" ++
  space ++
  view "" ++
  view "" ++
  color yellow "1:" ++
  space ++
  color green "added" ++
  newline

deletedLine = LineChange DeletedLine (2, 0) "deleted"

textForDeletedLine =
  color red "-" ++
  space ++
  view "2:" ++
  view "" ++
  color yellow "" ++
  space ++
  color red "deleted" ++
  newline

unchangedLine = LineChange UnchangedLine (3, 4) "unchanged"

textForUnchangedLine =
  view " " ++
  space ++
  view "3:" ++
  view " " ++
  color yellow "4:" ++
  space ++
  view "unchanged" ++
  newline

sectionOfLineChanges = [addedLine, deletedLine, unchangedLine]

textForSectionOfLineChanges =
  color green "+" ++
  space ++
  view "  " ++
  view " " ++
  color yellow "1:" ++
  space ++
  color green "added" ++
  newline ++
  color red "-" ++
  space ++
  view "2:" ++
  view " " ++
  color yellow "  " ++
  space ++
  color red "deleted" ++
  newline ++
  view " " ++
  space ++
  view "3:" ++
  view " " ++
  color yellow "4:" ++
  space ++
  view "unchanged" ++
  newline


unchangedLineWithLineNumbers :: LineNumbers -> LineChange
unchangedLineWithLineNumbers lineNumbers =
  LineChange UnchangedLine lineNumbers "unchanged"
