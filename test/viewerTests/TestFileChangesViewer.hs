module TestFileChangesViewer where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import FileChangesViewer
import FileChangeData
import Text
import Data


unitTests = testGroup "TestFileChangesViewer" testCases

testCases =
    [ testNoTextForNoFileChanges
    , testTextForAddedFile
    , testTextForDeletedFile
    , testTextForRenamedFile
    , testTextForEditedFile
    , testTextForMultipleChangedFiles
    ]

testNoTextForNoFileChanges =
    testCase "No text is shown for no file changes."
    $ viewFileChanges []
    @?= []

testTextForAddedFile =
    testCase "Text is shown for added file."
    $ viewFileChanges [addedFile]
    @?= textForAddedFile


testTextForDeletedFile =
    testCase "Text is shown for deleted file."
    $ viewFileChanges [deletedFile]
    @?= textForDeletedFile

testTextForRenamedFile =
    testCase "Text is shown for renamed file."
    $ viewFileChanges [renamedFile]
    @?= textForRenamedFile

testTextForEditedFile =
    testCase "Text is shown for edited file."
    $ viewFileChanges [editedFile]
    @?= textForEditedFile

testTextForMultipleChangedFiles =
    testCase "Text is shown for multiple changed files."
    $ viewFileChanges multipleChangedFiles
    @?= textForMultipleChangedFiles

addedFile = FileChange AddedFile ("Old.txt", "New.txt") [[addedLine]]

textForAddedFile =
  color cyan "@ "  ++
  color blue "New.txt" ++
  newline ++
  view "(" ++
  view "added" ++
  view ": " ++
  view "Old.txt" ++
  view ")" ++
  newline ++
  view "[" ++
  color green "+1" ++
  view " | " ++
  color red "-0" ++
  view "]" ++
  newline ++
  textForAddedLine

deletedFile = FileChange DeletedFile ("Old.txt", "New.txt") [[deletedLine]]

textForDeletedFile =
  color cyan "@ "  ++
  color blue "New.txt" ++
  newline ++
  view "(" ++
  view "deleted" ++
  view ": " ++
  view "Old.txt" ++
  view ")" ++
  newline ++
  view "[" ++
  color green "+0" ++
  view " | " ++
  color red "-1" ++
  view "]" ++
  newline ++
  textForDeletedLine

renamedFile = FileChange RenamedFile ("Old.txt", "New.txt") [[unchangedLine]]

textForRenamedFile =
  color cyan "@ "  ++
  color blue "New.txt" ++
  newline ++
  view "(" ++
  view "renamed" ++
  view ": " ++
  view "Old.txt" ++
  view ")" ++
  newline ++
  view "[" ++
  color green "+0" ++
  view " | " ++
  color red "-0" ++
  view "]" ++
  newline ++
  textForUnchangedLine

editedFile = FileChange EditedFile ("Old.txt", "New.txt") [sectionOfLineChanges]

textForEditedFile =
  color cyan "@ "  ++
  color blue "New.txt" ++
  newline ++
  view "(" ++
  view "edited" ++
  view ": " ++
  view "Old.txt" ++
  view ")" ++
  newline ++
  view "[" ++
  color green "+1" ++
  view " | " ++
  color red "-1" ++
  view "]" ++
  newline ++
  textForSectionOfLineChanges

multipleChangedFiles = [addedFile, deletedFile, renamedFile, editedFile]

textForMultipleChangedFiles =
  textForAddedFile ++
  newline ++
  textForDeletedFile ++
  newline ++
  textForRenamedFile ++
  newline ++
  textForEditedFile
