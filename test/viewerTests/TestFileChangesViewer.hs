module TestFileChangesViewer where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import FileChangesViewer
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
