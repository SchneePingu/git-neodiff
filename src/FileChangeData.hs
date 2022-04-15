module FileChangeData
( FileChange (..)
, TypeOfFileChange (..)
, FileNames
) where


import LineChangeData


type FileNames = (String, String)


data FileChange = FileChange { typeOfFileChange :: TypeOfFileChange
                             , fileNames :: FileNames
                             , lineChanges :: [[LineChange]]
                             } deriving (Show)


data TypeOfFileChange = AddedFile | DeletedFile | RenamedFile | EditedFile deriving (Show)
