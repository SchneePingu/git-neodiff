module LineChangeData
( LineChange (..)
, TypeOfLineChange (..)
, LineNumbers
, LineNumber
, Content
) where


import LineData


type LineNumbers = (LineNumber, LineNumber)
type LineNumber = Int
type Content = String


data LineChange = LineChange { typeOfLineChange :: TypeOfLineChange
                             , lineNumbers :: LineNumbers
                             , content :: Content
                             } deriving (Show)


data TypeOfLineChange = AddedLine | DeletedLine | UnchangedLine deriving (Eq, Show)
