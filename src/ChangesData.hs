module ChangesData
( Changes (..)
, Header
) where


import LineData
import FileChangeData


data Changes = Changes Header [FileChange]
type Header = [Line]
