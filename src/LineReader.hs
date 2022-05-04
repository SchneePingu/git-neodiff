module LineReader
( readLines
) where


import LineData


readLines :: IO [Line]
readLines = lines <$> getContents
