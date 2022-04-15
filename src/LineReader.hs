module LineReader
( readLines
) where


import LineData


readLines :: IO [Line]
readLines = do
  content <- getContents
  return $ lines content
