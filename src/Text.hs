module Text
( Text
, view
, color
, green
, red
, yellow
, cyan
, blue
, printText
, space
, newline
) where


import Text.Colour (Chunk, Colour, chunk, green, red, yellow, cyan, blue, fore, putChunksWith, TerminalCapabilities( With24BitColours ))
import Data.Text (pack)


type Text = [Chunk]


printText :: Text -> IO ()
printText = putChunksWith With24BitColours


view :: String -> Text
view string = [chunk $ pack string]


color :: Colour -> String -> Text
color colorOfText string = [fore colorOfText $ chunk $ pack string]

space = view " "
newline = view "\n"
