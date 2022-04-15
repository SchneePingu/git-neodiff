module Main where


import ChangesReader
import ChangesViewer


main :: IO ()
main = do
    changes <- readChanges
    viewChanges changes
