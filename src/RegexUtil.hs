module RegexUtil
( matches
, getMatchingGroups
) where


import Text.Regex.TDFA ((=~))


matches :: String -> String -> Bool
matches line regex = line =~ regex :: Bool


getMatchingGroups :: String -> String -> [String]
getMatchingGroups string regexWithGroups =
  let (_, _, _, groupsMatchingRegex) =
        string =~ regexWithGroups :: (String, String, String, [String])
  in  groupsMatchingRegex
