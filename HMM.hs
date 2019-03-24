import Data.Char

tokenize input = words (stringToLower (noPunc input))
  where
    stringToLower s = map toLower s
    noPunc s = filter (\x -> isAlpha x || isSpace x) s
