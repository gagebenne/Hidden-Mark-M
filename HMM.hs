import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

tokenize input = words (stringToLower (noPunc input))
  where
    stringToLower s = map toLower s
    noPunc s = filter (\x -> isAlpha x || isSpace x) s

learn :: [String] -> Map (Maybe String, Maybe String) [String]
learn (k1 : k2 : v : tokens)
  | tokens == [] = addToken (Just k1, Just k2) v initMap
  | otherwise = addToken (Just k1, Just k2) v (learn (k2 : v : tokens) )


initMap :: Map (Maybe String, Maybe String) [String]
initMap = Map.fromList [((Nothing, Nothing), [])]

addToken :: (Maybe String, Maybe String) -> String -> Map (Maybe String, Maybe String) [String] -> Map (Maybe String, Maybe String) [String]
addToken k v map = Map.insertWith (++) k [v] map
