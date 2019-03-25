import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

tokenize input = words (stringToLower (noPunc input))
  where
    stringToLower s = map toLower s
    noPunc s = filter (\x -> isAlpha x || isSpace x) s

learn :: [String] -> Map (Maybe String, Maybe String) [String]
learn (k1 : k2 : v : tokens)
  | tokens == [] = addToken (Just k1, Just k2) v initialMap
  | otherwise = addToken (Just k1, Just k2) v (learn (k2 : v : tokens) )


initialMap :: Map (Maybe String, Maybe String) [String]
initialMap = Map.fromList [((Nothing, Nothing), [])]

addToken :: (Maybe String, Maybe String) -> String -> Map (Maybe String, Maybe String) [String] -> Map (Maybe String, Maybe String) [String]
addToken k v m =  Map.insertWith (++) k [v] (
                    Map.insertWith (++) ((fst k), Nothing) [v] (
                        Map.insertWith (++) (Nothing, snd k) [v] m))
