import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

tokenize :: String -> [String]
tokenize input = words (format input)
  where format :: String -> String
        format s =  map toUpper (
          intercalate " " (terminate input))

terminate input = [ x ++ " ~" | x <- (lines input) ]


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
