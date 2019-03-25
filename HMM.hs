import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random

seed::Int
seed = 40
generator = mkStdGen seed

tokenize input = words (stringToLower (noPunc input))
  where
    stringToLower s = map toLower s
    noPunc s = filter (\x -> isAlpha x || isSpace x) s

learn :: [String] -> Map (Maybe String, Maybe String) [String]
learn (k1 : k2 : v : tokens)
  | tokens == [] = addToken (Just k1, Just k2) v initialMap
  | otherwise = addToken (Just k1, Just k2) v (learn (k2 : v : tokens) )


initialMap :: Map (Maybe String, Maybe String) [String]
initialMap = Map.fromList [((Nothing, Nothing), [""])]

addToken :: (Maybe String, Maybe String) -> String -> Map (Maybe String, Maybe String) [String] -> Map (Maybe String, Maybe String) [String]
addToken k v m =  Map.insertWith (++) k [v] (
                    Map.insertWith (++) ((fst k), Nothing) [v] (
                        Map.insertWith (++) (Nothing, snd k) [v] m))

predictWord :: (Maybe String, Maybe String) -> Map (Maybe String, Maybe String) [String] -> String
predictWord k m =
  case Map.lookup k m of
    Just x -> getRandomElement x
    Nothing -> case Map.lookup (Nothing, snd k) m of
      Just x -> getRandomElement x
      Nothing -> case Map.lookup (fst k, Nothing) m of
        Just x -> getRandomElement x
        Nothing -> case Map.lookup (Nothing, Nothing) m of
          Just x -> getRandomElement x
          Nothing -> ""

getRandomElement :: [String] -> String
getRandomElement l = l !! rand where
  n = length l
  (rand, _) = randomR (0,(n-1)) generator

