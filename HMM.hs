import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import Data.List
import System.IO.Unsafe

type Token = String
type Key = (Maybe Token, Maybe Token)
type Value = [Token]
type HMM = Map Key Value

-- convert a string into a list of words, punctuation, etc.
tokenize :: String -> [Token]
tokenize input = words (format input)
  where format :: String -> Token 
        format s =  map toUpper (
          intercalate " " (terminate input))

terminate input = [ x ++ " ~" | x <- (lines input) ]

-- create initial state of HMM with (Nothing, Nothing) -> [""] key-value pair
initialHMM :: HMM
initialHMM = Map.fromList [((Nothing, Nothing), [""])]

-- given a key, a token, and an HMM, produce an updated HMM
addToken :: Key -> Token -> HMM -> HMM
addToken k v m =  Map.insertWith (++) k [v] (
                    Map.insertWith (++) ((fst k), Nothing) [v] (
                        Map.insertWith (++) (Nothing, snd k) [v] m))

-- given a list of tokens, produce an HMM
learn :: [Token] -> HMM
learn (k1 : k2 : v : tokens)
  | tokens == [] = addToken (Just k1, Just k2) v initialHMM
  | k1 == "~" = addToken (Nothing, Nothing) k2 (learn (k2 : v : tokens) )
  | otherwise = addToken (Just k1, Just k2) v (learn (k2 : v : tokens) )

-- select a random element from a list
getRandomElement :: [a] -> a
getRandomElement l = l !! (unsafePerformIO (getStdRandom (randomR (0, (length l) - 1))))

-- predict a token given a key and an HMM
predictWord :: Key -> HMM -> Token 
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

-- produces an endless list of recursively predicted tokens based on two starter tokens
predictForever :: Token -> Token -> HMM -> [Token]
predictForever w1 w2 m = map fst output
    where
        output = [(w1, w2)] ++ [ ( snd w, predictWord ( Just (fst w),  Just (snd w)) m) | w <- output]

notEndToken :: Token -> Bool
notEndToken t = t /= "~"

-- take from a list until an element matches a condition
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

-- predict list of tokens until an end token is found
predictTweet :: Token -> Token -> HMM -> [Token]
predictTweet w1 w2 m = takeWhileInclusive notEndToken (predictForever w1 w2 m)
