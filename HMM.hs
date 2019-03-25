import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import Data.List
import System.IO.Unsafe
import Data.Text hiding (map, takeWhile, length)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

type Token = Text
type Key = (Maybe Token, Maybe Token)
type Value = [Token]
type HMM = Map Key Value
type Tweet = [Text]

main = do
  s <- readFile "trump_tweets/2018.tweets"
  print (learnTweets (tokenize (Text.pack s)))
  -- Text.putStrLn (Text.pack (predictTweet (Text.pack "MEXICO") (Text.pack "MEXICO") s))
  return 1

tokenize :: Text -> [Tweet]
tokenize input = map Text.words (Text.lines (Text.toUpper input))

learnTweets :: [Tweet] -> HMM
learnTweets (tweet : tweets)
  | tweets == [] = learnTweet tweet
  | otherwise = Map.union (learnTweet tweet) (learnTweets tweets)

learnTweet :: Tweet -> HMM
learnTweet (t1 : t2 : t3 : tokens)
  | tokens == [] = addToken (Just t1, Just t2) t3 initialHMM
  | otherwise = addToken (Just t1, Just t2) t3 (learnTweet ([t2] ++ [t3] ++ tokens) )
learnTweet [t1, t2] = error "TWO"
learnTweet [t1] = error "ONE"

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
          Nothing -> Text.pack ""

-- produces an endless list of recursively predicted tokens based on two starter tokens
predictForever :: Token -> Token -> HMM -> [Token]
predictForever w1 w2 m = map fst output
    where
        output = [(w1, w2)] ++ [ ( snd w, predictWord ( Just (fst w),  Just (snd w)) m) | w <- output]

-- predict list of tokens until an end token is found
--predictTweet :: Token -> Token -> HMM -> [Token]
--predictTweet w1 w2 m = takeWhileInclusive notEndToken (predictForever w1 w2 m)

predictTweet :: Text -> Text -> Text -> Text
predictTweet w1 w2 input = Text.unwords (takeWhile ((/=) (Text.pack "~")) (map fst output))
  where
    output = [(w1, w2)] ++ [ ( snd w, predictWord ( Just (fst w),  Just (snd w)) m) | w <- output]
      where m = learnTweets (tokenize input)