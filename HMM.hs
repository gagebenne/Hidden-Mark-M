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
  Text.putStrLn (predictTweet (Text.pack "MAKE") (Text.pack "AMERICA") (learn (Text.pack s)))
  return 1

learn :: Text -> HMM
learn input = Map.union (learnTweets (tokenize input)) (learnFirsts (tokenize input))

tokenize :: Text -> [Tweet]
tokenize input = map Text.words (Text.lines (Text.toUpper input))

learnFirsts :: [Tweet] -> HMM
learnFirsts (tweet : tweets) = Map.unionWith (++) (learnFirst tweet) (learnFirsts tweets)
learnFirsts [] = Map.empty

learnFirst :: Tweet -> HMM
learnFirst tweet = Map.fromList [((Nothing, Just (Text.pack "\n")), [tweet!!0]), ((Nothing, Nothing), [tweet!!0])]

learnTweets :: [Tweet] -> HMM
learnTweets (tweet : tweets) = Map.unionWith (++) (learnTweet tweet) (learnTweets tweets)
learnTweets [] = Map.empty

learnTweet :: Tweet -> HMM
learnTweet (t1 : t2 : t3 : tokens) = addToken (Just t1, Just t2) t3 (learnTweet ([t2] ++ [t3] ++ tokens))
learnTweet [t1, t2] = addToken (Just t1, Just t2) (Text.pack "\n") (learnTweet [])
learnTweet _ = Map.empty

-- given a key, a token, and an HMM, produce an updated HMM
addToken :: Key -> Token -> HMM -> HMM
addToken k v m =  Map.insertWith (++) k [v] (
                    Map.insertWith (++) ((fst k), Nothing) [v] (
                        Map.insertWith (++) (Nothing, snd k) [v] m))

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
predictionStream :: Token -> Token -> HMM -> [Token]
predictionStream w1 w2 hmm = map fst stream
  where
    stream = [(w1, w2)] ++ [ ( snd w, predictWord ( Just (fst w),  Just (snd w)) hmm) | w <- stream]

-- predict list of tokens until an end token is found
--predictTweet :: Token -> Token -> HMM -> [Token]
--predictTweet w1 w2 m = takeWhileInclusive notEndToken (predictForever w1 w2 m)

predictTweet :: Text -> Text -> HMM -> Text
predictTweet w1 w2 hmm = Text.unwords (takeWhile ((/=) (Text.pack "\n")) stream)
  where
    stream = predictionStream w1 w2 hmm
