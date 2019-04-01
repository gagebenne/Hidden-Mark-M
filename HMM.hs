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

tweet = do
  s <- Text.readFile "trump_tweets/2018.tweets"
  
  Text.putStr (Text.pack "First word: ")
  t1 <- Text.getLine
  Text.putStr (Text.pack "Second word: ")
  t2 <- Text.getLine

  Text.putStr (Text.pack "\n@realDonaldTrump\n")

  let hmm = learn s
  let tweet = predictTweet (predictionStream (prepareText t1) (prepareText t2) hmm) hmm
  return tweet

prepareText :: Text -> Maybe Text
prepareText t
  | Text.null t = Nothing
  | otherwise = Just (Text.toUpper t)

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
predictionStream :: (Maybe Token) -> (Maybe Token) -> HMM -> [Token]
predictionStream (Just w1) (Just w2) hmm = w1 : predictionStream (Just w2) (Just (predictWord (Just w1, Just w2) hmm)) hmm
predictionStream Nothing (Just w2) hmm = predictionStream (Just w2) (Just (predictWord (Nothing, Just w2) hmm)) hmm
predictionStream Nothing Nothing hmm = predictionStream Nothing (Just (predictWord (Nothing, Nothing) hmm)) hmm

predictTweet :: [Token] -> HMM -> Text
predictTweet stream hmm = Text.unwords (takeWhile ((/=) (Text.pack "\n")) stream)
