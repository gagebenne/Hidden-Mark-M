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


main = do
  s <- readFile "trump_tweets/2018.tweets"
  print (learnTweets (tokenize (Text.pack s)))
  -- Text.putStrLn (Text.pack (predictTweet (Text.pack "MEXICO") (Text.pack "MEXICO") s))
  return 1

predictTweet :: Text -> Text -> Text -> Text
predictTweet w1 w2 input = Text.unwords (takeWhile ((/=) (Text.pack "~")) (map fst output))
  where
    output = [(w1, w2)] ++ [ ( snd w, predictWord ( Just (fst w),  Just (snd w)) m) | w <- output]
      where m = learnTweets (tokenize input)

tokenize :: Text -> [[Text]]
tokenize input = map Text.words (Text.lines (Text.toUpper input))

learnTweets :: [[Text]] -> Map (Maybe Text, Maybe Text) [Text]
learnTweets (tweet : tweets)
  | tweets == [] = learnTweet tweet
  | otherwise = Map.union (learnTweet tweet) (learnTweets tweets)

learnTweet :: [Text] -> Map (Maybe Text, Maybe Text) [Text]
learnTweet (t1 : t2 : t3 : tokens)
  | tokens == [] = addToken (Just t1, Just t2) t3 initialMap
  | otherwise = addToken (Just t1, Just t2) t3 (learnTweet ([t2] ++ [t3] ++ tokens) )
learnTweet [t1, t2] = error "TWO"
learnTweet [t1] = error "ONE"


initialMap :: Map (Maybe Text, Maybe Text) [Text]
initialMap = Map.fromList [((Nothing, Nothing), [Text.pack ""])]

addToken :: (Maybe Text, Maybe Text) -> Text -> Map (Maybe Text, Maybe Text) [Text] -> Map (Maybe Text, Maybe Text) [Text]
addToken k v m =  Map.insertWith (++) k [v] (
                    Map.insertWith (++) ((fst k), Nothing) [v] (
                        Map.insertWith (++) (Nothing, snd k) [v] m))

predictWord :: (Maybe Text, Maybe Text) -> Map (Maybe Text, Maybe Text) [Text] -> Text
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

getRandomElement :: [Text] -> Text
getRandomElement l = l !! (unsafePerformIO (getStdRandom (randomR (0, (length l) - 1))))
