import Data.Map (Map)
import qualified Data.Map as Map

initMap :: Map (Maybe String, Maybe String) [String]
initMap = Map.fromList [((Nothing, Nothing), [])]

--initMap :: Maybe String -> Maybe String -> String -> Map (Maybe String, Maybe String) [String]
--initMap a b c = Map.fromList [((a,b), [c])]

addToken :: (Maybe String, Maybe String) -> String -> Map (Maybe String, Maybe String) [String] -> Map (Maybe String, Maybe String) [String]
addToken k v map = Map.insertWith (++) k [v] map

