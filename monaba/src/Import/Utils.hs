module Import.Utils where

import Prelude
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime, addUTCTime, secondsToDiffTime)
import qualified Data.Map.Strict as MapS
import Data.IP (IP)
import System.Random (randomRIO)
import Control.Monad (unless, when)

-- | Takes a random element from list
pick :: [a] -> IO (Maybe a)
pick xs
  | length xs == 0 = return Nothing
  | otherwise = (Just . (xs!!)) <$> randomRIO (0, length xs - 1)

whenM :: Monad m => m Bool -> m () -> m ()
whenM = (. flip when) . (>>=)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM = (. flip unless) . (>>=)

tshow :: Show a => a -> Text
tshow = pack . show

tread :: Read a => Text -> a
tread = read . unpack

readIP :: String -> IP
readIP = read

pair :: forall t1 t2 t3. (t1 -> t2) -> (t1 -> t3) -> t1 -> (t2, t3)
pair f g x = (f x, g x)

keyValuesToMap :: (Ord k) => [(k, a)] -> MapS.Map k [a]  
keyValuesToMap = MapS.fromListWith (++) . map (\(k,v) -> (k,[v]))

-- | Add UTCTime with Integer seconds
addUTCTime' :: Int -> UTCTime -> UTCTime
addUTCTime' sec t = addUTCTime (realToFrac $ secondsToDiffTime $ toInteger sec) t
