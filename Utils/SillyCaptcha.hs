{-# LANGUAGE TupleSections #-}
module Utils.SillyCaptcha
       (
         makeCaptcha
       ) where
------------------------------------------------------------------------------------------------
import Prelude
import Graphics.GD
import System.Random       (randomRIO)
import Control.Applicative ((<$>))
import Control.Arrow       (second)
import Control.Monad       (forM)
import Data.Char           (toLower)
import Data.Text           (pack, Text)
import Codec.Binary.UTF8.String
------------------------------------------------------------------------------------------------
data Style = Regular | Bold | Italic
  deriving (Show, Read, Eq, Bounded, Ord, Enum)
------------------------------------------------------------------------------------------------
-- | Takes a random element from list
pick :: [a] -> IO a
pick xs = (xs!!) <$> randomRIO (0, length xs - 1)
------------------------------------------------------------------------------------------------
grey  :: Color
grey  = rgb 238 238 238
black :: Color
black = rgb 0 0 0
------------------------------------------------------------------------------------------------
prefixPath :: String
prefixPath = "./fonts/"
------------------------------------------------------------------------------------------------
italicFonts    :: [(Style,String)]
boldFonts      :: [(Style,String)]
regularFonts   :: [(Style,String)]
fonts          :: [(Style,String)]
italicFonts    = map ((Italic,   ) . (++"-italic.ttf" )) ["times"]
boldFonts      = map ((Bold,     ) . (++"-bold.ttf"   )) ["DejaVuSans"]
regularFonts   = map ((Regular,  ) . (++"-regular.ttf")) ["times"]
fonts          = map (second (prefixPath++)) $ italicFonts ++ boldFonts ++ regularFonts
------------------------------------------------------------------------------------------------
makeCaptcha :: String        -> -- ^ Path to captcha
              String        -> -- ^ Captcha string
              IO (Text, Text) -- ^ (captcha style, captcha value)
makeCaptcha path chars' = do
  let len    = length chars'
      space  = 16   -- space between characters in px
      height = 35   -- image height
      fSize  = 17.0 -- font size
      minR   = 0    -- min angle in radians
      maxR   = 0    -- min angle in radians
      chars  = map (encodeString . (:[])) chars'
  img <- newImage (space*(len+2), height)
  fillImage grey img
  text <- fmap (filter ((/=" ") . snd)) $ forM (zip [1..] chars) $ \(i,char) -> do
    font   <- pick fonts
    x      <- randomRIO (-3,3) :: IO Int
    y      <- randomRIO (-3,3) :: IO Int
    angleR <- randomRIO (minR ,maxR ) :: IO Double
    angleL <- randomRIO (-minR,-maxR) :: IO Double
    angle  <- pick [angleR, angleL]
    _      <- drawString (snd font) fSize angle (x+space*i, truncate (fSize*1.5 :: Double)+y) char black img
    return (fst font, decodeString char)
    
  style <- pick $ map fst text
  savePngFile path img
  return (pack $ show style, pack $ map toLower $ concatMap snd $ filter ((==style).fst) text)
------------------------------------------------------------------------------------------------
-- main = do
--   (x,y) <- makeCaptcha "1.png" "lkdjflds"
--   print $ unpack x
--   putStrLn $ unpack y
