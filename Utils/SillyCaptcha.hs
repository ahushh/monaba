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
------------------------------------------------------------------------------------------------
data Style = Regular | Bold | Italic | Underline
  deriving (Show, Read, Eq, Bounded, Ord, Enum)
------------------------------------------------------------------------------------------------
-- | Takes a random element from list
pick :: [a] -> IO a
pick xs = (xs!!) <$> randomRIO (0, length xs - 1)
------------------------------------------------------------------------------------------------
prefixPath :: String
prefixPath = "./fonts/"
------------------------------------------------------------------------------------------------
-- | Background color
bgColor :: Color
bgColor = rgb 238 238 238
-- | Text color
black :: Color
black = rgb 0 0 0
------------------------------------------------------------------------------------------------
chars          :: [String]
italicFonts    :: [(Style,String)]
boldFonts      :: [(Style,String)]
regularFonts   :: [(Style,String)]
underlineFonts :: [(Style,String)]
fonts          :: [(Style,String)]
chars          = map (:[]) $ ['a'..'v']++['A'..'V']++"xX"
italicFonts    = map ((Italic,   ) . (++"-italic.ttf"   ))  ["times"]
boldFonts      = map ((Bold,     ) . (++"-bold.ttf"     ))  ["times"]
regularFonts   = map ((Regular,  ) . (++"-regular.ttf"  ))  ["times"]
underlineFonts = map ((Underline,) . (++"-underline.ttf"))  ["monospace"]
fonts          = map (second (prefixPath++)) $ italicFonts ++ boldFonts ++ regularFonts ++ underlineFonts 
------------------------------------------------------------------------------------------------
makeCaptcha :: Int           -> -- ^ Captcha length
              String        -> -- ^ Path to captcha
              IO (Text, Text) -- ^ (captcha style, captcha value)
makeCaptcha len path = do
  img <- newImage (25*(len+2), 60)
  fillImage bgColor img

  text <- forM [1..len] $ \i -> do
    font <- pick fonts
    char <- pick chars
    n    <- randomRIO (1,8) :: IO Int
    n'   <- randomRIO (-10,10) :: IO Int
    _    <- drawString (snd font) 22.0 0.0 (n+25*i, 40+n') char black img
    return (fst font, char)
    
  style <- pick $ map fst text
  savePngFile path img
    
  return (pack $ show style, pack $ map toLower $ concatMap snd $ filter ((==style).fst) text)
------------------------------------------------------------------------------------------------
-- main = do
--   (x,y) <- makeCaptcha "1.png"
--   print $ unpack x
--   print $ unpack y
