{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative             ((<$>))
import           Control.Arrow                   (second)
import           Control.Monad                   (forM)
import           Control.Monad.IO.Class          (liftIO)
import           Data.Text                       (pack, Text, concat)
import           Data.Text.Encoding              (encodeUtf8, decodeUtf8)
import           Data.Char                       (toLower)
import           Data.Monoid                     ((<>))
import           Graphics.ImageMagick.MagickWand
import           Prelude                         hiding (concat)
import           System.Random                   (randomRIO)
import           System.Environment             (getArgs)
------------------------------------------------------------------------------------------------
-- | Takes a random element from list
pick :: [a] -> IO a
pick xs = (xs!!) <$> randomRIO (0, length xs - 1)
------------------------------------------------------------------------------------------------
colors = ["black","blue","brown","cyan","gray","green","magenta","orange","pink","red","violet","white","yellow"]
chars = filter (`notElem`("Il"::String)) $ ['a'..'x']++['A'..'X']++['1'..'9']

makeCaptcha :: String    -> -- ^ Path to captcha
              IO (Text, Text)   -- ^ captcha value and hint
makeCaptcha path = withMagickWandGenesis $ localGenesis $ do
  (_, w) <- magickWand
  (_,dw) <- drawingWand
  pw     <- pixelWand

  let len    = 5
      space  = 12.0 :: Double   -- space between characters in px
      height = 30              -- image height
      fSize  = 25              -- font size
  newImage w (truncate space*(len+2)) height pw
  dw `setFontSize` fSize
  w `addNoiseImage` randomNoise
  blurImage w 0 1
  text <- forM [1..len] $ \i -> do
    x      <- liftIO (randomRIO (-1.0,1.0) :: IO Double)
    y      <- liftIO (randomRIO (-2.0,2.0) :: IO Double)
    char   <- liftIO $ pick chars
    color  <- liftIO $ pick colors
    pw `setColor` color
    dw `setStrokeColor` pw
    drawAnnotation dw (x+space*(fromIntegral i)) ((fSize :: Double)+y) (pack $ char:[])
    return (decodeUtf8 $ color, pack $ (:[]) $ toLower $ char)
  drawImage w dw
  trimImage w 0
  writeImage w $ Just $ pack path
  color        <- liftIO $ fst <$> pick text
  let filteredText = concat $ map snd $ filter ((==color).fst) text
  return (filteredText, "<div style='width:50px; height: 30px; display:inline-block; background-color:"<>color<>";'></div>")
------------------------------------------------------------------------------------------------
main = do
  putStrLn . show =<< makeCaptcha . head =<< getArgs
