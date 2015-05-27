{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative             ((<$>))
import           Control.Arrow                   (second)
import           Control.Monad                   (forM)
import           Control.Monad.IO.Class          (liftIO)
import           Data.Char                       (toLower)
import           Data.Text                       (pack)
import           Data.Text.Encoding              (encodeUtf8)
import           Data.Monoid                     ((<>))
import           Filesystem.Path.CurrentOS       (fromText)
import           Graphics.ImageMagick.MagickWand
import           Prelude
import           System.Random                   (randomRIO)
import           System.Environment             (getArgs)
------------------------------------------------------------------------------------------------
-- | Takes a random element from list
pick :: [a] -> IO a
pick xs = (xs!!) <$> randomRIO (0, length xs - 1)
------------------------------------------------------------------------------------------------
colors = ["AliceBlue","AntiqueWhite","aqua","aquamarine","azure","beige","bisque","black","BlanchedAlmond","blue","BlueViolet","brown","burlywood","CadetBlue","chartreuse","chocolate","coral","CornflowerBlue","cornsilk","crimson","cyan","DarkBlue","DarkCyan","DarkGoldenrod","DarkGray","DarkGreen","DarkGrey","DarkKhaki","DarkMagenta","DarkOliveGreen","DarkOrange","DarkOrchid","DarkRed","DarkSalmon","DarkSeaGreen","DarkSlateBlue","DarkSlateGray","DarkSlateGrey","DarkTurquoise","DarkViolet","DeepPink","DeepSkyBlue","DimGray","DimGrey","DodgerBlue","firebrick","FloralWhite","ForestGreen","fractal","freeze","fuchsia","gainsboro","GhostWhite","gold","goldenrod","gray","gray","green","green","GreenYellow","grey","honeydew","HotPink","IndianRed","indigo","ivory","khaki","lavender","LavenderBlush","LawnGreen","LemonChiffon","LightBlue","LightCoral","LightCyan","LightGoldenrod","LightGoldenrodYellow","LightGray","LightGreen","LightGrey","LightPink","LightSalmon","LightSeaGreen","LightSkyBlue","LightSlateBlue","LightSlateGray","LightSlateGrey","LightSteelBlue","LightYellow","lime","LimeGreen","linen","magenta","maroon","maroon","matte","MediumAquamarine","MediumBlue","MediumForestGreen","MediumGoldenRod","MediumOrchid","MediumPurple","MediumSeaGreen","MediumSlateBlue","MediumSpringGreen","MediumTurquoise","MediumVioletRed","MidnightBlue","MintCream","MistyRose","moccasin","NavajoWhite","navy","NavyBlue","none","OldLace","olive","OliveDrab","opaque","orange","OrangeRed","orchid","PaleGoldenrod","PaleGreen","PaleTurquoise","PaleVioletRed","PapayaWhip","PeachPuff","peru","pink","plum","PowderBlue","purple","purple","red","RosyBrown","RoyalBlue","SaddleBrown","salmon","SandyBrown","SeaGreen","seashell","sienna","silver","SkyBlue","SlateBlue","SlateGray","SlateGrey","snow","SpringGreen","SteelBlue","tan","teal","thistle","tomato","transparent","turquoise","violet","VioletRed","wheat","white","WhiteSmoke","yellow","YellowGreen"]
chars = filter (`notElem`"Il") $ ['a'..'x']++['A'..'X']++['1'..'9']

makeCaptcha :: String    -> -- ^ Path to captcha
              IO String   -- ^ captcha value
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
    x      <- liftIO (randomRIO (-2.0,2.0) :: IO Double)
    y      <- liftIO (randomRIO (-2.0,2.0) :: IO Double)
    char   <- liftIO $ pick chars
    color  <- liftIO $ pick colors
    pw `setColor` color
    dw `setStrokeColor` pw
    drawAnnotation dw (x+space*(fromIntegral i)) ((fSize :: Double)+y) (pack $ char:[])
    return $ toLower char
  drawImage w dw
  trimImage w 0
  writeImage w $ Just $ fromText $ pack path
  return text
------------------------------------------------------------------------------------------------
main = do
  putStrLn =<< makeCaptcha . head =<< getArgs
