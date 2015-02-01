{-# LANGUAGE OverloadedStrings #-}
module Utils.PlainCaptcha
       ( makeCaptcha
       ) where
------------------------------------------------------------------------------------------------
import           Control.Applicative             ((<$>))
import           Control.Arrow                   (second)
import           Control.Monad                   (forM)
import           Control.Monad.IO.Class          (liftIO)
import           Data.Text                       (Text, pack, unpack)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (encodeUtf8)
import           Data.Monoid                     ((<>))
import           Filesystem.Path.CurrentOS       (fromText)
import           Graphics.ImageMagick.MagickWand
import           Prelude
import           System.Random                   (randomRIO)
------------------------------------------------------------------------------------------------
-- | Takes a random element from list
pick :: [a] -> IO a
pick xs = (xs!!) <$> randomRIO (0, length xs - 1)
------------------------------------------------------------------------------------------------
chars = ['a'..'x']++['A'..'X']++['0'..'9']

makeCaptcha :: String  -> -- ^ Path to captcha
              IO Text   -- ^ captcha value
makeCaptcha path = withMagickWandGenesis $ localGenesis $ do
  (_, w) <- magickWand
  (_,dw) <- drawingWand
  pw     <- pixelWand

  let len    = 5
      space  = 10.0 :: Double   -- space between characters in px
      height = 25              -- image height
      fSize  = 20              -- font size
  -- Create a transparent image
  -- pw `setColor` "none"
  pw `setColor` "white"
  newImage w (truncate space*(len+2)) height pw
  -- Set text color and size
  pw `setColor` "black"
  dw `setFontSize` fSize
  dw `setTextAntialias` True
  -- Add the text
  text <- forM [1..len] $ \i -> do
    x    <- liftIO (randomRIO (-5.0,3.0) :: IO Double)
    y    <- liftIO (randomRIO (-1.0,1.0) :: IO Double)
    char <- liftIO $ pick chars
    drawAnnotation dw (x+space*(fromIntegral i)) ((fSize :: Double)+y) (pack $ char:[])
    return char
  drawImage w dw
  -- Trim the image down to include only the text
  trimImage w 0
  -- Draw the white shadow
  resetImagePage w Nothing
  (_,cloneW) <- cloneMagickWand w
  pw `setColor` "grey"
  w `setImageBackgroundColor` pw
  shadowImage w 25 3 1 1
  compositeImage w cloneW overCompositeOp 5 5
  (_,w') <- magickWand
  pw `setColor` "none"
  width  <- getImageWidth w
  newImage w' width height pw
  compositeImage w' w overCompositeOp 0 0
  writeImage w' $ Just $ fromText $ pack path
  return $ T.toLower $ pack text
------------------------------------------------------------------------------------------------
-- main = do
--   print =<< makeCaptcha "magick.png"
