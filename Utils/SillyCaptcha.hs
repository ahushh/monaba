{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Utils.SillyCaptcha
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
data Style = Regular | Bold | Italic
  deriving (Show, Read, Eq, Bounded, Ord, Enum)
------------------------------------------------------------------------------------------------
-- | Takes a random element from list
pick :: [a] -> IO a
pick xs = (xs!!) <$> randomRIO (0, length xs - 1)
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
prefixPath :: Text
prefixPath = "./fonts/"
------------------------------------------------------------------------------------------------
italicFonts    :: [(Style,Text)]
boldFonts      :: [(Style,Text)]
regularFonts   :: [(Style,Text)]
fonts          :: [(Style,Text)]
italicFonts    = map ((Italic,   ) . (<>"-italic.ttf" )) ["times"]
boldFonts      = map ((Bold,     ) . (<>"-bold.ttf"   )) ["DejaVuSans"]
regularFonts   = map ((Regular,  ) . (<>"-regular.ttf")) ["times"]
fonts          = map (second (prefixPath<>)) $ italicFonts <> boldFonts <> regularFonts
------------------------------------------------------------------------------------------------
makeCaptcha :: String        -> -- ^ Path to captcha
              Text          -> -- ^ Captcha string
              IO (Text, Text) -- ^ (captcha style, captcha value)
makeCaptcha path chars' = withMagickWandGenesis $ localGenesis $ do
  (_, w) <- magickWand
  (_,dw) <- drawingWand
  pw     <- pixelWand

  let len    = T.length chars'
      space  = 16.0 :: Double   -- space between characters in px
      height = 35              -- image height
      fSize  = 17              -- font size
      chars  = map (pack . (:[])) $ unpack chars'
  -- Create a transparent image
  pw `setColor` "none"
  newImage w (truncate space*(len+2)) height pw
  -- Set text color and size
  pw `setColor` "black"
  dw `setFontSize` fSize
  dw `setTextAntialias` True
  -- Add the text
  text <- fmap (filter ((/=" ") . snd)) $ forM (zip [1..] chars) $ \(i,char) -> do
    font   <- liftIO (pick fonts)
    x      <- liftIO (randomRIO (-3.0,3.0) :: IO Double)
    y      <- liftIO (randomRIO (-3.0,3.0) :: IO Double)

    dw `setFont` (encodeUtf8 $ snd font)
    drawAnnotation dw (x+space*i) ((fSize*1.5 :: Double)+y) char
    return (fst font, char)
  drawImage w dw
  -- Trim the image down to include only the text
  trimImage w 0
  -- Draw the white shadow
  resetImagePage w Nothing
  (_,cloneW) <- cloneMagickWand w
  pw `setColor` "white"
  w `setImageBackgroundColor` pw
  shadowImage w 98 3 1 1
  compositeImage w cloneW overCompositeOp 5 5
  (_,w') <- magickWand
  pw `setColor` "none"
  width  <- getImageWidth w
  newImage w' width height pw
  compositeImage w' w overCompositeOp 0 0

  writeImage w' $ Just $ fromText $ pack path
  style <- liftIO $ pick (map fst text)
  return (pack $ show style, T.toLower $ T.concat $ map snd $ filter ((==style).fst) text)
------------------------------------------------------------------------------------------------
-- main = do
--   (x,y) <- makeCaptcha "magick.png" "Magick rocks"
--   print x 
--   print y
