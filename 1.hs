{-# LANGUAGE OverloadedStrings #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/bunny.htm
-- This implements the command:
-- convert bunny_grass.gif ( bunny_anim.gif -repage 0x0+5+15! ) \
--            -coalesce -delete 0 -deconstruct -loop 0  bunny_bgnd.gif
-- from Anthony's examples at: http://www.imagemagick.org/Usage/anim_basics/#cleared

import           Control.Monad                   (forM_)
import           Control.Monad.Trans.Resource    (release)
import           Graphics.ImageMagick.MagickWand


main :: IO ()
main = withMagickWandGenesis $ do
  (_,mw) <- magickWand
  readImage mw "1.gif"
  (pointer, images) <- coalesceImages mw
  (_,mw1) <- magickWand
  n <- getNumberImages images
  forM_ [1..(n-1)] $ \i -> localGenesis $ do
    images `setIteratorIndex` i
    (_,image) <- getImage images
    resizeImage image 200 200 lanczosFilter 1
    addImage mw1 image
  resetIterator mw1
  release pointer
  writeImages mw1 "2.gif" True
