{-# LANGUAGE OverloadedStrings #-}
module Utils.Image
       ( makeThumbImg
       , makeThumbNonImg
       , getImageResolution
       ) where
------------------------------------------------------------------------------------------------
import           Data.Ratio
import           Filesystem.Path.CurrentOS       (fromText)
import           Graphics.ImageMagick.MagickWand hiding (resizeImage, getImageResolution)
import qualified Graphics.ImageMagick.MagickWand as IM
import           Import
import           System.Directory                (copyFile, createDirectory, doesDirectoryExist, doesFileExist)
import           System.FilePath                 ((</>))
------------------------------------------------------------------------------------------------
type ImageResolution = (Int, Int)
------------------------------------------------------------------------------------------------
getImageResolution :: FilePath -> IO ImageResolution
getImageResolution filepath = withMagickWandGenesis $ do
  (_,w) <- magickWand
  readImage w (fromText $ pack filepath)
  width  <- getImageWidth w
  height <- getImageHeight w
  return (width, height)

calcResolution :: ImageResolution -> ImageResolution -> ImageResolution
calcResolution (inW,inH) (outW,outH)
    | inAspect >  outAspect = (outW, round (fromIntegral outW / inAspect))
    | inAspect <  outAspect = (round (fromIntegral outH * inAspect), outH)
    | otherwise             = (outW, outH)
    where inAspect  = inW  % inH
          outAspect = outW % outH

-- | Resizes an image file and saves the result to a new file.
resizeImage :: FilePath           -- ^ Source image file
            -> FilePath           -- ^ Destination image file
            -> ImageResolution    -- ^ The maximum dimensions of the output file
            -> IO ImageResolution -- ^ The size of the output file
resizeImage from to maxSz = withMagickWandGenesis $ do
  (_,w) <- magickWand
  readImage w (fromText $ pack from)
  width  <- getImageWidth w
  height <- getImageHeight w
  let inSz                    = (width, height)
      outSz@(width', height') = calcResolution inSz maxSz
  IM.resizeImage w width' height' lanczosFilter 1
  setImageCompressionQuality w 95
  writeImages w (fromText $ pack to) True
  return outSz

-- | Make a thumbnail for an image file
makeThumbImg :: Int             ->  -- ^ The maximum thumbnail width and height
               FilePath        ->  -- ^ Source image file
               FilePath        ->  -- ^ Destination image _name_
               String          ->  -- ^ Destination file extension
               ImageResolution ->  -- ^ Width and height of the source file
               IO ImageResolution -- ^ Width and height of the destination file
makeThumbImg thumbSize filepath filename filetype (width, height) = do
  unlessM (doesDirectoryExist (thumbDirectory </> filetype)) $
    createDirectory (thumbDirectory </> filetype)
  if height > thumbSize || width > thumbSize
    then resizeImage filepath thumbpath (thumbSize,thumbSize)
    else copyFile filepath thumbpath >> return (width, height)
    where thumbpath = thumbFilePath thumbSize filetype filename

-- | Make a thumbnail for a non-image file
makeThumbNonImg :: FilePath -> -- ^ Destination file name
                  String   -> -- ^ Destination file extension
                  IO ()
makeThumbNonImg filename filetype =
  unlessM (doesFileExist $ thumbFilePath 0 filetype filename) $ do
    let defaultIconPath = staticDir </> "icons" </> "default" ++ "." ++ thumbIconExt
        newIconPath     = staticDir </> "icons" </> filetype  ++ "." ++ thumbIconExt
    copyFile defaultIconPath newIconPath

