{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Utils.File where

import           Import
import           Data.Digest.OpenSSL.MD5         (md5sum)
import           Data.Conduit                    (($$))
import qualified Data.ByteString                 as BS
import qualified Data.Conduit.List               as CL
import           Data.Ratio
import           System.Posix                    (getFileStatus, fileSize, FileOffset())
import           Text.Printf
import           System.Directory                (copyFile, doesFileExist, doesDirectoryExist, createDirectory, getDirectoryContents)
import           Filesystem.Path.CurrentOS       (fromText)
import           Graphics.ImageMagick.MagickWand hiding (resizeImage, getImageResolution)
import qualified Graphics.ImageMagick.MagickWand as IM
import           System.FilePath                 ((</>))
import           System.Process                  (readProcess)
-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------
insertFiles :: [FormResult (Maybe FileInfo)] -> -- ^ Files
               Int      -> -- ^ Thumbnail height and width
               Key Post -> -- ^ Post key
               HandlerT App IO ()
insertFiles []    _           _    = return ()
insertFiles files thumbSize postId = forM_ files (\formfile ->
  case formfile of
    FormSuccess (Just f) -> do
      hashsum    <- md5sum <$> BS.concat <$> (fileSource f $$ CL.consume) 
      uploadPath <- liftIO $ saveFile f hashsum
      filesize   <- liftIO $ formatFileSize <$> getFileSize uploadPath
      let filetype = detectFileType f
          filename = sanitizeFileName $ unpack $ fileName f
          newFile  = Attachedfile { attachedfileParentId    = postId
                                  , attachedfileHashsum     = hashsum
                                  , attachedfileName        = filename
                                  , attachedfileExtension   = fileExt f
                                  , attachedfileType        = detectFileType f
                                  , attachedfileSize        = filesize
                                  , attachedfileThumbSize   = thumbSize
                                  , attachedfileThumbWidth  = 0
                                  , attachedfileThumbHeight = 0
                                  , attachedfileInfo        = ""
                                  }
      case filetype of
        FileImage -> do
          (imgW  , imgH  ) <- liftIO $ getImageResolution uploadPath
          (thumbW, thumbH) <- liftIO $ makeThumbImg thumbSize uploadPath filename hashsum (imgW, imgH)
          void $ runDB $ insert $ newFile { attachedfileInfo        = (show imgW)++"x"++(show imgH)
                                          , attachedfileThumbWidth  = thumbW
                                          , attachedfileThumbHeight = thumbH
                                          }
        FileVideo -> do
          liftIO $ unlessM (doesDirectoryExist (thumbDirectory </> hashsum)) $ createDirectory (thumbDirectory </> hashsum)
          -- make thumbnail
          let thumbpath = thumbDirectory </> hashsum </> (show thumbSize ++ "thumb-" ++ filename++".png")
          void $ liftIO $ readProcess "/usr/bin/ffmpeg" ["-y","-i", uploadPath, "-vframes", "1", thumbpath] []
          (thumbW, thumbH) <- liftIO $ resizeImage thumbpath thumbpath (thumbSize,thumbSize)
          -- get video info
          info <- liftIO $ readProcess "/usr/bin/ffprobe" ["-v","quiet","-show_streams",uploadPath] []
          -- DANGEROUS! FIX THIS
          let width  = drop 1 $ dropWhile ((/=)'=') (lines info !! 9)
              height = drop 1 $ dropWhile ((/=)'=') (lines info !! 10)
          void $ runDB $ insert $ newFile { attachedfileInfo        = (show width)++"x"++(show height)
                                          , attachedfileThumbWidth  = thumbW
                                          , attachedfileThumbHeight = thumbH
                                          }
        _         -> void $ runDB $ insert newFile
    _                    -> return ())

saveFile :: FileInfo -> String -> IO FilePath
saveFile file hashsum = do
  let fn = sanitizeFileName $ unpack $ fileName file
  dirExists <- doesDirectoryExist (uploadDirectory </> hashsum)
  if dirExists
    then do
      fn':_ <- (\x -> if length x == 0 then ["ooops.404"] else x) . filter (`notElem`[".",".."]) <$> getDirectoryContents (uploadDirectory </> hashsum)
      return $ uploadFilePath fn' hashsum
    else do
      let path = uploadFilePath fn hashsum
      createDirectory (uploadDirectory </> hashsum)
      unlessM (doesFileExist path) $ fileMove file path 
      return path
-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------
detectFileType :: FileInfo -> FileType
detectFileType f
  | fileExt f `elem` video   = FileVideo
  | fileExt f `elem` audio   = FileAudio
  | fileExt f `elem` image   = FileImage
  | fileExt f `elem` flash   = FileFlash
  | fileExt f `elem` doc     = FileDoc
  | fileExt f `elem` source  = FileSource
  | fileExt f `elem` archive = FileArchive
  | otherwise                  = FileUndetected
  where video   = ["mkv", "mp4", "webm", "avi", "flv", "ogv", "wmv", "rm", "rmvb", "mpg", "mpeg"]
        audio   = ["mp3", "flac", "ogg", "aac", "m4a", "oga", "opus", "wav"]
        image   = ["jpg", "jpeg", "png", "gif", "bmp"]
        flash   = ["swf"]
        doc     = ["pdf", "djvu", "ps"]
        source  = ["c", "cpp", "hs", "lisp", "s", "m", "py", "js", "php", "pl", "rb", "java", "lua", "txt", "html"]
        archive = ["7z", "rar", "7zip", "zip", "tar", "gz", "bz2"]


getFileSize :: FilePath -> IO FileOffset
getFileSize path = fileSize <$> getFileStatus path

formatFileSize :: FileOffset -> String
formatFileSize size | b > kb    = (printf "%.2f" $ b/kb) ++ " KB"
                    | b > mb    = (printf "%.2f" $ b/mb) ++ " MB"
                    | otherwise = (printf "%.2f" $ b   ) ++ " B"
  where kb  = 1024     :: Double
        mb  = 1024^two :: Double
        two = 2 :: Int
        b   = fromIntegral size :: Double
-------------------------------------------------------------------------------------------------------------------
-- Images
-------------------------------------------------------------------------------------------------------------------
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
               FilePath        ->  -- ^ File path
               String          ->  -- ^ File name
               String          ->  -- ^ Hashsum of source file
               ImageResolution ->  -- ^ Width and height of the source file
               IO ImageResolution -- ^ Width and height of the destination file
makeThumbImg thumbSize filepath filename hashsum (width, height) = do
  unlessM (doesDirectoryExist (thumbDirectory </> hashsum)) $
    createDirectory (thumbDirectory </> hashsum)
  if height > thumbSize || width > thumbSize
    then resizeImage filepath thumbpath (thumbSize,thumbSize)
    else copyFile filepath thumbpath >> return (width, height)
    where thumbpath = thumbDirectory </> hashsum </> (show thumbSize ++ "thumb-" ++ filename)


