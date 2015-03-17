{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Utils.File where

import           Import
import           Prelude                         as P
import           Data.Digest.OpenSSL.MD5         (md5sum)
import           Data.Conduit                    (($$))
import qualified Data.ByteString                 as BS
import qualified Data.Conduit.List               as CL
import           Control.Monad                   (mplus)
import           Data.Ratio
import           Text.Printf
import           System.Directory                (copyFile, doesFileExist, doesDirectoryExist, createDirectory, getDirectoryContents, getCurrentDirectory)
import           Filesystem.Path.CurrentOS       (fromText)
import           Graphics.ImageMagick.MagickWand hiding (resizeImage, getImageResolution)
import qualified Graphics.ImageMagick.MagickWand as IM
import           System.FilePath                 ((</>))
import           System.Process                  (readProcess)
import           System.Posix                    (FileOffset())
import           System.Posix.Files              (createSymbolicLink, getFileStatus, fileSize)
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
      uploadPath <- saveFile f hashsum
      filesize   <- liftIO $ formatFileSize <$> getFileSize uploadPath
      let filetype = detectFileType f
          filename = sanitizeFileName $ unpack $ fileName f
          fileext  = fileExt f
          newFile  = Attachedfile { attachedfileParentId    = postId
                                  , attachedfileHashsum     = hashsum
                                  , attachedfileName        = filename
                                  , attachedfileExtension   = fileext
                                  , attachedfileType        = detectFileType f
                                  , attachedfilePath        = uploadPath
                                  , attachedfileSize        = filesize
                                  , attachedfileThumbSize   = thumbSize
                                  , attachedfileThumbWidth  = 0
                                  , attachedfileThumbHeight = 0
                                  , attachedfileInfo        = ""
                                  }
      case filetype of
        FileImage -> do
          (imgW  , imgH  ) <- liftIO $ getImageResolution uploadPath
          (thumbW, thumbH) <- liftIO $ makeThumbImg thumbSize uploadPath fileext hashsum (imgW, imgH)
          void $ runDB $ insert $ newFile { attachedfileInfo        = (show imgW)++"x"++(show imgH)
                                          , attachedfileThumbWidth  = thumbW
                                          , attachedfileThumbHeight = thumbH
                                          }
        FileVideo -> do
          liftIO $ unlessM (doesDirectoryExist thumbDirectory) $ createDirectory thumbDirectory
          -- make thumbnail
          let thumbpath = thumbDirectory </> (show thumbSize ++ "thumb-" ++ hashsum ++ ".png")
          void $ liftIO $ readProcess "/usr/bin/ffmpeg" ["-y","-i", uploadPath, "-vframes", "1", thumbpath] []
          (thumbW, thumbH) <- liftIO $ resizeImage thumbpath thumbpath (thumbSize,thumbSize)
          -- get video info
          info' <- liftIO $ readProcess "/usr/bin/exiftool" ["-t",uploadPath] []
          let info   = parseExifInfo info'
              width  = fromMaybe "0" $ lookup "Image Width" info
              height = fromMaybe "0" $ lookup "Image Height" info
              duration = fromMaybe "N/A" $ lookup "Duration" info
          void $ runDB $ insert $ newFile { attachedfileInfo        = width++"x"++height++", "++duration
                                          , attachedfileThumbWidth  = thumbW
                                          , attachedfileThumbHeight = thumbH
                                          }
        FileAudio -> do
          info' <- liftIO $ readProcess "/usr/bin/exiftool" ["-t",uploadPath] []
          let info      = parseExifInfo info'
              bitrate1  = lookup "Audio Bitrate" info
              bitrate2  = lookup "Nominal Bitrate" info
              bitrate   = fromMaybe "0 kbps" $ mplus bitrate1 bitrate2
              duration  = takeWhile (/=' ') $ fromMaybe "0" $ lookup "Duration" info
          void $ runDB $ insert $ newFile { attachedfileInfo        = bitrate++", "++duration
                                          }
        _         -> void $ runDB $ insert newFile
    _                    -> return ())

saveFile :: FileInfo -> String -> Handler FilePath
saveFile file hashsum = do
  let fn = sanitizeFileName $ unpack $ fileName file
  n <- storageUploadDir . entityVal . fromJust <$> runDB (selectFirst ([]::[Filter Storage]) [])
  dirExists  <- liftIO $ doesDirectoryExist (uploadDirectory </> show n)
  unless dirExists $ liftIO $ createDirectory (uploadDirectory </> show n)
  files <- liftIO $ getDirectoryContents (uploadDirectory </> show n)
  let sameName = (>0) $ length $ filter ((==) $ unpack $ fileName file) files
  if sameName
    then do
      runDB $ updateWhere ([]::[Filter Storage]) [StorageUploadDir +=. 1]
      dirExists <- liftIO $ doesDirectoryExist (uploadDirectory </> show (n+1))
      liftIO $ createDirectory (uploadDirectory </> show (n+1))
      let path = uploadDirectory </> show (n+1) </> (unpack $ fileName file)
      liftIO $ fileMove file path
      return path
    else do
      fileExists <- runDB $ selectFirst [AttachedfileHashsum ==. hashsum] []
      if isJust fileExists
        then do
          cd <- liftIO $ getCurrentDirectory
          let oldPath = cd ++ "/" ++ (attachedfilePath . entityVal . fromJust $ fileExists)
              path    = uploadDirectory </> show n </> (unpack $ fileName file)
          liftIO $ createSymbolicLink oldPath path
          return path
        else do
          let path = uploadDirectory </> show n </> (unpack $ fileName file)
          liftIO $ fileMove file path
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
formatFileSize size | b > mb    = (printf "%.2f" $ b/mb) ++ " MB"
                    | b > kb    = (printf "%.2f" $ b/kb) ++ " KB"
                    | otherwise = (printf "%.0f" $ b   ) ++ " B"
  where kb  = 1024    :: Double
        mb  = 1048576 :: Double
        b   = fromIntegral size :: Double

parseExifInfo :: String -> [(String,String)]
parseExifInfo = filter f2 . map f1 . lines
  where f1 x = let k  = takeWhile (/='\t') x
                   v' = dropWhile (/='\t') x
                   v  = if length v' > 0 then P.tail v' else ""
               in (k,v)
        f2 (x,y) = x /= y && x /= "" && y /= ""

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
               String          ->  -- ^ File extentions
               String          ->  -- ^ Hashsum of source file
               ImageResolution ->  -- ^ Width and height of the source file
               IO ImageResolution -- ^ Width and height of the destination file
makeThumbImg thumbSize filepath fileext hashsum (width, height) = do
  unlessM (doesDirectoryExist (thumbDirectory </> hashsum)) $
    createDirectory (thumbDirectory </> hashsum)
  if height > thumbSize || width > thumbSize
    then resizeImage filepath thumbpath (thumbSize,thumbSize)
    else copyFile filepath thumbpath >> return (width, height)
    where thumbpath = thumbDirectory </> (show thumbSize ++ "thumb-" ++ hashsum ++ "." ++ fileext)


