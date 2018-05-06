{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Utils.File where

import           Import
import           Data.Digest.OpenSSL.MD5         (md5sum)
import           Data.Conduit                    (($$))
import qualified Data.ByteString                 as BS
import qualified Data.Conduit.List               as CL
import           Data.Ratio
import           Data.Text                       (isPrefixOf)
import           Text.Printf
import           System.Directory                (copyFile, doesDirectoryExist, createDirectory, getDirectoryContents, getCurrentDirectory)
--import           Filesystem.Path.CurrentOS       (fromText)
import           Graphics.ImageMagick.MagickWand hiding (resizeImage, getImageResolution)
import qualified Graphics.ImageMagick.MagickWand as IM
import           Control.Monad.Trans.Resource    (release)
import           System.FilePath                 ((</>))
import           System.Process                  (readProcess)
import           System.Posix                    (FileOffset())
import           System.Posix.Files              (createSymbolicLink, getFileStatus, fileSize)
-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------
insertFiles :: [FormResult (Maybe FileInfo)] -> -- ^ Files
              [FormResult Censorship]       -> -- ^ Censorship ratings
              Int      -> -- ^ Thumbnail height and width
              Key Post -> -- ^ Post key
              Bool     -> -- ^ Onion
              Handler ()
insertFiles []    _       _         _      _     = return ()
insertFiles files ratings thumbSize postId onion = do
  AppSettings{..} <- appSettings <$> getYesod
  let appUploadDir' = if onion then appUploadDir </> "onion" else appUploadDir
  forM_ (zip files ratings) (\(formfile, rating) ->
    case formfile of
      FormSuccess (Just f) -> do
        hashsum    <- md5sum <$> BS.concat <$> (fileSource f $$ CL.consume) 
        uploadPath <- saveFile f hashsum onion
        filesize   <- liftIO $ formatFileSize <$> getFileSize uploadPath
        let filetype = detectFileType f
            filename = sanitizeFileName $ unpack $ fileName f
            fileext  = fileExt f
            newFile  = Attachedfile { attachedfileParentId    = postId
                                    , attachedfileHashsum     = hashsum
                                    , attachedfileName        = filename
                                    , attachedfileExtension   = fileext
                                    , attachedfileFiletype    = filetype
                                    , attachedfilePath        = uploadPath
                                    , attachedfileSize        = filesize
                                    , attachedfileThumbSize   = thumbSize
                                    , attachedfileThumbWidth  = 0
                                    , attachedfileThumbHeight = 0
                                    , attachedfileInfo        = ""
                                    , attachedfileRating      = (\form -> case form of
                                                                    FormSuccess r -> tshow r
                                                                    FormFailure _ -> "SFW") rating
                                    , attachedfileOnion       = onion
                                    }
        case filetype of
          FileImage -> do
            (imgW  , imgH  ) <- liftIO $ getImageResolution uploadPath
            (thumbW, thumbH) <- liftIO $ makeThumbImg thumbSize appUploadDir' uploadPath fileext hashsum (imgW, imgH) appAnimatedThumbs
            void $ runDB $ insert $ newFile { attachedfileInfo        = (show imgW)++"x"++(show imgH)
                                            , attachedfileThumbWidth  = thumbW
                                            , attachedfileThumbHeight = thumbH
                                            }
          FileVideo -> do
            liftIO $ unlessM (doesDirectoryExist $ appUploadDir' </> thumbDirectory) $ createDirectory (appUploadDir' </> thumbDirectory)
            -- make thumbnail
            let thumbpath = appUploadDir' </> thumbDirectory </> (show thumbSize ++ "thumb-" ++ hashsum ++ ".png")
            void $ liftIO $ readProcess (unpack appFfmpeg) ["-y","-i", uploadPath, "-vframes", "1", thumbpath] []
            (thumbW, thumbH) <- liftIO $ resizeImage thumbpath thumbpath (thumbSize,thumbSize) False False
            -- get video info
            info' <- liftIO $ readProcess (unpack appExiftool) ["-t",uploadPath] []
            let info   = parseExifInfo info'
                width  = fromMaybe "0" $ lookup "Image Width" info
                height = fromMaybe "0" $ lookup "Image Height" info
                duration = fromMaybe "N/A" $ lookup "Duration" info
            void $ runDB $ insert $ newFile { attachedfileInfo        = width++"x"++height++", "++duration
                                            , attachedfileThumbWidth  = thumbW
                                            , attachedfileThumbHeight = thumbH
                                            }
          FileAudio -> do
            info' <- liftIO $ readProcess (unpack appExiftool) ["-t",uploadPath] []
            let info      = parseExifInfo info'
                bitrate1  = lookup "Audio Bitrate" info
                bitrate2  = lookup "Nominal Bitrate" info
                bitrate   = fromMaybe "0 kbps" $ mplus bitrate1 bitrate2
                duration  = takeWhile (/=' ') $ fromMaybe "0" $ lookup "Duration" info
            void $ runDB $ insert $ newFile { attachedfileInfo        = bitrate++", "++duration
                                            }
          _         -> void $ runDB $ insert newFile
      _                    -> return ())

saveFile :: FileInfo -> String -> Bool -> Handler FilePath
saveFile file hashsum onion = do
  AppSettings{..} <- appSettings <$> getYesod
  let appUploadDir' = if onion then appUploadDir </> "onion" else appUploadDir
  let fn = sanitizeFileName $ unpack $ fileName file
  n <- storageUploadDir . entityVal . fromJust <$> runDB (selectFirst ([]::[Filter Storage]) [])
  dirExists'  <- liftIO $ doesDirectoryExist appUploadDir'
  unless dirExists' $ liftIO $ createDirectory appUploadDir
  dirExists  <- liftIO $ doesDirectoryExist (appUploadDir' </> show n)
  unless dirExists $ liftIO $ createDirectory (appUploadDir' </> show n)
  files <- liftIO $ getDirectoryContents (appUploadDir' </> show n)
  let sameName = (>0) $ length $ filter ((==) fn) files
  if sameName
    then do
      runDB $ updateWhere ([]::[Filter Storage]) [StorageUploadDir +=. 1]
      dirExists'' <- liftIO $ doesDirectoryExist (appUploadDir' </> show (n+1))
      unless dirExists'' $ liftIO $ createDirectory (appUploadDir' </> show (n+1))
      let path = appUploadDir' </> show (n+1) </> fn
      liftIO $ fileMove file path
      return path
    else do
      fileExists <- runDB $ selectFirst [AttachedfileHashsum ==. hashsum] []
      if isJust fileExists
        then do
          cd <- liftIO $ getCurrentDirectory
          let oldPath = cd ++ "/" ++ (attachedfilePath . entityVal . fromJust $ fileExists)
              path    = appUploadDir' </> show n </> fn
          liftIO $ createSymbolicLink oldPath path
          return path
        else do
          let path = appUploadDir' </> show n </> fn
          liftIO $ fileMove file path
          return path
-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------
detectFileType :: FileInfo -> FileType
detectFileType f
  | "video" `isPrefixOf` fType                  = FileVideo
  | "audio" `isPrefixOf` fType                  = FileAudio
  | "image" `isPrefixOf` fType                  = FileImage
  | flash == fType                               = FileFlash
  | fType `elem` docs                           = FileDoc
  | "text" `isPrefixOf` fType || fType `elem` js = FileSource
  | fType `elem` archive                        = FileArchive
  | otherwise                                   = FileUndetected
  where fType   = fileContentType f
        docs    = ["image/vnd.djvu", "image/x-djvu", "application/pdf"]
        js      = ["application/javascript", "application/x-javascript", "application/json"]
        flash   = "application/x-shockwave-flash"
        archive = ["application/rar", "application/zip", "application/gzip", "application/x-gzip", "application/x-rar-compressed", "application/x-7z-compressed"]

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
                   v  = if length v' > 0 then tail v' else ""
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
  readImage w (pack filepath)
  width  <- getImageWidth w
  height <- getImageHeight w
  return (width, height)

-- -- | Resizes an image file and saves the result to a new file.
-- resizeImage :: FilePath           -- ^ Source image file
--             -> FilePath           -- ^ Destination image file
--             -> ImageResolution    -- ^ The maximum dimensions of the output file
--             -> IO ImageResolution -- ^ The size of the output file
-- resizeImage from to maxSz = do
--   void $ liftIO $ readProcess "/usr/bin/convert" [from, "-coalesce", to] []
--   void $ liftIO $ readProcess "/usr/bin/convert" ["-thumbnail", show (fst maxSz)++"x"++show (snd maxSz), to, to] []
--   outSz <- liftIO $ getImageResolution to
--   return outSz

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
            -> Bool               -- ^ Is a gif or not
            -> Bool               -- ^ Animated thumbnails
            -> IO ImageResolution -- ^ The size of the output file
resizeImage from to maxSz gif animatedThumbs = withMagickWandGenesis $ do
  (_,w) <- magickWand
  readImage w (pack from)
  width  <- getImageWidth w
  height <- getImageHeight w
  let inSz                    = (width, height)
      outSz@(width', height') = calcResolution inSz maxSz
  (pointer, images) <- coalesceImages w
  numberImages <- getNumberImages images
  if gif && numberImages > 1
    then do
      (_,w1) <- magickWand
      let n = if animatedThumbs then numberImages else 2
      forM_ [1..(n-1)] $ \i -> localGenesis $ do
        images `setIteratorIndex` i
        (_,image) <- getImage images
        IM.resizeImage w width' height' lanczosFilter 1
        addImage w1 image
      resetIterator w1
      release pointer
      writeImages w1 (pack to) True
      return outSz
    else do
      IM.resizeImage w width' height' lanczosFilter 1
      setImageCompressionQuality w 95
      writeImages w (pack to) True
      release pointer
      return outSz

-- | Make a thumbnail for an image file
makeThumbImg :: Int             ->  -- ^ The maximum thumbnail width and height
               FilePath        ->  -- ^ Upload dir 
               FilePath        ->  -- ^ File path
               String          ->  -- ^ File extentions
               String          ->  -- ^ Hashsum of source file
               ImageResolution ->  -- ^ Width and height of the source file
               Bool            ->  -- ^ Animated thumbnails
               IO ImageResolution -- ^ Width and height of the destination file
makeThumbImg thumbSize appUploadDir filepath fileext hashsum (width, height) animatedThumbs = do
  unlessM (doesDirectoryExist (appUploadDir </> thumbDirectory)) $
    createDirectory (appUploadDir </> thumbDirectory)
  if height > thumbSize || width > thumbSize || fileext == "gif"
    then resizeImage filepath thumbpath (thumbSize,thumbSize) (fileext == "gif") animatedThumbs
    else copyFile filepath thumbpath >> return (width, height)
    where thumbpath = appUploadDir </> thumbDirectory </> (show thumbSize ++ "thumb-" ++ hashsum ++ "." ++ fileext)


