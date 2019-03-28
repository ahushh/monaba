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
import           System.Directory                (copyFile, doesDirectoryExist, createDirectory, getDirectoryContents, getCurrentDirectory, renameFile)
--import           Filesystem.Path.CurrentOS       (fromText)
--import           Graphics.ImageMagick.MagickWand hiding (resizeImage, getImageResolution)
--import qualified Graphics.ImageMagick.MagickWand as IM
import           System.FilePath                 ((</>))
import           System.Process                  (readProcess)
import           System.Posix                    (FileOffset())
import           System.Posix.Files              (createSymbolicLink, getFileStatus, fileSize)
import           System.Posix.Temp               (mkstemp)
import           GHC.IO.Handle                   (hClose)
-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------
createTempFile :: FileInfo -> Censorship -> Handler (Key TempFile)
createTempFile file rating = do
  AppSettings{..} <- appSettings <$> getYesod
  hashsum   <- md5sum <$> BS.concat <$> (fileSource file $$ CL.consume)
  (path, handle) <- liftIO $ mkstemp (appUploadDir </> "tmp/monaba") -- TODO: autoremove file after inserting
  liftIO $ hClose handle
  liftIO $ fileMove file path
  filesize  <- liftIO $ formatFileSize <$> getFileSize path
  let filetype = detectFileType file
      filename = sanitizeFileName $ unpack $ fileName file
      fileext  = fileExt file
      newFile  = TempFile { tempFileHashsum    = hashsum
                          , tempFileName       = filename
                          , tempFileExtension  = fileext
                          , tempFileFiletype   = filetype
                          , tempFilePath       = path
                          , tempFileSize       = filesize
                          , tempFileRating     = tshow rating
                          }
  runDB $ insert newFile


insertTempFile :: TempFile -> Int -> Key Post -> Bool -> Handler (Maybe (Entity Attachedfile))
insertTempFile file thumbSize postId onion = do
  uploadPath <- saveFile (pack $ tempFileName file) (tempFileHashsum file) onion (renameFile (tempFilePath file))
  let newFile  = Attachedfile { attachedfileParentId    = postId
                              , attachedfileHashsum     = tempFileHashsum file
                              , attachedfileName        = tempFileName file
                              , attachedfileExtension   = tempFileExtension file
                              , attachedfileFiletype    = tempFileFiletype file
                              , attachedfilePath        = uploadPath
                              , attachedfileSize        = tempFileSize file
                              , attachedfileThumbSize   = thumbSize
                              , attachedfileThumbWidth  = 0
                              , attachedfileThumbHeight = 0
                              , attachedfileInfo        = ""
                              , attachedfileRating      = tempFileRating file
                              , attachedfileOnion       = onion
                              }
  -- TODO: make a function from the code below
  AppSettings{..} <- appSettings <$> getYesod
  let appUploadDir' = if onion then appUploadDir </> "onion" else appUploadDir
      hashsum       = tempFileHashsum file
      filetype      = tempFileFiletype file 
      fileext       = tempFileExtension file 
  case filetype of
    FileImage -> do
      (imgW  , imgH  ) <- liftIO $ getImageResolution uploadPath (unpack appExiftool)
      (thumbW, thumbH) <- liftIO $ makeThumbImg thumbSize appUploadDir' uploadPath fileext hashsum (imgW, imgH) appAnimatedThumbs (unpack appExiftool)
      key <- runDB $ insert $ newFile { attachedfileInfo        = (show imgW)++"x"++(show imgH)
                                     , attachedfileThumbWidth  = thumbW
                                     , attachedfileThumbHeight = thumbH
                                     }
      val <- runDB $ get key
      return $ (fmap (Entity key) val)
    FileVideo -> do
      liftIO $ unlessM (doesDirectoryExist $ appUploadDir' </> thumbDirectory) $ createDirectory (appUploadDir' </> thumbDirectory)
      -- make thumbnail
      let thumbpath = appUploadDir' </> thumbDirectory </> (show thumbSize ++ "thumb-" ++ hashsum ++ ".png")
      void $ liftIO $ readProcess (unpack appFfmpeg) ["-y","-i", uploadPath, "-vframes", "1", thumbpath] []
      (thumbW, thumbH) <- liftIO $ resizeImage thumbpath thumbpath (thumbSize,thumbSize) False False (unpack appExiftool)
      -- get video info
      info' <- liftIO $ readProcess (unpack appExiftool) ["-t",uploadPath] []
      let info   = parseExifInfo info'
          width  = fromMaybe "0" $ lookup "Image Width" info
          height = fromMaybe "0" $ lookup "Image Height" info
          duration = fromMaybe "N/A" $ lookup "Duration" info
      key <- runDB $ insert $ newFile { attachedfileInfo        = width++"x"++height++", "++duration
                                     , attachedfileThumbWidth  = thumbW
                                     , attachedfileThumbHeight = thumbH
                                     }
      val <- runDB $ get key
      return $ (fmap (Entity key) val)
    FileAudio -> do
      info' <- liftIO $ readProcess (unpack appExiftool) ["-t",uploadPath] []
      let info      = parseExifInfo info'
          bitrate1  = lookup "Audio Bitrate" info
          bitrate2  = lookup "Nominal Bitrate" info
          bitrate   = fromMaybe "0 kbps" $ mplus bitrate1 bitrate2
          duration  = takeWhile (/=' ') $ fromMaybe "0" $ lookup "Duration" info
      key <- runDB $ insert $ newFile { attachedfileInfo = bitrate++", "++duration }
      val <- runDB $ get key
      return $ (fmap (Entity key) val)
    

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
        uploadPath <- saveFile (fileName f) hashsum onion (fileMove f)
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
            (imgW  , imgH  ) <- liftIO $ getImageResolution uploadPath (unpack appExiftool)
            (thumbW, thumbH) <- liftIO $ makeThumbImg thumbSize appUploadDir' uploadPath fileext hashsum (imgW, imgH) appAnimatedThumbs (unpack appExiftool)
            void $ runDB $ insert $ newFile { attachedfileInfo        = (show imgW)++"x"++(show imgH)
                                            , attachedfileThumbWidth  = thumbW
                                            , attachedfileThumbHeight = thumbH
                                            }
          FileVideo -> do
            liftIO $ unlessM (doesDirectoryExist $ appUploadDir' </> thumbDirectory) $ createDirectory (appUploadDir' </> thumbDirectory)
            -- make thumbnail
            let thumbpath = appUploadDir' </> thumbDirectory </> (show thumbSize ++ "thumb-" ++ hashsum ++ ".png")
            void $ liftIO $ readProcess (unpack appFfmpeg) ["-y","-i", uploadPath, "-vframes", "1", thumbpath] []
            (thumbW, thumbH) <- liftIO $ resizeImage thumbpath thumbpath (thumbSize,thumbSize) False False (unpack appExiftool)
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

-- saveFile :: Text -> String -> Bool -> Handler FilePath
saveFile :: Text -> [Char] -> Bool -> (FilePath -> IO a) -> HandlerFor App FilePath
saveFile filename hashsum onion move = do
  AppSettings{..} <- appSettings <$> getYesod
  let appUploadDir' = if onion then appUploadDir </> "onion" else appUploadDir
  let fn = sanitizeFileName $ unpack filename
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
      liftIO $ move path
      -- liftIO $ fileMove file path
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
          -- liftIO $ fileMove file path
          liftIO $ move path
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
getImageResolution :: FilePath -> FilePath -> IO ImageResolution
getImageResolution filepath exiftoolPath = do
  info' <- liftIO $ readProcess exiftoolPath ["-t", filepath] []
  let info   = parseExifInfo info'
      width  = fromMaybe "0" $ lookup "Image Width" info
      height = fromMaybe "0" $ lookup "Image Height" info

  return (read width, read height)

-- | Resizes an image file and saves the result to a new file.
resizeImage :: FilePath           -- ^ Source image file
            -> FilePath           -- ^ Destination image file
            -> ImageResolution    -- ^ The maximum dimensions of the output file
            -> Bool               -- ^ Is a gif or not
            -> Bool               -- ^ Animated thumbnails
            -> FilePath           -- ^ Exiftool path
            -> IO ImageResolution -- ^ The size of the output file
resizeImage from to maxSz gif animatedThumbs exiftoolPath = do
  void $ liftIO $ readProcess "/usr/bin/convert" [from, "-coalesce", to] []
  void $ liftIO $ readProcess "/usr/bin/convert" ["-thumbnail", show (fst maxSz)++"x"++show (snd maxSz), to, to] []
  outSz <- liftIO $ getImageResolution to exiftoolPath
  return outSz

calcResolution :: ImageResolution -> ImageResolution -> ImageResolution
calcResolution (inW,inH) (outW,outH)
    | inAspect >  outAspect = (outW, round (fromIntegral outW / inAspect))
    | inAspect <  outAspect = (round (fromIntegral outH * inAspect), outH)
    | otherwise             = (outW, outH)
    where inAspect  = inW  % inH
          outAspect = outW % outH

-- | Make a thumbnail for an image file
makeThumbImg :: Int             ->  -- ^ The maximum thumbnail width and height
               FilePath        ->  -- ^ Upload dir 
               FilePath        ->  -- ^ File path
               String          ->  -- ^ File extentions
               String          ->  -- ^ Hashsum of source file
               ImageResolution ->  -- ^ Width and height of the source file
               Bool            ->  -- ^ Animated thumbnails
               FilePath        ->   -- ^ Exiftool path
               IO ImageResolution -- ^ Width and height of the destination file
makeThumbImg thumbSize appUploadDir filepath fileext hashsum (width, height) animatedThumbs exiftoolPath = do
  unlessM (doesDirectoryExist (appUploadDir </> thumbDirectory)) $
    createDirectory (appUploadDir </> thumbDirectory)
  if height > thumbSize || width > thumbSize || fileext == "gif"
    then resizeImage filepath thumbpath (thumbSize,thumbSize) (fileext == "gif") animatedThumbs exiftoolPath
    else copyFile filepath thumbpath >> return (width, height)
    where thumbpath = appUploadDir </> thumbDirectory </> (show thumbSize ++ "thumb-" ++ hashsum ++ "." ++ fileext)


