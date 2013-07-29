{-# LANGUAGE ExistentialQuantification #-}

module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text, unpack, pack)
 
import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
-------------------------------------------------------------------------------------------------------------------
import Data.Time     as Import (UTCTime, getCurrentTime, utctDayTime, diffUTCTime)
import Data.Maybe    as Import (fromMaybe, fromJust, isJust, isNothing, catMaybes)
import Data.List     as Import (nub)
import Control.Monad as Import (unless, when, forM, forM_, void)
import ModelTypes    as Import 
-------------------------------------------------------------------------------------------------------------------
import           Yesod.Auth
import           Database.Persist.Sql    (SqlBackend)
import           System.FilePath         ((</>))
import           System.Directory        (doesFileExist, doesDirectoryExist, createDirectory, copyFile)
import           System.Posix            (getFileStatus, fileSize, FileOffset())
import           Network.Wai
import           Text.Printf
import           System.Process          (runCommand, waitForProcess)
import           Graphics.GD
import           Data.Time.Format        (formatTime)
import           System.Locale           (defaultTimeLocale)
import           GHC.Int                 (Int64)
import           Text.Blaze.Html         (preEscapedToHtml)
import           Data.Char               (toLower)
import           Data.Time               (addUTCTime, secondsToDiffTime)
import qualified Data.Map.Strict          as Map

import qualified Data.ByteString.UTF8     as B

import           Control.Applicative     (liftA2)
import           Data.Digest.OpenSSL.MD5 (md5sum)
import           System.Random           (randomIO)
-------------------------------------------------------------------------------------------------------------------
-- Templates helpers
-------------------------------------------------------------------------------------------------------------------
myFormatTime :: UTCTime -> String
myFormatTime t = formatTime defaultTimeLocale "%d %B %Y (%a) %H:%M:%S" t
-------------------------------------------------------------------------------------------------------------------
truncateFileName :: String -> String
truncateFileName s = if len > maxLen then result else s
  where maxLen   = 47
        len      = length s
        excess   = len - maxLen
        halfLen  = round $ (fromIntegral len)    / (2 :: Double)
        halfExc  = round $ (fromIntegral excess) / (2 :: Double)
        splitted = splitAt halfLen s
        left     = reverse $ drop (halfExc + 2) $ reverse $ fst splitted
        right    = drop (halfExc + 2) $ snd splitted
        result   = left ++ "[..]" ++ right
-------------------------------------------------------------------------------------------------------------------
-- Widgets
-------------------------------------------------------------------------------------------------------------------
markupWidget :: Textarea -> Widget
markupWidget = toWidget . preEscapedToHtml . unTextarea

opPostWidget :: Maybe (Entity Person) -> Entity Post -> [Entity Attachedfile] -> Bool -> WidgetT App IO () 
opPostWidget muserW eOpPostW opPostFilesW isInThread = $(widgetFile "op-post")

replyPostWidget :: Maybe (Entity Person) -> Entity Post -> [Entity Attachedfile] -> WidgetT App IO ()
replyPostWidget muserW eReplyW replyFilesW = $(widgetFile "reply-post")

headerWidget :: Maybe (Entity Person) -> [Entity Board] -> [Text] -> WidgetT App IO ()
headerWidget muserW boardsW categoriesW = $(widgetFile "header")

footerWidget :: WidgetT App IO ()
footerWidget = $(widgetFile "footer")

adminNavbarWidget :: Maybe (Entity Person) -> WidgetT App IO ()
adminNavbarWidget muserW = $(widgetFile "admin/navbar")
-------------------------------------------------------------------------------------------------------------------
bareLayout :: Yesod site => WidgetT site IO () -> HandlerT site IO Html
bareLayout widget = do
    pc <- widgetToPageContent widget
    giveUrlRenderer [hamlet| ^{pageBody pc} |]
-------------------------------------------------------------------------------------------------------------------
-- Paths
-------------------------------------------------------------------------------------------------------------------
uploadDirectory :: FilePath
uploadDirectory = staticDir </> "files"

imageFilePath :: String -> String -> FilePath
imageFilePath filetype filename = uploadDirectory </> filetype </> filename

imageUrlPath :: String -> String -> FilePath
imageUrlPath filetype filename = ("/" </>) $ imageFilePath filetype filename

captchaFilePath :: String -> String
captchaFilePath file = staticDir </> "captcha" </> file
-------------------------------------------------------------------------------------------------------------------
thumbIconExt :: String
thumbIconExt = "png"

thumbDirectory :: FilePath
thumbDirectory = staticDir </> "thumb"

thumbFilePath :: Int -> String -> String -> FilePath
thumbFilePath size filetype filename
  | isImageFile filetype = thumbDirectory </> filetype </> (show size ++ "-" ++ filename)
  | otherwise            = staticDir </> "icons" </> filetype ++ "." ++ thumbIconExt

thumbUrlPath :: Int -> String -> String -> FilePath
thumbUrlPath size filetype filename = ("/" </>) $ thumbFilePath size filetype filename
-------------------------------------------------------------------------------------------------------------------
-- File processing
-------------------------------------------------------------------------------------------------------------------
typeOfFile :: FileInfo -> String
typeOfFile = map toLower . reverse . takeWhile (/='.') . reverse . unpack . fileName

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
writeToServer :: FileInfo -> String -> IO (FilePath, FilePath)
writeToServer file md5 = do
    let filetype = typeOfFile file
        filename = md5 ++ "." ++ filetype
        path     = imageFilePath filetype filename
    
    unlessM (doesDirectoryExist (uploadDirectory </> filetype)) $
      createDirectory (uploadDirectory </> filetype)
      
    unlessM (liftIO $ doesFileExist path) $
      fileMove file path 
    return (unpack $ fileName file, filename)
-------------------------------------------------------------------------------------------------------------------
-- Images
-------------------------------------------------------------------------------------------------------------------
getImageResolution :: FilePath -> String -> IO (Int, Int)
getImageResolution filepath filetype = do
  imageSize =<< loadImage filepath filetype
  where loadImage p t | t == "jpeg" || t == "jpg" = loadJpegFile p
                      | t == "png"              = loadPngFile  p
                      | t == "gif"              = loadGifFile  p
        loadImage _ _ = error "error: unknown image type at getImageResolution"
        
makeThumbImg :: Int -> FilePath -> FilePath -> String -> (Int,Int) -> IO ()
makeThumbImg thumbSize filepath filename filetype imageresolution = do
  unlessM (doesDirectoryExist (thumbDirectory </> filetype)) $
    createDirectory (thumbDirectory </> filetype)
  if ((snd imageresolution) > thumbSize || (fst imageresolution) > thumbSize)
    then runCommand cmd >>= waitForProcess >> return ()
    else copyFile filepath thumbpath >> return ()
    where cmd       = "convert -resize "++ show thumbSize ++"x"++ show thumbSize ++ "\\> " ++ filepath ++ " " ++ thumbpath
          thumbpath = thumbFilePath thumbSize filetype filename

makeThumbNonImg :: FilePath -> String -> IO ()
makeThumbNonImg filename filetype = do
  unlessM (doesFileExist $ thumbFilePath 0 filetype filename) $ do
    let defaultIconPath = staticDir </> "icons" </> "default" ++ "." ++ thumbIconExt
        newIconPath     = staticDir </> "icons" </> filetype  ++ "." ++ thumbIconExt
    copyFile defaultIconPath newIconPath
-------------------------------------------------------------------------------------------------------------------
-- Misc stuff
-------------------------------------------------------------------------------------------------------------------
fromKey :: forall backend entity. KeyBackend backend entity -> Int64
fromKey = (\(PersistInt64 n) -> n) . unKey 

toKey :: forall backend entity a. Integral a => a -> KeyBackend backend entity
toKey i = Key $ PersistInt64 $ fromIntegral i
-------------------------------------------------------------------------------------------------------------------
whenM :: Monad m => m Bool -> m () -> m ()
whenM = (. flip when) . (>>=)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM = (. flip unless) . (>>=)
-------------------------------------------------------------------------------------------------------------------
keyValuesToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]  
keyValuesToMap = Map.fromListWith (++) . map (\(k,v) -> (k,[v]))

isImageFile :: String -> Bool
isImageFile filetype = filetype `elem` ["jpeg", "jpg", "gif", "png"]

addUTCTime' :: Int -> UTCTime -> UTCTime
addUTCTime' sec t = addUTCTime (realToFrac $ secondsToDiffTime $ toInteger sec) t


getConfig :: forall b site.
             (YesodPersist site,
              PersistQuery (YesodPersistBackend site (HandlerT site IO)),
              PersistMonadBackend (YesodPersistBackend site (HandlerT site IO))
              ~ SqlBackend) =>
             (Config -> b) -> HandlerT site IO b
getConfig f = f . entityVal . fromJust <$> (runDB $ selectFirst ([]::[Filter Config]) [])
-------------------------------------------------------------------------------------------------------------------
getIp :: forall (m :: * -> *). MonadHandler m => m String
getIp = do
  maybeIp <- getIpFromHeader 
  case maybeIp of
    Just ip -> return $ B.toString ip
    Nothing -> getIpFromHost
       
getIpFromHeader :: forall (f :: * -> *). MonadHandler f => f (Maybe B.ByteString)
getIpFromHeader = lookup "X-Real-IP" . requestHeaders <$> waiRequest

getIpFromHost :: forall (f :: * -> *). MonadHandler f => f [Char]
getIpFromHost = takeWhile (not . (`elem` ":")) . show . remoteHost . reqWaiRequest <$> getRequest

isAjaxRequest :: forall (m :: * -> *). MonadHandler m => m Bool
isAjaxRequest = do
  maybeHeader <- lookup "X-Requested-With" . requestHeaders <$> waiRequest
  return $ maybe False (=="XMLHttpRequest") maybeHeader
-------------------------------------------------------------------------------------------------------------------
getPosterId :: Handler Text
getPosterId = do
  maybePosterId <- lookupSession "posterId"
  case maybePosterId of
    Just posterId -> return posterId
    Nothing       -> do
      posterId <- liftIO $ pack . md5sum . B.fromString <$> liftA2 (++) (show <$> (randomIO :: IO Int)) (show <$> getCurrentTime)
      setSession "posterId" posterId
      return posterId
