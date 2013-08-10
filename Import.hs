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
import Data.List     as Import (nub, intercalate)
import Control.Monad as Import (unless, when, forM, forM_, void, join)
import Control.Arrow as Import (second, first, (&&&), (***))
import ModelTypes    as Import 
-------------------------------------------------------------------------------------------------------------------
import           System.FilePath         ((</>))
import           System.Directory        (doesFileExist, doesDirectoryExist, createDirectory, copyFile)
import           System.Posix            (getFileStatus, fileSize, FileOffset())
import           Data.Ratio
import           Network.Wai
import           Text.Printf
import qualified Graphics.GD             as GD
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

import qualified Data.Text               as T (concat, toLower, append, null)

import           Data.Geolocation.GeoIP

import           Text.HTML.TagSoup      (parseTagsOptions, parseOptionsFast, Tag(TagText))
-------------------------------------------------------------------------------------------------------------------
type ImageResolution = (Int, Int)
-------------------------------------------------------------------------------------------------------------------
-- Templates helpers
-------------------------------------------------------------------------------------------------------------------
myFormatTime :: Int -> UTCTime -> String
myFormatTime offset t = formatTime defaultTimeLocale "%d %B %Y (%a) %H:%M:%S" $ addUTCTime' offset t
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

opPostWidget :: Maybe (Entity User)   ->
               Entity Post           ->
               [Entity Attachedfile] ->
               Bool                  -> -- show or not "[ Open ]" link
               Bool                  -> -- show or not extra buttons such as [>]
               [Permission]          ->
               [(Key Post,(Text,Text))]  -> -- (key, (country code, country name))
               Int                   -> -- time offset in minutes
               WidgetT App IO () 
opPostWidget muserW eOpPostW opPostFilesW isInThreadW canPostW permissionsW geoIpsW tOffsetW = $(widgetFile "op-post")

replyPostWidget :: Maybe (Entity User)   ->
                  Entity Post           ->
                  [Entity Attachedfile] ->
                  Bool                  -> -- show or not extra buttons such as [>]
                  Bool                  -> -- show or not parent thread in the corner
                  [Permission]          ->
                  [(Key Post,(Text,Text))]  -> -- (key, (country code, country name))
                  Int                   -> -- time offset in minutes
                  WidgetT App IO ()
replyPostWidget muserW eReplyW replyFilesW canPostW showThreadW permissionsW geoIpsW tOffsetW = $(widgetFile "reply-post")

adminNavbarWidget :: Maybe (Entity User) -> [Permission] -> WidgetT App IO ()
adminNavbarWidget muserW permissionsW = $(widgetFile "admin/navbar")
-------------------------------------------------------------------------------------------------------------------
bareLayout :: Yesod site => WidgetT site IO () -> HandlerT site IO Html
bareLayout widget = do
    pc <- widgetToPageContent widget
    giveUrlRenderer [hamlet| ^{pageBody pc} |]
-------------------------------------------------------------------------------------------------------------------
-- Paths
-------------------------------------------------------------------------------------------------------------------
geoIconPath :: Text -> Text
geoIconPath code = T.concat ["/static/geoicons/", T.toLower code, ".png"]

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
loadImage :: FilePath -> String -> IO GD.Image
loadImage p t | t == "jpeg" || t == "jpg" = GD.loadJpegFile p
              | t == "png"              = GD.loadPngFile  p
              | t == "gif"              = GD.loadGifFile  p
loadImage _ t = error $ "error: unknown image type '"++t++"' at loadImage"

saveImage :: FilePath -> GD.Image -> [Char] -> IO ()
saveImage path img t | t == "jpeg" || t == "jpg" = GD.saveJpegFile (-1) path img
                     | t == "png"              = GD.savePngFile  path img
                     | t == "gif"              = GD.saveGifFile  path img
saveImage _    _   t = error $ "error: unknown image type '"++t++"' at saveImage"

getImageResolution :: FilePath -> String -> IO ImageResolution
getImageResolution filepath filetype = GD.imageSize =<< loadImage filepath filetype

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
            -> String             -- ^ File extension without dot
            -> IO ImageResolution -- ^ The size of the output file
resizeImage from to maxSz ext = 
    do img  <- loadImage from ext
       inSz <- GD.imageSize img
       let outSz@(w,h) = calcResolution inSz maxSz
       img' <- GD.resizeImage w h img
       saveImage to img' ext
       return outSz

makeThumbImg :: Int -> FilePath -> FilePath -> String -> ImageResolution -> IO ImageResolution
makeThumbImg thumbSize filepath filename filetype (width, height) = do
  unlessM (doesDirectoryExist (thumbDirectory </> filetype)) $
    createDirectory (thumbDirectory </> filetype)
  if (height > thumbSize || width > thumbSize)
    then resizeImage filepath thumbpath (thumbSize,thumbSize) filetype
    else copyFile filepath thumbpath >> return (width, height)
    where thumbpath = thumbFilePath thumbSize filetype filename

makeThumbNonImg :: FilePath -> String -> IO ()
makeThumbNonImg filename filetype = do
  unlessM (doesFileExist $ thumbFilePath 0 filetype filename) $ do
    let defaultIconPath = staticDir </> "icons" </> "default" ++ "." ++ thumbIconExt
        newIconPath     = staticDir </> "icons" </> filetype  ++ "." ++ thumbIconExt
    copyFile defaultIconPath newIconPath
-------------------------------------------------------------------------------------------------------------------
-- Misc stuff
-------------------------------------------------------------------------------------------------------------------
checkAccessToReply :: Maybe (Entity Group) -> Board -> Bool
checkAccessToReply mgroup boardVal =
  let group  = (groupName . entityVal) <$> mgroup
      access = boardReplyAccess boardVal
  in not (isJust access && access /= group)

checkAccessToNewThread :: Maybe (Entity Group) -> Board -> Bool
checkAccessToNewThread mgroup boardVal =
  let group  = (groupName . entityVal) <$> mgroup
      access = boardThreadAccess boardVal
  in not (isJust access && access /= group)

checkViewAccess :: forall (m :: * -> *). MonadHandler m => Maybe (Entity Group) -> Board -> m () 
checkViewAccess mgroup boardVal =
  let group  = (groupName . entityVal) <$> mgroup
      access = boardViewAccess boardVal
  in when (isJust access && access /= group) notFound

getPermissions :: Maybe (Entity Group) -> [Permission]
getPermissions = maybe [] (groupPermissions . entityVal)

getMaybeGroup :: Maybe (Entity User) -> Handler (Maybe (Entity Group))
getMaybeGroup muser = case muser of
    Just (Entity _ u) -> runDB $ getBy $ GroupUniqName $ userGroup u
    _                 -> return Nothing
    
getBoardVal404 :: Text -> Handler Board
getBoardVal404 board = do
  maybeBoard <- runDB $ getBy $ BoardUniqName board
  when (isNothing maybeBoard) notFound
  return $ entityVal $ fromJust maybeBoard

getTimeZone :: Handler Int
getTimeZone = do
  defaultZone <- extraTimezone <$> getExtra
  timezone    <- lookupSession "timezone"
  return $ maybe defaultZone (read . unpack) timezone
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

getConfig :: forall b. (Config -> b) -> Handler b
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

getCountry :: Text -> Handler (Maybe (Text,Text))
getCountry ip = do
  dbPath   <- unpack . extraGeoIPCityPath <$> getExtra
  geoIpRes <- liftIO $ openGeoDB memory_cache dbPath >>= (flip geoLocateByIPAddress $ B.fromString $ unpack ip)
  return $ ((p . geoCountryCode) &&& (p . geoCountryName)) <$> geoIpRes
    where p = pack . B.toString

getCountries :: forall t. [(Entity Post, t)] -> Handler [(Key Post, (Text, Text))]
getCountries posts = do
  geoIps' <- forM posts $ \((Entity pId p),_) -> do
    c <- getCountry $ postIp p
    return (pId, c)
  return $ map (second fromJust) $ filter (isJust . snd) geoIps'
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
-------------------------------------------------------------------------------------------------------------------
enumerate :: forall b. [b] -> [(Int, b)]
enumerate = zip [0..]
-------------------------------------------------------------------------------------------------------------------
stripTags :: Text -> Text
stripTags = foldr (T.append . textOnly) "" . parseTagsOptions parseOptionsFast
  where textOnly (TagText t) = t
        textOnly           _ = ""
