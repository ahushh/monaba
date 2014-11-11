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
import Database.Persist
import Database.Persist.Sql
-------------------------------------------------------------------------------------------------------------------
import Text.Blaze.Html as Import (preEscapedToHtml)
import Data.Time       as Import (UTCTime, getCurrentTime, utctDayTime, diffUTCTime)
import Data.Maybe      as Import (fromMaybe, fromJust, isJust, isNothing, catMaybes)
import Data.List       as Import (nub, intercalate)
import Control.Monad   as Import (unless, when, forM, forM_, void, join)
import Control.Arrow   as Import (second, first, (&&&), (***))
import ModelTypes      as Import 
-------------------------------------------------------------------------------------------------------------------
import           System.FilePath         ((</>))
import           System.Directory        (doesFileExist, doesDirectoryExist, createDirectory, copyFile)
import           System.Posix            (getFileStatus, fileSize, FileOffset())
import           Data.Ratio
import           Network.Wai
import           Text.Printf
import           Data.Time.Format        (formatTime)
import           System.Locale           (defaultTimeLocale)
import           GHC.Int                 (Int64)
import           Data.Char               (toLower)
import           Data.Time               (addUTCTime, secondsToDiffTime)
import qualified Data.Map.Strict          as Map

import qualified Data.ByteString.UTF8     as B

import           Control.Applicative     (liftA2)
import           Data.Digest.OpenSSL.MD5 (md5sum)
import           System.Random           (randomIO)

import qualified Data.Text               as T (concat, toLower, append, length)

import           Data.Geolocation.GeoIP

import           Text.HTML.TagSoup      (parseTagsOptions, parseOptionsFast, Tag(TagText))
-------------------------------------------------------------------------------------------------------------------
-- Templates helpers
-------------------------------------------------------------------------------------------------------------------
checkAbbr :: Int  -> -- ^ Message length
            Bool -> -- ^ Show full message
            Bool
checkAbbr len t = len > postAbbrLength && not t

-- | The maximum length of an abbreviated message
postAbbrLength :: Int
postAbbrLength = 1500

enumerate :: forall b. [b] -> [(Int, b)]
enumerate = zip [0..]
-------------------------------------------------------------------------------------------------------------------
myFormatTime :: Int     -> -- ^ Time offset in seconds
               UTCTime -> -- ^ UTCTime
               String
myFormatTime offset t = formatTime defaultTimeLocale "%d %B %Y (%a) %H:%M:%S" $ addUTCTime' offset t
-------------------------------------------------------------------------------------------------------------------
-- | Truncate file name if it's length greater than 47
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
opPostWidget :: Maybe (Entity User)      ->
               Entity Post              -> 
               [Entity Attachedfile]    -> 
               Bool                     -> -- ^ Show or not "[ Open ]" link
               Bool                     -> -- ^ Show or not the extra buttons such as "[>]"
               [Permission]             -> -- ^ List of the all permissions
               [(Key Post,(Text,Text))] -> -- ^ (Post key, (country code, country name))
               Int                      -> -- ^ Time offset in seconds
               WidgetT App IO () 
opPostWidget muserW eOpPostW opPostFilesW isInThreadW canPostW permissionsW geoIpsW tOffsetW = $(widgetFile "op-post")

replyPostWidget :: Maybe (Entity User)      ->
                  Entity Post              ->
                  [Entity Attachedfile]    ->
                  Bool                     -> -- ^ Show full (True) or abbreviated (False) messagees
                  Bool                     -> -- ^ Show or not the extra buttons such as [>]
                  Bool                     -> -- ^ Show or not parent thread in the upper right corner
                  [Permission]             -> -- ^ List of the all permissions
                  [(Key Post,(Text,Text))] -> -- ^ (Post key, (country code, country name))
                  Int                      -> -- ^ Time offset in seconds
                  WidgetT App IO ()
replyPostWidget muserW eReplyW replyFilesW isInThreadW canPostW showThreadW permissionsW geoIpsW tOffsetW = $(widgetFile "reply-post")

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
isImageFile :: String -> Bool
isImageFile filetype = filetype `elem` ["jpeg", "jpg", "gif", "png"]
-------------------------------------------------------------------------------------------------------------------
-- Access checkers
-------------------------------------------------------------------------------------------------------------------
checkAccessToReply :: Maybe (Entity Group) -> Board -> Bool
checkAccessToReply mgroup boardVal =
  let group  = (groupName . entityVal) <$> mgroup
      access = boardReplyAccess boardVal
  in isNothing access || (isJust group && elem (fromJust group) (fromJust access))

checkAccessToNewThread :: Maybe (Entity Group) -> Board -> Bool
checkAccessToNewThread mgroup boardVal =
  let group  = (groupName . entityVal) <$> mgroup
      access = boardThreadAccess boardVal
  in isNothing access || (isJust group && elem (fromJust group) (fromJust access))

checkViewAccess :: forall (m :: * -> *). MonadHandler m => Maybe (Entity Group) -> Board -> m () 
checkViewAccess mgroup boardVal =
  let group  = (groupName . entityVal) <$> mgroup
      access = boardViewAccess boardVal
  in when ( (isJust access && isNothing group) ||
            (isJust access && notElem (fromJust group) (fromJust access))
          ) notFound

getPermissions :: Maybe (Entity Group) -> [Permission]
getPermissions = maybe [] (groupPermissions . entityVal)
-------------------------------------------------------------------------------------------------------------------
-- Misc stuff
-------------------------------------------------------------------------------------------------------------------
-- | Check if request has X-Requested-With header
isAjaxRequest :: forall (m :: * -> *). MonadHandler m => m Bool
isAjaxRequest = do
  maybeHeader <- lookup "X-Requested-With" . requestHeaders <$> waiRequest
  return $ maybe False (=="XMLHttpRequest") maybeHeader

keyValuesToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]  
keyValuesToMap = Map.fromListWith (++) . map (\(k,v) -> (k,[v]))

-- | Add UTCTime with Integer seconds
addUTCTime' :: Int -> UTCTime -> UTCTime
addUTCTime' sec t = addUTCTime (realToFrac $ secondsToDiffTime $ toInteger sec) t

-- | Remove all HTML tags
stripTags :: Text -> Text
stripTags = foldr (T.append . textOnly) "" . parseTagsOptions parseOptionsFast
  where textOnly (TagText t) = t
        textOnly           _ = ""
-------------------------------------------------------------------------------------------------------------------
-- Some getters
-------------------------------------------------------------------------------------------------------------------
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

getPosterId :: Handler Text
getPosterId = do
  maybePosterId <- lookupSession "posterId"
  case maybePosterId of
    Just posterId -> return posterId
    Nothing       -> do
      posterId <- liftIO $ pack . md5sum . B.fromString <$> liftA2 (++) (show <$> (randomIO :: IO Int)) (show <$> getCurrentTime)
      setSession "posterId" posterId
      return posterId

getConfig :: forall b. (Config -> b) -> Handler b
getConfig f = f . entityVal . fromJust <$> (runDB $ selectFirst ([]::[Filter Config]) [])
-------------------------------------------------------------------------------------------------------------------
-- IP getter
-------------------------------------------------------------------------------------------------------------------
-- | Gets IP from X-Real-IP or remote-host header
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
-------------------------------------------------------------------------------------------------------------------
-- Keys
-------------------------------------------------------------------------------------------------------------------
fromKey = fromSqlKey

toKey i = toSqlKey $ fromIntegral i
-------------------------------------------------------------------------------------------------------------------
-- Monadic when and unless
-------------------------------------------------------------------------------------------------------------------
whenM :: Monad m => m Bool -> m () -> m ()
whenM = (. flip when) . (>>=)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM = (. flip unless) . (>>=)
-------------------------------------------------------------------------------------------------------------------
-- Geo IP
-------------------------------------------------------------------------------------------------------------------  
getCountry :: Text ->                      -- ^ IP adress
             Handler (Maybe (Text,Text)) -- ^ (country code, country name)
getCountry ip = do
  dbPath   <- unpack . extraGeoIPCityPath <$> getExtra
  geoIpRes <- liftIO $ openGeoDB memory_cache dbPath >>= (flip geoLocateByIPAddress $ B.fromString $ unpack ip)
  return $ ((p . geoCountryCode) &&& (p . geoCountryName)) <$> geoIpRes
    where p = pack . B.toString

getCountries :: forall t. [(Entity Post, t)] ->          -- ^ List of (entity post, files) tuples
               Handler [(Key Post, (Text, Text))] -- ^ [(Post key, (country code, country name))]
getCountries posts = do
  geoIps' <- forM posts $ \((Entity pId p),_) -> do
    c <- getCountry $ postIp p
    return (pId, c)
  return $ map (second fromJust) $ filter (isJust . snd) geoIps'
