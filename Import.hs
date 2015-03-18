{-# LANGUAGE ExistentialQuantification, TupleSections #-}

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
import Control.Arrow        as Import (second, first, (&&&), (***))
import Control.Monad        as Import (unless, when, forM, forM_, void, join)
import Data.List            as Import (nub, intercalate)
import Data.Maybe           as Import (fromMaybe, fromJust, isJust, isNothing, catMaybes, mapMaybe)
import Data.Time            as Import (UTCTime, getCurrentTime, utctDayTime, diffUTCTime)
import Database.Persist.Sql as Import (toSqlKey, fromSqlKey)
import Text.Blaze.Html      as Import (preEscapedToHtml)
import ModelTypes           as Import 
-------------------------------------------------------------------------------------------------------------------
import           Control.Applicative     (liftA2)
import           Data.Char               (toLower, isPrint)
import           Data.Digest.OpenSSL.MD5 (md5sum)
import           Data.Geolocation.GeoIP
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           Data.Time               (addUTCTime, secondsToDiffTime)
import           Data.Time.Format        (formatTime)
import           Network.Wai
import           System.FilePath         ((</>))
import           System.Locale           (defaultTimeLocale)
import           System.Random           (randomIO)
import           Text.HTML.TagSoup      (parseTagsOptions, parseOptionsFast, Tag(TagText))
import qualified Data.ByteString.UTF8    as B
import qualified Data.Map.Strict         as MapS
import qualified Data.Text               as T (concat, toLower, append, length)
-------------------------------------------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------------------------------------------
titleDelimiter :: Text
titleDelimiter = " / "
-------------------------------------------------------------------------------------------------------------------
-- Handful functions
-------------------------------------------------------------------------------------------------------------------
whenM :: Monad m => m Bool -> m () -> m ()
whenM = (. flip when) . (>>=)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM = (. flip unless) . (>>=)

showText :: Show a => a -> Text
showText = pack . show

readText :: Read a => Text -> a
readText = read . unpack

pair :: forall t1 t2 t3. (t1 -> t2) -> (t1 -> t3) -> t1 -> (t2, t3)
pair f g x = (f x, g x)

keyValuesToMap :: (Ord k) => [(k, a)] -> MapS.Map k [a]  
keyValuesToMap = MapS.fromListWith (++) . map (\(k,v) -> (k,[v]))

-- | Add UTCTime with Integer seconds
addUTCTime' :: Int -> UTCTime -> UTCTime
addUTCTime' sec t = addUTCTime (realToFrac $ secondsToDiffTime $ toInteger sec) t
-------------------------------------------------------------------------------------------------------------------
-- Files
-------------------------------------------------------------------------------------------------------------------
thumbFileTypes :: [FileType]
thumbFileTypes = [FileVideo, FileImage, FileSource, FileDoc]

sanitizeFileName :: String -> String
sanitizeFileName = filter (\x -> x `notElem` "\\/" && isPrint x)

fileExt :: FileInfo -> String
fileExt = map toLower . reverse . takeWhile (/='.') . reverse . sanitizeFileName . unpack . fileName

extractFileExt :: String -> String
extractFileExt = map toLower . reverse . takeWhile (/='.') . reverse
-------------------------------------------------------------------------------------------------------------------
-- Paths
-------------------------------------------------------------------------------------------------------------------
geoIconPath :: Text -> Text
geoIconPath code = T.concat ["/static/geoicons/", T.toLower code, ".png"]

uploadDirectory :: FilePath
uploadDirectory = staticDir </> "upload"

captchaFilePath :: String -> String
captchaFilePath file = staticDir </> "captcha" </> file
-- Thumbnails
choseFileIcon :: FileType -> String
choseFileIcon ft = case ft of
    FileAudio      -> "audio"
    FileFlash      -> "flash"
    FileArchive    -> "archive"
    FileUndetected -> "default"
    _              -> "default"

thumbIconExt :: String
thumbIconExt = "png"

thumbDirectory :: FilePath
thumbDirectory = staticDir </> "thumb"

thumbUrlPath :: Int -> FileType -> String -> String -> FilePath
thumbUrlPath size filetype fileext hashsum = "/" </> (thumbFilePath size filetype fileext hashsum)

thumbFilePath :: Int -> FileType -> String -> String -> FilePath
thumbFilePath size filetype fileext hashsum
  | filetype == FileVideo           = thumbDirectory </> (show size ++ "thumb-" ++ hashsum ++ ".png")
  | filetype `elem` thumbFileTypes = thumbDirectory </> (show size ++ "thumb-" ++ hashsum ++ "." ++ fileext)
  | otherwise                      = staticDir </> "fileicons" </> ((choseFileIcon filetype) ++ "." ++ thumbIconExt)

-------------------------------------------------------------------------------------------------------------------
-- Handler helpers
-------------------------------------------------------------------------------------------------------------------
listPages :: Int -> Int -> [Int]
listPages elemsPerPage numberOfElems =
  [0..pagesFix $ floor $ (fromIntegral numberOfElems :: Double) / (fromIntegral elemsPerPage :: Double)]
  where pagesFix x
          | numberOfElems > 0 && numberOfElems `mod` elemsPerPage == 0 = x - 1
          | otherwise                                                = x

ignoreBoards :: Maybe Text -> Entity Board -> Maybe Text
ignoreBoards group (Entity _ b)
  | boardHidden b ||
    ( (isJust (boardViewAccess b) && isNothing group) ||
      (isJust (boardViewAccess b) && notElem (fromJust group) (fromJust $ boardViewAccess b))
    ) = Just $ boardName b
  | otherwise = Nothing

-- | Remove all HTML tags
stripTags :: Text -> Text
stripTags = foldr (T.append . textOnly) "" . parseTagsOptions parseOptionsFast
  where textOnly (TagText t) = t
        textOnly           _ = ""

-- | Check if request has X-Requested-With header
isAjaxRequest :: forall (m :: * -> *). MonadHandler m => m Bool
isAjaxRequest = do
  maybeHeader <- lookup "X-Requested-With" . requestHeaders <$> waiRequest
  return $ maybe False (=="XMLHttpRequest") maybeHeader
-------------------------------------------------------------------------------------------------------------------
-- Template helpers
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

ifelse :: Bool -> Text -> Text -> Text
ifelse x y z = if x then y else z

myFormatTime :: Int     -> -- ^ Time offset in seconds
               UTCTime -> -- ^ UTCTime
               String
myFormatTime offset t = formatTime defaultTimeLocale "%d %B %Y (%a) %H:%M:%S" $ addUTCTime' offset t

-- | Truncate file name if it's length greater than 47
truncateFileName :: String -> String
truncateFileName s = if len > maxLen then result else s
  where maxLen   = 47
        len      = length s
        excess   = len - maxLen
        halfLen  = round $ fromIntegral len    / (2 :: Double)
        halfExc  = round $ fromIntegral excess / (2 :: Double)
        splitted = splitAt halfLen s
        left     = reverse $ drop (halfExc + 2) $ reverse $ fst splitted
        right    = drop (halfExc + 2) $ snd splitted
        result   = left ++ "[..]" ++ right
-------------------------------------------------------------------------------------------------------------------
-- Widgets
-------------------------------------------------------------------------------------------------------------------
postWidget :: Maybe (Entity User)      ->
             Entity Post              -> 
             [Entity Attachedfile]    -> 
             Bool                     -> -- ^ Are we in a thread
             Bool                     -> -- ^ Have access to post
             Bool                     -> -- ^ Show parent board/thread in the upper right corner
             Bool                     -> -- ^ If geo ip enabled
             [Permission]             -> -- ^ List of the all permissions
             Int                      -> -- ^ Time offset in seconds
             Widget
postWidget muser ePost eFiles inThread canPost showParent geoIp permissions tOffset = 
  let postVal        = entityVal ePost
      sPostLocalId   = show $ postLocalId $ entityVal ePost
      postLocalId'   = postLocalId $ entityVal ePost
      sPostId        = show $ fromSqlKey  $ entityKey ePost
      postId         = fromSqlKey  $ entityKey ePost
      sThreadLocalId = show $ postParent  $ entityVal ePost
      threadLocalId  = postParent  $ entityVal ePost
      board          = postBoard $ entityVal ePost
      isThread       = sThreadLocalId == "0"
      pClass         = (if isThread then "op-post" else "reply-post") :: Text
  in $(widgetFile "post")

paginationWidget page pages route = $(widgetFile "pagination")

deleteWidget :: [Permission] -> Widget
deleteWidget permissions = $(widgetFile "delete")

adminNavbarWidget :: Maybe (Entity User) -> [Permission] -> Widget
adminNavbarWidget _ permissionsW = $(widgetFile "admin/navbar")
-------------------------------------------------------------------------------------------------------------------
bareLayout :: Yesod site => WidgetT site IO () -> HandlerT site IO Html
bareLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer [hamlet| ^{pageBody pc} |]
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
-- Some getters
-------------------------------------------------------------------------------------------------------------------
getMaybeGroup :: Maybe (Entity User) -> Handler (Maybe (Entity Group))
getMaybeGroup muser = case muser of
    Just (Entity _ u) -> runDB $ getBy $ GroupUniqName $ userGroup u
    _                 -> return Nothing
    
getBoardVal404 :: Text -> Handler Board
getBoardVal404 board = runDB (getBy $ BoardUniqName board) >>= maybe notFound (return . entityVal)

getTimeZone :: Handler Int
getTimeZone = do
  defaultZone <- extraTimezone <$> getExtra
  timezone    <- lookupSession "timezone"
  return $ maybe defaultZone readText timezone

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
getConfig f = f . entityVal . fromJust <$> runDB (selectFirst ([]::[Filter Config]) [])

getConfigEntity :: Handler Config
getConfigEntity = entityVal . fromJust <$> runDB (selectFirst ([]::[Filter Config]) [])
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
  where getIpFromHeader = lookup "X-Real-IP" . requestHeaders <$> waiRequest
        getIpFromHost = takeWhile (not . (`elem` ":")) . show . remoteHost . reqWaiRequest <$> getRequest
-------------------------------------------------------------------------------------------------------------------
-- Geo IP
-------------------------------------------------------------------------------------------------------------------  
getCountry :: Text ->                      -- ^ IP adress
             Handler (Maybe (Text,Text)) -- ^ (country code, country name)
getCountry ip = do
  dbPath <- unpack . extraGeoIPCityPath <$> getExtra
  geoIpRes <- liftIO $ openGeoDB memory_cache dbPath >>= flip geoLocateByIPAddress (encodeUtf8 ip)
  return $ ((decodeUtf8 . geoCountryCode) &&& (decodeUtf8 . geoCountryName)) <$> geoIpRes
