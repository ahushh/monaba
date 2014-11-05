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
import Text.Blaze.Html as Import (preEscapedToHtml)
import Data.Time       as Import (UTCTime, getCurrentTime, utctDayTime, diffUTCTime)
import Data.Maybe      as Import (fromMaybe, fromJust, isJust, isNothing, catMaybes, mapMaybe)
import Data.List       as Import (nub, intercalate)
import Control.Monad   as Import (unless, when, forM, forM_, void, join)
import Control.Arrow   as Import (second, first, (&&&), (***))
import ModelTypes      as Import 
-------------------------------------------------------------------------------------------------------------------
import           Control.Applicative     (liftA2)

import qualified Data.Text               as T
import qualified Data.ByteString.UTF8    as B
import qualified Data.Map.Strict         as MapS
import           Data.Char               (toLower)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)

import           System.Random           (randomIO)
import           System.FilePath         ((</>))
import           System.Directory        (doesFileExist, doesDirectoryExist, createDirectory)
import           System.Posix            (getFileStatus, fileSize, FileOffset())

import           Data.Digest.OpenSSL.MD5 (md5sum)

import           Data.Time               (addUTCTime, secondsToDiffTime)
import           Data.Time.Format        (formatTime)
import           System.Locale           (defaultTimeLocale)
import           Text.Printf

import           Network.Wai
import           Yesod.Routes.Class      (Route(..))

import           GHC.Int                 (Int64)
import           Data.Geolocation.GeoIP
import           Text.HTML.TagSoup       (parseTagsOptions, parseOptionsFast, Tag(TagText))

import           Yesod.Auth              (maybeAuth)

import           Database.Persist
import           Database.Persist.Sql
-------------------------------------------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------------------------------------------
data APIError = ApiNoSuchThread
              | ApiNoDeletedPosts
              | ApiNoLastPosts
              | ApiEmptyThread
              | ApiNoNewPosts
              | ApiBadThreadID
              | ApiNoCaptchaInDB
              | ApiBadBoardName
              deriving (Show, Ord, Read, Eq, Bounded, Enum)

instance ToJSON APIError where
  toJSON x = String $ pack $ show x
-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------
titleDelimiter :: Text
titleDelimiter = " :: "
-------------------------------------------------------------------------------------------------------------------
-- Templates helpers
-------------------------------------------------------------------------------------------------------------------
checkRating :: Text -> Censorship -> Bool
checkRating ratingF rating = T.null ratingF || (read $ unpack ratingF) <= rating

int64ToInt :: Int64 -> Int
int64ToInt = fromIntegral

ifelse :: Bool -> Text -> Text -> Text
ifelse x y z = if x then y else z

enumerate :: forall b. [b] -> [(Int, b)]
enumerate = zip [0..]

checkHellbanned :: Entity Post -> [Permission] -> Text -> Bool
checkHellbanned post permissions posterId = not (postHellbanned $ entityVal post) ||
                                            elem HellBanP permissions           ||
                                            (postPosterId (entityVal post) == posterId)

checkAbbr :: Int  -> -- ^ Message length
            Bool -> -- ^ Show full message
            Bool
checkAbbr len t = len > postAbbrLength && not t

-- | The maximum length of an abbreviated message
postAbbrLength :: Int
postAbbrLength = 1500

myFormatTime :: Int     -> -- ^ Time offset in seconds
               UTCTime -> -- ^ UTCTime to format
               String
myFormatTime offset t = formatTime defaultTimeLocale "%d %B %Y (%a) %H:%M:%S" $ addUTCTime' offset t

-- | Truncate a file name if its length is greater than maxLen
truncateFileName :: Int -> String -> String
truncateFileName maxLen s = if len > maxLen then result else s
  where len      = length s
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
postWidget :: Entity Post              -> 
             [Entity Attachedfile]    -> 
             Censorship               -> -- ^ Max allowed rating
             Bool                     -> -- ^ Display sage icon
             Bool                     -> -- ^ Are we in a thread
             Bool                     -> -- ^ Have access to post
             Bool                     -> -- ^ Show parent board/thread in the upper right corner
             Bool                     -> -- ^ If geo ip enabled
             [Permission]             -> -- ^ List of the all permissions
             Int                      -> -- ^ Time offset in seconds
             Int                      -> -- ^ Max file name length
             Bool                     -> -- ^ Show date
             Bool                     -> -- ^ Show editing history
             Widget
postWidget ePost eFiles rating sage inThread canPost
  showParent geoIp permissions tOffset maxLenOfFileName showPostDate showEditHistory = 
  let postVal   = entityVal ePost
      sPostId   = show $ postLocalId $ entityVal ePost
      sThreadId = show $ postParent  $ entityVal ePost
      sPostKey  = show $ fromKey     $ entityKey ePost
      board     = postBoard $ entityVal ePost
      isThread  = sThreadId == "0"
      pClass    = (if isThread then "opening post" else "reply post") :: Text
      pId       = if isThread then "post-"++sPostId++"-0-"++unpack board else "post-"++sPostId++"-"++sThreadId++"-"++unpack board
  in $(widgetFile "post")
             
adminNavbarWidget :: Maybe (Entity User) -> [Permission] -> Widget
adminNavbarWidget muser permissions = $(widgetFile "admin/navbar")

-- pageSwitcherWidget :: Int -> [Int] -> (Int -> Route App) -> Widget
pageSwitcherWidget page pages route = $(widgetFile "page-switcher")

deleteWidget :: [Permission] -> Widget
deleteWidget permissions = $(widgetFile "delete")
-------------------------------------------------------------------------------------------------------------------
bareLayout :: Widget -> Handler Html
bareLayout widget = do
    pc <- widgetToPageContent widget
    giveUrlRenderer [hamlet| ^{pageBody pc} |]
-------------------------------------------------------------------------------------------------------------------
-- Paths
-------------------------------------------------------------------------------------------------------------------
geoIconPath :: Text -> Text
geoIconPath code = "/static/geoicons/" <> T.toLower code <> ".png"

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
formatFileSize size | b > mb    = printf "%.2f" (b/mb) ++ " MB"
                    | b > kb    = printf "%.2f" (b/kb) ++ " KB"
                    | otherwise = printf "%.0f" b      ++ " B"
  where kb  = 1024    :: Double
        mb  = 1048576 :: Double
        b   = fromIntegral size :: Double
-------------------------------------------------------------------------------------------------------------------
-- | Save a file to the upload directory
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

-- FIXME: rename to checkViewAccess
bcheckViewAccess :: Maybe (Entity Group) -> Board -> Bool
bcheckViewAccess mgroup boardVal =
  let group  = (groupName . entityVal) <$> mgroup
      access = boardViewAccess boardVal
  in not ((isJust access && isNothing group) || (isJust access && notElem (fromJust group) (fromJust access)))

-- FIXME: rename to checkViewAccess404
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
-- | Check if a request has X-Requested-With header
isAjaxRequest :: forall (m :: * -> *). MonadHandler m => m Bool
isAjaxRequest = do
  maybeHeader <- lookup "X-Requested-With" . requestHeaders <$> waiRequest
  return $ maybe False (=="XMLHttpRequest") maybeHeader

keyValuesToMap :: (Ord k) => [(k, a)] -> MapS.Map k [a]  
keyValuesToMap = MapS.fromListWith (++) . map (\(k,v) -> (k,[v]))

-- | Add a UTCTime with Integer seconds
addUTCTime' :: Int -> UTCTime -> UTCTime
addUTCTime' sec t = addUTCTime (realToFrac $ secondsToDiffTime $ toInteger sec) t

-- | Remove all HTML tags
stripTags :: Text -> Text
stripTags = foldr (T.append . textOnly) "" . parseTagsOptions parseOptionsFast
  where textOnly (TagText t) = t
        textOnly           _ = ""

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

pair :: forall t1 t2 t3. (t1 -> t2) -> (t1 -> t3) -> t1 -> (t2, t3)
pair f g x = (f x, g x)

pair' :: a -> (a,a)
pair' x = (x,x)

showText :: Show a => a -> Text
showText = pack . show

readText :: Read a => Text -> a
readText = read . unpack
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

getCensorshipRating :: Handler Censorship
getCensorshipRating = do
  mRating <- lookupSession "censorship-rating"
  case mRating of
    Just rating -> return $ readText rating
    Nothing     -> setSession "censorship-rating" "SFW" >> return SFW

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

getHiddenThreads :: Text -> Handler [Int]
getHiddenThreads board = do
  ht <- lookupSession "hidden-threads"
  case ht of
    Just xs -> return $ fromMaybe [] $ lookup board (read (unpack xs) :: [(Text, [Int])])
    Nothing -> setSession "hidden-threads" "[]" >> return []

getAllHiddenThreads :: Handler [(Text, [Int])]
getAllHiddenThreads = do
  ht <- lookupSession "hidden-threads"
  case ht of
    Just xs -> return $ readText xs
    Nothing -> setSession "hidden-threads" "[]" >> return []

getRecentBoards :: Handler [Text]
getRecentBoards = do
  bs <- lookupSession "recent-ignore-boards"
  case bs of
    Just xs -> return $ readText xs
    Nothing -> setSession "recent-ignore-boards" "[]" >> return []
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
        getIpFromHost   = takeWhile (not . (`elem` ":")) . show . remoteHost . reqWaiRequest <$> getRequest
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
  geoIpRes <- liftIO $ openGeoDB memory_cache dbPath >>= flip geoLocateByIPAddress (encodeUtf8 ip)
  return $ ((decodeUtf8 . geoCountryCode) &&& (decodeUtf8 . geoCountryName)) <$> geoIpRes
-------------------------------------------------------------------------------------------------------------------
-- Board stats
-------------------------------------------------------------------------------------------------------------------
getBoardStats :: Handler [(Text,Int,Int)]
getBoardStats = do
  mgroup     <- getMaybeGroup =<< maybeAuth
  maybeStats <- lookupSession "board-stats"
  case maybeStats of
    Just s  -> return $ readText s
    Nothing -> do
      posterId <- getPosterId
      boards <- mapMaybe (ignoreBoards' $ fmap (groupName . entityVal) mgroup) <$> runDB (selectList ([]::[Filter Board]) [])
      hiddenThreads <- getAllHiddenThreads
      stats  <- runDB $ forM boards $ \b -> do
                  lastPost <- selectFirst [PostBoard ==. b, PostDeleted ==. False, PostHellbanned ==. False, PostPosterId !=. posterId
                                         ,PostParent /<-. concatMap snd (filter ((==b).fst) hiddenThreads)] [Desc PostLocalId]
                  return (b, maybe 0 (postLocalId . entityVal) lastPost, 0)
      saveBoardStats stats
      return stats
  where ignoreBoards' group (Entity _ b)
          | boardHidden b ||
            ( (isJust (boardViewAccess b) && isNothing group) ||
              (isJust (boardViewAccess b) && notElem (fromJust group) (fromJust $ boardViewAccess b))
            ) = Nothing
          | otherwise = Just $ boardName b

saveBoardStats :: [(Text,Int,Int)] -> Handler ()
saveBoardStats stats = do
  deleteSession "board-stats"
  setSession "board-stats" $ showText stats

cleanBoardStats :: Text -> Handler ()
cleanBoardStats board = do
  hiddenThreads <- getAllHiddenThreads
  oldStats <- getBoardStats
  newStats <- forM oldStats $ \s@(b,_,_) ->
    if b == board
    then do
      lastPost <- runDB $ selectFirst [PostBoard ==. b, PostDeleted ==. False, PostHellbanned ==. False
                                      ,PostParent /<-. concatMap snd (filter ((==b).fst) hiddenThreads)] [Desc PostLocalId]
      return (b, maybe 0 (postLocalId . entityVal) lastPost, 0)
    else return s
  saveBoardStats newStats
