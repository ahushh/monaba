module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

import Control.Arrow        as Import (first, second, (&&&), (***))
import Database.Persist.Sql as Import (toSqlKey, fromSqlKey)

import           Control.Applicative     (liftA2, (<|>))
import           Data.Char               (toLower, isPrint)
import           Data.Digest.OpenSSL.MD5 (md5sum)
import           Data.Geolocation.GeoIP
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           Data.Time               (addUTCTime, secondsToDiffTime)
import           Data.Time.Format        (formatTime)
import           Network.Wai
import           System.FilePath         ((</>))
import           Data.Time.Format        (defaultTimeLocale)
import           System.Random           (randomIO, randomRIO)
import           Text.HTML.TagSoup       (parseTagsOptions, parseOptionsFast, Tag(TagText))
import qualified Data.ByteString.UTF8    as B
import qualified Data.Map.Strict         as MapS
import qualified Data.Text               as T (concat, toLower, append)

-------------------------------------------------------------------------------------------------------------------
-- | If ajax request, redirects to page that makes JSON from message and status string.
--   If regular request, redirects to given URL.
trickyRedirect status msg url = do
  setMessageI msg
  t <- isAjaxRequest
  if t
    then redirect (JsonFromMsgR status)
    else redirect url
-------------------------------------------------------------------------------------------------------------------      
showPermission :: Permission -> AppMessage
showPermission p = fromJust $ lookup p xs
  where xs = [(ManageThreadP    , MsgManageThread    )
             ,(ManageBoardP     , MsgManageBoard     )
             ,(ManageUsersP     , MsgManageUsers     )
             ,(ManageConfigP    , MsgManageConfig    )
             ,(DeletePostsP     , MsgDeletePosts     )
             ,(ManagePanelP     , MsgManagePanel     )
             ,(ManageBanP       , MsgManageBan       )
             ,(EditPostsP       , MsgEditPosts       )
             ,(ShadowEditP      , MsgShadowEdit      ) 
             ,(AdditionalMarkupP, MsgAdditionalMarkup)
             ,(ViewModlogP      , MsgViewModlog      )
             ,(ViewIPAndIDP     , MsgViewIPAndID     )
             ,(HellBanP         , MsgHellbanning     )
             ,(ChangeFileRatingP, MsgChangeFileRating)
             ]

data GroupConfigurationForm = GroupConfigurationForm
                              Text -- ^ Group name
                              Bool -- ^ Permission to manage threads
                              Bool -- ^ ... boards
                              Bool -- ^ ... users
                              Bool -- ^ ... config
                              Bool -- ^ to delete posts
                              Bool -- ^ to view admin panel
                              Bool -- ^ to manage bans
                              Bool -- ^ to edit any post
                              Bool -- ^ Permission to edit any post without saving history  
                              Bool -- ^ to use additional markup
                              Bool -- ^ to view moderation log 
                              Bool -- ^ to view ip and uid
                              Bool -- ^ to use hellbanning 
                              Bool -- ^ to change censorship rating

data BoardConfigurationForm = BoardConfigurationForm
                              (Maybe Text)   -- ^ Name
                              (Maybe Text)   -- ^ Board title
                              (Maybe Int)    -- ^ Bump limit
                              (Maybe Int)    -- ^ Number of files
                              (Maybe Text)   -- ^ Allowed file types
                              (Maybe Text)   -- ^ Default name
                              (Maybe Int )   -- ^ The maximum message length
                              (Maybe Int )   -- ^ Thumbnail size
                              (Maybe Int )   -- ^ Threads per page
                              (Maybe Int )   -- ^ Previews post per thread
                              (Maybe Int )   -- ^ Thread limit
                              (Maybe Text)   -- ^ OP file
                              (Maybe Text)   -- ^ Reply file
                              (Maybe Text)   -- ^ Is hidden (Enable,Disable,DoNotChange)
                              (Maybe Text)   -- ^ Enable captcha (Enable,Disable,DoNotChange)
                              (Maybe Text)   -- ^ Category
                              (Maybe [Text]) -- ^ View access
                              (Maybe [Text]) -- ^ Reply access
                              (Maybe [Text]) -- ^ Thread access
                              (Maybe Text  ) -- ^ Allow OP moderate his/her thread
                              (Maybe Text  ) -- ^ Extra rules
                              (Maybe Text  ) -- ^ Enable geo IP
                              (Maybe Text  ) -- ^ Enable OP editing
                              (Maybe Text  ) -- ^ Enable post editing
                              (Maybe Text  ) -- ^ Show or not editing history
                              (Maybe Text  ) -- ^ Show or not post date
                              (Maybe Text  ) -- ^ Summary
                              (Maybe Text  ) -- ^ Enable forced anonymity (no name input)
                              (Maybe Text  ) -- ^ Required thread title
                              (Maybe Int   ) -- ^ Index
-------------------------------------------------------------------------------------------------------------------
-- Handful functions
-------------------------------------------------------------------------------------------------------------------
-- | Takes a random element from list
pick :: [a] -> IO (Maybe a)
pick xs
  | length xs == 0 = return Nothing
  | otherwise = (Just . (xs!!)) <$> randomRIO (0, length xs - 1)

whenM :: Monad m => m Bool -> m () -> m ()
whenM = (. flip when) . (>>=)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM = (. flip unless) . (>>=)

tshow :: Show a => a -> Text
tshow = pack . show

tread :: Read a => Text -> a
tread = read . unpack

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
thumbFileTypes = [FileVideo, FileImage] --, FileSource, FileDoc] TODO: thumbnails for docs and source files

sanitizeFileName :: String -> String
sanitizeFileName = filter (\x -> x `notElem` ("\\/"::String) && isPrint x)

fileExt :: FileInfo -> String
fileExt = map toLower . reverse . takeWhile (/='.') . reverse . sanitizeFileName . unpack . fileName

extractFileExt :: String -> String
extractFileExt = map toLower . reverse . takeWhile (/='.') . reverse
-------------------------------------------------------------------------------------------------------------------
-- Paths
-------------------------------------------------------------------------------------------------------------------
geoIconPath :: String -> Text -> Text
geoIconPath staticDir code = pack $  "/" </> staticDir </> "geoicons" </> (unpack $ (T.toLower code) <> ".png")

captchaFilePath :: String -> String -> String
captchaFilePath staticDir file = staticDir </> "captcha" </> file
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
thumbDirectory = "thumb"

thumbUrlPath :: String -> String -> Int -> FileType -> String -> String -> FilePath
thumbUrlPath uploadDir staticDir size filetype fileext hashsum = "/" </> (thumbFilePath uploadDir staticDir size filetype fileext hashsum)

thumbFilePath :: String -> String -> Int -> FileType -> String -> String -> FilePath
thumbFilePath uploadDir staticDir size filetype fileext hashsum
  | filetype == FileVideo           = uploadDir </> thumbDirectory </> (show size ++ "thumb-" ++ hashsum ++ ".png")
  | filetype `elem` thumbFileTypes = uploadDir </> thumbDirectory </> (show size ++ "thumb-" ++ hashsum ++ "." ++ fileext)
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

getIgnoredBoard :: Maybe Text -> Entity Board -> Maybe Text
getIgnoredBoard group board@(Entity _ b) = if isBoardHidden group board then Just $ boardName b else Nothing

isBoardHidden :: Maybe Text -> Entity Board -> Bool
isBoardHidden group (Entity _ b) = boardHidden b ||
                                   ( (isJust (boardViewAccess b) && isNothing group) ||
                                     (isJust (boardViewAccess b) && notElem (fromJust group) (fromJust $ boardViewAccess b))
                                   )

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
inc :: Int -> Int
inc = (+1)

makeFileInfo :: Attachedfile -> String
makeFileInfo file = extractFileExt (attachedfileName file) ++", "++ attachedfileSize file ++i
  where i = if length (attachedfileInfo file) > 0 then ", "++ attachedfileInfo file else ""

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

ifelseall :: forall a. Bool -> a -> a -> a
ifelseall x y z = if x then y else z

myFormatTime :: Int     -> -- ^ Time offset in seconds
               UTCTime -> -- ^ UTCTime
               String
myFormatTime offset t = formatTime defaultTimeLocale "%d %B %Y (%a) %H:%M:%S" $ addUTCTime' offset t

-- | Truncate file name if it's length greater than specified
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

defaultTitleMsg title = do
  AppSettings{..} <- appSettings <$> getYesod
  msgrender       <- getMessageRender
  setTitle $ toHtml $ T.concat [appSiteName, appTitleDelimiter, msgrender title]

defaultTitle title = do
  AppSettings{..} <- appSettings <$> getYesod
  setTitle $ toHtml $ T.concat [appSiteName, appTitleDelimiter, title]

defaultTitleReverse title = do
  AppSettings{..} <- appSettings <$> getYesod
  setTitle $ toHtml $ T.concat $ reverse [appSiteName, appTitleDelimiter, title]
-------------------------------------------------------------------------------------------------------------------
-- Widgets
-------------------------------------------------------------------------------------------------------------------
postWidget :: Entity Post              -> 
             [Entity Attachedfile]    -> 
             Bool                     -> -- ^ Are we in a thread
             Bool                     -> -- ^ Have access to post
             Bool                     -> -- ^ Show parent board/thread in the upper right corner
             Bool                     -> -- ^ If geo ip enabled
             Bool                     -> -- ^ Show post date
             [Permission]             -> -- ^ List of the all permissions
             Widget
postWidget ePost eFiles inThread canPost showParent geoIp showPostDate permissions = 
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
  in do
    timeZone        <- handlerToWidget getTimeZone
    rating          <- handlerToWidget getCensorshipRating
    AppSettings{..} <- handlerToWidget $ appSettings <$> getYesod
    $(widgetFile "post")

paginationWidget page pages route = $(widgetFile "pagination")

deleteWidget :: [Permission] -> Widget
deleteWidget permissions = $(widgetFile "delete")

adminNavbarWidget :: Widget
adminNavbarWidget = do
  permissions <- handlerToWidget $ ((fmap getPermissions) . getMaybeGroup) =<< maybeAuth
  $(widgetFile "admin/navbar")
-------------------------------------------------------------------------------------------------------------------
bareLayout :: Yesod site => WidgetT site IO () -> HandlerT site IO Html
bareLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer [hamlet| ^{pageBody pc} |]
-------------------------------------------------------------------------------------------------------------------
-- Access checkers
-------------------------------------------------------------------------------------------------------------------
checkHellbanned :: Post -> [Permission] -> Text -> Bool
checkHellbanned post permissions posterId = not (postHellbanned post) ||
                                            elem HellBanP permissions ||
                                            (postPosterId post) == posterId
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
  defaultZone <- appTimezone . appSettings <$> getYesod
  timezone    <- lookupSession "timezone"
  return $ maybe defaultZone tread timezone

getCensorshipRating :: Handler Censorship
getCensorshipRating = do
  mRating <- lookupSession "censorship-rating"
  case mRating of
    Just rating -> return $ tread rating
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

getConfigEntity :: Handler Config
getConfigEntity = entityVal . fromJust <$> runDB (selectFirst ([]::[Filter Config]) [])

getFeedBoards :: Handler [Text]
getFeedBoards = do
  bs <- lookupSession "feed-ignore-boards"
  case bs of
   Just xs -> return $ tread xs
   Nothing -> setSession "feed-ignore-boards" "[]" >> return []

getHiddenThreads :: Text -> Handler [(Int,Int)]
getHiddenThreads board = do
  ht <- lookupSession "hidden-threads"
  case ht of
   Just xs -> return $ fromMaybe [] $ lookup board (read (unpack xs) :: [(Text, [(Int,Int)])])
   Nothing -> setSession "hidden-threads" "[]" >> return []

getAllHiddenThreads :: Handler [(Text, [(Int,Int)])]
getAllHiddenThreads = do
  ht <- lookupSession "hidden-threads"
  case ht of
   Just xs -> return $ read $ unpack xs
   Nothing -> setSession "hidden-threads" "[]" >> return []

getAllHiddenPostsIds :: [Text] -> Handler [Key Post]
getAllHiddenPostsIds boards = do
  threadsIds <- concat <$> forM boards (\b -> map (toSqlKey . fromIntegral . snd) <$> getHiddenThreads b)
  threads    <- runDB $ selectList [PostId <-. threadsIds] []
  replies    <- runDB $ forM threads $ \(Entity _ t) -> selectList [PostBoard ==. postBoard t, PostParent ==. postLocalId t] []
  return $ map f threads ++ map f (concat replies)
  where f (Entity k _) = k
-------------------------------------------------------------------------------------------------------------------
-- IP getter
-------------------------------------------------------------------------------------------------------------------
-- | Gets IP from X-Real-IP/CF-Connecting-I or remote-host header
getIp :: forall (m :: * -> *). MonadHandler m => m String
getIp = do
  realIp <- getIpReal
  cfIp   <- getIpCF
  hostIp <- getIpFromHost
  return $ fromJust ((B.toString <$> cfIp) <|> (B.toString <$> realIp) <|> Just hostIp)
  where getIpReal      = lookup "X-Real-IP" . requestHeaders <$> waiRequest
        getIpCF        = lookup "CF-Connecting-IP" . requestHeaders <$> waiRequest
        getIpFromHost  = takeWhile (not . (`elem` (":"::String))) . show . remoteHost . reqWaiRequest <$> getRequest
-------------------------------------------------------------------------------------------------------------------
-- Geo IP
-------------------------------------------------------------------------------------------------------------------  
getCountry :: Text ->                      -- ^ IP adress
             Handler (Maybe (Text,Text)) -- ^ (country code, country name)
getCountry ip = do
  dbPath   <- unpack . appGeoIPCityPath . appSettings <$> getYesod
  geoIpRes <- liftIO $ openGeoDB memory_cache dbPath >>= flip geoLocateByIPAddress (encodeUtf8 ip)
  return $ ((decodeUtf8 . geoCountryCode) &&& (decodeUtf8 . geoCountryName)) <$> geoIpRes
-------------------------------------------------------------------------------------------------------------------
-- Board stats
-------------------------------------------------------------------------------------------------------------------
getBoardStats :: Handler [(Text,Int,Int)]
getBoardStats = do
  mgroup     <- (fmap $ userGroup . entityVal) <$> maybeAuth
  maybeStats <- lookupSession "board-stats"
  case maybeStats of
    Just s  -> return $ tread s
    Nothing -> do
      posterId <- getPosterId
      boards <- map (boardName . entityVal) . filter (not . isBoardHidden mgroup) <$> runDB (selectList ([]::[Filter Board]) [])
      hiddenThreads <- getAllHiddenThreads
      stats  <- runDB $ forM boards $ \b -> do
                  lastPost <- selectFirst [PostBoard ==. b, PostDeleted ==. False, PostPosterId !=. posterId, PostHellbanned ==. False
                                         ,PostParent /<-. concatMap (map fst . snd) (filter ((==b).fst) hiddenThreads)] [Desc PostLocalId]
                  return (b, maybe 0 (postLocalId . entityVal) lastPost, 0)
      saveBoardStats stats
      return stats

saveBoardStats :: [(Text,Int,Int)] -> Handler ()
saveBoardStats stats = do
  deleteSession "board-stats"
  setSession "board-stats" $ tshow stats

cleanAllBoardsStats :: Handler ()
cleanAllBoardsStats = do
  mgroup <- (fmap $ userGroup . entityVal) <$> maybeAuth
  boards <- map (boardName . entityVal) . filter (not . isBoardHidden mgroup) <$> runDB (selectList ([]::[Filter Board]) [])
  forM_ boards cleanBoardStats
  
cleanBoardStats :: Text -> Handler ()
cleanBoardStats board = do
  hiddenThreads <- getAllHiddenThreads
  oldStats <- getBoardStats
  newStats <- forM oldStats $ \s@(b,_,_) ->
    if b == board
    then do
      lastPost <- runDB $ selectFirst [PostBoard ==. b, PostDeleted ==. False, PostHellbanned ==. False
                                      ,PostParent /<-. concatMap (map fst . snd) (filter ((==b).fst) hiddenThreads)] [Desc PostLocalId]
      return (b, maybe 0 (postLocalId . entityVal) lastPost, 0)
    else return s
  saveBoardStats newStats
