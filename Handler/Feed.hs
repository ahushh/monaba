module Handler.Feed where

import           Import
import           Handler.Posting (chooseBanner, postForm, editForm)
-------------------------------------------------------------------------------------------------------------
getFeedR :: Handler Html
getFeedR = getAjaxFeedOffsetR 0

getAjaxGetPostFormR :: Text -> Handler Html
getAjaxGetPostFormR board = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  unless (checkAccessToReply mgroup boardVal) notFound
  let maxMessageLength = boardMaxMsgLength boardVal
  (formWidget, formEnctype) <- generateFormPost $ postForm False boardVal muser
  bareLayout [whamlet|<form .quick-post-form #post-form method=post enctype=#{formEnctype} data-board=#{board} data-max-msg-length=#{maxMessageLength} data-board=#{board}>
                        ^{formWidget}
                     |]

getAjaxFeedOffsetR :: Int -> Handler Html
getAjaxFeedOffsetR offset = do
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup
  -------------------------------------------------------------------------------------------------------------------      
  posterId  <- getPosterId
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  ignoredBoards <- getFeedBoards
  let boardsWhereShowDate = map boardName $ filter boardShowPostDate    $ map entityVal boards
      boards'             = mapMaybe (getIgnoredBoard group) boards ++ ignoredBoards
  ignoredPostsIds <- getAllHiddenPostsIds $ filter (`notElem`boards') $ map (boardName . entityVal) boards
  let selectPostsAll = [PostId /<-. ignoredPostsIds, PostDeletedByOp ==. False, PostBoard /<-. boards', PostDeleted ==. False]
      selectPostsHB  = [PostId /<-. ignoredPostsIds, PostDeletedByOp ==. False, PostBoard /<-. boards', PostDeleted ==. False, PostHellbanned ==. False] ||.
                       [PostId /<-. ignoredPostsIds, PostDeletedByOp ==. False, PostBoard /<-. boards', PostDeleted ==. False, PostHellbanned ==. True, PostPosterId ==. posterId]
      selectPosts    = if elem HellBanP permissions then selectPostsAll else selectPostsHB
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts, OffsetBy offset]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  geoIpEnabled'' <- runDB $ mapM (getBy . BoardUniqName) $ nub $ map (postBoard . entityVal) posts
  let geoIpEnabled = map (boardName . entityVal) $ filter (boardEnableGeoIp . entityVal) $ catMaybes geoIpEnabled''
  -------------------------------------------------------------------------------------------------------------------
  AppSettings{..}  <- appSettings <$> getYesod
  msgrender        <- getMessageRender
  mBanner          <- chooseBanner
  (editFormWidget, _) <- generateFormPost editForm
  if offset == 0
     then defaultLayout $ do
            setUltDestCurrent
            defaultTitleMsg MsgFeed
            $(widgetFile "feed")
    else bareLayout $(widgetFile "feed")
  
getAjaxNewFeedR :: Int -> Handler Html
getAjaxNewFeedR lastPostId = do
  lastPost <- runDB $ get404 $ toSqlKey $ fromIntegral lastPostId
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup
      lastPostDate= postDate lastPost
  -------------------------------------------------------------------------------------------------------------------      
  posterId  <- getPosterId
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  ignoredBoards <- getFeedBoards
  let boardsWhereShowDate    = map boardName $ filter boardShowPostDate    $ map entityVal boards
      boards'                = mapMaybe (getIgnoredBoard group) boards ++ ignoredBoards
  ignoredPostsIds <- getAllHiddenPostsIds $ filter (`notElem`boards') $ map (boardName . entityVal) boards
  let selectPostsAll = [PostId /<-. ignoredPostsIds, PostDeletedByOp ==. False, PostBoard /<-. boards', PostDeleted ==. False, PostDate >. lastPostDate]
      selectPostsHB  = [PostId /<-. ignoredPostsIds, PostDeletedByOp ==. False, PostBoard /<-. boards', PostDeleted ==. False, PostHellbanned ==. False, PostDate >. lastPostDate] ||.
                       [PostId /<-. ignoredPostsIds, PostDeletedByOp ==. False, PostBoard /<-. boards', PostDeleted ==. False, PostHellbanned ==. True, PostPosterId ==. posterId, PostDate >. lastPostDate]
      selectPosts    = if elem HellBanP permissions then selectPostsAll else selectPostsHB
  posts     <- runDB $ selectList selectPosts [Desc PostDate]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  geoIpEnabled'' <- runDB $ mapM (getBy . BoardUniqName) $ nub $ map (postBoard . entityVal) posts
  let geoIpEnabled = map (boardName . entityVal) $ filter (boardEnableGeoIp . entityVal) $ catMaybes geoIpEnabled''
  -------------------------------------------------------------------------------------------------------------------
  AppSettings{..}  <- appSettings <$> getYesod
  msgrender        <- getMessageRender
  (editFormWidget, _) <- generateFormPost editForm
  let offset  = -1 :: Int
      mBanner = Nothing :: Maybe (String, String)
  bareLayout $(widgetFile "feed")
  
