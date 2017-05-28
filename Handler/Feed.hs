module Handler.Feed where

import           Import
import           Handler.Posting (takeBanner, randomBanner, postForm, editForm)
import           Handler.Captcha (captchaWidget)
-------------------------------------------------------------------------------------------------------------
getFeedR :: Handler TypedContent
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

  adaptiveCaptcha <- getConfig configAdaptiveCaptcha
  pc <- lookupSession "post-count"
  let isCaptchaEnabled = boardEnableCaptcha boardVal && maybe True (\x -> tread x <= adaptiveCaptcha) pc && isNothing muser
  captchaImg <- if isCaptchaEnabled then Just <$> widgetToPageContent captchaWidget else return Nothing

  bareLayout [whamlet|<form .quick-post-form #post-form method=post enctype=#{formEnctype} data-board=#{board} data-max-msg-length=#{maxMessageLength} data-board=#{board}>
                        ^{formWidget captchaImg}
                     |]

getAjaxFeedOffsetR :: Int -> Handler TypedContent
getAjaxFeedOffsetR offset = do
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup
  -------------------------------------------------------------------------------------------------------------------      
  ip        <- pack <$> getIp
  posterId  <- getPosterId
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  ignoredBoards <- getFeedBoards
  let boardsWhereShowDate = map boardName $ filter boardShowPostDate $ map entityVal boards
      boardsWhereEnablePM = map boardName $ filter boardEnablePM     $ map entityVal boards
      onionBoards         = if not (isOnion ip) then map boardName $ filter boardOnion $ map entityVal boards else []
      boards'             = mapMaybe (getIgnoredBoard group) boards ++ ignoredBoards ++ onionBoards
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
  mBanner          <- if appRandomBanners then randomBanner else takeBanner "feed"
  (editFormWidget, _) <- generateFormPost $ editForm permissions
  ((_, searchWidget), _) <- runFormGet $ searchForm $ Just ""
  if offset == 0
     then selectRep $ do
            provideRep $ defaultLayout $ do
              setUltDestCurrent
              defaultTitleMsg MsgFeed
              $(widgetFile "feed")
            provideJson $ object [ "posts" .= postsAndFiles
                                 , "banner" .= mBanner
                                 ]
    else selectRep $ do
           provideRep $ bareLayout $(widgetFile "feed")
           provideJson $ object [ "posts" .= postsAndFiles
                                , "banner" .= mBanner
                                ]

  
getAjaxNewFeedR :: Int -> Handler Html
getAjaxNewFeedR lastPostId = do
  lastPost <- runDB $ get404 $ toSqlKey $ fromIntegral lastPostId
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup
      lastPostDate= postDate lastPost
  -------------------------------------------------------------------------------------------------------------------      
  ip        <- pack <$> getIp
  posterId  <- getPosterId
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  ignoredBoards <- getFeedBoards
  let boardsWhereShowDate = map boardName $ filter boardShowPostDate $ map entityVal boards
      boardsWhereEnablePM = map boardName $ filter boardEnablePM     $ map entityVal boards
      onionBoards         = if not (isOnion ip) then map boardName $ filter boardOnion $ map entityVal boards else []
      boards'             = mapMaybe (getIgnoredBoard group) boards ++ ignoredBoards ++ onionBoards
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
  (editFormWidget, _) <- generateFormPost $ editForm permissions
  let offset  = -1 :: Int
      mBanner = Nothing :: Maybe (String, String)
  ((_, searchWidget), _) <- runFormGet $ searchForm $ Just ""
  bareLayout $(widgetFile "feed")

