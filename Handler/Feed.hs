{-# LANGUAGE OverloadedStrings #-}
module Handler.Feed where

import           Import
import           Yesod.Auth
import qualified Data.Text  as T
import           Handler.Posting (postForm, editForm)
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
  maxLenOfPostTitle <- extraMaxLenOfPostTitle <$> getExtra
  maxLenOfPostName  <- extraMaxLenOfPostName  <$> getExtra
  let maxMessageLength = boardMaxMsgLength boardVal
  (formWidget, formEnctype) <- generateFormPost $ postForm maxLenOfPostTitle maxLenOfPostName boardVal muser
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
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  let boardsWhereShowDate    = map boardName $ filter boardShowPostDate    $ map entityVal boards
      boards' = mapMaybe (ignoreBoards group) boards
  posts     <- runDB $ selectList [PostDeletedByOp ==. False, PostBoard /<-. boards', PostDeleted ==. False] [Desc PostDate, LimitTo showPosts, OffsetBy offset]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  geoIpEnabled' <- runDB $ mapM (getBy . BoardUniqName) $ nub $ map (postBoard . entityVal) posts
  let geoIpEnabled = map (boardName . entityVal) $ filter (boardEnableGeoIp . entityVal) $ catMaybes geoIpEnabled' 
  -------------------------------------------------------------------------------------------------------------------
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  nameOfTheBoard   <- extraSiteName <$> getExtra
  msgrender        <- getMessageRender
  timeZone         <- getTimeZone  
  (editFormWidget, _) <- generateFormPost editForm
  if offset == 0
     then defaultLayout $ do
            setUltDestCurrent
            setTitle $ toHtml $ T.concat [nameOfTheBoard, titleDelimiter, msgrender MsgFeed]
            $(widgetFile "feed")
    else bareLayout $(widgetFile "feed")
  
getAjaxNewFeedR :: Int -> Handler Html
getAjaxNewFeedR lastPostId = do
  lastPost <- runDB $ get404 $ toSqlKey $ fromIntegral lastPostId
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  timeZone <- getTimeZone
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup
      lastPostDate= postDate lastPost
  -------------------------------------------------------------------------------------------------------------------      
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  let boardsWhereShowDate    = map boardName $ filter boardShowPostDate    $ map entityVal boards
      boards'                = mapMaybe (ignoreBoards group) boards
  posts     <- runDB $ selectList [PostDeletedByOp ==. False, PostBoard /<-. boards', PostDate >. lastPostDate, PostDeleted ==. False] [Desc PostDate]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  geoIpEnabled' <- runDB $ mapM (getBy . BoardUniqName) $ nub $ map (postBoard . entityVal) posts
  let geoIpEnabled = map (boardName . entityVal) $ filter (boardEnableGeoIp . entityVal) $ catMaybes geoIpEnabled' 
  -------------------------------------------------------------------------------------------------------------------
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  msgrender        <- getMessageRender
  (editFormWidget, _) <- generateFormPost editForm
  let offset = -1 :: Int
  bareLayout $(widgetFile "feed")
  
