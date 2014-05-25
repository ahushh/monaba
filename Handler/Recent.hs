{-# LANGUAGE OverloadedStrings #-}
module Handler.Recent where

import           Import
import           Yesod.Auth
-------------------------------------------------------------------------------------------------------------
getRecentR :: Handler Html
getRecentR = do
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup
  -------------------------------------------------------------------------------------------------------------------      
  posterId  <- getPosterId
  showPosts <- getConfig configShowRecentPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  selectedBoards <- getRecentBoards
  hiddenThreads <- (concatMap snd) <$> getAllHiddenThreads
  let boardsWhereShowDate    = map boardName $ filter boardShowPostDate    $ map entityVal boards
      boardsWhereShowHistory = map boardName $ filter boardShowEditHistory $ map entityVal boards
      boards'        = mapMaybe (ignoreBoards group) boards ++ selectedBoards
      selectPostsAll = [PostDeletedByOp ==. False, PostBoard /<-. boards', PostDeleted ==. False, PostLocalId /<-. hiddenThreads
                       ,PostParent /<-. hiddenThreads]
      selectPostsHB  = [PostDeletedByOp ==. False, PostBoard /<-. boards', PostDeleted ==. False, PostHellbanned ==. False
                       ,PostLocalId /<-. hiddenThreads, PostParent /<-. hiddenThreads] ||.
                       [PostDeletedByOp ==. False, PostBoard /<-. boards', PostDeleted ==. False, PostHellbanned ==. True
                       ,PostPosterId ==. posterId, PostLocalId /<-. hiddenThreads, PostParent /<-. hiddenThreads]
      selectPosts    = if HellBanP `elem` permissions then selectPostsAll else selectPostsHB
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  geoIpEnabled' <- runDB $ mapM (getBy . BoardUniqName) $ nub $ map (postBoard . entityVal) posts
  let geoIpEnabled = map (boardName . entityVal) $ filter (boardEnableGeoIp . entityVal) $ catMaybes geoIpEnabled' 
  -------------------------------------------------------------------------------------------------------------------
  nameOfTheBoard <- extraSiteName <$> getExtra
  msgrender      <- getMessageRender
  timeZone       <- getTimeZone  
  rating         <- getCensorshipRating
  displaySage    <- getConfig configDisplaySage
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ nameOfTheBoard <> titleDelimiter <> msgrender MsgRecentPosts
    addScript (StaticR js_eventsource_js)
    $(widgetFile "recent")
  
