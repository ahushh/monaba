{-# LANGUAGE OverloadedStrings #-}
module Handler.Live where

import           Import
import           Yesod.Auth
import qualified Data.Text  as T
import           Data.Maybe (mapMaybe)
-------------------------------------------------------------------------------------------------------------
getLiveR :: Handler Html
getLiveR = do
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup
  -------------------------------------------------------------------------------------------------------------------      
  posterId  <- getPosterId
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  hiddenThreads <- (concatMap snd) <$> getAllHiddenThreads
  let boards'        = mapMaybe (ignoreBoards group) boards
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
  geoIps <- getCountries $ filter ((`elem`geoIpEnabled) . postBoard . entityVal . fst) postsAndFiles
  -------------------------------------------------------------------------------------------------------------------
  nameOfTheBoard <- extraSiteName <$> getExtra
  msgrender      <- getMessageRender
  timeZone       <- getTimeZone  
  rating         <- getCensorshipRating
  displaySage    <- getConfig configDisplaySage
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, titleDelimiter, msgrender MsgLatestPosts]
    $(widgetFile "live")
  
