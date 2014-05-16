{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin.Search where

import           Import
import           Yesod.Auth
-------------------------------------------------------------------------------------------------------------
getAdminSearchUIDNoPageR :: Text -> Handler Html
getAdminSearchUIDNoPageR = flip getAdminSearchUIDR 0

getAdminSearchUIDR :: Text -> Int -> Handler Html
getAdminSearchUIDR = helperUID False

getAdminSearchUIDOnlyHBR :: Text -> Int -> Handler Html
getAdminSearchUIDOnlyHBR = helperUID True

getAdminSearchUIDOnlyHBNoPageR :: Text -> Handler Html
getAdminSearchUIDOnlyHBNoPageR = flip getAdminSearchUIDOnlyHBR 0

getAdminSearchIPNoPageR :: Text -> Handler Html
getAdminSearchIPNoPageR = flip getAdminSearchIPR 0

getAdminSearchIPR :: Text -> Int -> Handler Html
getAdminSearchIPR ip page = do
  muser                <- maybeAuth
  (permissions, group) <- pair getPermissions ((groupName . entityVal)<$>) <$> getMaybeGroup muser
  -------------------------------------------------------------------------------------------------------------------
  showPosts <- getConfig configShowRecentPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count [PostDeleted ==. False, PostIp ==. ip]
  let boards'     = mapMaybe (ignoreBoards group) boards
      selectPosts = [PostBoard /<-. boards', PostDeleted ==. False, PostIp ==. ip]
      pages       = listPages showPosts numberOfPosts
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts, OffsetBy $ page*showPosts]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  geoIpEnabled' <- runDB $ mapM (getBy . BoardUniqName) $ nub $ map (postBoard . entityVal) posts
  let geoIpEnabled = map (boardName . entityVal) $ filter (boardEnableGeoIp . entityVal) $ catMaybes geoIpEnabled' 
  geoIps <- getCountries $ filter ((`elem`geoIpEnabled) . postBoard . entityVal . fst) postsAndFiles
  -------------------------------------------------------------------------------------------------------------------
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  timeZone        <- getTimeZone
  displaySage     <- getConfig configDisplaySage
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ nameOfTheBoard <> titleDelimiter <> msgrender MsgPostsByUserID
    $(widgetFile "admin/search/ip")

-------------------------------------------------------------------------------------------------------------
helperUID :: Bool -> Text -> Int -> Handler Html
helperUID onlyHellbanned posterId page = do
  muser                <- maybeAuth
  (permissions, group) <- pair getPermissions ((groupName . entityVal)<$>) <$> getMaybeGroup muser
  -------------------------------------------------------------------------------------------------------------------
  showPosts <- getConfig configShowRecentPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count ([PostDeleted ==. False, PostPosterId ==. posterId] ++
                                 [PostHellbanned ==. True | onlyHellbanned])
  let boards'     = mapMaybe (ignoreBoards group) boards
      selectPosts = [PostBoard /<-. boards', PostDeleted ==. False, PostPosterId ==. posterId] ++
                    [PostHellbanned ==. True | onlyHellbanned]
      pages       = listPages showPosts numberOfPosts
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts, OffsetBy $ page*showPosts]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  geoIpEnabled' <- runDB $ mapM (getBy . BoardUniqName) $ nub $ map (postBoard . entityVal) posts
  let geoIpEnabled = map (boardName . entityVal) $ filter (boardEnableGeoIp . entityVal) $ catMaybes geoIpEnabled' 
  geoIps <- getCountries $ filter ((`elem`geoIpEnabled) . postBoard . entityVal . fst) postsAndFiles
  -------------------------------------------------------------------------------------------------------------------
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  timeZone        <- getTimeZone
  displaySage     <- getConfig configDisplaySage
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ nameOfTheBoard <> titleDelimiter <> msgrender MsgPostsByUserID
    $(widgetFile "admin/search/uid")

