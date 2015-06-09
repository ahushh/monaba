{-# LANGUAGE OverloadedStrings #-}
module Handler.Admin.Search where

import Import
-------------------------------------------------------------------------------------------------------------
getAdminSearchIPNoPageR :: Text -> Handler Html
getAdminSearchIPNoPageR = flip getAdminSearchIPR 0

getAdminSearchIPR :: Text -> Int -> Handler Html
getAdminSearchIPR ip page = do
  permissions <- ((fmap getPermissions) . getMaybeGroup) =<< maybeAuth
  group       <- (fmap $ userGroup . entityVal) <$> maybeAuth
  showPosts   <- getConfig configShowLatestPosts
  boards      <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count [PostDeleted ==. False, PostIp ==. ip]
  let boards'     = mapMaybe (ignoreBoards group) boards
      selectPosts = [PostBoard /<-. boards', PostDeleted ==. False, PostIp ==. ip]
      pages       = listPages showPosts numberOfPosts
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts, OffsetBy $ page*showPosts]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleMsg MsgPostsByUserID
    $(widgetFile "admin/search/ip")
-------------------------------------------------------------------------------------------------------------
uidSearchHelper :: Bool -> Text -> Int -> Handler Html
uidSearchHelper onlyHellbanned posterId page = do
  permissions <- ((fmap getPermissions) . getMaybeGroup) =<< maybeAuth
  group       <- (fmap $ userGroup . entityVal) <$> maybeAuth
  showPosts   <- getConfig configShowLatestPosts
  boards      <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count (if onlyHellbanned then [PostDeleted ==. False, PostPosterId ==. posterId, PostHellbanned ==. True] else [PostDeleted ==. False, PostPosterId ==. posterId])
  let boards'        = mapMaybe (ignoreBoards group) boards
      selectPostsAll = [PostBoard /<-. boards', PostDeleted ==. False, PostPosterId ==. posterId]
      selectPostsHB  = [PostBoard /<-. boards', PostDeleted ==. False, PostPosterId ==. posterId, PostHellbanned ==. True]
      selectPosts    = if onlyHellbanned then selectPostsHB else selectPostsAll
      pages          = listPages showPosts numberOfPosts
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts, OffsetBy $ page*showPosts]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleMsg MsgPostsByUserID
    $(widgetFile "admin/search/uid")

getAdminSearchUIDNoPageR :: Text -> Handler Html
getAdminSearchUIDNoPageR = flip getAdminSearchUIDR 0

getAdminSearchUIDR :: Text -> Int -> Handler Html
getAdminSearchUIDR posterId page = uidSearchHelper False posterId page

getAdminSearchHBUIDNoPageR :: Text -> Handler Html
getAdminSearchHBUIDNoPageR posterId = flip getAdminSearchHBUIDR 0 posterId

getAdminSearchHBUIDR :: Text -> Int -> Handler Html
getAdminSearchHBUIDR posterId page = uidSearchHelper True posterId page

