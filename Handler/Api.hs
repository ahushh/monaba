{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Api where

import           Import
import           Yesod.Auth
--------------------------------------------------------------------------------------------------------- 
getPostsHelper :: YesodDB App [Entity Post] -> -- ^ Post selector: selectList [...] [...]
                 Text -> -- ^ Board name
                 Int  -> -- ^ Thread internal ID
                 Text -> -- ^ Error string
                 Handler TypedContent
getPostsHelper selectPosts board thread errorString = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let permissions  = getPermissions   mgroup
      geoIpEnabled = boardEnableGeoIp boardVal
  let selectFiles p = runDB $ selectList [AttachedfileParentId ==. entityKey p] []
  postsAndFiles <- reverse <$> runDB selectPosts >>= mapM (\p -> do
    files <- selectFiles p
    return (p, files))
  t <- runDB $ count [PostBoard ==. board, PostLocalId ==. thread, PostParent ==. 0, PostDeleted ==. False]
  geoIps    <- getCountries (if geoIpEnabled then postsAndFiles else [])
  timeZone <- getTimeZone
  case () of
    _ | t == 0              -> selectRep $ do
          provideRep  $ bareLayout [whamlet|No such thread|]
          provideJson $ object [("error", toJSON ("No such thread"::Text))]
      | null postsAndFiles -> selectRep $ do
          provideRep  $ bareLayout [whamlet|#{errorString}|]
          provideJson $ object [("error", toJSON errorString)]
      | otherwise          -> selectRep $ do
          provideRep  $ bareLayout [whamlet|
                               $forall (post, files) <- postsAndFiles
                                   ^{postWidget muser post files True True False permissions geoIps timeZone}
                               |]
          provideJson $ map (entityVal *** map entityVal) postsAndFiles

getApiDeletedPostsR :: Text -> Int -> Handler TypedContent
getApiDeletedPostsR board thread = getPostsHelper selectPosts board thread errorString
  where selectPosts = selectList [PostDeletedByOp ==. True, PostBoard ==. board,
                                  PostParent ==. thread, PostDeleted ==. False] [Desc PostDate]
        errorString = "No such posts"

getApiAllPostsR :: Text -> Int -> Handler TypedContent
getApiAllPostsR board thread = getPostsHelper selectPosts board thread errorString
  where selectPosts = selectList [PostDeletedByOp ==. False, PostBoard ==. board,
                                  PostParent ==. thread, PostDeleted ==. False] [Desc PostDate]
        errorString = "No posts in this thread"

getApiNewPostsR :: Text -> Int -> Int -> Handler TypedContent
getApiNewPostsR board thread postId = getPostsHelper selectPosts board thread errorString
  where selectPosts = selectList [PostDeletedByOp ==. False, PostBoard ==. board,
                                  PostParent ==. thread, PostLocalId >. postId, PostDeleted ==. False] [Desc PostDate]
        errorString = "No new posts"

getApiLastPostsR :: Text -> Int -> Int -> Handler TypedContent
getApiLastPostsR board thread postCount = getPostsHelper selectPosts board thread errorString
  where selectPosts = selectList [PostDeletedByOp ==. False, PostBoard ==. board,
                                  PostParent ==. thread, PostDeleted ==. False] [Desc PostDate, LimitTo postCount]
        errorString = "No such posts"
---------------------------------------------------------------------------------------------------------
getApiPostR :: Text -> Int -> Handler TypedContent
getApiPostR board postId = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let permissions  = getPermissions   mgroup
      geoIpEnabled = boardEnableGeoIp boardVal
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. postId, PostDeleted ==. False] []
  when (isNothing maybePost) notFound
  let post    = fromJust maybePost
      postKey = entityKey $ fromJust maybePost
  files  <- runDB $ selectList [AttachedfileParentId ==. postKey] []
  geoIps <- getCountries [(post, files) | geoIpEnabled]
  timeZone <- getTimeZone
  let postAndFiles = (entityVal post, map entityVal files)
      widget       = if postParent (entityVal $ fromJust maybePost) == 0
                       then postWidget muser post files False True False permissions geoIps timeZone
                       else postWidget muser post files False True False permissions geoIps timeZone
  selectRep $ do
    provideRep $ bareLayout widget
    provideJson postAndFiles
