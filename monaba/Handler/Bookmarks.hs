module Handler.Bookmarks where

import           Import
import qualified Data.Text       as T

getBookmarksR :: Handler Html
getBookmarksR = do
  permissions <- ((fmap getPermissions) . getMaybeGroup) =<< maybeAuth
  posterId    <- getPosterId
  bm <- getBookmarks
  let toKey = toSqlKey . fromIntegral . fst
      toKey :: (Int, Int) -> Key Post
      postIds       = map toKey bm
      postsLastSeen = map snd bm
  posts'      <- catMaybes <$> forM postIds  (runDB . get)
  let posts = filter (\p -> and [not (postDeleted p), ( not (postHellbanned p) || (postHellbanned p && postPosterId p == posterId))]) posts'
  postFiles  <- forM postIds $ \p -> runDB $ selectList [AttachedfileParentId ==. p] []
  newReplies <- forM (zip posts postsLastSeen) $ \(p,r) -> runDB $ count [PostParent ==. postLocalId p, PostBoard ==. postBoard p, PostLocalId >. r]
  let postsAndFiles = zip3 (zipWith Entity postIds posts) postFiles newReplies

  defaultLayout $ do  
    defaultTitleMsg MsgBookmarks
    $(widgetFile "bookmarks")

getLastReply post = (maybe 0 (postLocalId . entityVal) ) <$> (runDB $ selectFirst [PostParent ==. postLocalId post] [Desc PostDate])

bookmarksUpdateLastReply ePost = do
  replyLocalId <- getLastReply $ entityVal ePost
  let postId = fromIntegral $ fromSqlKey $ entityKey ePost
  bm <- lookupSession "bookmarks"
  case bm of
    Just bm' ->
     let xs' = tread bm' :: [(Int, Int)]
         isInBookmarks = (/=0) $ length $ filter ((==postId).fst) xs'
         xs  = filter ((/=postId).fst) xs'
         new = tshow ( (postId, replyLocalId) : xs)
      in if isInBookmarks then setSession "bookmarks" new else return ()
    Nothing -> setSession "bookmarks" $ T.concat [ "[(",tshow postId,",",tshow replyLocalId,")]" ]

getBookmarksUpdR :: Int -> Handler TypedContent
getBookmarksUpdR postId = do
  let toKey = toSqlKey . fromIntegral
      toKey :: Int -> Key Post
      postKey = toKey postId
  post <- runDB $ get404 postKey
  bookmarksUpdateLastReply $ Entity postKey post
  selectRep $ do
    provideRep $ bareLayout [whamlet|ok|]
    provideJson $ object [("ok", "updatedg")]

getBookmarksAddR :: Int -> Handler TypedContent
getBookmarksAddR postId = do
  post      <- runDB $ get404 (toSqlKey $ fromIntegral postId)
  lastReply <- getLastReply post

  bm <- lookupSession "bookmarks"
  case bm of
    Just bm' ->
     let xs' = tread bm' :: [(Int, Int)]
         xs  = filter ((/=postId).fst) xs'
         new = tshow ( (postId, lastReply) : xs)
      in setSession "bookmarks" new
    Nothing -> setSession "bookmarks" $ T.concat [ "[(",tshow postId,",",tshow lastReply,")]" ]
  selectRep $ do
    provideRep $ bareLayout [whamlet|ok|]
    provideJson $ object [("ok", "added")]

getBookmarksDelR :: Int -> Handler TypedContent
getBookmarksDelR postId = do
  bm <- lookupSession "bookmarks"
  case bm of
    Just bm' ->
      let xs' = tread bm' :: [(Int, Int)]
          xs  = filter ((/=postId).fst) xs'
          new = tshow xs
      in setSession "bookmarks" new
    Nothing -> setSession "bookmarks" "[]"
  selectRep $ do
    provideRep $ bareLayout [whamlet|ok|]
    provideJson $ object [("ok", "deleted")]
