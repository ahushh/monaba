{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Ajax where

import           Import
import           Yesod.Auth
import qualified Data.Text as T (concat)
---------------------------------------------------------------------------------------------------------
-- Get multiple posts
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
  let permissions   = getPermissions   mgroup
      geoIpEnabled  = boardEnableGeoIp boardVal
      selectFiles p = runDB $ selectList [AttachedfileParentId ==. entityKey p] []
      showPostDate    = boardShowPostDate boardVal
  postsAndFiles <- reverse <$> runDB selectPosts >>= mapM (\p -> do
    files <- selectFiles p
    return (p, files))
  t <- runDB $ count [PostBoard ==. board, PostLocalId ==. thread, PostParent ==. 0, PostDeleted ==. False]
  timeZone <- getTimeZone
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
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
                                   ^{postWidget muser post files True True False geoIpEnabled showPostDate permissions timeZone maxLenOfFileName}
                               |]
          provideJson $ map (entityVal *** map entityVal) postsAndFiles

getAjaxDeletedPostsR :: Text -> Int -> Handler TypedContent
getAjaxDeletedPostsR board thread = getPostsHelper selectPosts board thread errorString
  where selectPosts = selectList [PostDeletedByOp ==. True, PostBoard ==. board,
                                  PostParent ==. thread, PostDeleted ==. False] [Desc PostDate]
        errorString = "No such posts"

getAjaxAllPostsR :: Text -> Int -> Handler TypedContent
getAjaxAllPostsR board thread = getPostsHelper selectPosts board thread errorString
  where selectPosts = selectList [PostDeletedByOp ==. False, PostBoard ==. board,
                                  PostParent ==. thread, PostDeleted ==. False] [Desc PostDate]
        errorString = "No posts in this thread"

getAjaxNewPostsR :: Text -> Int -> Int -> Handler TypedContent
getAjaxNewPostsR board thread postId = getPostsHelper selectPosts board thread errorString
  where selectPosts = selectList [PostDeletedByOp ==. False, PostBoard ==. board,
                                  PostParent ==. thread, PostLocalId >. postId, PostDeleted ==. False] [Desc PostDate]
        errorString = "No new posts"

getAjaxLastPostsR :: Text -> Int -> Int -> Handler TypedContent
getAjaxLastPostsR board thread postCount = getPostsHelper selectPosts board thread errorString
  where selectPosts = selectList [PostDeletedByOp ==. False, PostBoard ==. board,
                                  PostParent ==. thread, PostDeleted ==. False] [Desc PostDate, LimitTo postCount]
        errorString = "No such posts"
---------------------------------------------------------------------------------------------------------
-- Get single post
---------------------------------------------------------------------------------------------------------
getAjaxPostByIdR :: Int -> Handler TypedContent
getAjaxPostByIdR postId = do
  let postKey = toSqlKey $ fromIntegral postId
  maybePost <- runDB $ get postKey
  muser     <- maybeAuth
  mgroup    <- getMaybeGroup muser
  let permissions  = getPermissions   mgroup
      post         = Entity postKey (fromJust maybePost)
  when (isNothing maybePost) notFound

  let board = postBoard $ entityVal post
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let geoIpEnabled    = boardEnableGeoIp boardVal
      showPostDate    = boardShowPostDate boardVal
  files  <- runDB $ selectList [AttachedfileParentId ==. postKey] []
  timeZone <- getTimeZone
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  let postAndFiles = (entityVal post, map entityVal files)
      widget       = if postParent (entityVal post) == 0
                       then postWidget muser post files False True False geoIpEnabled showPostDate permissions timeZone maxLenOfFileName
                       else postWidget muser post files False True False geoIpEnabled showPostDate permissions timeZone maxLenOfFileName
  selectRep $ do
    provideRep $ bareLayout widget
    provideJson postAndFiles

getAjaxPostR :: Text -> Int -> Handler TypedContent
getAjaxPostR board postId = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let permissions  = getPermissions   mgroup
      geoIpEnabled = boardEnableGeoIp boardVal
      showPostDate    = boardShowPostDate boardVal
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. postId, PostDeleted ==. False] []
  when (isNothing maybePost) notFound
  let post    = fromJust maybePost
      postKey = entityKey $ fromJust maybePost
  files  <- runDB $ selectList [AttachedfileParentId ==. postKey] []
  timeZone <- getTimeZone
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  let postAndFiles = (entityVal post, map entityVal files)
      widget       = if postParent (entityVal $ fromJust maybePost) == 0
                       then postWidget muser post files False True False geoIpEnabled showPostDate permissions timeZone maxLenOfFileName
                       else postWidget muser post files False True False geoIpEnabled showPostDate permissions timeZone maxLenOfFileName
  selectRep $ do
    provideRep $ bareLayout widget
    provideJson postAndFiles
---------------------------------------------------------------------------------------------------------
-- Thread hiding
---------------------------------------------------------------------------------------------------------
getAjaxHideThreadR :: Text -> Int -> Int -> Handler TypedContent
getAjaxHideThreadR board threadId postId = do
  ht <- lookupSession "hidden-threads"
  case ht of
   Just xs' ->
     let xs = read (unpack xs') :: [(Text,[(Int,Int)])]
         ys = fromMaybe [] $ lookup board xs
         zs = filter ((/=board).fst) xs
         new = pack $ show ((board, (threadId,postId):ys):zs)
     in setSession "hidden-threads" new
   Nothing -> setSession "hidden-threads" $ T.concat ["[(",board,",[(",showText threadId,",",showText postId,")])]"]
  selectRep $ do
    provideRep $ bareLayout [whamlet|ok|]
    provideJson $ object [("ok", "hidden")]

getAjaxUnhideThreadR :: Text -> Int -> Handler TypedContent
getAjaxUnhideThreadR board threadId = do
  ht <- lookupSession "hidden-threads"
  case ht of
   Just xs' ->
     let xs = read (unpack xs') :: [(Text,[(Int,Int)])]
         ys = fromMaybe [] $ lookup board xs
         zs = filter ((/=board).fst) xs
         ms = filter ((/=threadId).fst) ys
         new = pack $ show (if null ms then zs else (board, ms):zs)
     in setSession "hidden-threads" new
   Nothing -> setSession "hidden-threads" "[]"
  selectRep $ do
    provideRep $ bareLayout [whamlet|ok|]
    provideJson $ object [("ok", "showed")]
---------------------------------------------------------------------------------------------------------
-- Board stats
---------------------------------------------------------------------------------------------------------
getAjaxBoardStatsR :: Handler TypedContent
getAjaxBoardStatsR = do
  diff       <- getBoardStats
  posterId   <- getPosterId
  hiddenThreads <- getAllHiddenThreads
  newDiff <- runDB $ forM diff $ \(board, lastId, _) -> do
    newPosts <- count [PostBoard ==. board, PostLocalId >. lastId, PostPosterId !=. posterId
                     ,PostDeleted ==. False, PostParent /<-. concatMap (map fst . snd) (filter ((==board).fst) hiddenThreads)]
    return (board, lastId, newPosts)
  saveBoardStats newDiff
  selectRep $ 
    provideJson $ object $ map (\(b,_,n) -> b .= n) newDiff
