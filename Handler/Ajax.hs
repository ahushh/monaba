{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Ajax where

import           Import
import qualified Data.Text as T (concat)
import qualified Data.Map as M
import           Utils.YobaMarkup   (doYobaMarkup)
---------------------------------------------------------------------------------------------------------
-- Get multiple posts
---------------------------------------------------------------------------------------------------------
getPostsHelper :: YesodDB App [Entity Post] -> -- ^ Post selector: selectList [...] [...]
                 YesodDB App [Entity Post] -> -- ^ Post selector: selectList [...] [...]
                 Text -> -- ^ Board name
                 Int  -> -- ^ Thread internal ID
                 Text -> -- ^ Error string
                 Handler TypedContent
getPostsHelper selectPostsAll selectPostsHB board thread errorString = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let permissions   = getPermissions   mgroup
      geoIpEnabled  = boardEnableGeoIp boardVal
      enablePM      = boardEnablePM boardVal
      selectPosts   = if elem HellBanP permissions then selectPostsAll else selectPostsHB 
      selectFiles p = runDB $ selectList [AttachedfileParentId ==. entityKey p] []
      showPostDate    = boardShowPostDate boardVal
  postsAndFiles <- reverse <$> runDB selectPosts >>= mapM (\p -> do
    files <- selectFiles p
    return (p, files))
  t <- runDB $ count [PostBoard ==. board, PostLocalId ==. thread, PostParent ==. 0, PostDeleted ==. False]
  case () of
    _ | t == 0              -> selectRep $ do
          provideRep  $ bareLayout [whamlet|No such thread|]
--          provideJson $ object [("error", toJSON ("No such thread"::Text))]
      | null postsAndFiles -> selectRep $ do
          provideRep  $ bareLayout [whamlet|#{errorString}|]
--          provideJson $ object [("error", toJSON errorString)]
      | otherwise          -> selectRep $ do
          provideRep  $ bareLayout [whamlet|
                               $forall (post, files) <- postsAndFiles
                                   ^{postWidget post files True True False geoIpEnabled showPostDate permissions 0 enablePM}
                               |]
--          provideJson $ map (entityVal *** map entityVal) postsAndFiles

getAjaxDeletedPostsR :: Text -> Int -> Handler TypedContent
getAjaxDeletedPostsR board thread = do
  posterId <- getPosterId
  let selectPostsAll = selectList [PostDeletedByOp ==. True, PostBoard ==. board, PostParent ==. thread, PostDeleted ==. False] [Desc PostDate]
      selectPostsHB  = selectList ([PostDeletedByOp ==. True, PostBoard ==. board, PostParent ==. thread, PostDeleted ==. False, PostHellbanned ==. False] ||.
                                   [PostDeletedByOp ==. True, PostBoard ==. board, PostParent ==. thread, PostDeleted ==. False, PostHellbanned ==. True, PostPosterId ==. posterId]
                                  ) [Desc PostDate]
      errorString    = "No such posts"
  getPostsHelper selectPostsAll selectPostsHB board thread errorString

getAjaxAllPostsR :: Text -> Int -> Handler TypedContent
getAjaxAllPostsR board thread = do
  posterId <- getPosterId                                
  let selectPostsAll = selectList [PostDeletedByOp ==. False, PostBoard ==. board, PostParent ==. thread, PostDeleted ==. False] [Desc PostDate]
      selectPostsHB  = selectList ([PostDeletedByOp ==. False, PostBoard ==. board, PostParent ==. thread, PostDeleted ==. False, PostHellbanned ==. False] ||.
                                   [PostDeletedByOp ==. False, PostBoard ==. board, PostParent ==. thread, PostDeleted ==. False, PostHellbanned ==. True, PostPosterId ==. posterId]
                                  ) [Desc PostDate]
      errorString    = "No posts in this thread"
  getPostsHelper selectPostsAll selectPostsHB board thread errorString

getAjaxNewPostsR :: Text -> Int -> Int -> Handler TypedContent
getAjaxNewPostsR board thread postId = do
  posterId <- getPosterId
  let selectPostsAll = selectList [PostDeletedByOp ==. False, PostBoard ==. board, PostParent ==. thread, PostLocalId >. postId, PostDeleted ==. False] [Desc PostDate]
      selectPostsHB  = selectList ([PostDeletedByOp ==. False, PostBoard ==. board, PostParent ==. thread, PostLocalId >. postId, PostDeleted ==. False, PostHellbanned ==. False] ||.
                                   [PostDeletedByOp ==. False, PostBoard ==. board, PostParent ==. thread, PostLocalId >. postId, PostDeleted ==. False, PostHellbanned ==. True, PostPosterId ==. posterId]
                                  ) [Desc PostDate]
      errorString    = "No new posts"
  getPostsHelper selectPostsAll selectPostsHB board thread errorString

getAjaxLastPostsR :: Text -> Int -> Int -> Handler TypedContent
getAjaxLastPostsR board thread postCount = do
  posterId <- getPosterId
  let selectPostsAll = selectList [PostDeletedByOp ==. False, PostBoard ==. board, PostParent ==. thread, PostDeleted ==. False] [Desc PostDate, LimitTo postCount]
      selectPostsHB  = selectList ([PostDeletedByOp ==. False, PostBoard ==. board, PostParent ==. thread, PostDeleted ==. False, PostHellbanned ==. False] ||.
                                     [PostDeletedByOp ==. False, PostBoard ==. board, PostParent ==. thread, PostDeleted ==. False, PostHellbanned ==. True, PostPosterId ==. posterId]
                                  ) [Desc PostDate, LimitTo postCount]
      errorString    = "No such posts"
  getPostsHelper selectPostsAll selectPostsHB board thread errorString
---------------------------------------------------------------------------------------------------------
-- Get single post
---------------------------------------------------------------------------------------------------------
getAjaxPostByIdR :: Int -> Handler TypedContent
getAjaxPostByIdR postId = do
  let postKey = (toSqlKey $ fromIntegral postId) :: Key Post
  maybePost <- runDB $ get postKey
  muser     <- maybeAuth
  mgroup    <- getMaybeGroup muser
  let permissions  = getPermissions   mgroup
      post         = Entity postKey (fromJust maybePost)
  posterId <- getPosterId
  when (isNothing maybePost) notFound
  unless (checkHellbanned (entityVal post) permissions posterId) $ notFound

  let board = postBoard $ entityVal post
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let geoIpEnabled    = boardEnableGeoIp boardVal
      showPostDate    = boardShowPostDate boardVal
      enablePM        = boardEnablePM boardVal
  files  <- runDB $ selectList [AttachedfileParentId ==. postKey] []
  let postAndFiles = (entityVal post, map entityVal files)
      widget       = if postParent (entityVal post) == 0
                       then postWidget post files False True False geoIpEnabled showPostDate permissions 0 enablePM
                       else postWidget post files False True False geoIpEnabled showPostDate permissions 0 enablePM
  selectRep $ do
    provideRep $ bareLayout widget
    provideJson $ postAndFiles

getAjaxPostR :: Text -> Int -> Handler TypedContent
getAjaxPostR board postId = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let permissions  = getPermissions   mgroup
      geoIpEnabled = boardEnableGeoIp boardVal
      showPostDate = boardShowPostDate boardVal
      enablePM     = boardEnablePM boardVal
  posterId  <- getPosterId
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. postId] []
  when (isNothing maybePost) notFound
  when (postDeleted (entityVal $ fromJust maybePost) && DeletePostsP `notElem` permissions) $ notFound
  unless (checkHellbanned (entityVal $ fromJust maybePost) permissions posterId) $ notFound
  let post    = fromJust maybePost
      postKey = entityKey $ fromJust maybePost
  files  <- runDB $ selectList [AttachedfileParentId ==. postKey] []
  let postAndFiles = (entityVal post, map entityVal files)
      widget       = if postParent (entityVal $ fromJust maybePost) == 0
                       then postWidget post files False True False geoIpEnabled showPostDate permissions 0 enablePM
                       else postWidget post files False True False geoIpEnabled showPostDate permissions 0 enablePM
  selectRep $ do
    provideRep $ bareLayout widget

getAjaxPostRawMsgR :: Text -> Int -> Handler TypedContent
getAjaxPostRawMsgR board postId = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let permissions  = getPermissions   mgroup
      geoIpEnabled = boardEnableGeoIp boardVal
      showPostDate = boardShowPostDate boardVal
      enablePM     = boardEnablePM boardVal
  posterId  <- getPosterId
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. postId] []
  when (isNothing maybePost) notFound
  when (postDeleted (entityVal $ fromJust maybePost) && DeletePostsP `notElem` permissions) $ notFound
  unless (checkHellbanned (entityVal $ fromJust maybePost) permissions posterId) $ notFound
  let post         = fromJust maybePost
      itsforMe uid = maybe True (==uid) (postDestUID $ entityVal post) || uid == (postPosterId $ entityVal post)
  selectRep $ do
    provideJson $ object [("rawMessage" .= if enablePM && not (itsforMe posterId) then (""::Text) else postRawMessage $ entityVal post)]
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
   Nothing -> setSession "hidden-threads" $ T.concat ["[(",board,",[(",tshow threadId,",",tshow postId,")])]"]
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
    newPosts <- count [PostBoard ==. board, PostLocalId >. lastId, PostPosterId !=. posterId, PostHellbanned ==. False
                     ,PostDeleted ==. False, PostParent /<-. concatMap (map fst . snd) (filter ((==board).fst) hiddenThreads)]
    return (board, lastId, newPosts)
  saveBoardStats newDiff
  selectRep $ 
    provideJson $ object $ map (\(b,_,n) -> b .= n) newDiff

getAjaxBoardStatsReadR :: Handler TypedContent
getAjaxBoardStatsReadR = do
  cleanAllBoardsStats
  selectRep $ do
    provideRep $ bareLayout [whamlet|ok|]
    provideJson $ object [("ok","marked as read")]
---------------------------------------------------------------------------------------------------------
-- Post preview
---------------------------------------------------------------------------------------------------------
postAjaxPostPreviewR :: Handler Html
postAjaxPostPreviewR = do
  value <- requireJsonBody
  let message = Textarea <$> M.lookup ("msg" :: Text) value
      board   = fromMaybe "error " $ M.lookup ("board" :: Text) value
      thread  = maybe (0::Int) tread $ M.lookup ("thread" :: Text) value
  messageFormatted <- doYobaMarkup message board thread
  bareLayout [whamlet|#{preEscapedToHtml $ unTextarea messageFormatted}|]
