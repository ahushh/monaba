{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Api where

import           Import
import           Yesod.Auth
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
  let permissions  = getPermissions   mgroup
      geoIpEnabled = boardEnableGeoIp boardVal
      showPostDate = boardShowPostDate boardVal
      showEditHistory = boardShowEditHistory boardVal
      selectPosts  = if HellBanP `elem` permissions then selectPostsAll else selectPostsHB
      selectFiles p = runDB $ selectList [AttachedfileParentId ==. entityKey p] []
  postsAndFiles <- reverse <$> runDB selectPosts >>= mapM (\p -> do
    files <- selectFiles p
    return (p, files))
  let postsAndFiles' = map (\((Entity k p), fs) -> (Entity k (stripFields p permissions), fs)) postsAndFiles
  t <- runDB $ count [PostBoard ==. board, PostLocalId ==. thread, PostParent ==. 0, PostDeleted ==. False]
  timeZone    <- getTimeZone
  rating      <- getCensorshipRating
  displaySage <- getConfig configDisplaySage
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
                                   ^{postWidget post files rating displaySage True True False geoIpEnabled permissions timeZone maxLenOfFileName showPostDate showEditHistory}
                               |]
          provideJson $ object [ "posts"  .= postsAndFiles'
                               , "config" .= object [ "rating"       .= rating
                                                    , "sage"         .= displaySage
                                                    , "in_thread"    .= True
                                                    , "can_post"     .= True
                                                    , "show_parent"  .= False
                                                    , "permissions"  .= permissions
                                                    , "time_zone"    .= timeZone
                                                    , "max_file_len" .= maxLenOfFileName
                                                    , "show_date"    .= showPostDate
                                                    , "show_history" .= showEditHistory
                                                    ]
                               ]

getApiDeletedPostsR :: Text -> Int -> Handler TypedContent
getApiDeletedPostsR board thread = do
  posterId <- getPosterId
  let selectPostsAll = selectList [PostDeletedByOp ==. True, PostBoard ==. board, PostParent ==. thread, PostDeleted ==. False] [Desc PostDate]
      selectPostsHB  = selectList ( [PostDeletedByOp ==. True, PostBoard ==. board, PostParent ==. thread
                                    ,PostDeleted ==. False, PostHellbanned ==. False] ||.
                                    [PostDeletedByOp ==. True, PostBoard ==. board, PostParent ==. thread
                                    ,PostDeleted ==. False, PostHellbanned ==. True, PostPosterId ==. posterId]
                                  ) [Desc PostDate]
      errorString = "No such posts"
  getPostsHelper selectPostsAll selectPostsHB board thread errorString


getApiAllPostsR :: Text -> Int -> Handler TypedContent
getApiAllPostsR board thread = do
  posterId <- getPosterId
  let selectPostsAll = selectList [PostDeletedByOp ==. False, PostBoard ==. board,
                                   PostParent ==. thread, PostDeleted ==. False] [Desc PostDate]
      selectPostsHB  = selectList ( [PostDeletedByOp ==. False, PostBoard ==. board,
                                     PostParent ==. thread, PostDeleted ==. False, PostHellbanned ==. False] ||.
                                    [PostDeletedByOp ==. False, PostBoard ==. board,
                                     PostParent ==. thread, PostDeleted ==. False, PostHellbanned ==. True,
                                     PostPosterId ==. posterId]
                                  ) [Desc PostDate]
      errorString = "No posts in this thread"
  getPostsHelper selectPostsAll selectPostsHB board thread errorString

getApiNewPostsR :: Text -> Int -> Int -> Handler TypedContent
getApiNewPostsR board thread postId = do
  posterId <- getPosterId
  let selectPostsAll = selectList [PostDeletedByOp ==. False, PostBoard ==. board
                                  ,PostParent ==. thread, PostLocalId >. postId, PostDeleted ==. False] [Desc PostDate]
      selectPostsHB  = selectList ( [PostDeletedByOp ==. False, PostBoard ==. board
                                    ,PostParent ==. thread, PostLocalId >. postId, PostDeleted ==. False, PostHellbanned ==. False] ||.
                                    [PostDeletedByOp ==. False, PostBoard ==. board
                                    ,PostParent ==. thread, PostLocalId >. postId, PostDeleted ==. False, PostHellbanned ==. True
                                    ,PostPosterId ==. posterId]
                                  ) [Desc PostDate]
      errorString = "No new posts"
  getPostsHelper selectPostsAll selectPostsHB board thread errorString

getApiLastPostsR :: Text -> Int -> Int -> Handler TypedContent
getApiLastPostsR board thread postCount = do
  posterId <- getPosterId
  let selectPostsAll = selectList [PostDeletedByOp ==. False, PostBoard ==. board
                                  ,PostParent ==. thread, PostDeleted ==. False] [Desc PostDate, LimitTo postCount]
      selectPostsHB  = selectList ( [PostDeletedByOp ==. False, PostBoard ==. board
                                    ,PostParent ==. thread, PostDeleted ==. False, PostHellbanned ==. False] ||.
                                    [PostDeletedByOp ==. False, PostBoard ==. board
                                    ,PostParent ==. thread, PostDeleted ==. False, PostHellbanned ==. True, PostPosterId ==. posterId]
                                  ) [Desc PostDate, LimitTo postCount]
      errorString = "No such posts"
  getPostsHelper selectPostsAll selectPostsHB board thread errorString

---------------------------------------------------------------------------------------------------------
stripFields :: Post -> [Permission] -> Post
stripFields post permissions
  | ViewIPAndIDP `elem` permissions = post { postPassword = "", postOwner = Nothing }
  | otherwise                       = post { postPassword = "", postIp = "", postCountry = Nothing, postPosterId = "", postOwner = Nothing }

getApiPostR :: Text -> Int -> Handler TypedContent
getApiPostR board postId = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  posterId <- getPosterId
  checkViewAccess mgroup boardVal
  let permissions  = getPermissions   mgroup
      geoIpEnabled = boardEnableGeoIp boardVal
      showPostDate = boardShowPostDate boardVal
      showEditHistory = boardShowEditHistory boardVal
      selectPosts    = if HellBanP `elem` permissions then selectPostsAll else selectPostsHB
      selectPostsAll = [PostBoard ==. board, PostLocalId ==. postId, PostDeleted ==. False]
      selectPostsHB  = [PostBoard ==. board, PostLocalId ==. postId, PostDeleted ==. False, PostHellbanned ==. False] ||.
                       [PostBoard ==. board, PostLocalId ==. postId, PostDeleted ==. False, PostHellbanned ==. True, PostPosterId ==. posterId]
  mePost <- runDB $ selectFirst selectPosts []
  when (isNothing mePost) notFound
  let ePost = fromJust mePost
      post  = entityVal ePost
      post' = stripFields post permissions
      postKey = entityKey $ fromJust mePost
  files  <- runDB $ selectList [AttachedfileParentId ==. postKey] []
  timeZone    <- getTimeZone
  rating      <- getCensorshipRating
  displaySage <- getConfig configDisplaySage
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  let widget = if postParent post == 0
               then postWidget ePost files rating displaySage True True False geoIpEnabled permissions timeZone maxLenOfFileName showPostDate showEditHistory
               else postWidget ePost files rating displaySage True True False geoIpEnabled permissions timeZone maxLenOfFileName showPostDate showEditHistory
  selectRep $ do
    provideRep $ bareLayout widget
    provideJson $ object [ "post"   .= (post', files)
                         , "config" .= object [ "rating"       .= rating
                                              , "sage"         .= displaySage
                                              , "in_thread"    .= True
                                              , "can_post"     .= True
                                              , "show_parent"  .= False
                                              , "permissions"  .= permissions
                                              , "time_zone"    .= timeZone
                                              , "max_file_len" .= maxLenOfFileName
                                              , "show_date"    .= showPostDate
                                              , "show_history" .= showEditHistory
                                              ]
                         ]
---------------------------------------------------------------------------------------------------------
getApiHideThreadR :: Text -> Int -> Handler TypedContent
getApiHideThreadR board threadId
  | threadId <= 0 = selectRep $ do
      provideRep  $ bareLayout [whamlet|Error: bad thread ID|]
      provideJson $ object [("error", "bad thread ID")]
  | otherwise = do  
      ht <- lookupSession "hidden-threads"
      case ht of
        Just xs' ->
          let xs  = read (unpack xs') :: [(Text,[Int])]
              ys  = fromMaybe [] $ lookup board xs
              zs  = filter ((/=board).fst) xs
              new = showText ((board, threadId:ys):zs)
          in setSession "hidden-threads" new
        Nothing -> setSession "hidden-threads" $ "[("<>board<>",["<>showText threadId<>"])]"
      selectRep $ do
        provideRep  $ bareLayout [whamlet|ok: hidden|]
        provideJson $ object [("ok", "hidden")]

getApiUnhideThreadR :: Text -> Int -> Handler TypedContent
getApiUnhideThreadR board threadId
  | threadId <= 0 = selectRep $ do
      provideRep  $ bareLayout [whamlet|Error: bad thread ID|]
      provideJson $ object [("error", "bad thread ID")]
  | otherwise = do
      ht <- lookupSession "hidden-threads"
      case ht of
        Just xs' ->
          let xs  = read (unpack xs') :: [(Text,[Int])]
              ys  = fromMaybe [] $ lookup board xs
              zs  = filter ((/=board).fst) xs
              ms  = filter (/=threadId) ys
              new = showText (if null ms then zs else (board, ms):zs)
          in setSession "hidden-threads" new
        Nothing -> setSession "hidden-threads" "[]"
      selectRep $ do
        provideRep  $ bareLayout [whamlet|ok: showed|]
        provideJson $ object [("ok", "showed")]
---------------------------------------------------------------------------------------------------------
getApiBoardStatsR :: Handler TypedContent
getApiBoardStatsR = do
  diff       <- getBoardStats
  posterId   <- getPosterId
  hiddenThreads <- getAllHiddenThreads
  newDiff <- runDB $ forM diff $ \(board, lastId, _) -> do
    newPosts <- count [PostBoard ==. board, PostLocalId >. lastId, PostPosterId !=. posterId
                     ,PostDeleted ==. False, PostHellbanned ==. False, PostParent /<-. concatMap snd (filter ((==board).fst) hiddenThreads)]
    return (board, lastId, newPosts)
  saveBoardStats newDiff
  selectRep $ 
    provideJson $ object $ map (\(b,_,n) -> (b, toJSON n)) newDiff
