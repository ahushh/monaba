module Handler.Admin where

import           Import
import           Handler.Admin.Modlog (addModlogEntry) 
import           Utils.YobaMarkup     (fixReferences, doYobaMarkup, makeExternalRef)
import           System.Process       (readProcess, runCommand)
import           Yesod.Auth.HashDB    (setPassword)
-------------------------------------------------------------------------------------------------------------
-- Main page
-------------------------------------------------------------------------------------------------------------
getAdminR :: Handler Html
getAdminR = do
  permissions <- ((fmap getPermissions) . getMaybeGroup) =<< maybeAuth
  defaultLayout $ do
    defaultTitleMsg MsgManagement
    $(widgetFile "admin")

getAdminSetupR :: Handler Html
getAdminSetupR = do
  runDB $ do
    storage <- count ([]::[Filter Storage])
    when (storage == 0) $ void $ insert $ Storage { storageUploadDir = 1 }
    config <- count ([]::[Filter Config])
    when (config == 0) $ void $ insert $ Config { configReplyDelay = 7
                                               , configThreadDelay = 30
                                               , configBoardCategories = []
                                               , configNewsBoard = "news"
                                               , configShowNews = 2
                                               , configMaxEditings = 10
                                               , configShowLatestPosts = 15
                                               , configAbout = Textarea "nothing to see here"
                                               , configHome = Textarea "<img id=\"main-image\" style=\"display:block; margin: 0 auto;\" src=\"static/home.jpg\">"
                                               , configModlogMaxEntries = 3000
                                               , configModlogEntriesPerPage =30
                                               , configAdaptiveCaptcha = 5
                                               , configHomeRecentPics = 4
                                               , configGlobalHellban = False
                                               }
    group <- count ([]::[Filter Group])
    when (group == 0) $ void $ insert $ Group { groupName = "Admin"
                                             , groupPermissions = [ ManageThreadP
                                                                  , ManageBoardP
                                                                  , ManageUsersP
                                                                  , ManageConfigP
                                                                  , DeletePostsP
                                                                  , ManagePanelP
                                                                  , ManageBanP
                                                                  , EditPostsP
                                                                  , ViewModlogP
                                                                  , AdditionalMarkupP
                                                                  , ViewIPAndIDP
                                                                  , HellBanP
                                                                  , ChangeFileRatingP
                                                                  , ShadowEditP
                                                                  , AppControlP
                                                                  , WordfilterP
                                                                  ]
                                             }
    user <- count ([]::[Filter User])
    when (user == 0) $ do
      let u = User { userName = "admin"
                   , userPassword = Nothing
                   , userGroup = "Admin"
                   }
      u' <- liftIO $ setPassword "admin" u
      void $ insert u'
  redirect $ AuthR LoginR

-------------------------------------------------------------------------------------------------------------
-- Lock editing
-------------------------------------------------------------------------------------------------------------
getAdminLockEditingR :: Int -> Handler Html
getAdminLockEditingR postKey = do
  let k = (toSqlKey $ fromIntegral postKey) :: Key Post
  post <- runDB $ get404 k
  p <- makeExternalRef (postBoard post) (postLocalId post)
  addModlogEntry ((if postLockEditing post then MsgModlogEnablePostEditing else MsgModlogDisablePostEditing) p)
  runDB $ update k [PostLockEditing =. not (postLockEditing post)]
  trickyRedirect "ok" (Left MsgSuccessEx) HomeR
-------------------------------------------------------------------------------------------------------------
-- Application control
-------------------------------------------------------------------------------------------------------------
getAdminRestartR :: Handler Html
getAdminRestartR = do
  AppSettings{..} <- appSettings <$> getYesod
  addModlogEntry MsgModlogAppRestart
  liftIO $ runCommand $ unpack appRestartCmd
  defaultLayout $ [whamlet|
                    ^{adminNavbarWidget}
                     ok
                  |]

getAdminGitPullR :: Handler Html
getAdminGitPullR = do
  addModlogEntry MsgModlogExecuteGitPull
  out <- liftIO $ readProcess "git" ["pull"] []
  defaultLayout $ [whamlet|
                    ^{adminNavbarWidget}
                    <pre>
                      #{out}
                  |]
-------------------------------------------------------------------------------------------------------------
-- Thread management
-------------------------------------------------------------------------------------------------------------
getMoveThreadR :: Text -> Int -> Text -> Handler Html
getMoveThreadR srcBoard thread dstBoard = do
  when (srcBoard == dstBoard) $ redirectUltDest (BoardNoPageR srcBoard)
  mThread <- runDB $ selectFirst [PostBoard ==. srcBoard, PostParent ==. 0, PostLocalId ==. thread] []
  when (isNothing mThread) $ (setMessageI MsgNoSuchThread) >> (redirect $ BoardNoPageR srcBoard)

  lastPostId <- (fmap (postLocalId . entityVal)) <$> runDB (selectFirst [PostBoard ==. dstBoard] [Desc PostLocalId])
  oldIds <- ((thread:) . map (postLocalId . entityVal)) <$> runDB (selectList [PostBoard ==. srcBoard, PostParent ==. thread] [Desc PostLocalId])
  let newId = maybe 1 (+1) lastPostId
  -- update OP post
  runDB $ updateWhere [PostBoard ==. srcBoard, PostLocalId ==. thread, PostParent ==. 0] [PostBoard =. dstBoard, PostLocalId =. newId]
  -- update replies
  runDB $ forM_ (zip (reverse oldIds) [newId+1..]) $ \(oldReplyId, newReplyId) -> do
    updateWhere [PostBoard ==. srcBoard, PostParent ==. thread, PostLocalId ==. oldReplyId]
                [PostBoard =. dstBoard, PostParent =. newId, PostLocalId =. newReplyId]
  -- fix referencies
  replies <- runDB (selectList  [PostBoard ==. dstBoard, PostParent ==. newId] [Desc PostLocalId])
  opPost  <- fromJust <$> runDB (selectFirst [PostBoard ==. dstBoard, PostParent ==. 0, PostLocalId ==. newId] [])
  let newIds = (newId:) $ map (postLocalId . entityVal) replies
  -- fix in replies
  forM_ replies $ \(Entity k p) -> do
    fixedMsg     <- liftIO $ fixReferences srcBoard (zip oldIds newIds) (Textarea $ postRawMessage p)
    msgFormatted <- doYobaMarkup (Just fixedMsg) dstBoard newId
    runDB $ update k [PostMessage =. msgFormatted, PostRawMessage =. unTextarea fixedMsg]
  -- fix in OP post
  fixedOpMsg     <- liftIO $ fixReferences srcBoard (zip oldIds newIds) (Textarea $ postRawMessage $ entityVal opPost)
  opMsgFormatted <- doYobaMarkup (Just fixedOpMsg) dstBoard 0
  runDB $ update (entityKey opPost) [PostMessage =. opMsgFormatted, PostRawMessage =. unTextarea fixedOpMsg]
  
  from <- makeExternalRef srcBoard thread
  to   <- makeExternalRef dstBoard thread
  addModlogEntry $ MsgModlogMoveThread from to

  redirect $ BoardNoPageR dstBoard

getChangeThreadR :: Int -> Int -> Handler Html
getChangeThreadR postKey threadLocalId = do
  let k = (toSqlKey $ fromIntegral postKey) :: Key Post
  mPost <- runDB $ get k
  case mPost of
    Nothing   -> (setMessageI MsgNoSuchPost) >> redirectUltDest HomeR
    Just post -> do
      let board = postBoard post
      thr <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. threadLocalId, PostParent ==. 0] []
      when (threadLocalId /= 0 && isNothing thr) $ (setMessageI MsgNoSuchThread) >> (redirectUltDest $ BoardNoPageR board)
      replies <- runDB $ selectList [PostBoard ==. board, PostParent ==. postLocalId post] []
      forM_ ((Entity k post):replies) $ \(Entity k' _) -> runDB $ update k' [PostParent =. threadLocalId]

      from <- makeExternalRef (postBoard post) (postParent post)
      to   <- makeExternalRef (postBoard post) threadLocalId
      p    <- makeExternalRef (postBoard post) (postLocalId post)
      addModlogEntry $ MsgModlogChangeThread from to p

      redirect $ BoardNoPageR board

getStickR :: Text -> Int -> Handler Html
getStickR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> do
      t <- makeExternalRef board thread
      runDB $ update pId [PostSticked =. not (postSticked p)]
      addModlogEntry ((if postSticked p then MsgModlogUnstickThread else MsgModlogStickThread) t)
      trickyRedirect "ok" (Left MsgSuccessEx) AdminR
    _                   -> trickyRedirect "error" (Left MsgNoSuchThread) AdminR
      
getLockR :: Text -> Int -> Handler Html
getLockR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> do
      t <- makeExternalRef board thread
      runDB $ update pId [PostLocked =. not (postLocked p)]
      addModlogEntry ((if postLocked p then MsgModlogUnlockThread else MsgModlogLockThread) t)
      trickyRedirect "ok" (Left MsgSuccessEx) AdminR
    _                   -> trickyRedirect "error" (Left MsgNoSuchThread) AdminR
      
getAutoSageR :: Text -> Int -> Handler Html
getAutoSageR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> do
      t <- makeExternalRef board thread
      runDB $ update pId [PostAutosage =. not (postAutosage p)]
      addModlogEntry ((if postAutosage p then MsgModlogAutosageOffThread else MsgModlogAutosageOnThread) t)
      trickyRedirect "ok" (Left MsgSuccessEx) AdminR
    _                   -> trickyRedirect "error" (Left MsgNoSuchThread) AdminR

-------------------------------------------------------------------------------------------------------------
-- Censorship management
-------------------------------------------------------------------------------------------------------------
getManageCensorshipR :: Int -> Censorship -> Handler Html
getManageCensorshipR fileId rating = do
  let key = toSqlKey $ fromIntegral fileId
  file <- runDB (get404 key)
  post <- runDB $ get404 $ attachedfileParentId file
  runDB $ update key [AttachedfileRating =. tshow rating]
  p <- makeExternalRef (postBoard post) (postLocalId post)
  addModlogEntry $ MsgModlogChangeRating (pack $ attachedfileName file) p (attachedfileRating file) (tshow rating)
  trickyRedirect "ok" (Left MsgSuccessEx) AdminR
