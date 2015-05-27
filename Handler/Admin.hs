{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin where

import           Import
import           Yesod.Auth
import qualified Data.Text         as T
import           Handler.Admin.Modlog (addModlogEntry) 
import           Utils.YobaMarkup     (fixReferences, doYobaMarkup, makeExternalRef)
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

-------------------------------------------------------------------------------------------------------------
getAdminR :: Handler Html
getAdminR = do
  muser           <- maybeAuth
  permissions     <- getPermissions <$> getMaybeGroup muser
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, titleDelimiter, msgrender MsgManagement]
    $(widgetFile "admin")

-------------------------------------------------------------------------------------------------------------
-- Thread management
-------------------------------------------------------------------------------------------------------------
getStickR :: Text -> Int -> Handler Html
getStickR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> do
      t <- makeExternalRef board thread
      addModlogEntry $ MsgModlogStickThread t
      runDB $ update pId [PostSticked =. not (postSticked p)]
      redirectUltDest AdminR
    _                   -> setMessageI MsgNoSuchThread >> redirectUltDest AdminR
      
getLockR :: Text -> Int -> Handler Html
getLockR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> do
      t <- makeExternalRef board thread
      addModlogEntry $ MsgModlogLockThread t
      runDB $ update pId [PostLocked =. not (postLocked p)]
      redirectUltDest AdminR
    _                   -> setMessageI MsgNoSuchThread >> redirectUltDest AdminR
      
getAutoSageR :: Text -> Int -> Handler Html
getAutoSageR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> do
      t <- makeExternalRef board thread
      addModlogEntry $ MsgModlogAutosageThread t
      runDB $ update pId [PostAutosage =. not (postAutosage p)]
      redirectUltDest AdminR
    _                   -> setMessageI MsgNoSuchThread >> redirectUltDest AdminR
