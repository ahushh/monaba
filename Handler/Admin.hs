{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin where

import           Import
import           Yesod.Auth
import           Yesod.Auth.HashDB    (setPassword)
import qualified Data.Text            as T
import           Handler.Admin.Modlog (addModlogEntry)
import           Utils.YobaMarkup     (fixReferences, doYobaMarkup)
-------------------------------------------------------------------------------------------------------------
getAdminR :: Handler Html
getAdminR = getAccountR
-------------------------------------------------------------------------------------------------------------
-- Account  
-------------------------------------------------------------------------------------------------------------
newPasswordForm :: Html -> MForm Handler (FormResult Text, Widget)
newPasswordForm extra = do
  (newPasswordRes, newPasswordView) <- mreq textField "" Nothing
  let widget = $(widgetFile "admin/account-form")
  return (newPasswordRes, widget)

getAccountR :: Handler Html
getAccountR = do
  muser       <- maybeAuth
  permissions <- getPermissions <$> getMaybeGroup muser

  (formWidget, formEnctype) <- generateFormPost newPasswordForm

  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ nameOfTheBoard <> titleDelimiter <> msgrender MsgAccount
    $(widgetFile "admin/account")
                 
postNewPasswordR :: Handler Html
postNewPasswordR = do
  ((result, _), _) <- runFormPost newPasswordForm
  let msgRedirect msg = setMessageI msg >> redirect AccountR
  case result of
    FormFailure []          -> msgRedirect MsgBadFormData
    FormFailure xs          -> msgRedirect (MsgError $ T.intercalate "; " xs) 
    FormMissing             -> msgRedirect MsgNoFormData
    FormSuccess newPassword -> do
      eUser               <- fromJust <$> maybeAuth
      userWithNewPassword <- liftIO $ setPassword newPassword (entityVal eUser)
      void $ runDB $ replace (entityKey eUser) userWithNewPassword
      msgRedirect MsgPasswordChanged
-------------------------------------------------------------------------------------------------------------
-- Thread management
-------------------------------------------------------------------------------------------------------------
getStickR :: Text -> Int -> Handler Html
getStickR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> addModlogEntry (MsgModlogStickThread thread board) >>
                          runDB (update pId [PostSticked =. not (postSticked p)]) >> redirectUltDest AdminR
    _                   -> setMessageI MsgNoSuchThread >> redirectUltDest AdminR
      
getLockR :: Text -> Int -> Handler Html
getLockR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> addModlogEntry (MsgModlogLockThread thread board) >>
                          runDB (update pId [PostLocked =. not (postLocked p)]) >> redirectUltDest AdminR
    _                   -> setMessageI MsgNoSuchThread >> redirectUltDest AdminR
      
getAutoSageR :: Text -> Int -> Handler Html
getAutoSageR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> addModlogEntry (MsgModlogAutosageThread thread board) >>
                          runDB (update pId [PostAutosage =. not (postAutosage p)]) >> redirectUltDest AdminR
    _                   -> setMessageI MsgNoSuchThread >> redirectUltDest AdminR

getMoveThreadR :: Text -> Int -> Text -> Handler Html
getMoveThreadR srcBoard thread dstBoard = do
  when (srcBoard == dstBoard) $ redirectUltDest (BoardNoPageR srcBoard)
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

  redirect $ BoardNoPageR dstBoard
-------------------------------------------------------------------------------------------------------------
-- Censorship management
-------------------------------------------------------------------------------------------------------------
getManageCensorshipR :: Int -> Censorship -> Handler Html
getManageCensorshipR fileId rating = do
  let fileKey = toKey fileId :: Key Attachedfile
  runDB $ update fileKey [AttachedfileRating =. showText rating]
  redirectUltDest HomeR
