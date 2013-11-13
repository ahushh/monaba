{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin where

import           Import
import           Yesod.Auth
import           Yesod.Auth.HashDB    (setPassword)
import qualified Data.Text            as T
import           Handler.Admin.Modlog (addModlogEntry)
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
    setTitle $ toHtml $ T.concat [nameOfTheBoard, titleDelimiter, msgrender MsgAccount]
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

-------------------------------------------------------------------------------------------------------------
-- Censorship management
-------------------------------------------------------------------------------------------------------------
getManageCensorshipR :: Int -> Censorship -> Handler Html
getManageCensorshipR fileId rating = do
  let fileKey = toKey fileId :: Key Attachedfile
  runDB $ update fileKey [AttachedfileRating =. showText rating]
  redirectUltDest HomeR
