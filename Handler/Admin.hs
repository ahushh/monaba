{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin where

import           Import
import           Yesod.Auth
import qualified Data.Text            as T
import           Handler.Admin.Modlog (addModlogEntry)
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
  runDB $ update fileKey [AttachedfileRating =. pack (show rating)]
  redirectUltDest HomeR
