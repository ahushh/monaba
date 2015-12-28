module Handler.Admin.Delete where

import           Import
import           Handler.Admin.Modlog (addModlogEntry) 
import           Utils.YobaMarkup     (makeExternalRef)
import qualified Data.Text            as T
-------------------------------------------------------------------------------------------------------------
-- View and recover deleted posts
-------------------------------------------------------------------------------------------------------------
getAdminDeletedFilteredR :: Text -> Int -> Int -> Handler Html
getAdminDeletedFilteredR board thread page = do
  permissions <- ((fmap getPermissions) . getMaybeGroup) =<< maybeAuth
  group       <- (fmap $ userGroup . entityVal) <$> maybeAuth
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count [PostDeleted ==. True, PostBoard ==. board, PostParent ==. thread]
  let boards'      = mapMaybe (getIgnoredBoard group) boards
      selectPosts' = [PostBoard /<-. boards', PostDeleted ==. True, PostBoard ==. board]
      selectPosts  = if thread == -1 then selectPosts' else (PostParent ==. thread):selectPosts'
      pages        = listPages showPosts numberOfPosts
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts, OffsetBy $ page*showPosts]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  AppSettings{..}  <- appSettings <$> getYesod
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleMsg MsgAdminDeletedPosts
    $(widgetFile "admin/deleted-filtered")

getAdminDeletedR :: Int -> Handler Html
getAdminDeletedR page = do
  permissions <- ((fmap getPermissions) . getMaybeGroup) =<< maybeAuth
  group       <- (fmap $ userGroup . entityVal) <$> maybeAuth
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count [PostDeleted ==. True]
  let boards'     = mapMaybe (getIgnoredBoard group) boards
      selectPosts = [PostBoard /<-. boards', PostDeleted ==. True]
      pages       = listPages showPosts numberOfPosts
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts, OffsetBy $ page*showPosts]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  AppSettings{..}  <- appSettings <$> getYesod
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleMsg MsgAdminAllDeletedPosts
    $(widgetFile "admin/deleted")

getAdminRecoverDeletedR :: Handler Html
getAdminRecoverDeletedR = do
  query  <- reqGetParams <$> getRequest
  let requestIds = map (toSqlKey . fromIntegral . tread . snd) $ reverse query
  runDB $ updateWhere [PostId <-. requestIds] [PostDeleted =. False]
  posts <- runDB $ selectList [PostId <-. requestIds] []
  bt <- forM posts $ \(Entity _ p) -> makeExternalRef (postBoard p) (postLocalId p)
  addModlogEntry $ MsgModlogRecoverPosts $ T.concat bt
  setMessageI MsgAdminRecoveredDeletedPosts
  redirectUltDest HomeR

