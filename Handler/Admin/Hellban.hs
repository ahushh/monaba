module Handler.Admin.Hellban where

import           Import
import           Handler.Admin.Modlog (addModlogEntry) 
import           Utils.YobaMarkup     (makeExternalRef)
-------------------------------------------------------------------------------------------------------------
getHellBanNoPageR :: Handler Html
getHellBanNoPageR = getHellBanR 0

getHellBanR :: Int -> Handler Html
getHellBanR page = do
  permissions <- ((fmap getPermissions) . getMaybeGroup) =<< maybeAuth
  group       <- (fmap $ userGroup . entityVal) <$> maybeAuth
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count [PostDeleted ==. False, PostHellbanned ==. True]
  let boards'     = mapMaybe (getIgnoredBoard group) boards
      selectPosts = [PostBoard /<-. boards', PostDeleted ==. False, PostHellbanned ==. True]
      pages       = listPages showPosts numberOfPosts
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts, OffsetBy $ page*showPosts]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  AppSettings{..}  <- appSettings <$> getYesod
  defaultLayout $ do
    defaultTitleMsg MsgHellbanning
    $(widgetFile "admin/hellban")
-- ------------------------------------------------------------------------------------------------------------
getHellBanDoR :: Int  -> -- ^ Post internal ID
                Text -> -- ^ 'none' - don't hide this post; 'one' - hide it; 'all' - hide all this user's posts
                Bool -> -- ^ Hellban or not this user
               Handler Html
getHellBanDoR postId action ban = do
 let postKey = (toSqlKey $ fromIntegral postId) :: Key Post
 post <- runDB $ get404 postKey
 let posterId = postPosterId post
 case action of
   "one" -> do
     void $ runDB $ update postKey [PostHellbanned =. True]
     p <- makeExternalRef (postBoard post) (postLocalId post)
     addModlogEntry $ MsgModlogHellbanHidePost p
   "all" -> do
     void $ runDB $ updateWhere [PostPosterId ==. posterId] [PostHellbanned =. True]
     addModlogEntry $ MsgModlogHellbanHideAllPosts posterId
   _     -> return ()
 void $ when ban $ do
   addModlogEntry $ MsgModlogHellban posterId
   void $ runDB $ insert $ Hellban { hellbanUid = posterId, hellbanIp = postIp post }
 redirectUltDest HomeR

getHellBanUndoR :: Int  -> -- ^ Post internal ID
                 Text -> -- ^ 'show' - show this post; 'unban' - unban this user; 'both' - show post and unban
                 Handler Html
getHellBanUndoR postId action = do
 let postKey = (toSqlKey $ fromIntegral postId) :: Key Post
 post <- runDB $ get404 postKey
 let posterId = postPosterId post
 case action of
   "show"  -> do
     runDB $ update postKey [PostHellbanned =. False]
     p <- makeExternalRef (postBoard post) (postLocalId post)
     addModlogEntry $ MsgModlogHellbanShowPost p
   "unban" -> do
     addModlogEntry (MsgModlogUnhellban posterId)
     runDB $ deleteWhere [HellbanUid ==. postPosterId post]
   "both"  -> do
     addModlogEntry (MsgModlogUnhellban posterId)
     addModlogEntry $ MsgModlogHellbanShowAllPosts posterId
     runDB (update postKey [PostHellbanned =. False])
     runDB (deleteWhere [HellbanUid ==. postPosterId post])
 redirectUltDest HomeR
