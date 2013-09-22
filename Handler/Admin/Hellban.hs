{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin.Hellban where

import           Import
import           Yesod.Auth
import qualified Data.Text            as T
import           Data.Maybe           (mapMaybe)
import           Handler.Admin.Modlog (addModlogEntry)
import           Handler.EventSource  (sendDeletedPosts)
-------------------------------------------------------------------------------------------------------------
getHellBanNoPageR :: Handler Html
getHellBanNoPageR = getHellBanR 0

getHellBanR :: Int -> Handler Html
getHellBanR page = do
  muser  <- maybeAuth
  (permissions, group) <- pair getPermissions ((groupName . entityVal)<$>) <$> getMaybeGroup muser
  -------------------------------------------------------------------------------------------------------------------
  posterId  <- getPosterId
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count [PostHellbanned ==. True, PostDeleted ==. False]
  let boards'     = mapMaybe (ignoreBoards group) boards
      selectPosts = [PostBoard /<-. boards', PostDeleted ==. False, PostHellbanned ==. True]
      pages       = listPages showPosts numberOfPosts
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts, OffsetBy $ page*showPosts]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  geoIpEnabled' <- runDB $ mapM (getBy . BoardUniqName) $ nub $ map (postBoard . entityVal) posts
  let geoIpEnabled = map (boardName . entityVal) $ filter (boardEnableGeoIp . entityVal) $ catMaybes geoIpEnabled' 
  geoIps <- getCountries $ filter ((`elem`geoIpEnabled) . postBoard . entityVal . fst) postsAndFiles
  -------------------------------------------------------------------------------------------------------------------
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  timeZone        <- getTimeZone
  displaySage     <- getConfig configDisplaySage
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, titleDelimiter, msgrender MsgHellbanning]
    $(widgetFile "admin/hellban")
-------------------------------------------------------------------------------------------------------------
getHellBanDoR :: Int  -> -- ^ Post internal ID
                Text -> -- ^ 'none' - don't hide this post; 'one' - hide it; 'all' - hide all this user's posts
                Bool -> -- ^ Hellban or not this user
                Handler Html
getHellBanDoR postId action ban = do
  let postKey = toKey postId :: Key Post
  post <- runDB $ get404 postKey
  let posterId = postPosterId post
  case action of
    "one" -> void (runDB $ update postKey [PostHellbanned =. True]) >> sendDeletedPosts [post]
    "all" -> do
      void $ runDB $ updateWhere [PostPosterId ==. posterId] [PostHellbanned =. True]
      posts <- runDB $ selectList [PostPosterId ==. posterId] []
      sendDeletedPosts (map entityVal posts)
    _     -> return ()
  void $ when ban $ do
    addModlogEntry $ MsgModlogHellban posterId
    void $ runDB $ insert Hellban { hellbanUserId = posterId, hellbanUserIp = postIp post }
  redirectUltDest HomeR

getHellBanUndoR :: Int  -> -- ^ Post internal ID
                  Text -> -- ^ 'show' - show this post; 'unban' - unban this user; 'both' - show post and unban
                  Handler Html
getHellBanUndoR postId action = do
  let postKey = toKey postId :: Key Post
  post <- runDB $ get404 postKey
  let posterId = postPosterId post
  case action of
    "show"  -> runDB $ update postKey [PostHellbanned =. False]
    "unban" -> addModlogEntry (MsgModlogUnhellban posterId) >> runDB (deleteWhere [HellbanUserId ==. posterId])
    _       -> addModlogEntry (MsgModlogUnhellban posterId) >> runDB (update postKey [PostHellbanned =. False]) >>
              runDB (deleteWhere [HellbanUserId ==. posterId])
  redirectUltDest HomeR
