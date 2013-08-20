{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin.Hellban where

import           Import
import           Yesod.Auth
import qualified Data.Text  as T
import           Data.Maybe (mapMaybe)
-------------------------------------------------------------------------------------------------------------
getHellBanNoPageR :: Handler Html
getHellBanNoPageR = getHellBanR 0

getHellBanR :: Int -> Handler Html
getHellBanR page = do
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup
  -------------------------------------------------------------------------------------------------------------------
  posterId  <- getPosterId
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count [PostHellbanned ==. True, PostDeleted ==. False]
  let f (Entity _ b) | isJust (boardViewAccess b) && notElem (fromJust group) (fromJust $ boardViewAccess b) = Just $ boardName b
                     | otherwise = Nothing
      boards'     = mapMaybe f boards
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
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", msgrender MsgHellbanning]
    $(widgetFile "admin/hellban")
-------------------------------------------------------------------------------------------------------------
getHellBanDoR :: Int  -> -- ^ Post internal ID
                Text -> -- ^ 'none' - don't hide this post; 'one' - hide it; 'all' - hide all this user's posts
                Bool -> -- ^ Hellban or not this user
                -- Handler TypedContent
                Handler Html
getHellBanDoR postId action ban = do
  let postKey = (toKey postId) :: Key Post
  post <- runDB $ get404 postKey
  -- post <- runDB $ get postKey
  -- void $ when (isNothing post) (trickyRedirect "error" (MsgError "fix i18n: нот фаунд ёпта") HomeR) -- FIX i18n
  -- let posterId = postPosterId $ fromJust post
  let posterId = postPosterId post
  case action of
    "one" -> void $ runDB $ update postKey [PostHellbanned =. True]
    "all" -> void $ runDB $ updateWhere [PostPosterId ==. posterId] [PostHellbanned =. True]
    _     -> return ()
  void $ when ban $
    -- void $ runDB $ insert $ Hellban { hellbanUserId = posterId, hellbanUserIp = (postIp $ fromJust post) }
    void $ runDB $ insert $ Hellban { hellbanUserId = posterId, hellbanUserIp = postIp post }
  -- trickyRedirect "ok" (MsgError "fix i18n: все норм") HomeR -- FIX i18n
  redirectUltDest HomeR

getHellBanUndoR :: Int  -> -- ^ Post internal ID
                  Text -> -- ^ 'show' - show this post; 'unban' - unban this user; 'both' - show post and unban
                  Handler Html
getHellBanUndoR postId action = do
  let postKey = (toKey postId) :: Key Post
  post <- runDB $ get404 postKey
  case action of
    "show"  -> runDB $ update postKey [PostHellbanned =. False]
    "unban" -> runDB $ deleteWhere [HellbanUserId ==. postPosterId post]
    "both"  -> runDB (update postKey [PostHellbanned =. False]) >> runDB (deleteWhere [HellbanUserId ==. postPosterId post])
  redirectUltDest HomeR
