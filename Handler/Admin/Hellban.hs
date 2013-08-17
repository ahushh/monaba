{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin.Hellban where

import           Import
import           Yesod.Auth
import qualified Data.Text         as T
import           Handler.Posting (trickyRedirect)
-------------------------------------------------------------------------------------------------------------
getHellBanR :: Handler Html
getHellBanR = undefined

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
