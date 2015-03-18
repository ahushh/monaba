{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import qualified Prelude as P (head)
---------------------------------------------------------------------------------------------
getHomeR :: Handler Html
getHomeR = do
  muser <- maybeAuth
  (_, group) <- pair getPermissions ((groupName . entityVal)<$>) <$> getMaybeGroup muser
    
  boards          <- runDB $ selectList ([]::[Filter Board]) []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  config          <- getConfigEntity
  let boardCategories = configBoardCategories config
      newsBoard       = configNewsBoard       config
      showNews        = configShowNews        config
      homeContent     = configHome            config
      selectFiles p   = runDB $ selectList [AttachedfileParentId ==. entityKey p] []
      selectNews      = selectList [PostBoard ==. newsBoard, PostParent ==. 0, PostDeleted ==. False]
                                   [Desc PostLocalId, LimitTo showNews]
  newsAndFiles <- runDB selectNews >>= mapM (\p -> selectFiles p >>= (\files -> return (p, files)))
  timeZone     <- getTimeZone
  defaultLayout $ do
    setTitle $ toHtml nameOfTheBoard
    $(widgetFile "homepage")
