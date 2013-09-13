{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import qualified Prelude as P (head)
---------------------------------------------------------------------------------------------
getHomeR :: Handler Html
getHomeR = do
  muser                <- maybeAuth
  (permissions, group) <- pair getPermissions ((groupName . entityVal)<$>) <$> getMaybeGroup muser
  
  boards <- runDB $ selectList ([]::[Filter Board]) []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  boardCategories <- getConfig configBoardCategories

  newsBoard  <- getConfig configNewsBoard
  showNews   <- getConfig configShowNews
  latestNews <- runDB $ selectList [PostBoard ==. newsBoard, PostParent ==. 0, PostDeleted ==. False, PostHellbanned ==. False]
                                  [Desc PostLocalId, LimitTo showNews]
  timeZone   <- getTimeZone
  defaultLayout $ do
    setTitle $ toHtml nameOfTheBoard
    $(widgetFile "homepage")
