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
  boardCategories <- getConfig configBoardCategories
  newsBoard       <- getConfig configNewsBoard
  showNews        <- getConfig configShowNews
  let selectFiles p = runDB $ selectList [AttachedfileParentId ==. entityKey p] []
      selectNews    = selectList [PostBoard ==. newsBoard, PostParent ==. 0, PostDeleted ==. False]
                                 [Desc PostLocalId, LimitTo showNews]
  newsAndFiles <- runDB selectNews >>= mapM (\p -> selectFiles p >>= (\files -> return (p, files)))
  timeZone     <- getTimeZone
  defaultLayout $ do
    setTitle $ toHtml nameOfTheBoard
    $(widgetFile "homepage")
