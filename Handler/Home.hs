{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import qualified Prelude as P (head)
---------------------------------------------------------------------------------------------
getHomeR :: Handler Html
getHomeR = do
    muser   <- maybeAuth
    mgroup  <- case muser of
      Just (Entity _ u) -> runDB $ getBy $ GroupUniqName $ userGroup u
      _                 -> return Nothing
    let group  = (groupName . entityVal) <$> mgroup
    
    boards <- runDB $ selectList ([]::[Filter Board]) []
    nameOfTheBoard  <- extraSiteName <$> getExtra
    boardCategories <- getConfig configBoardCategories

    newsBoard  <- getConfig configNewsBoard
    showNews   <- getConfig configShowNews
    latestNews <- runDB $ selectList [PostBoard ==. newsBoard, PostParent ==. 0] [Desc PostLocalId, LimitTo showNews]
    timeZone   <- getTimeZone
    defaultLayout $ do
        setTitle $ toHtml nameOfTheBoard
        $(widgetFile "homepage")
