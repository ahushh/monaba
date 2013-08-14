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
    let permissions = maybe [] (groupPermissions . entityVal) mgroup
    let group  = (groupName . entityVal) <$> mgroup
    
    boards <- runDB $ selectList ([]::[Filter Board]) []
    p <- runDB $ count ([]::[Filter User])
    when (p == 0) $ do
      void $ runDB $ insert Group { groupName   = "Admin"
                                 , groupPermissions = [minBound..maxBound] :: [Permission]
                                 }
      void $ runDB $ insert User { userName     = "admin"
                                 , userPassword = "de41b7fb99201d8334c23c014db35ecd92df81bc"
                                 , userSalt     = "1"
                                 , userGroup    = "Admin"
                                 }

    c <- runDB $ count ([]::[Filter Config])
    when (c == 0) $ do
      void $ runDB $ insert Config { configCaptchaLength   = 10
                                   , configACaptchaGuards  = 3
                                   , configCaptchaTimeout  = 36000
                                   , configReplyDelay      = 7
                                   , configThreadDelay     = 30
                                   , configBoardCategories = []
                                   , configNewsBoard       = "news"
                                   , configShowNews        = 2
                                   , configMaxEditings     = 10
                                   , configShowLatestPosts = 15
                                   }
      redirect HomeR
    nameOfTheBoard  <- extraSiteName <$> getExtra
    boardCategories <- getConfig configBoardCategories

    newsBoard  <- getConfig configNewsBoard
    showNews   <- getConfig configShowNews
    latestNews <- runDB $ selectList [PostBoard ==. newsBoard, PostParent ==. 0] [Desc PostLocalId, LimitTo showNews]
    timeZone   <- getTimeZone
    defaultLayout $ do
        setTitle $ toHtml nameOfTheBoard
        $(widgetFile "homepage")
