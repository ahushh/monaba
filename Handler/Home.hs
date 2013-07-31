{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import qualified Prelude as P (head)
---------------------------------------------------------------------------------------------
getHomeR :: Handler Html
getHomeR = do
    muser  <- maybeAuth
    boards <- runDB $ selectList ([]::[Filter Board]) []
    p <- runDB $ count ([]::[Filter Person])
    when (p == 0) $
      void $ runDB $ insert Person { personName     = "admin"
                                   , personPassword = "de41b7fb99201d8334c23c014db35ecd92df81bc"
                                   , personSalt     = "1"
                                   , personRole     = Admin
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
                                   }
      redirect HomeR
    nameOfTheBoard  <- extraSiteName <$> getExtra
    boardCategories <- getConfig configBoardCategories

    newsBoard  <- getConfig configNewsBoard
    showNews   <- getConfig configShowNews
    latestNews <- runDB $ selectList [PostBoard ==. newsBoard, PostParent ==. 0] [Desc PostLocalId, LimitTo showNews]
    defaultLayout $ do
        setTitle $ toHtml nameOfTheBoard
        $(widgetFile "homepage")
