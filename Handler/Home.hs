{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
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
      runDB $ insert Config { configCaptchaLength  = 10
                            , configACaptchaGuards = 3
                            , configCaptchaTimeout = 36000
                            , configReplyDelay     = 7
                            , configThreadDelay    = 30
                            }
      redirect HomeR
    nameOfTheBoard <- extraSiteName <$> getExtra
    defaultLayout $ do
        setTitle $ toHtml nameOfTheBoard
        $(widgetFile "homepage")
