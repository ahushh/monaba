{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Help where

import Import
import Yesod.Auth
---------------------------------------------------------------------------------------------
getHelpR :: Handler Html
getHelpR = do
    muser  <- maybeAuth
    boards <- runDB $ selectList ([]::[Filter Board]) []
    defaultLayout $ do
        setTitleI MsgHelp
        $(widgetFile "help")
