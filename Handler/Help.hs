{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Help where

import Import
import Yesod.Auth
---------------------------------------------------------------------------------------------
getHelpR :: Handler Html
getHelpR = do
    muser  <- maybeAuth
    boards <- runDB $ selectList ([]::[Filter Board]) []
    boardCategories  <- getConfig configBoardCategories
    defaultLayout $ do
        setTitleI MsgHelp
        $(widgetFile "help")

getHelpMarkupR :: Handler Html
getHelpMarkupR = do
    muser  <- maybeAuth
    boards <- runDB $ selectList ([]::[Filter Board]) []
    boardCategories  <- getConfig configBoardCategories
    defaultLayout $ do
        setTitleI MsgMarkup
        $(widgetFile "help/markup")
