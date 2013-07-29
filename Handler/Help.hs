{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Help where

import Import
import Yesod.Auth
import qualified Data.Text as T
---------------------------------------------------------------------------------------------
getHelpR :: Handler Html
getHelpR = do
    muser  <- maybeAuth
    boards <- runDB $ selectList ([]::[Filter Board]) []
    boardCategories  <- getConfig configBoardCategories
    nameOfTheBoard   <- extraSiteName <$> getExtra
    msgrender        <- getMessageRender
    defaultLayout $ do
        setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", msgrender MsgHelp]
        $(widgetFile "help")

getHelpMarkupR :: Handler Html
getHelpMarkupR = do
    muser  <- maybeAuth
    boards <- runDB $ selectList ([]::[Filter Board]) []
    boardCategories  <- getConfig configBoardCategories
    nameOfTheBoard   <- extraSiteName <$> getExtra
    msgrender        <- getMessageRender
    defaultLayout $ do
        setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", msgrender MsgMarkup]
        $(widgetFile "help/markup")

getHelpApiR :: Handler Html
getHelpApiR = do
    muser  <- maybeAuth
    boards <- runDB $ selectList ([]::[Filter Board]) []
    boardCategories  <- getConfig configBoardCategories
    nameOfTheBoard   <- extraSiteName <$> getExtra
    msgrender        <- getMessageRender
    defaultLayout $ do
        setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", msgrender MsgApi]
        $(widgetFile "help/api")
  
