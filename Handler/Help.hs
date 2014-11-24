{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Help where

import Import
import Yesod.Auth
import qualified Data.Text as T
---------------------------------------------------------------------------------------------
getHelpR :: Handler Html
getHelpR = do
  nameOfTheBoard   <- extraSiteName <$> getExtra
  msgrender        <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", msgrender MsgHelp]
    $(widgetFile "help")

getHelpMarkupR :: Handler Html
getHelpMarkupR = do
  muser       <- maybeAuth
  permissions <- getPermissions <$> getMaybeGroup muser
  
  nameOfTheBoard   <- extraSiteName <$> getExtra
  msgrender        <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", msgrender MsgMarkup]
    $(widgetFile "help/markup")

getHelpApiR :: Handler Html
getHelpApiR = do
  nameOfTheBoard   <- extraSiteName <$> getExtra
  msgrender        <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", msgrender MsgApi]
    $(widgetFile "help/api")
  
