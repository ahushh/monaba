{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Help where

import Import
import Yesod.Auth
---------------------------------------------------------------------------------------------
getHelpR :: Handler Html
getHelpR = do
  nameOfTheBoard <- extraSiteName <$> getExtra
  msgrender      <- getMessageRender
  about          <- getConfig configAbout
  defaultLayout $ do
    setTitle $ toHtml $ nameOfTheBoard <> titleDelimiter <> msgrender MsgHelp
    $(widgetFile "help")

getHelpMarkupR :: Handler Html
getHelpMarkupR = do
  muser       <- maybeAuth
  permissions <- getPermissions <$> getMaybeGroup muser
  
  nameOfTheBoard   <- extraSiteName <$> getExtra
  msgrender        <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ nameOfTheBoard <> titleDelimiter <> msgrender MsgMarkup
    $(widgetFile "help/markup")

getHelpApiR :: Handler Html
getHelpApiR = do
  nameOfTheBoard   <- extraSiteName <$> getExtra
  msgrender        <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ nameOfTheBoard <> titleDelimiter <> msgrender MsgApi
    $(widgetFile "help/api")
  
