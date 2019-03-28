module Handler.Help where

import Import

getHelpR :: Handler Html
getHelpR = do
  about <- getConfig configAbout
  defaultLayout $ do
    defaultTitleMsg MsgHelp
    $(widgetFile "help")

getHelpMarkupR :: Handler Html
getHelpMarkupR = do
  permissions <- ((fmap getPermissions) . getMaybeGroup) =<< maybeAuth
  defaultLayout $ do
    defaultTitleMsg MsgMarkup
    $(widgetFile "help/markup")

getHelpSearchR :: Handler Html
getHelpSearchR = do
  defaultLayout $ do
    defaultTitleMsg MsgSearch
    $(widgetFile "help/search")
