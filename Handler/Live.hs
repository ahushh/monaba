{-# LANGUAGE OverloadedStrings #-}
module Handler.Live where

import           Import
import           Yesod.Auth
import qualified Data.Text         as T
-------------------------------------------------------------------------------------------------------------
getLiveR :: Handler Html
getLiveR = do
  muser     <- maybeAuth
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  posts     <- runDB $ selectList [] [Desc PostDate, LimitTo 15]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  boardCategories <- getConfig configBoardCategories
  let postsAndFiles = zip posts postFiles
  setUltDestCurrent

  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", "Last posts"]
    $(widgetFile "live")
  
