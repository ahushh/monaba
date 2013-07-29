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
  let hiddenBoards = catMaybes $ map (\(Entity _ b) -> if boardHidden b then Just $ boardName b else Nothing) boards
  posts     <- runDB $ selectList [PostBoard /<-. hiddenBoards] [Desc PostDate, LimitTo 15]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  boardCategories <- getConfig configBoardCategories
  msgrender       <- getMessageRender
  let postsAndFiles = zip posts postFiles
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", msgrender MsgLatestPosts]
    $(widgetFile "live")
  
