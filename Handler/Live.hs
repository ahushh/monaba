{-# LANGUAGE OverloadedStrings #-}
module Handler.Live where

import           Import
import           Yesod.Auth
import qualified Data.Text  as T
-------------------------------------------------------------------------------------------------------------
getLiveR :: Handler Html
getLiveR = do
  muser   <- maybeAuth
  mgroup  <- case muser of
    Just (Entity _ u) -> runDB $ getBy $ GroupUniqName $ userGroup u
    _                 -> return Nothing
  let permissions = maybe [] (groupPermissions . entityVal) mgroup
  let group       = (groupName . entityVal) <$> mgroup
      
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  let f (Entity _ b) | boardHidden b || (isJust (boardViewAccess b) && group /= boardViewAccess b) = Just $ boardName b
                     | otherwise                                                                = Nothing
      boards'  = catMaybes $ map f boards
  posts     <- runDB $ selectList [PostDeletedByOp ==. False, PostBoard /<-. boards'] [Desc PostDate, LimitTo 15]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  boardCategories <- getConfig configBoardCategories
  msgrender       <- getMessageRender
  let postsAndFiles = zip posts postFiles
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", msgrender MsgLatestPosts]
    $(widgetFile "live")
  
