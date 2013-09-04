{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin.Search where

import           Import
import           Yesod.Auth
import qualified Data.Text  as T
import           Data.Maybe (mapMaybe)
-------------------------------------------------------------------------------------------------------------
getAdminSearchNoPageR :: Text -> Handler Html
getAdminSearchNoPageR = flip getAdminSearchR 0

getAdminSearchR :: Text -> Int -> Handler Html
getAdminSearchR = helper False

getAdminSearchOnlyHBR :: Text -> Int -> Handler Html
getAdminSearchOnlyHBR = helper True

getAdminSearchOnlyHBNoPageR :: Text -> Handler Html
getAdminSearchOnlyHBNoPageR = flip getAdminSearchOnlyHBR 0
-------------------------------------------------------------------------------------------------------------
helper :: Bool -> Text -> Int -> Handler Html
helper onlyHellbanned posterId page = do
  muser                <- maybeAuth
  (permissions, group) <- pair getPermissions ((groupName . entityVal)<$>) <$> getMaybeGroup muser
  -------------------------------------------------------------------------------------------------------------------
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count ([PostDeleted ==. False, PostPosterId ==. posterId] ++
                                 [PostHellbanned ==. True | onlyHellbanned])
  let boards'     = mapMaybe (ignoreBoards group) boards
      selectPosts = [PostBoard /<-. boards', PostDeleted ==. False, PostPosterId ==. posterId] ++
                    if onlyHellbanned then [PostHellbanned ==. True] else []
      pages       = listPages showPosts numberOfPosts
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts, OffsetBy $ page*showPosts]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  geoIpEnabled' <- runDB $ mapM (getBy . BoardUniqName) $ nub $ map (postBoard . entityVal) posts
  let geoIpEnabled = map (boardName . entityVal) $ filter (boardEnableGeoIp . entityVal) $ catMaybes geoIpEnabled' 
  geoIps <- getCountries $ filter ((`elem`geoIpEnabled) . postBoard . entityVal . fst) postsAndFiles
  -------------------------------------------------------------------------------------------------------------------
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  timeZone        <- getTimeZone
  displaySage     <- getConfig configDisplaySage
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, titleDelimiter, msgrender MsgPostsByUserID]
    $(widgetFile "admin/search/uid")

