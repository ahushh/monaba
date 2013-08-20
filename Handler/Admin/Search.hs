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
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup
  -------------------------------------------------------------------------------------------------------------------
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count (if onlyHellbanned
                                  then [PostDeleted ==. False, PostPosterId ==. posterId, PostHellbanned ==. True]
                                  else [PostDeleted ==. False, PostPosterId ==. posterId])
  let f (Entity _ b) | isJust (boardViewAccess b) && notElem (fromJust group) (fromJust $ boardViewAccess b) = Just $ boardName b
                     | otherwise = Nothing
      boards'     = mapMaybe f boards
      selectPosts = if onlyHellbanned
                      then [PostBoard /<-. boards', PostDeleted ==. False, PostPosterId ==. posterId, PostHellbanned ==. True]
                      else [PostBoard /<-. boards', PostDeleted ==. False, PostPosterId ==. posterId]
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
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", msgrender MsgPostsByUserID]
    $(widgetFile "admin/search/uid")

