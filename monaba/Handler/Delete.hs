module Handler.Delete where

import           Import
import qualified Database.Esqueleto as E
import qualified Data.Text          as T
import qualified Data.Map.Strict    as Map
import           Handler.Admin.Modlog (addModlogEntry)
import           Handler.Admin.Ban (addBan)
import           Utils.YobaMarkup     (makeExternalRef)
import           Data.Digest.OpenSSL.MD5 (md5sum)
import qualified Data.ByteString.UTF8    as B
import           Handler.Common
---------------------------------------------------------------------------------------------
getDeletedByOpR :: Text -> Int -> Handler Html
getDeletedByOpR board thread = do
  when (thread == 0) notFound
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let permissions     = getPermissions       mgroup
      geoIpEnabled    = boardEnableGeoIp     boardVal
      boardDesc       = boardTitle     boardVal
      boardSummaryVal = boardSummary boardVal
      showPostDate    = boardShowPostDate boardVal
  unless (boardOpModeration boardVal) notFound  
  -------------------------------------------------------------------------------------------------------
  allPosts' <- runDB $ E.select $ E.from $ \(post `E.LeftOuterJoin` file) -> do
    E.on $ (E.just (post E.^. PostId)) E.==. (file E.?. AttachedfileParentId)
    E.where_ ((post E.^. PostBoard       ) E.==. (E.val board ) E.&&.
              (post E.^. PostDeletedByOp ) E.==. (E.val True  ) E.&&.
              (post E.^. PostParent      ) E.==. (E.val thread))
    E.orderBy [E.asc (post E.^. PostId)]
    return (post, file)
  let allPosts = map (second catMaybes) $ Map.toList $ keyValuesToMap allPosts'
  ------------------------------------------------------------------------------------------------------- 
  AppSettings{..}  <- appSettings <$> getYesod
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleMsg MsgDeletedPosts
    $(widgetFile "deleted")

getDeleteFileR :: Int -> Handler Html
getDeleteFileR fileId = do
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let fileKey           = toSqlKey $ fromIntegral fileId
      errorRedirect msg = setMessageI msg >> redirectUltDest HomeR
      nopasreq          = maybe False ((DeletePostsP `elem`) . groupPermissions . entityVal) mgroup
  file <- runDB $ get404 fileKey
  post <- runDB $ get404 $ attachedfileParentId file
  posterId <- getPosterId
  when (not nopasreq && postPosterId post /= posterId) $ errorRedirect MsgFileNotYours
  when (nopasreq && postPosterId post /= posterId) $ do
    p <- makeExternalRef (postBoard post) (postLocalId post)
    addModlogEntry $ MsgModlogDeleteFile (pack $ attachedfileName file) p
  deleteFile $ Entity fileKey file
  redirectUltDest HomeR

getDeleteR :: Handler Html
getDeleteR = do
  query  <- reqGetParams <$> getRequest
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let errorRedirect msg = setMessageI msg >> redirectUltDest HomeR
      nopasreq          = maybe False ((DeletePostsP `elem`) . groupPermissions . entityVal) mgroup
      readSqlKey x      = (toSqlKey . fromIntegral) ((read $ unpack $ snd x) :: Int )
  case reverse query of
    ("postpassword",pswd):("opmoderation",threadId):zs | null zs   -> errorRedirect MsgDeleteNoPosts
                                                       | otherwise -> do
      thread   <- runDB $ get ((toSqlKey . fromIntegral $ ((read $ unpack threadId) :: Int)) :: Key Post)
      when (isNothing thread) notFound

      let board = postBoard $ fromJust thread
      boardVal    <- getBoardVal404 board
      unless (boardOpModeration boardVal) notFound

      posterId <- getPosterId
      when (postPosterId (fromJust thread) /= posterId &&
            (unpack $ postPassword $ fromJust thread) /= (md5sum $ B.fromString $ unpack pswd)
           ) $ errorRedirect MsgYouAreNotOp
      let requestIds = map readSqlKey $ filter ((=="postdelete").fst) zs
          myFilterPr (Entity _ p) = postBoard       p == board &&
                                    postParent      p == postLocalId (fromJust thread) &&
                                    postDeletedByOp p == False
      posts <- filter myFilterPr <$> runDB (selectList [PostId <-. requestIds] [])
      case posts of
        [] -> errorRedirect MsgDeleteNoPosts
        _  -> deletePostsByOp posts >> redirectUltDest HomeR
    ---------------------------------------------------------------------------------------------
    ("postpassword",_):("wipe",_):zs | null zs   -> errorRedirect MsgDeleteNoPosts
                                     | otherwise -> do
      let requestIds  = map readSqlKey $ filter ((=="postdelete").fst) zs
          permissions = getPermissions mgroup
      posts <- runDB (selectList [PostId <-. requestIds] [])
      when (nopasreq && elem ManageBanP permissions) $ do
        void $ forM posts $ \(Entity _ p) -> do
          deletePostsByIP $ postIp p
          let ip = postIp p
          bId <- addBan (tread ip) (tread ip) "wipe" [] Nothing
          addModlogEntry $ MsgModlogBanAdded (ip <> " - " <> ip) "wipe" (fromIntegral $ fromSqlKey bId)
      redirectUltDest HomeR
    ---------------------------------------------------------------------------------------------
    ("postpassword",pswd):zs | null zs   -> errorRedirect MsgDeleteNoPosts
                             | otherwise -> do
      let onlyfiles    = lookup "onlyfiles" zs :: Maybe Text
          requestIds   = map readSqlKey $ filter ((=="postdelete").fst) zs
          myFilterPr e = nopasreq || ((unpack $ postPassword $ entityVal e) == (md5sum $ B.fromString $ unpack $ pswd))
      posts <- filter myFilterPr <$> runDB (selectList [PostId <-. requestIds] [])
      posterId <- getPosterId

      when nopasreq  $ do
        let posts' = filter (\(Entity _ p) -> postPosterId p /= posterId) posts
        bt <- forM posts' $ \(Entity _ p) -> makeExternalRef (postBoard p) (postLocalId p)           
        addModlogEntry $ MsgModlogDeletePosts $ T.concat bt

      case posts of
        [] -> errorRedirect MsgDeleteWrongPassword
        _  -> deletePosts posts (isJust onlyfiles) >> redirectUltDest HomeR
    _                           -> errorRedirect MsgUnknownError

