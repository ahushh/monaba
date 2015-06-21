module Handler.Delete where

import           Import
import qualified Database.Esqueleto as E
import qualified Data.Text          as T
import qualified Data.Map.Strict    as Map
import           System.Directory   (removeFile)--, removeDirectory, getDirectoryContents)
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

getDeleteR :: Handler Html
getDeleteR = do
  query  <- reqGetParams <$> getRequest
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let errorRedirect msg = setMessageI msg >> redirectUltDest HomeR
      nopasreq          = maybe False ((DeletePostsP `elem`) . groupPermissions . entityVal) mgroup
      helper x          = (toSqlKey . fromIntegral) ((read $ unpack $ snd x) :: Int )
  case reverse query of
    ("postpassword",pswd):("opmoderation",threadId):zs | null zs   -> errorRedirect MsgDeleteNoPosts
                                                       | otherwise -> do
      let xs = if fst (head zs) == "onlyfiles" then tail zs else zs
      thread   <- runDB $ get ((toSqlKey . fromIntegral $ ((read $ unpack threadId) :: Int)) :: Key Post)
      when (isNothing thread) notFound

      let board = postBoard $ fromJust thread
      boardVal    <- getBoardVal404 board
      unless (boardOpModeration boardVal) notFound

      posterId <- getPosterId
      when (postPosterId (fromJust thread) /= posterId &&
            postPassword (fromJust thread) /= pswd
           ) $ errorRedirect MsgYouAreNotOp
      let requestIds = map helper xs
          myFilterPr (Entity _ p) = postBoard       p == board &&
                                    postParent      p == postLocalId (fromJust thread) &&
                                    postDeletedByOp p == False
      posts <- filter myFilterPr <$> runDB (selectList [PostId <-. requestIds] [])
      case posts of
        [] -> errorRedirect MsgDeleteNoPosts
        _  -> deletePostsByOp posts >> redirectUltDest HomeR

    ("postpassword",pswd):zs | null zs   -> errorRedirect MsgDeleteNoPosts
                             | otherwise -> do
      let onlyfiles    = fst (head zs) == "onlyfiles"
          xs           = if onlyfiles then tail zs else zs
          requestIds   = map helper xs
          myFilterPr e = nopasreq || (postPassword (entityVal e) == pswd)
      posts <- filter myFilterPr <$> runDB (selectList [PostId <-. requestIds] [])
      -- when nopasreq $ addModlogEntry MsgModlogDeletePosts $ T.intercalate "," $ map (postLocalId $ 
      case posts of
        [] -> errorRedirect MsgDeleteWrongPassword
        _  -> deletePosts posts onlyfiles >> redirectUltDest HomeR
    _                           -> errorRedirect MsgUnknownError


deleteFiles :: [Key Post] -> Handler ()
deleteFiles idsToRemove = do  
  AppSettings{..} <- appSettings <$> getYesod
  -- let removeDirIfEmpty d = whenM ( ((==0) . length . filter (`notElem`[".",".."])) <$> liftIO (getDirectoryContents d)) $ liftIO (removeDirectory d)
  files <- runDB $ selectList [AttachedfileParentId <-. idsToRemove] []
  forM_ files $ \(Entity fId f) -> do
    sameFilesCount <- runDB $ count [AttachedfileHashsum ==. attachedfileHashsum f, AttachedfileId !=. fId]
    let ft = attachedfileType f
        fe = attachedfileExtension f
        hs = attachedfileHashsum f
        ts = attachedfileThumbSize f
    case sameFilesCount `compare` 0 of
      GT -> do -- this file belongs to several posts so don't delete it from disk
        filesWithSameThumbSize <- runDB $ count [AttachedfileThumbSize ==. ts, AttachedfileId !=. fId]
        unless (filesWithSameThumbSize > 0) $
          when (ft `elem` thumbFileTypes) $ do
            void $ liftIO $ removeFile $ thumbFilePath appStaticDir ts ft fe hs
      _  -> do
        liftIO $ removeFile $ attachedfilePath f
        when (ft `elem` thumbFileTypes) $ liftIO $ removeFile $ thumbFilePath appStaticDir ts ft fe hs
        runDB $ deleteWhere [AttachedfileParentId <-. idsToRemove]
---------------------------------------------------------------------------------------------
-- used by Handler/Admin and Handler/Board
---------------------------------------------------------------------------------------------
deletePostsByOp :: [Entity Post] -> Handler ()
deletePostsByOp = runDB . mapM_ (\(Entity pId _) -> update pId [PostDeletedByOp =. True])

deletePosts :: [Entity Post] -> Bool -> Handler ()
deletePosts posts onlyfiles = do
  let boards         = nub $ map (postBoard . entityVal) posts
      boardsAndPosts = map (\b -> (b, filter ((==b) . postBoard . entityVal) posts)) boards
      boardsAndPosts :: [(Text,[Entity Post])]

  childs <- runDB $ forM boardsAndPosts $ \(b,ps) ->
    selectList [PostBoard ==. b, PostParent <-. map (postLocalId . entityVal) ps] []

  let idsToRemove = concat (map (map entityKey . snd) boardsAndPosts) ++ map entityKey (concat childs)
  unless onlyfiles $
    runDB (updateWhere [PostId <-. idsToRemove] [PostDeleted =. True])
  deleteFiles idsToRemove
