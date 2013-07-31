{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Delete where

import           Import
import           Yesod.Auth
import qualified Database.Esqueleto as E
import qualified Data.Text          as T
import qualified Data.Map.Strict    as Map
import           Control.Arrow      (second)
import           System.Directory   (removeFile)
---------------------------------------------------------------------------------------------
getDeletedByOpR :: Text -> Int -> Handler Html
getDeletedByOpR board thread = do
  when (thread == 0) notFound
  muser       <- maybeAuth
  maybeBoard  <- runDB $ getBy $ BoardUniqName board
  when (isNothing maybeBoard || (boardOpModeration $ entityVal $ fromJust maybeBoard) == False) notFound  
  let userRole = maybe Nothing (Just . personRole . entityVal) muser
    in when (userRole < (boardViewAccess $ entityVal $ fromJust maybeBoard)) notFound
  boards      <- runDB $ selectList ([]::[Filter Board]) []
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
  nameOfTheBoard   <- extraSiteName <$> getExtra
  boardCategories  <- getConfig configBoardCategories
  msgrender        <- getMessageRender
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", board, " - ", msgrender MsgDeletedPosts]
    $(widgetFile "deleted")

getDeleteR :: Handler Html
getDeleteR = do
  query      <- reqGetParams <$> getRequest
  muser      <- maybeAuth
  let errorRedirect msg = setMessageI msg >> redirectUltDest HomeR
      nopasreq          = maybe False (\u -> personRole (entityVal u) >= Moderator) muser
      helper x          = toKey ((read $ unpack $ snd x) :: Int )
  case reverse query of
    ("postpassword",pswd):("opmoderation",threadId):xs | null xs   -> errorRedirect MsgDeleteNoPosts
                                                       | otherwise -> do
      thread   <- runDB $ get ((toKey $ read $ unpack threadId) :: Key Post)
      when (isNothing thread) notFound

      let board = postBoard $ fromJust thread
      maybeBoard  <- runDB $ getBy $ BoardUniqName board
      when (isNothing maybeBoard || (boardOpModeration $ entityVal $ fromJust maybeBoard) == False) notFound

      posterId <- getPosterId
      when (postPosterId (fromJust thread) /= posterId &&
            postPassword (fromJust thread) /= pswd
           ) $ errorRedirect MsgYouAreNotOp
      let requestIds = map helper xs
          myFilterPr (Entity _ p) = postBoard       p == board &&
                                    postParent      p == (postLocalId $ fromJust thread) &&
                                    postDeletedByOp p == False
      posts <- filter myFilterPr <$> runDB (selectList [PostId <-. requestIds] [])
      case posts of
        [] -> errorRedirect MsgDeleteNoPosts
        _  -> deletePostsByOp posts >> redirectUltDest HomeR

    ("postpassword",pswd):xs | null xs   -> errorRedirect MsgDeleteNoPosts
                             | otherwise -> do
      let requestIds   = map helper xs
          myFilterPr e = nopasreq || (postPassword (entityVal e) == pswd)
      posts <- filter myFilterPr <$> runDB (selectList [PostId <-. requestIds] [])
      case posts of
        [] -> errorRedirect MsgDeleteWrongPassword
        _  -> deletePosts posts >> redirectUltDest HomeR
    _                           -> errorRedirect MsgUnknownError

---------------------------------------------------------------------------------------------
-- used by Handler/Admin and Handler/Board
---------------------------------------------------------------------------------------------
deletePostsByOp :: [Entity Post] -> HandlerT App IO ()
deletePostsByOp = runDB . mapM_ (\(Entity pId _) -> update pId [PostDeletedByOp =. True])

deletePosts :: [Entity Post] -> HandlerT App IO ()
deletePosts posts = do
  let boards         = nub $ map (postBoard . entityVal) posts
      boardsAndPosts = map (\b -> (b, filter ((==b) . postBoard . entityVal) posts)) boards
      boardsAndPosts :: [(Text,[Entity Post])]

  childs <- runDB $ forM boardsAndPosts $ \(b,ps) ->
    selectList [PostBoard ==. b, PostParent <-. map (postLocalId . entityVal) ps] []

  let idsToRemove = (concat $ map ((map entityKey) . snd) boardsAndPosts) ++ map entityKey (concat childs)
  runDB $ deleteWhere [PostId <-. idsToRemove]
  files <- runDB $ selectList [AttachedfileParentId <-. idsToRemove] []
  
  forM_ files $ \(Entity fId f) -> do
    sameFilesCount <- runDB $ count [AttachedfileMd5 ==. attachedfileMd5 f, AttachedfileId !=. fId]
    case sameFilesCount `compare` 0 of
      GT -> do -- this file belongs to several posts so don't delete it
        filesWithSameThumbSize <- runDB $ count [AttachedfileThumbSize ==. attachedfileThumbSize f, AttachedfileId !=. fId]
        unless (filesWithSameThumbSize > 0) $
          void $ liftIO $ removeFile $ thumbFilePath (attachedfileThumbSize f) (attachedfileType f) (attachedfileName f)
      _  -> do
        let t = attachedfileType f
        liftIO $ removeFile $ imageFilePath t $ attachedfileName f
        when (isImageFile t) $ 
          liftIO $ removeFile $ thumbFilePath (attachedfileThumbSize f) t $ attachedfileName f
  runDB $ deleteWhere [AttachedfileParentId <-. idsToRemove]
