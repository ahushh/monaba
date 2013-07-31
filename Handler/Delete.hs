{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Delete where

import Import
import Yesod.Auth
import System.Directory (removeFile)
---------------------------------------------------------------------------------------------
getDeleteR :: Handler Html
getDeleteR = do
  query      <- reqGetParams <$> getRequest
  muser      <- maybeAuth
  let errorRedirect msg = setMessageI msg >> redirectUltDest HomeR
      nopasreq          = maybe False (\u -> personRole (entityVal u) >= Moderator) muser
  case reverse query of
    ("postpassword",pswd):xs | null xs   -> errorRedirect MsgDeleteNoPosts
                             | otherwise -> do
      let requestIds   = map g xs
          g x          = toKey ((read $ unpack $ snd x) :: Int )
          myFilterPr e = nopasreq || (postPassword (entityVal e) == pswd)
      posts <- filter myFilterPr <$> runDB (selectList [PostId <-. requestIds] [])
      case posts of
        [] -> errorRedirect MsgDeleteWrongPassword
        _  -> deletePosts posts >> redirectUltDest HomeR
    _                           -> errorRedirect MsgUnknownError

---------------------------------------------------------------------------------------------
-- used by Handler/Admin and Handler/Board
---------------------------------------------------------------------------------------------
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
