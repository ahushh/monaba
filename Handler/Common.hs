-- | Common handler functions.
module Handler.Common where

import System.Directory   (removeFile)--, removeDirectory, getDirectoryContents)
import Handler.Admin.Modlog (addModlogEntry)
import Utils.YobaMarkup     (makeExternalRef)
import Data.FileEmbed (embedFile)
import qualified Data.Text          as T
import           System.FilePath                 ((</>))
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.png")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

---------------------------------------------------------------------------------------------
-- Deletion
---------------------------------------------------------------------------------------------
deleteFiles :: [Key Post] -> Handler ()
deleteFiles idsToRemove = do  
  AppSettings{..} <- appSettings <$> getYesod
  -- let removeDirIfEmpty d = whenM ( ((==0) . length . filter (`notElem`[".",".."])) <$> liftIO (getDirectoryContents d)) $ liftIO (removeDirectory d)
  files <- runDB $ selectList [AttachedfileParentId <-. idsToRemove] []
  forM_ files $ \(Entity fId f) -> do
    sameFilesCount <- runDB $ count [AttachedfileHashsum ==. attachedfileHashsum f, AttachedfileId !=. fId]
    let ft = attachedfileFiletype f
        fe = attachedfileExtension f
        hs = attachedfileHashsum f
        ts = attachedfileThumbSize f
        o  = attachedfileOnion f
    case sameFilesCount `compare` 0 of
      GT -> do -- this file belongs to several posts so don't delete it from disk
        filesWithSameThumbSize <- runDB $ count [AttachedfileThumbSize ==. ts, AttachedfileId !=. fId]
        unless (filesWithSameThumbSize > 0) $
          when (ft `elem` thumbFileTypes) $ do
            void $ liftIO $ removeFile $ thumbFilePath (if o then appUploadDir </> "onion" else appUploadDir) appStaticDir ts ft fe hs
        runDB $ deleteWhere [AttachedfileParentId <-. idsToRemove]
      _  -> do
        liftIO $ removeFile $ attachedfilePath f
        when (ft `elem` thumbFileTypes) $ liftIO $ removeFile $ thumbFilePath (if o then appUploadDir </> "onion" else appUploadDir) appStaticDir ts ft fe hs
        runDB $ deleteWhere [AttachedfileParentId <-. idsToRemove]

deleteFile :: Entity Attachedfile -> Handler ()
deleteFile (Entity fId f) = do
  AppSettings{..} <- appSettings <$> getYesod
  sameFilesCount <- runDB $ count [AttachedfileHashsum ==. attachedfileHashsum f, AttachedfileId !=. fId]
  let ft = attachedfileFiletype f
      fe = attachedfileExtension f
      hs = attachedfileHashsum f
      ts = attachedfileThumbSize f
      o  = attachedfileOnion f
  case sameFilesCount `compare` 0 of
    GT -> do -- this file belongs to several posts so don't delete it from disk
      filesWithSameThumbSize <- runDB $ count [AttachedfileThumbSize ==. ts, AttachedfileId !=. fId]
      unless (filesWithSameThumbSize > 0) $
        when (ft `elem` thumbFileTypes) $ do
          void $ liftIO $ removeFile $ thumbFilePath (if o then appUploadDir </> "onion" else appUploadDir) appStaticDir ts ft fe hs
      runDB $ delete fId
    _  -> do
      liftIO $ removeFile $ attachedfilePath f
      when (ft `elem` thumbFileTypes) $ liftIO $ removeFile $ thumbFilePath (if o then appUploadDir </> "onion" else appUploadDir) appStaticDir ts ft fe hs
      runDB $ delete fId


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

deletePostsByIP :: Text -> Handler ()
deletePostsByIP ip = do
  posts <- runDB $ selectList [PostIp ==. ip] []
  bt <- forM posts $ \(Entity _ p) -> makeExternalRef (postBoard p) (postLocalId p)           
  addModlogEntry $ MsgModlogDeletePosts $ T.concat bt
  deletePosts posts False

