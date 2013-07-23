{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Api where

import           Import
import           Yesod.Auth
import           Control.Arrow ((***))
--------------------------------------------------------------------------------------------------------- 
getApiEntireThreadR :: Text -> Int -> Handler TypedContent
getApiEntireThreadR _ _ = undefined

getApiNewPostsR :: Text -> Int -> Int -> Handler TypedContent
getApiNewPostsR board thread postId = do
  muser     <- maybeAuth
  let selectPosts   = runDB $ selectList [PostBoard ==. board, PostParent ==. thread, PostLocalId >. postId] [Desc PostDate]
      selectFiles p = runDB $ selectList [AttachedfileParentId ==. entityKey p] []
  postsAndFiles <- reverse <$> selectPosts >>= mapM (\p -> do
    files <- selectFiles p
    return (p, files))
  selectRep $ do
    provideRep $ bareLayout [whamlet|
        $forall (post, files) <- postsAndFiles
            ^{replyPostWidget muser post files}
    |]
    provideJson $ map (entityVal *** (map entityVal)) postsAndFiles

getApiLastPostsR :: Text -> Int -> Int -> Handler TypedContent
getApiLastPostsR board thread postCount = do
  muser     <- maybeAuth
  let selectPosts   = runDB $ selectList [PostBoard ==. board, PostParent ==. thread] [Desc PostDate, LimitTo postCount]
      selectFiles p = runDB $ selectList [AttachedfileParentId ==. entityKey p] []
  postsAndFiles <- reverse <$> selectPosts >>= mapM (\p -> do
    files <- selectFiles p
    return (p, files))
  selectRep $ do
    provideRep $ bareLayout [whamlet|
        $forall (post, files) <- postsAndFiles
            ^{replyPostWidget muser post files}
    |]
    provideJson $ map (entityVal *** (map entityVal)) postsAndFiles
    
getApiPostR :: Text -> Int -> Handler TypedContent
getApiPostR board postId = do
  muser     <- maybeAuth
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. postId] []
  when (isNothing maybePost) notFound -- TODO: fix error handling
  let post    = fromJust maybePost
      postKey = entityKey $ fromJust maybePost
  files <- runDB $ selectList [AttachedfileParentId ==. postKey] []
  let postAndFiles = (entityVal post, map entityVal files)
  selectRep $ do
    provideRep $ bareLayout $ replyPostWidget muser post files
    provideJson postAndFiles
    
---------------------------------------------------------------------------------------------------------     
bareLayout :: Yesod site => WidgetT site IO () -> HandlerT site IO Html
bareLayout widget = do
    pc <- widgetToPageContent widget
    giveUrlRenderer [hamlet| ^{pageBody pc} |]
