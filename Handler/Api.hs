{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Api where

import           Import
import           Yesod.Auth
import           Control.Arrow ((***))
--------------------------------------------------------------------------------------------------------- 
getPostsHelper :: YesodDB App [Entity Post] -> HandlerT App IO TypedContent
getPostsHelper selectPosts = do
  muser <- maybeAuth
  let selectFiles p = runDB $ selectList [AttachedfileParentId ==. entityKey p] []
  postsAndFiles <- reverse <$> (runDB selectPosts) >>= mapM (\p -> do
    files <- selectFiles p
    return (p, files))
  selectRep $ do
    provideRep $ bareLayout [whamlet|
        $forall (post, files) <- postsAndFiles
            ^{replyPostWidget muser post files}
    |]
    provideJson $ map (entityVal *** (map entityVal)) postsAndFiles

getApiAllPostsR :: Text -> Int -> Handler TypedContent
getApiAllPostsR board thread = getPostsHelper selectPosts
  where selectPosts = selectList [PostBoard ==. board, PostParent ==. thread] [Desc PostDate]

getApiNewPostsR :: Text -> Int -> Int -> Handler TypedContent
getApiNewPostsR board thread postId = getPostsHelper selectPosts
  where selectPosts = selectList [PostBoard ==. board, PostParent ==. thread, PostLocalId >. postId] [Desc PostDate]

getApiLastPostsR :: Text -> Int -> Int -> Handler TypedContent
getApiLastPostsR board thread postCount = getPostsHelper selectPosts
  where selectPosts = selectList [PostBoard ==. board, PostParent ==. thread] [Desc PostDate, LimitTo postCount]
---------------------------------------------------------------------------------------------------------
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

getApiThreadR :: Text -> Int -> Handler TypedContent
getApiThreadR board threadId = do
  muser     <- maybeAuth
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. threadId, PostParent ==. 0] []
  when (isNothing maybePost) notFound -- TODO: fix error handling
  let post    = fromJust maybePost
      postKey = entityKey $ fromJust maybePost
  files <- runDB $ selectList [AttachedfileParentId ==. postKey] []
  let postAndFiles = (entityVal post, map entityVal files)
  selectRep $ do
    provideRep $ bareLayout $ opPostWidget muser post files False
    provideJson postAndFiles
---------------------------------------------------------------------------------------------------------     
bareLayout :: Yesod site => WidgetT site IO () -> HandlerT site IO Html
bareLayout widget = do
    pc <- widgetToPageContent widget
    giveUrlRenderer [hamlet| ^{pageBody pc} |]
