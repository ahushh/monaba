module Handler.RSS where

import Import
import Yesod.RssFeed
--import Utils.YobaMarkup (makeExternalRef)

getRssR :: Text -> Handler TypedContent
getRssR board' = do
  AppSettings{..} <- appSettings <$> getYesod
  now       <- liftIO getCurrentTime
  msgrender <- getMessageRender
  -------------------------------------------------------------------------------------------------------------------      
  ip     <- pack <$> getIp
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  ignoredBoards <- getFeedBoards
  let onionBoards = if not (isOnion ip) then map boardName $ filter boardOnion $ map entityVal boards else []
      boards'     = mapMaybe (getIgnoredBoard group) boards ++ onionBoards
  when (board' `elem` boards') notFound
  -------------------------------------------------------------------------------------------------------------------      
  let selector = if board' == "feed" then [PostDeleted ==. False, PostHellbanned ==. False, PostBoard /<-. (boards'++ignoredBoards)] else [PostBoard ==. board', PostDeleted ==. False, PostHellbanned ==. False]
  posts'    <- runDB $ selectList selector [Desc PostDate, LimitTo 20]
--  renderedPosts <- mapM (\p -> bareLayout $ postWidget p [] False False False False False []) posts'
  let renderedPosts = map (preEscapedToHtml . unTextarea . postMessage . entityVal) posts'
      posts         = zip posts' renderedPosts

  lastPost <- runDB (selectFirst (if board' == "feed" then [] else [PostBoard ==. board']) [Desc PostLocalId])
  let e = (flip map) posts $ \((Entity _ p), pc) -> FeedEntry { feedEntryLink     = if postParent p == 0 then BoardNoPageR (postBoard p) else ThreadR (postBoard p) (postParent p)
                                                             , feedEntryUpdated  = postDate p
                                                             , feedEntryTitle    = postTitle p <> " >>/" <> (postBoard p) <> "/" <> (tshow $ postLocalId p)
                                                             , feedEntryContent  = pc
                                                             , feedEntryEnclosure = Nothing
                                                             }
      f = Feed { feedTitle       = msgrender $ MsgRssFeed appSiteName board'
               , feedLinkSelf    = if board' == "feed" then FeedR else BoardNoPageR board'
               , feedLinkHome    = HomeR
               , feedAuthor      = ""
               , feedDescription = preEscapedToHtml $ msgrender $ MsgRssRecentPosts board'
               , feedLanguage    = ""
               , feedUpdated     = maybe now (postDate . entityVal) lastPost
               , feedLogo        = Nothing
               , feedEntries     = e
               }

  rss <- rssFeed f  
  return $ TypedContent typeRss $ toContent rss
