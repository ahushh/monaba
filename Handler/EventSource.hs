{-# LANGUAGE OverloadedStrings #-}
module Handler.EventSource where

import           Import
import           Yesod.Auth

import           Yesod.EventSource
import           Network.Wai.EventSource            (ServerEvent (..))
import           Blaze.ByteString.Builder.Char.Utf8 (fromText)

import qualified Text.Blaze.Html.Renderer.Text   as RHT

import           Data.Ord               (comparing)
import qualified Data.ByteString.Base64 as Base64
import           Data.Text.Encoding     (encodeUtf8, decodeUtf8)
import           Data.Text.Lazy         (toStrict)

import           Data.Conduit (yield)
import           Control.Monad (forever)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM (atomically)

import qualified Data.Map as Map
import           Data.List (sortBy)
-------------------------------------------------------------------------------------------------------------------
maxConnections :: Int
maxConnections = 500

deleteClient :: Text -> Handler ()
deleteClient posterId = (\clientsRef -> liftIO $ atomically $ modifyTVar' clientsRef (Map.delete posterId)) =<< sseClients <$> getYesod

getEventR :: Handler TypedContent
getEventR = do
  posterId   <- getPosterId
  clientsRef <- sseClients <$> getYesod
  chan       <- sseChan    <$> getYesod
  clients    <- liftIO $ readTVarIO clientsRef
  let client = Map.lookup posterId clients
  -- delete excess clients if the connection pool is overfilled
  when (Map.size clients > maxConnections) $
    liftIO $ atomically $ modifyTVar' clientsRef (Map.fromList . take (maxConnections-1) .
                                                  sortBy (comparing $ sseClientConnected . snd) . Map.toList)
  -- add new client to the connection pool
  when (isNothing client) $ do
    muser       <- maybeAuth
    permissions <- getPermissions <$> getMaybeGroup muser
    rating      <- getCensorshipRating
    timeZone    <- getTimeZone
    now         <- liftIO getCurrentTime
    ignoredBoards <- getLiveBoards
    let newClient = SSEClient { sseClientUser        = muser
                              , sseClientPermissions = permissions
                              , sseClientRating      = rating
                              , sseClientTimeZone    = timeZone
                              , sseClientConnected   = now
                              , sseClientLiveIgnoredBoards = ignoredBoards
                              }
    liftIO $ atomically $ modifyTVar' clientsRef (Map.insert posterId newClient)
  chan' <- liftIO $ atomically $ dupTChan chan
  repEventSource $ \pf -> do
    yield $ ServerEvent Nothing Nothing [fromText $ "Eventsource works. Used polyfill: " <> showText pf]
    forever $ do
      (name, content) <- liftIO $ atomically $ readTChan chan'
      yield $ ServerEvent (Just $ fromText $ name) Nothing [fromText $ content]
      yield $ ServerEvent Nothing Nothing [fromText $ name <> " : " <> content]

sendPost :: Board -> Int -> Entity Post -> [Entity Attachedfile] -> Bool -> Text -> Handler ()
sendPost boardVal thread ePost files hellbanned posterId = do
  let board = boardName boardVal
      showPostDate = boardShowPostDate boardVal
  geoIps           <- getCountries [(ePost, files) | boardEnableGeoIp boardVal]
  displaySage      <- getConfig configDisplaySage
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra

  clientsRef <- sseClients <$> getYesod
  chan       <- sseChan    <$> getYesod
  clients    <- liftIO $ readTVarIO clientsRef
  let access             = boardViewAccess boardVal
      checkViewAccess' u = (isJust access && isNothing ((userGroup . entityVal) <$> u)) ||
                           (isJust access && notElem (fromJust ((userGroup . entityVal) <$> u)) (fromJust access))
      filteredClients = [(k,x) | (k,x) <- Map.toList clients, not hellbanned || k==posterId || elem HellBanP (sseClientPermissions x)
                                                           , not (checkViewAccess' $ sseClientUser x)]
  forM_ filteredClients $ \(posterId', client) -> do
    when (thread /= 0) $ do
      renderedPost  <- renderPost client ePost displaySage geoIps maxLenOfFileName showPostDate
      let name        = board <> "-" <> showText thread <> "-" <> posterId'
          encodedPost = decodeUtf8 $ Base64.encode $ encodeUtf8 $ toStrict $ RHT.renderHtml renderedPost
      liftIO $ atomically $ writeTChan chan (name, encodedPost)

    when (board `notElem` sseClientLiveIgnoredBoards client) $ do
      renderedPost' <- renderPostLive client ePost geoIps maxLenOfFileName showPostDate
      let nameLive     = "live-" <> posterId'
          encodedPost' = decodeUtf8 $ Base64.encode $ encodeUtf8 $ toStrict $ RHT.renderHtml renderedPost'
      liftIO $ atomically $ writeTChan chan (nameLive, encodedPost')
  where renderPost client post displaySage geoIps maxLenOfFileName showPostDate =
          bareLayout $ postWidget post
                       files (sseClientRating client) displaySage True True False
                       (sseClientPermissions client) geoIps
                       (sseClientTimeZone client) maxLenOfFileName showPostDate
        renderPostLive client post geoIps maxLenOfFileName showPostDate =
          bareLayout $ postWidget  post
                       files (sseClientRating client) False True True True
                       (sseClientPermissions client) geoIps
                       (sseClientTimeZone client) maxLenOfFileName showPostDate

sendDeletedPosts :: [Post] -> Handler ()
sendDeletedPosts posts = do
  clientsRef <- sseClients <$> getYesod
  chan       <- sseChan    <$> getYesod
  clients    <- liftIO $ readTVarIO clientsRef
  let boards  = map postBoard  posts
      threads = map postParent posts
      posts'  = map (\(b,t) -> (b,t,filter (\p -> postBoard p == b && postParent p == t) posts)) $ zip boards threads
  forM_ (Map.keys clients) (\posterId -> forM_ posts' (\(b,t,ps) -> do
      let name     = b <> "-" <> showText t <> "-deleted-" <> posterId
          nameLive = "live-deleted-" <> posterId
          postIDs  = map (\x -> "post-" <> showText (postLocalId x) <> "-" <> showText t <> "-" <> b) ps
      liftIO $ atomically $ writeTChan chan (name    , showText postIDs)
      liftIO $ atomically $ writeTChan chan (nameLive, showText postIDs)
      ))

sendEditedPost :: Text -> Text -> Int -> Int -> Maybe UTCTime -> Handler ()
sendEditedPost msg board thread post time = do
  clientsRef <- sseClients <$> getYesod
  chan       <- sseChan    <$> getYesod
  clients    <- liftIO $ readTVarIO clientsRef
  forM_ (Map.toList clients) $ \(posterId,client) -> do
      let thread'         = if thread == 0 then post else thread
          name            = board <> "-" <> showText thread' <> "-edited-" <> posterId
          nameLive        = "live-edited-" <> posterId
          encodedMsg      = decodeUtf8 $ Base64.encode $ encodeUtf8 msg
          timeZone        = sseClientTimeZone client
          lastModified    = maybe "" (pack . myFormatTime timeZone) time
      liftIO $ atomically $ writeTChan chan (name    , showText [board, showText thread, showText post, encodedMsg, lastModified])
      liftIO $ atomically $ writeTChan chan (nameLive, showText [board, showText thread, showText post, encodedMsg, lastModified])
