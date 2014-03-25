{-# LANGUAGE OverloadedStrings #-}
module Handler.EventSource where

import           Import
import           Yesod.Auth

import           Control.Concurrent.Chan            (writeChan, dupChan)
import           Network.Wai.EventSource            (ServerEvent (..), eventSourceAppChan)
import           Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)

import qualified Text.Blaze.Html.Renderer.Text   as RHT

import qualified Data.Text              as T
import qualified Data.ByteString.Base64 as Base64
import           Data.Text.Encoding     (encodeUtf8, decodeUtf8)
import           Data.Text.Lazy         (toStrict)

import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM (atomically)
import qualified Data.Map as Map
import           Data.List (sortBy)
-------------------------------------------------------------------------------------------------------------------
deleteClient :: Text -> Handler ()
deleteClient posterId = (\clientsRef -> liftIO $ atomically $ modifyTVar' clientsRef (Map.delete posterId)) =<< sseClients <$> getYesod

maxConnections :: Int
maxConnections = 500

getReceiveR :: Handler TypedContent
getReceiveR = do
  posterId   <- getPosterId
  clientsRef <- sseClients <$> getYesod
  chan       <- sseChan    <$> getYesod
  clients    <- liftIO $ readTVarIO clientsRef
  let client = Map.lookup posterId clients
  when (Map.size clients > maxConnections) $
    liftIO $ atomically $ modifyTVar' clientsRef (Map.fromList . take (maxConnections-1) . sortBy
                                                  (\(_,c1) (_,c2) -> sseClientConnected c1 `compare` sseClientConnected c2) . Map.toList)
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
  chan' <- liftIO $ dupChan chan
  req   <- waiRequest
  res   <- liftIO $ eventSourceAppChan chan' req
  sendWaiResponse res

sendPost :: Board -> Int -> Entity Post -> [Entity Attachedfile] -> Bool -> Text -> Handler ()
sendPost boardVal thread ePost files hellbanned posterId = do
  let board = boardName boardVal
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
      renderedPost  <- renderPost client ePost displaySage geoIps maxLenOfFileName
      let sourceEventName = Just $ fromText $ T.concat [board, "-", showText thread, "-", posterId']
          encodedPost     = fromText $ decodeUtf8 $ Base64.encode $ encodeUtf8 $ toStrict $ RHT.renderHtml renderedPost
      liftIO $ writeChan chan $ ServerEvent sourceEventName Nothing $ return encodedPost

    when (board `notElem` sseClientLiveIgnoredBoards client) $ do
      renderedPost' <- renderPostLive client ePost displaySage geoIps maxLenOfFileName
      let sourceEventName'= Just $ fromText $ T.concat ["live-", posterId']
          encodedPost'    = fromText $ decodeUtf8 $ Base64.encode $ encodeUtf8 $ toStrict $ RHT.renderHtml renderedPost'
      liftIO $ writeChan chan $ ServerEvent sourceEventName' Nothing $ return encodedPost'
  where renderPost client post displaySage geoIps maxLenOfFileName =
          bareLayout $ postWidget (sseClientUser client) post
                       files (sseClientRating client) displaySage True True False
                       (sseClientPermissions client) geoIps
                       (sseClientTimeZone client) maxLenOfFileName
        renderPostLive client post displaySage geoIps maxLenOfFileName =
          bareLayout $ postWidget (sseClientUser client) post
                       files (sseClientRating client) False True True True
                       (sseClientPermissions client) geoIps
                       (sseClientTimeZone client) maxLenOfFileName

sendDeletedPosts :: [Post] -> Handler ()
sendDeletedPosts posts = do
  clientsRef <- sseClients <$> getYesod
  chan       <- sseChan    <$> getYesod
  clients    <- liftIO $ readTVarIO clientsRef
  let boards  = map postBoard  posts
      threads = map postParent posts
      posts'  = map (\(b,t) -> (b,t,filter (\p -> postBoard p == b && postParent p == t) posts)) $ zip boards threads
  forM_ (Map.keys clients) (\posterId -> forM_ posts' (\(b,t,ps) -> do
      let sourceEventName = Just $ fromText $ T.concat [b, "-", showText t, "-deleted-", posterId]
          sourceEventName'= Just $ fromText $ T.concat ["live-deleted-", posterId]
          ps'             = map (\x -> T.concat ["post-", showText (postLocalId x), "-", showText t, "-", b]) ps
      liftIO $ writeChan chan $ ServerEvent sourceEventName Nothing $ return $ fromString $ show ps'
      liftIO $ writeChan chan $ ServerEvent sourceEventName' Nothing $ return $ fromString $ show ps'
      ))

sendEditedPost :: Text -> Text -> Int -> Int -> Maybe UTCTime -> Handler ()
sendEditedPost msg board thread post time = do
  clientsRef <- sseClients <$> getYesod
  chan       <- sseChan    <$> getYesod
  clients    <- liftIO $ readTVarIO clientsRef
  forM_ (Map.toList clients) $ \(posterId,client) -> do
      let thread'         = if thread == 0 then post else thread
          sourceEventName = Just $ fromText $ T.concat [board, "-", showText thread', "-edited-", posterId]
          sourceEventName'= Just $ fromText $ T.concat ["live-edited-", posterId]
          encodedMsg      = decodeUtf8 $ Base64.encode $ encodeUtf8 msg
          timeZone        = sseClientTimeZone client
          lastModified    = maybe "" (pack . myFormatTime timeZone) time
      liftIO $ writeChan chan $
        ServerEvent sourceEventName Nothing $ return $ fromString $ show [board, showText thread, showText post, encodedMsg, lastModified]
      liftIO $ writeChan chan $
        ServerEvent sourceEventName' Nothing $ return $ fromString $ show [board, showText thread, showText post, encodedMsg, lastModified]
