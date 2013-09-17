{-# LANGUAGE OverloadedStrings #-}
module Handler.EventSource where

import           Import
import           Yesod.Auth

import           Control.Concurrent.Chan            (newChan, writeChan)
import           Network.Wai.EventSource            (ServerEvent (..), eventSourceAppChan)
import           Blaze.ByteString.Builder.Char.Utf8 (fromText)
import           Control.Monad.Trans.Resource       (register)

import qualified Text.Blaze.Html.Renderer.Text   as RHT

import qualified Data.Text              as T
import qualified Data.ByteString.Base64 as Base64
import           Data.Text.Encoding     (encodeUtf8, decodeUtf8)
import           Data.Text.Lazy         (toStrict)

import           Data.IORef
import qualified Data.Map as Map
-------------------------------------------------------------------------------------------------------------------
deleteClient :: Text -> Handler ()
deleteClient posterId = (\clientsRef -> liftIO $ modifyIORef clientsRef $ Map.delete posterId) =<< sseClients <$> getYesod

getReceiveR :: Handler TypedContent
getReceiveR = do
  posterId   <- getPosterId
  clientsRef <- sseClients <$> getYesod
  clients    <- liftIO $ readIORef clientsRef
  let client = Map.lookup posterId clients
      -- delete client from the list if user disconnects
      autoDeleteClient = register . liftIO $ modifyIORef clientsRef $ Map.delete posterId
  if isNothing client 
    then do -- add this user to the list of connected clients
      muser            <- maybeAuth
      permissions      <- getPermissions <$> getMaybeGroup muser
      rating           <- getCensorshipRating
      timeZone         <- getTimeZone
      chan             <- liftIO newChan
      let newClient = SSEClient { sseClientUser        = muser
                                , sseClientPermissions = permissions 
                                , sseClientRating      = rating
                                , sseClientTimeZone    = timeZone
                                , sseClientEvent       = chan
                                }
      liftIO $ modifyIORef' clientsRef (Map.insert posterId newClient)
      req <- waiRequest
      res <- liftResourceT $ eventSourceAppChan chan req
      void $ autoDeleteClient
      sendWaiResponse res
    else do -- user already in the list
      let chan = sseClientEvent $ fromJust client
      req <- waiRequest
      res <- liftResourceT $ eventSourceAppChan chan req
      void autoDeleteClient
      sendWaiResponse res

sendPost :: Text -> Int -> Int -> Bool -> Text -> Handler ()
sendPost board thread postId hellbanned posterId = do
  boardVal         <- getBoardVal404 board
  maybePost        <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. postId, PostDeleted ==. False] []
  when (isNothing maybePost) (return ())
  files            <- runDB $ selectList  [AttachedfileParentId ==. entityKey (fromJust maybePost)] []
  geoIps           <- getCountries [(fromJust maybePost, files) | boardEnableGeoIp boardVal]
  displaySage      <- getConfig configDisplaySage
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra

  clientsRef <- sseClients <$> getYesod
  clients    <- liftIO $ readIORef clientsRef
  let access             = boardViewAccess boardVal
      checkViewAccess' u = (isJust access && isNothing ((userGroup . entityVal) <$> u)) ||
                           (isJust access && notElem (fromJust ((userGroup . entityVal) <$> u)) (fromJust $ access))
      filteredClients = [x | (k,x) <- Map.toList clients, not hellbanned || k==posterId || elem HellBanP (sseClientPermissions x)
                                                       , not (checkViewAccess' $ sseClientUser x)]
  forM_ filteredClients (\client -> do
    renderedPost <- renderPost client (fromJust maybePost) files displaySage geoIps maxLenOfFileName
    let sourceEventName = Just $ fromText $ T.concat [board, "-", pack (show thread)]
        encodedPost     = fromText $ decodeUtf8 $ Base64.encode $ encodeUtf8 $ toStrict $ RHT.renderHtml renderedPost
    liftIO $ writeChan (sseClientEvent client) $ ServerEvent sourceEventName Nothing $ return encodedPost)
  where renderPost client post files displaySage geoIps maxLenOfFileName = 
          bareLayout $ replyPostWidget (sseClientUser client) post
                       files (sseClientRating client) False True False
                       displaySage (sseClientPermissions client) geoIps
                       (sseClientTimeZone client) maxLenOfFileName



