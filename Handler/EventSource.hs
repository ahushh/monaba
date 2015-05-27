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

import           Data.Conduit (yield, bracketP)
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM (atomically)
import qualified Data.Map as Map
import           Data.List (sortBy)
-------------------------------------------------------------------------------------------------------------------
getOnlineR :: Handler TypedContent
getOnlineR = do
  now        <- liftIO getCurrentTime
  posterId   <- getPosterId
  clientsRef <- sseClients <$> getYesod
  repEventSource $ \_ -> bracketP (return ())
    (\_ -> liftIO $  do
      atomically $ modifyTVar' clientsRef (Map.delete posterId)
    )
    $ \_ -> forever $ do
        clients <- liftIO $ readTVarIO clientsRef
        let client = Map.lookup posterId clients
        when (isNothing client) $ liftIO $ atomically $ modifyTVar' clientsRef (Map.insert posterId now)
        clients <- liftIO $ readTVarIO clientsRef
        yield $ ServerEvent (Just $ fromText "online") Nothing [fromText $ (showText $ Map.size clients)]
        liftIO $ threadDelay (1000000*3) -- 2 seconds

getEventR :: Handler TypedContent
getEventR = do
  chan  <- sseChan <$> getYesod
  chan' <- liftIO $ atomically $ dupTChan chan
  repEventSource $ \pf -> do
    forever $ do
      (name, content) <- liftIO $ atomically $ readTChan chan'
      yield $ ServerEvent (Just $ fromText name) Nothing [fromText content]
