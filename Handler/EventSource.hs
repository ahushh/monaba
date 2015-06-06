{-# LANGUAGE OverloadedStrings #-}
module Handler.EventSource where

import           Import
import           Yesod.EventSource
import           Network.Wai.EventSource            (ServerEvent (..))
import           Blaze.ByteString.Builder.Char.Utf8 (fromText)

import           Data.Conduit (yield, bracketP)
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM (atomically)
import qualified Data.Map as Map
-------------------------------------------------------------------------------------------------------------------
getOnlineR :: Handler TypedContent
getOnlineR = do
  chan <- sseChan <$> getYesod
  now  <- liftIO getCurrentTime
  ip   <- pack <$> getIp
  clientsRef <- sseClients <$> getYesod
  repEventSource $ \_ -> bracketP (return ())
    (\_ -> liftIO $  do
      atomically $ modifyTVar' clientsRef (Map.delete ip)
    )
    $ \_ -> forever $ do
        clients <- liftIO $ readTVarIO clientsRef
        let client = Map.lookup ip clients
        when (isNothing client) $ liftIO $ atomically $ modifyTVar' clientsRef (Map.insert ip now)
        clients' <- liftIO $ readTVarIO clientsRef
        yield $ ServerEvent (Just $ fromText "online") Nothing [fromText $ (showText $ Map.size clients')]
        liftIO $ atomically $ writeTChan chan ("ping", "ping")
        liftIO $ threadDelay (1000000*3) -- 3 seconds

getEventR :: Handler TypedContent
getEventR = do
  chan  <- sseChan <$> getYesod
  chan' <- liftIO $ atomically $ dupTChan chan
  repEventSource $ \_ -> do
    forever $ do
      (name, content) <- liftIO $ atomically $ readTChan chan'
      yield $ ServerEvent (Just $ fromText name) Nothing [fromText content]

sendNewPostES :: Text -> Handler ()
sendNewPostES board = do
  chan <- sseChan <$> getYesod
  liftIO $ atomically $ writeTChan chan ("new-post", board)
