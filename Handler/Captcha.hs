{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Captcha where

import           Import
import           Yesod.Auth
import           Data.Char        (toLower)
import qualified Data.Text        as T
import           System.Directory (removeFile)
import           System.Random    (randomIO)
import           SillyCaptcha     (makeCaptcha)
---------------------------------------------------------------------------------------------------------------------------
getCaptchaR :: Handler Html
getCaptchaR = do
  -- muser     <- maybeAuth
  -- acaptcha  <- lookupSession "acaptcha"  
  -- when (isNothing acaptcha && isNothing muser) $
  --   recordCaptcha =<< getConfig configCaptchaLength

  maybeCaptchaId <- lookupSession "captchaId"
  when (isJust maybeCaptchaId) $
      sendFile typePng $ captchaFilePath (unpack (fromJust maybeCaptchaId) ++ captchaExt)
  notFound

getCaptchaInfoR :: Handler Html
getCaptchaInfoR = do
  acaptcha  <- lookupSession "acaptcha"
  muser     <- maybeAuth
  when (isNothing acaptcha && isNothing muser) $
    recordCaptcha =<< getConfig configCaptchaLength
    
  maybeCaptchaInfo <- getCaptchaInfo
  bareLayout [whamlet|
            $maybe c <- maybeCaptchaInfo
                _{MsgTypeOnly} 
                $if c == "Bold"
                    _{MsgBoldChars}
                $elseif c == "Italic"
                    _{MsgItalicChars}
                $elseif c == "Underline"
                    _{MsgUnderlineChars}
                $elseif c == "Regular"
                    _{MsgRegularChars}
           |]

---------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------
recordCaptcha :: Int -> HandlerT App IO ()
recordCaptcha captchaLength = do
  maybeCaptchaId <- lookupSession "captchaId"
  when (isJust maybeCaptchaId) $ return ()
  --------------------------------------------------------------  
  ip <- pack <$> getIp
  maybeCaptchaEntity <- runDB $ selectFirst [CaptchaIp ==. ip] []
  case maybeCaptchaEntity of
    Just (Entity _ cap) -> do
      setSession "captchaId"   (pack $ show $ captchaLocalId cap)
      setSession "captchaInfo" (captchaInfo cap)
      return ()
    _                   -> newCaptcha captchaLength ip
---------------------------------------------------------------------------------------------------------------------------
newCaptcha :: Int -> Text -> HandlerT App IO ()
newCaptcha captchaLength ip = do
  cId <- liftIO (abs <$> randomIO :: IO Int)
  (info, value) <- liftIO $ makeCaptcha captchaLength $ captchaFilePath (show cId ++ captchaExt)
  setSession "captchaId"   (pack $ show cId)
  setSession "captchaInfo" info
  captchaTimeout <- getConfig configCaptchaTimeout
  now <- liftIO getCurrentTime
  void $ runDB $ insert Captcha { captchaIp      = ip
                                , captchaLocalId = cId
                                , captchaInfo    = info
                                , captchaValue   = value
                                , captchaExpires = addUTCTime' captchaTimeout now
                                }
---------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------
captchaExt :: String
captchaExt = ".png"
---------------------------------------------------------------------------------------------------------------------------
checkCaptcha :: Text -> HandlerT App IO () -> HandlerT App IO ()
checkCaptcha captcha wrongCaptchaRedirect = do
  maybeCaptchaId <- lookupSession "captchaId"
  case maybeCaptchaId of
    Just cId -> do
      maybeCaptchaEntity <- runDB $ getBy (CaptchaUniqueLocalId (read $ unpack cId))
      let value = captchaValue $ entityVal $ fromJust maybeCaptchaEntity
      -- delete entered captcha from DB
      deleteSession "captchaId"
      deleteSession "captchaInfo"
      void $ runDB $ delete $ entityKey $ fromJust maybeCaptchaEntity
      liftIO $ removeFile $ captchaFilePath (show (captchaLocalId $ entityVal $ fromJust maybeCaptchaEntity) ++ captchaExt)
      -- delete old captchas
      now <- liftIO getCurrentTime
      oldCaptchas <- runDB $ selectList [CaptchaExpires <=. now] []
      let oldCaptchaIds = map entityKey oldCaptchas
      void $ runDB $ deleteWhere [CaptchaId <-. oldCaptchaIds]
      forM_ oldCaptchas $ \cap -> 
        liftIO $ removeFile $ captchaFilePath (show (captchaLocalId $ entityVal cap) ++ captchaExt)
      when (T.map toLower captcha /= value) wrongCaptchaRedirect
    _        -> wrongCaptchaRedirect
---------------------------------------------------------------------------------------------------------------------------
updateAdaptiveCaptcha :: Maybe Text -> HandlerT App IO ()
updateAdaptiveCaptcha acaptcha =
  when (isNothing acaptcha) $ do
    posted <- lookupSession "posted"
    let p  = fromMaybe "0" posted
        p' = read (unpack p) + 1 :: Int
    setSession "posted" (pack $ show p')
    aCaptchaGuards <- getConfig configACaptchaGuards
    when (p' >= aCaptchaGuards) $ do
      deleteSession "posted"
      setSession "acaptcha" "1"
---------------------------------------------------------------------------------------------------------------------------
getCaptchaInfo :: HandlerT App IO (Maybe Text)
getCaptchaInfo = do
  maybeCaptchaInfo <- lookupSession "captchaInfo"
  case maybeCaptchaInfo of
    Just _ -> return maybeCaptchaInfo
    _      -> do
      ip           <- getIp
      maybeCaptcha <- runDB $ selectFirst [CaptchaIp ==. pack ip] []
      return $ (captchaInfo . entityVal) <$> maybeCaptcha
