{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Captcha where

import           Import
import           Yesod.Auth
import           Data.Char        (toLower)
import qualified Data.Text        as T
import           System.Directory (removeFile)
import           System.Random    (randomIO, randomRIO)
import           Utils.SillyCaptcha (makeCaptcha)
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

getCaptchaInfoR :: Handler TypedContent
getCaptchaInfoR = do
  acaptcha  <- lookupSession "acaptcha"
  muser     <- maybeAuth
  msgrender <- getMessageRender
  when (isNothing acaptcha && isNothing muser) $
    recordCaptcha =<< getConfig configCaptchaLength
  maybeCaptchaInfo <- getCaptchaInfo
  case () of
    _ | isJust acaptcha         -> selectRep $ provideJson $ object [("acaptcha", toJSON $ msgrender MsgYouDontNeedCaptcha)]
      | isJust maybeCaptchaInfo -> selectRep $ provideJson $ object
                                  [("info", toJSON $ T.concat [msgrender MsgTypeOnly
                                                              , " "
                                                              , msgrender (chooseMsg $ fromJust maybeCaptchaInfo) ])]
      | otherwise               -> selectRep $ provideJson $ object [("error",toJSON $ msgrender MsgReloadPage)]
  where chooseMsg "Bold"    = MsgBoldChars
        chooseMsg "Italic"  = MsgItalicChars
        chooseMsg "Regular" = MsgRegularChars
        chooseMsg _         = MsgReloadPage
---------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------
recordCaptcha :: Int -> Handler ()
recordCaptcha captchaLength = do
  maybeCaptchaId <- lookupSession "captchaId"
  when (isNothing maybeCaptchaId) $ do
    ip <- pack <$> getIp
    maybeCaptchaEntity <- runDB $ selectFirst [CaptchaIp ==. ip] []
    case maybeCaptchaEntity of
      Just (Entity _ cap) -> do
        setSession "captchaId"   (showText $ captchaLocalId cap)
        setSession "captchaInfo" (captchaInfo cap)
      _                   -> newCaptcha captchaLength ip
---------------------------------------------------------------------------------------------------------------------------
newCaptcha :: Int -> Text -> Handler ()
newCaptcha captchaLength ip = do
  cId    <- liftIO (abs <$> randomIO :: IO Int)
  langs  <- languages
  let lang = if "ru" `elem` langs then "ru" else "en"
  wCount <- runDB $ count [CaptchaDictLang ==. lang]
  cWords <- runDB $ forM [1..captchaLength] $ \_ -> do
    offset <- liftIO (randomRIO (1,wCount) :: IO Int)
    selectFirst [CaptchaDictLang ==. lang] [OffsetBy offset]

  (info, value) <- liftIO $ makeCaptcha (captchaFilePath (show cId ++ captchaExt))
                                       (unwords $ map (unpack . captchaDictWord . entityVal) $ catMaybes cWords)
  setSession "captchaId"   (showText cId)
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
checkCaptcha :: Text -> Handler () -> Handler ()
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
updateAdaptiveCaptcha :: Maybe Text -> Handler ()
updateAdaptiveCaptcha acaptcha =
  when (isNothing acaptcha) $ do
    posted <- lookupSession "posted"
    let p  = fromMaybe "0" posted
        p' = readText p + 1 :: Int
    setSession "posted" (showText p')
    aCaptchaGuards <- getConfig configACaptchaGuards
    when (p' >= aCaptchaGuards) $ do
      deleteSession "posted"
      setSession "acaptcha" "1"
---------------------------------------------------------------------------------------------------------------------------
getCaptchaInfo :: Handler (Maybe Text)
getCaptchaInfo = do
  maybeCaptchaInfo <- lookupSession "captchaInfo"
  case maybeCaptchaInfo of
    Just _ -> return maybeCaptchaInfo
    _      -> do
      ip           <- getIp
      maybeCaptcha <- runDB $ selectFirst [CaptchaIp ==. pack ip] []
      return $ (captchaInfo . entityVal) <$> maybeCaptcha
