{-# LANGUAGE OverloadedStrings #-}
module Handler.Captcha where
 
import Import
import Utils.PlainCaptcha (makeCaptcha)
import System.Random (randomIO)
import System.Directory (removeFile, doesFileExist)
import qualified Data.Text as T

captchaExt :: String
captchaExt = ".png"

getCaptchaR :: Handler Html
getCaptchaR = do
  oldCId <- lookupSession "captchaId"
  let path = captchaFilePath (unpack $ fromJust oldCId) ++ captchaExt
    in when (isJust oldCId) $ whenM (liftIO $ doesFileExist path) $ liftIO $ removeFile path
  cId <- liftIO (abs <$> randomIO :: IO Int)
  setSession "captchaId" (showText cId)
  value <- liftIO $ makeCaptcha $ captchaFilePath (show cId) ++ captchaExt
  setSession "captchaValue" value
  sendFile typePng $ captchaFilePath (show cId) ++ captchaExt

checkCaptcha :: Maybe Text -> Handler () -> Handler ()
checkCaptcha mCaptcha wrongCaptchaRedirect = do
  mCaptchaValue <- lookupSession "captchaValue"
  mCaptchaId    <- lookupSession "captchaId"
  deleteSession "captchaValue"
  deleteSession "captchaId"
  case mCaptchaId of
   Just cId -> do
     let path = captchaFilePath (unpack cId) ++ captchaExt
     whenM (liftIO $ doesFileExist path) $
       liftIO $ removeFile path
     when (mCaptchaValue /= (T.toLower <$> mCaptcha)) wrongCaptchaRedirect
   _        -> wrongCaptchaRedirect
