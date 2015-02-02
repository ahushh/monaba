{-# LANGUAGE OverloadedStrings #-}
module Handler.Captcha where
 
import Import
import Utils.PlainCaptcha (makeCaptcha)
import System.Random (randomIO)
import System.Directory (removeFile)

captchaExt :: String
captchaExt = ".png"

getCaptchaR :: Handler Html
getCaptchaR = do
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
     liftIO $ removeFile $ captchaFilePath (unpack cId) ++ captchaExt
     when (mCaptchaValue /= mCaptcha) wrongCaptchaRedirect
   _        -> wrongCaptchaRedirect
