{-# LANGUAGE OverloadedStrings #-}
module Handler.Captcha where
 
import Import
import System.Random (randomIO)
import System.Directory (removeFile, doesFileExist)
import System.Process
import qualified Data.Text as T

captchaExt :: String
captchaExt = ".png"

makeCaptcha :: String -> Handler Text
makeCaptcha path = do
  captcha <- extraCaptcha <$> getExtra
  liftIO $ pack <$> readProcess (unpack captcha) [path] ""

getCaptchaR :: Handler Html
getCaptchaR = do
  oldCId <- lookupSession "captchaId"
  let path = captchaFilePath (unpack $ fromJust oldCId) ++ captchaExt
    in when (isJust oldCId) $ whenM (liftIO $ doesFileExist path) $ liftIO $ removeFile path
  cId <- liftIO (abs <$> randomIO :: IO Int)
  setSession "captchaId" (showText cId)
  value <- makeCaptcha $ captchaFilePath (show cId) ++ captchaExt
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
