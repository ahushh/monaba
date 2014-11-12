{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin.Ban where

import           Import
import           Yesod.Auth
import qualified Data.Text         as T
-------------------------------------------------------------------------------------------------------------
banByIpForm :: Text -> -- ^ IP adress
              Text -> -- ^ Board name
              Html -> -- ^ Extra token
              MForm Handler (FormResult
                             ( Text       -- ^ IP
                             , Text       -- ^ Reason
                             , Maybe Text -- ^ Board name
                             , Maybe Int  -- ^ Expires in hours
                             ), Widget)
banByIpForm ip board extra = do
  (ipRes     , ipView     ) <- mreq textField  "" (Just ip)
  (reasonRes , reasonView ) <- mreq textField  "" Nothing
  (boardRes  , boardView  ) <- mopt textField  "" (Just $ Just board)
  (expiresRes, expiresView) <- mopt intField   "" Nothing
  let result = (,,,) <$> ipRes <*> reasonRes <*> boardRes <*> expiresRes
      widget = $(widgetFile "admin/ban-form")
  return (result, widget)
                                          
getBanByIpR :: Text -> Text -> Handler Html
getBanByIpR board ip = do
  muser       <- maybeAuth
  permissions <- getPermissions <$> getMaybeGroup muser
  timeZone   <- getTimeZone

  (formWidget, _) <- generateFormPost $ banByIpForm ip board
  
  bans            <- runDB $ selectList ([]::[Filter Ban]) []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", msgrender MsgBanManagement]
    $(widgetFile "admin/ban")

postBanByIpR :: Text -> Text -> Handler Html
postBanByIpR _ _ = do
  ((result, _), _) <- runFormPost $ banByIpForm "" ""
  let msgRedirect msg = setMessageI msg >> redirect (BanByIpR "" "")
  case result of
    FormFailure []                  -> msgRedirect MsgBadFormData
    FormFailure xs                  -> msgRedirect (MsgError $ T.intercalate "; " xs) 
    FormMissing                     -> msgRedirect MsgNoFormData
    FormSuccess (ip, reason, board, expires) -> do
      now <- liftIO getCurrentTime
      let newBan = Ban { banIp      = ip
                       , banReason  = reason
                       , banBoard   = board
                       , banExpires = (\n -> addUTCTime' (60*60*n) now) <$> expires
                       }
      void $ runDB $ insert newBan
      msgRedirect MsgBanAdded

getBanDeleteR :: Int -> Handler Html
getBanDeleteR bId = runDB (delete ((toSqlKey . fromIntegral) bId :: Key Ban)) >> setMessageI MsgBanDeleted >> redirect (BanByIpR "" "")
