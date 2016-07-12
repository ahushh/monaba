module Handler.Admin.Ban where

import           Import
import           Handler.Admin.Modlog (addModlogEntry) 
import qualified Data.Text as T (intercalate)
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
  timeZone        <- getTimeZone
  (formWidget, _) <- generateFormPost $ banByIpForm ip board
  bans            <- runDB $ selectList ([]::[Filter Ban]) []
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleMsg MsgBanManagement
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
      bId <- addBan ip reason board expires
      addModlogEntry $ MsgModlogBanAdded ip reason (fromIntegral $ fromSqlKey bId)
      msgRedirect MsgBanAdded

getBanDeleteR :: Int -> Handler Html
getBanDeleteR bId = do
  runDB $ delete ((toSqlKey . fromIntegral) bId :: Key Ban)
  addModlogEntry $ MsgModlogDelBan bId
  setMessageI MsgBanDeleted >> redirect (BanByIpR "" "")

addBan :: Text -> Text -> Maybe Text -> Maybe Int -> Handler BanId
addBan ip reason board expires = do
  now <- liftIO getCurrentTime
  let newBan = Ban { banIp      = ip
                   , banReason  = reason
                   , banBoard   = board
                   , banExpires = (\n -> addUTCTime' (60*60*n) now) <$> expires
                   }
  runDB $ insert newBan
  
