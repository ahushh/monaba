module Handler.Admin.Ban where

import           Import
import           Handler.Admin.Modlog (addModlogEntry) 
import qualified Data.Text as T (intercalate)
import           Data.IP
-------------------------------------------------------------------------------------------------------------
banByIpForm :: Text -> -- ^ IP adress
              Text -> -- ^ Board name
              Html -> -- ^ Extra token
              MForm Handler (FormResult
                             ( Text       -- ^ IP range start
                             , Text       -- ^ IP range end
                             , Text       -- ^ Reason
                             , Maybe Text -- ^ Board name
                             , Maybe Int  -- ^ Expires in hours
                             ), Widget)
banByIpForm ip board extra = do
  (ipBegRes  , ipBegView  ) <- mreq textField  "" (Just ip)
  (ipEndRes  , ipEndView  ) <- mreq textField  "" (Just ip)
  (reasonRes , reasonView ) <- mreq textField  "" Nothing
  (boardRes  , boardView  ) <- mopt textField  "" (Just $ Just board)
  (expiresRes, expiresView) <- mopt intField   "" Nothing
  let result = (,,,,) <$> ipBegRes <*> ipEndRes <*> reasonRes <*> boardRes <*> expiresRes
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
    FormSuccess (ipBegin, ipEnd, reason, board, expires) -> do
      bId <- addBan (tread ipBegin) (tread ipEnd) reason board expires
      addModlogEntry $ MsgModlogBanAdded (tshow ipBegin <> tshow ipEnd) reason (fromIntegral $ fromSqlKey bId)
      msgRedirect MsgBanAdded

getBanDeleteR :: Int -> Handler Html
getBanDeleteR bId = do
  runDB $ delete ((toSqlKey . fromIntegral) bId :: Key Ban)
  addModlogEntry $ MsgModlogDelBan bId
  setMessageI MsgBanDeleted >> redirect (BanByIpR "" "")

addBan :: IP -> IP -> Text -> Maybe Text -> Maybe Int -> Handler BanId
addBan ipBegin ipEnd reason board expires = do
  now <- liftIO getCurrentTime
  let newBan = Ban { banIpBegin = ipBegin
                   , banIpEnd   = ipEnd
                   , banReason  = reason
                   , banBoard   = board
                   , banExpires = (\n -> addUTCTime' (60*60*n) now) <$> expires
                   }
  runDB $ insert newBan
  
