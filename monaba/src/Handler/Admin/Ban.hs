module Handler.Admin.Ban where

import           Import
import           Handler.Admin.Modlog (addModlogEntry) 
import           Handler.Common       (deletePostsByIP)
import qualified Data.Text          as T
-------------------------------------------------------------------------------------------------------------
banByIpForm :: Text          -> -- ^ IP adress
              [(Text,Text)] -> -- ^ All boards
              Maybe Text    -> -- ^ Board 
              Html          -> -- ^ Extra token
              MForm Handler (FormResult
                             ( Text       -- ^ IP range start
                             , Text       -- ^ IP range end
                             , Text       -- ^ Reason
                             , [Text]      -- ^ Board name
                             , Maybe Int  -- ^ Expires in hours
                             ), Widget)
banByIpForm ip allBoards board extra = do
  (ipBegRes  , ipBegView  ) <- mreq textField  "" (Just ip)
  (ipEndRes  , ipEndView  ) <- mreq textField  "" (Just ip)
  (reasonRes , reasonView ) <- mreq textField  "" Nothing
  (boardRes  , boardView  ) <- mreq (multiSelectFieldList allBoards)   "" ((:[])<$>board)
  (expiresRes, expiresView) <- mopt intField   "" Nothing
  let result = (,,,,) <$> ipBegRes <*> ipEndRes <*> reasonRes <*> boardRes <*> expiresRes
      widget = $(widgetFile "admin/ban-form")
  return (result, widget)
                                          
fetchAllBoards = do
  mgroup <- (fmap $ userGroup . entityVal) <$> maybeAuth
  map ((boardTitle &&& boardName) . entityVal) . filter (not . isBoardHidden' mgroup) <$> runDB (selectList ([]::[Filter Board]) [])

getBanByIpAndDeleteR :: Text -> Text -> Handler Html
getBanByIpAndDeleteR board ip = do
  deletePostsByIP ip
  getBanByIpR board ip

getDeletePostsByIPR :: Text -> Handler Html
getDeletePostsByIPR ip = do
  deletePostsByIP ip
  redirectUltDest HomeR

getBanByIpR :: Text -> Text -> Handler Html
getBanByIpR board ip = do
  timeZone  <- getTimeZone
  allBoards <- fetchAllBoards
  (formWidget, _) <- generateFormPost $ banByIpForm ip allBoards (if (T.length board == 0) then Nothing else Just board)
  bans            <- runDB $ selectList ([]::[Filter Ban]) []
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleMsg MsgBanManagement
    $(widgetFile "admin/ban")

postBanByIpR :: Text -> Text -> Handler Html
postBanByIpR _ _ = do
  allBoards     <- fetchAllBoards
  ((result, _), _) <- runFormPost $ banByIpForm "" allBoards Nothing
  let msgRedirect msg = setMessageI msg >> redirect (BanByIpR "" "")
  case result of
    FormFailure []                  -> msgRedirect MsgBadFormData
    FormFailure xs                  -> msgRedirect (MsgError $ T.intercalate "; " xs) 
    FormMissing                     -> msgRedirect MsgNoFormData
    FormSuccess (ipBegin, ipEnd, reason, boards, expires) -> do
      bId <- addBan (tread ipBegin) (tread ipEnd) reason boards expires
      addModlogEntry $ MsgModlogBanAdded (tshow ipBegin <> " - " <> tshow ipEnd) reason (fromIntegral $ fromSqlKey bId)
      msgRedirect MsgBanAdded

getBanDeleteR :: Int -> Handler Html
getBanDeleteR bId = do
  runDB $ delete ((toSqlKey . fromIntegral) bId :: Key Ban)
  addModlogEntry $ MsgModlogDelBan bId
  setMessageI MsgBanDeleted >> redirect (BanByIpR "" "")

addBan :: IP -> IP -> Text -> [Text] -> Maybe Int -> Handler BanId
addBan ipBegin ipEnd reason boards expires = do
  now <- liftIO getCurrentTime
  let newBan = Ban { banIpBegin = ipBegin
                   , banIpEnd   = ipEnd
                   , banReason  = reason
                   , banBoards  = boards
                   , banExpires = (\n -> addUTCTime' (60*60*n) now) <$> expires
                   }
  runDB $ insert newBan
  
