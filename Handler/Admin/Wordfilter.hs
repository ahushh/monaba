module Handler.Admin.Wordfilter where

import           Import
import           Handler.Admin.Modlog (addModlogEntry) 
import qualified Data.Text as T (intercalate)

form :: Html -> MForm Handler (FormResult (Maybe Text, WordfilterDataType, Textarea, [WordfilterAction], Textarea, Maybe Textarea) , Widget)
form extra = do
  msgrender <- getMessageRender
  let types :: [(Text, WordfilterDataType)]
      types = map (first msgrender) [(MsgWordfilterWords, WordfilterWords),(MsgWordfilterExactMatch,WordfilterExactMatch), (MsgWordfilterRegex, WordfilterRegex)]
      actions :: [(Text, WordfilterAction)]
      actions = map (first msgrender) [(MsgWordfilterBan, WordfilterBan), (MsgWordfilterHB, WordfilterHB), (MsgWordfilterHBHide, WordfilterHBHide), (MsgWordfilterDeny, WordfilterDeny),(MsgWordfilterReplace, WordfilterReplace)]
  (boardRes , boardView ) <- mopt textField "" Nothing
  (typeRes  , typeView  ) <- mreq (selectFieldList types) "" Nothing
  (dataRes  , dataView  ) <- mreq textareaField "" Nothing
  (actionRes, actionView) <- mreq (multiSelectFieldList actions) "" Nothing
  (msgRes   , msgView   ) <- mreq textareaField "" Nothing
  (replaceRes, replaceView   ) <- mopt textareaField "" Nothing
  let result = (,,,,,) <$> boardRes <*> typeRes <*> dataRes <*> actionRes <*> msgRes <*> replaceRes
      widget = $(widgetFile "admin/wordfilter-form")
  return (result, widget)

getAdminWordfilterR :: Handler Html
getAdminWordfilterR = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  (formWidget, formEnctype) <- generateFormPost form
  bs       <- runDB $ selectList ([]::[Filter Wordfilter]) []
  msgrender <- getMessageRender
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleMsg MsgWordfilter
    $(widgetFile "admin/wordfilter")

postAdminWordfilterR :: Handler Html
postAdminWordfilterR = do
  let msgRedirect msg = setMessageI msg >> redirect AdminWordfilterR
  ((result, _), _) <- runFormPost form
  case result of
   FormFailure []                     -> msgRedirect MsgBadFormData
   FormFailure xs                     -> msgRedirect $ MsgError $ T.intercalate "; " xs
   FormMissing                        -> msgRedirect MsgNoFormData
   FormSuccess (boardVal, typeVal, dataVal, actionVal, msg, replaceVal) -> do
      let wordfilter = Wordfilter { wordfilterBoard    = boardVal
                                  , wordfilterDataType = typeVal
                                  , wordfilterData     = unTextarea dataVal
                                  , wordfilterAction   = actionVal
                                  , wordfilterActionMsg= unTextarea msg
                                  , wordfilterReplacement  = unTextarea <$> replaceVal
                                  }
      void $ runDB $ insert wordfilter
      redirect AdminWordfilterR

getAdminWordfilterDeleteR :: Int -> Handler Html
getAdminWordfilterDeleteR bId = do
  runDB $ delete ((toSqlKey . fromIntegral) bId :: Key Wordfilter)
--  addModlogEntry $ MsgModlogDelBan bId
--  setMessageI MsgBanDeleted >> redirect (BanByIpR "" "")
  redirect AdminWordfilterR
