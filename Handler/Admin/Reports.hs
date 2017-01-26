module Handler.Admin.Reports where

import           Import
import           Handler.Admin.Modlog (addModlogEntry) 
import           Utils.YobaMarkup     (makeExternalRef)
import qualified Data.Text as T

reportForm :: Int           -> 
             Html          -> -- ^ Extra token
             MForm Handler (FormResult
                             ( Int
                             , Text
                             ), Widget)
reportForm postId extra = do
  msgrender <- getMessageRender
  let reasonInput lbl = lbl { fsAttrs = [("placeholder",msgrender MsgReportReason)] }
  (idRes, idView  ) <- mreq hiddenField  "" (Just postId)
  (reasonRes  , reasonView  ) <- mreq textField  (reasonInput "") Nothing
  let result = (,) <$> idRes <*> reasonRes
      widget = $(widgetFile "admin/report-form")
  return (result, widget)

getReportFormR :: Int -> Handler Html
getReportFormR postId = do 
  (formWidget, _) <- generateFormPost $ reportForm postId
  bareLayout formWidget

getAdminReportsR :: Handler Html
getAdminReportsR = do
  reports  <- runDB $ selectList ([]::[Filter Report]) []
  posts    <- runDB $ mapM (get . reportPostId . entityVal) reports
  urls     <- forM posts $ \mP -> case mP of
                                  Just post -> makeExternalRef (postBoard post) (postLocalId post)
                                  Nothing    -> return ("error: post not found"::Text)
  let xs = zip reports urls
  defaultLayout $ do
    defaultTitleMsg MsgReports
    $(widgetFile "admin/reports")
  

getAdminReportsDelR :: Int -> Handler Html
getAdminReportsDelR rId = do
  runDB $ delete ((toSqlKey . fromIntegral) rId :: Key Report)
  redirect AdminReportsR

postReportPostR :: Handler TypedContent
postReportPostR = do
  ((result, _), _) <- runFormPost $ reportForm 0
  let msgRedirect msg = setMessageI msg >> redirect HomeR
  msgrender <- getMessageRender
  case result of
    FormFailure []                  -> selectRep $ provideJson $ object [("error", toJSON $ msgrender MsgBadFormData)]
    FormFailure xs                  -> selectRep $ provideJson $ object [("error", toJSON $ msgrender (MsgError $ T.intercalate "; " xs))]
    FormMissing                     -> selectRep $ provideJson $ object [("error", toJSON $ msgrender  MsgNoFormData)]
    FormSuccess (postId, reason) -> do
      let postKey = (toSqlKey . fromIntegral) postId :: Key Post
          report = Report { reportPostId = postKey
                          , reportReason = reason
                          }
      runDB $ insert report
      selectRep $ do
        provideJson $ object [("ok","reported")]
        provideRep $ defaultLayout $ [whamlet|ok|]
