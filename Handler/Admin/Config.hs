{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin.Config where

import           Import
import           Yesod.Auth
import qualified Data.Text            as T
import           Handler.Admin.Modlog (addModlogEntry)
-------------------------------------------------------------------------------------------------------------
configForm :: Config ->
             Html   ->
             MForm Handler (FormResult ( Maybe Int  -- ^ Captcha length
                                       , Maybe Int  -- ^ Number of adaptive captcha guards
                                       , Maybe Int  -- ^ Captcha timeout
                                       , Maybe Int  -- ^ Reply delay
                                       , Maybe Int  -- ^ New thread delay
                                       , Maybe Text -- ^ Board categories separated by comma
                                       , Maybe Text -- ^ Board for news
                                       , Maybe Int  -- ^ How many news show
                                       , Maybe Int  -- ^ The maximum number of post editings
                                       , Maybe Int  -- ^ How many latest posts show
                                       , Maybe Bool -- ^ Display sage icon
                                       , Maybe Int  -- ^ Max modlog entries
                                       , Maybe Int  -- ^ Modlog entries per page
                                       , Maybe Textarea  -- ^ About
                                       )
                           , Widget)
configForm config extra = do
  let f g = Just $ Just $ g config
      f :: forall a. (Config -> a) -> Maybe (Maybe a)
      categories' | null (configBoardCategories config) = Nothing
                  | otherwise                           = Just $ Just $ T.intercalate "," $ configBoardCategories config
  msgrender <- getMessageRender
  let showRecentPostsField = checkBool (>0) (msgrender $ MsgMustBeGreaterThan (msgrender MsgShowRecentPosts) 0) intField
      bigInput lbl         = lbl { fsAttrs = [("size","55")] }
  (captchaLengthRes   , captchaLengthView  ) <- mopt intField  "" (f configCaptchaLength  )
  (acaptchaGuardsRes  , acaptchaGuardsView ) <- mopt intField  "" (f configACaptchaGuards )
  (captchaTimeoutRes  , captchaTimeoutView ) <- mopt intField  "" (f configCaptchaTimeout )
  (replyDelayRes      , replyDelayView     ) <- mopt intField  "" (f configReplyDelay     )
  (threadDelayRes     , threadDelayView    ) <- mopt intField  "" (f configThreadDelay    )
  (boardCategoriesRes , boardCategoriesView) <- mopt textField (bigInput "") categories'
  (newsBoardRes       , newsBoardView      ) <- mopt textField "" (f configNewsBoard      )
  (showNewsRes        , showNewsView       ) <- mopt intField  "" (f configShowNews       )
  (maxEditingsRes     , maxEditingsView    ) <- mopt intField  "" (f configMaxEditings    )
  (showRecentPostsRes , showRecentPostsView) <- mopt showRecentPostsField "" (f configShowRecentPosts)
  (displaySageRes     , displaySageView    ) <- mopt checkBoxField "" (f configDisplaySage)
  (modlogMaxEntriesRes    , modlogMaxEntriesView    ) <- mopt intField "" (f configModlogMaxEntries)
  (modlogEntriesPerPageRes, modlogEntriesPerPageView) <- mopt intField "" (f configModlogEntriesPerPage)
  (aboutRes           , aboutView          ) <- mopt textareaField "" (f configAbout)
  let result = (,,,,,,,,,,,,,) <$>
               captchaLengthRes   <*> acaptchaGuardsRes <*> captchaTimeoutRes   <*>
               replyDelayRes      <*> threadDelayRes    <*> boardCategoriesRes  <*>
               newsBoardRes       <*> showNewsRes       <*> maxEditingsRes      <*>
               showRecentPostsRes <*> displaySageRes    <*> modlogMaxEntriesRes <*>
               modlogEntriesPerPageRes <*> aboutRes
      widget = $(widgetFile "admin/config-form")
  return (result, widget)
  
getConfigR :: Handler Html
getConfigR = do
  muser       <- maybeAuth
  permissions <- getPermissions <$> getMaybeGroup muser

  configVal <- entityVal . fromJust <$> runDB (selectFirst ([]::[Filter Config]) [])
  (formWidget, formEnctype) <- generateFormPost $ configForm configVal

  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ nameOfTheBoard <> titleDelimiter <> msgrender MsgConfig
    $(widgetFile "admin/config")

postConfigR :: Handler Html
postConfigR = do
  oldConfig        <- fromJust <$> runDB (selectFirst ([]::[Filter Config]) []  )
  ((result, _), _) <- runFormPost $ configForm (entityVal oldConfig)
  let oldConfigVal = entityVal oldConfig
      oldConfigKey = entityKey oldConfig
      msgRedirect msg = setMessageI msg >> redirect ConfigR
  case result of
    FormFailure []                     -> msgRedirect MsgBadFormData
    FormFailure xs                     -> msgRedirect $ MsgError $ T.intercalate "; " xs
    FormMissing                        -> msgRedirect MsgNoFormData
    FormSuccess (captchaLength, aCaptchaGuards, captchaTimeout, replyDelay     , threadDelay, boardCategories,
                 newsBoard    , showNews      , maxEditings   , showRecentPosts, displaySage, modlogMaxEntries,
                 modlogEntriesPerPage, about
                ) -> do
      let newConfig = Config { configCaptchaLength   = fromMaybe (configCaptchaLength   oldConfigVal) captchaLength
                             , configACaptchaGuards  = fromMaybe (configACaptchaGuards  oldConfigVal) aCaptchaGuards
                             , configCaptchaTimeout  = fromMaybe (configCaptchaTimeout  oldConfigVal) captchaTimeout
                             , configReplyDelay      = fromMaybe (configReplyDelay      oldConfigVal) replyDelay
                             , configThreadDelay     = fromMaybe (configThreadDelay     oldConfigVal) threadDelay
                             , configBoardCategories = maybe     [] (T.splitOn ",") boardCategories
                             , configNewsBoard       = fromMaybe (configNewsBoard       oldConfigVal) newsBoard
                             , configShowNews        = fromMaybe (configShowNews        oldConfigVal) showNews
                             , configMaxEditings     = fromMaybe (configMaxEditings     oldConfigVal) maxEditings
                             , configShowRecentPosts = fromMaybe (configShowRecentPosts oldConfigVal) showRecentPosts
                             , configDisplaySage     = fromMaybe (configDisplaySage     oldConfigVal) displaySage
                             , configModlogMaxEntries     = fromMaybe (configModlogMaxEntries     oldConfigVal) modlogMaxEntries
                             , configModlogEntriesPerPage = fromMaybe (configModlogEntriesPerPage oldConfigVal) modlogEntriesPerPage
                             , configAbout           = fromMaybe (configAbout oldConfigVal) about
                             }
      void $ runDB $ replace oldConfigKey newConfig
      addModlogEntry MsgModlogUpdateConfig
      msgRedirect MsgConfigUpdated
