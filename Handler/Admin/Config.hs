{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin.Config where

import           Import
import           Yesod.Auth
import qualified Data.Text         as T

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
                                       )
                           , Widget)
configForm config extra = do
  let f g = Just $ Just $ g config
      f :: forall a. (Config -> a) -> Maybe (Maybe a)
  msgrender <- getMessageRender
  let showLatestPostsField = checkBool (>0) (msgrender $ MsgMustBeGreaterThan (msgrender MsgShowLatestPosts) 0) intField
  (captchaLengthRes   , captchaLengthView  ) <- mopt intField  "" (f configCaptchaLength  )
  (acaptchaGuardsRes  , acaptchaGuardsView ) <- mopt intField  "" (f configACaptchaGuards )
  (captchaTimeoutRes  , captchaTimeoutView ) <- mopt intField  "" (f configCaptchaTimeout )
  (replyDelayRes      , replyDelayView     ) <- mopt intField  "" (f configReplyDelay     )
  (threadDelayRes     , threadDelayView    ) <- mopt intField  "" (f configThreadDelay    )
  (boardCategoriesRes , boardCategoriesView) <- mopt textField "" (Just $ Just $ T.intercalate "," $ configBoardCategories config)
  (newsBoardRes       , newsBoardView      ) <- mopt textField "" (f configNewsBoard      )
  (showNewsRes        , showNewsView       ) <- mopt intField  "" (f configShowNews       )
  (maxEditingsRes     , maxEditingsView    ) <- mopt intField  "" (f configMaxEditings    )
  (showLatestPostsRes , showLatestPostsView) <- mopt showLatestPostsField "" (f configShowLatestPosts)

  let result = (,,,,,,,,,) <$>
               captchaLengthRes   <*> acaptchaGuardsRes <*> captchaTimeoutRes  <*>
               replyDelayRes      <*> threadDelayRes    <*> boardCategoriesRes <*>
               newsBoardRes       <*> showNewsRes       <*> maxEditingsRes     <*>
               showLatestPostsRes
      widget = $(widgetFile "admin/config-form")
  return (result, widget)
  
getConfigR :: Handler Html
getConfigR = do
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let permissions = getPermissions mgroup

  configVal <- entityVal . fromJust <$> runDB (selectFirst ([]::[Filter Config]) [])
  (formWidget, _) <- generateFormPost $ configForm configVal

  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", msgrender MsgConfig]
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
                 newsBoard    , showNews      , maxEditings   , showLatestPosts
                ) -> do
      let newConfig = Config { configCaptchaLength   = fromMaybe (configCaptchaLength   oldConfigVal) captchaLength
                             , configACaptchaGuards  = fromMaybe (configACaptchaGuards  oldConfigVal) aCaptchaGuards
                             , configCaptchaTimeout  = fromMaybe (configCaptchaTimeout  oldConfigVal) captchaTimeout
                             , configReplyDelay      = fromMaybe (configReplyDelay      oldConfigVal) replyDelay
                             , configThreadDelay     = fromMaybe (configThreadDelay     oldConfigVal) threadDelay
                             , configBoardCategories = maybe     (configBoardCategories oldConfigVal) (T.splitOn ",") boardCategories
                             , configNewsBoard       = fromMaybe (configNewsBoard       oldConfigVal) newsBoard
                             , configShowNews        = fromMaybe (configShowNews        oldConfigVal) showNews
                             , configMaxEditings     = fromMaybe (configMaxEditings     oldConfigVal) maxEditings
                             , configShowLatestPosts = fromMaybe (configShowLatestPosts oldConfigVal) showLatestPosts
                             }
      void $ runDB $ replace oldConfigKey newConfig
      msgRedirect MsgConfigUpdated
