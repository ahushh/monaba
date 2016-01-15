module Handler.Admin.Config where

import           Import
import           Handler.Admin.Modlog (addModlogEntry) 
import qualified Data.Text as T (intercalate, splitOn)
-------------------------------------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------------------------------------
configForm :: Config ->
             Html   ->
             MForm Handler (FormResult ( Maybe Int  -- ^ Reply delay
                                       , Maybe Int  -- ^ New thread delay
                                       , Maybe Text -- ^ Board categories separated by comma
                                       , Maybe Text -- ^ Board for news
                                       , Maybe Int  -- ^ How many news show
                                       , Maybe Int  -- ^ The maximum number of post editings
                                       , Maybe Int  -- ^ How many latest posts show
                                       , Maybe Textarea -- ^ Html of home page
                                       , Maybe Textarea -- ^ Html of about page
                                       , Maybe Int  -- ^ Max modlog entries
                                       , Maybe Int  -- ^ Modlog entries per page
                                       )
                           , Widget)
configForm config extra = do
  let f g = Just $ Just $ g config
      f :: forall a. (Config -> a) -> Maybe (Maybe a)
  msgrender <- getMessageRender
  let showLatestPostsField = checkBool (>0) (msgrender $ MsgMustBeGreaterThan (msgrender MsgShowLatestPosts) 0) intField
  (replyDelayRes      , replyDelayView     ) <- mopt intField  "" (f configReplyDelay     )
  (threadDelayRes     , threadDelayView    ) <- mopt intField  "" (f configThreadDelay    )
  (boardCategoriesRes , boardCategoriesView) <- mopt textField "" (Just $ Just $ T.intercalate "," $ configBoardCategories config)
  (newsBoardRes       , newsBoardView      ) <- mopt textField "" (f configNewsBoard      )
  (showNewsRes        , showNewsView       ) <- mopt intField  "" (f configShowNews       )
  (maxEditingsRes     , maxEditingsView    ) <- mopt intField  "" (f configMaxEditings    )
  (showLatestPostsRes , showLatestPostsView) <- mopt showLatestPostsField "" (f configShowLatestPosts)
  (aboutRes           , aboutView)           <- mopt textareaField "" (f configAbout)
  (homeRes            , homeView )           <- mopt textareaField "" (f configHome)
  (modlogMaxEntriesRes    , modlogMaxEntriesView    ) <- mopt intField "" (f configModlogMaxEntries)
  (modlogEntriesPerPageRes, modlogEntriesPerPageView) <- mopt intField "" (f configModlogEntriesPerPage)

  let result = (,,,,,,,,,,) <$>
               replyDelayRes      <*> threadDelayRes    <*> boardCategoriesRes <*>
               newsBoardRes       <*> showNewsRes       <*> maxEditingsRes     <*>
               showLatestPostsRes <*> aboutRes          <*> homeRes            <*>
               modlogMaxEntriesRes <*> modlogEntriesPerPageRes
      widget = $(widgetFile "admin/config-form")
  return (result, widget)
  
getConfigR :: Handler Html
getConfigR = do
  configVal <- entityVal . fromJust <$> runDB (selectFirst ([]::[Filter Config]) [])
  (formWidget, _) <- generateFormPost $ configForm configVal
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleMsg MsgConfig
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
    FormSuccess (replyDelay , threadDelay, boardCategories,
                 newsBoard  , showNews   , maxEditings    , showLatestPosts, about, home,
                 modlogMaxEntries, modlogEntriesPerPage
                ) -> do
      let newConfig = Config { configReplyDelay      = fromMaybe (configReplyDelay      oldConfigVal) replyDelay
                             , configThreadDelay     = fromMaybe (configThreadDelay     oldConfigVal) threadDelay
                             , configBoardCategories = maybe     (configBoardCategories oldConfigVal) (T.splitOn ",") boardCategories
                             , configNewsBoard       = fromMaybe (configNewsBoard       oldConfigVal) newsBoard
                             , configShowNews        = fromMaybe (configShowNews        oldConfigVal) showNews
                             , configMaxEditings     = fromMaybe (configMaxEditings     oldConfigVal) maxEditings
                             , configShowLatestPosts = fromMaybe (configShowLatestPosts oldConfigVal) showLatestPosts
                             , configAbout           = fromMaybe (configAbout           oldConfigVal) about
                             , configHome            = fromMaybe (configHome            oldConfigVal) home
                             , configModlogMaxEntries     = fromMaybe (configModlogMaxEntries     oldConfigVal) modlogMaxEntries
                             , configModlogEntriesPerPage = fromMaybe (configModlogEntriesPerPage oldConfigVal) modlogEntriesPerPage
                             }
      void $ runDB $ replace oldConfigKey newConfig
      addModlogEntry MsgModlogUpdateConfig 
      msgRedirect MsgConfigUpdated
