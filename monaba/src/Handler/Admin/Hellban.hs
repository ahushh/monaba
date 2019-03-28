module Handler.Admin.Hellban where

import           Import
import qualified Data.Text as T
import           Handler.Admin.Modlog (addModlogEntry) 
import           Utils.YobaMarkup     (makeExternalRef)
-------------------------------------------------------------------------------------------------------------
getHellBanNoPageR :: Handler Html
getHellBanNoPageR = getHellBanR 0

getHellBanR :: Int -> Handler Html
getHellBanR page = do
  permissions <- ((fmap getPermissions) . getMaybeGroup) =<< maybeAuth
  group       <- (fmap $ userGroup . entityVal) <$> maybeAuth
  showPosts <- getConfig configShowLatestPosts
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  numberOfPosts <- runDB $ count [PostDeleted ==. False, PostHellbanned ==. True]
  let boards'     = mapMaybe (getIgnoredBoard' group) boards
      selectPosts = [PostBoard /<-. boards', PostDeleted ==. False, PostHellbanned ==. True]
      pages       = listPages showPosts numberOfPosts
  posts     <- runDB $ selectList selectPosts [Desc PostDate, LimitTo showPosts, OffsetBy $ page*showPosts]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  let postsAndFiles = zip posts postFiles
  -------------------------------------------------------------------------------------------------------------------
  AppSettings{..}  <- appSettings <$> getYesod
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleMsg MsgHellbanning
    $(widgetFile "admin/hellban")
------------------------------------------------------------------------------------------------------------
data HellbanFormAction = HellbanFormDo | HellbanFormUndo | HellbanFormDoNothing
     deriving (Show, Ord, Read, Eq, Bounded, Enum)

data HellbanFormVisibility = HellbanFormShow | HellbanFormShowAll | HellbanFormHide | HellbanFormHidelAll | HellbanFormVisibilityNothing
     deriving (Show, Ord, Read, Eq, Bounded, Enum)

postHellBanActionR :: Int -> Handler TypedContent
postHellBanActionR postId = do
  msgrender <- getMessageRender
  let postKey = (toSqlKey $ fromIntegral postId) :: Key Post
  post <- runDB $ get404 postKey
  let posterId = postPosterId post
      posterIp = postIp post
  ((result, _), _) <- runFormPost hellbanForm
  case result of
    FormFailure []                     -> selectRep $ provideJson $ object [("error", toJSON $ msgrender MsgBadFormData)]
    FormFailure xs                     -> selectRep $ provideJson $ object [("error", toJSON $ msgrender (MsgError $ T.intercalate "; " xs))]
    FormMissing                        -> selectRep $ provideJson $ object [("error", toJSON $ msgrender MsgNoFormData)]
    FormSuccess (ip, uid, action, visibility)   -> do
      case action of
        HellbanFormDo -> do
          when (uid || ip) $ do
            addModlogEntry $ MsgModlogHellban ( (if uid then posterId else "") <> " " <> (if ip then posterIp else ""))
            void $ runDB $ insert $ Hellban { hellbanUid = if uid then posterId else "", hellbanIp = if ip then posterIp else "" }
        HellbanFormUndo -> do
          when (uid || ip) $ 
            addModlogEntry $ MsgModlogUnhellban ((if uid then posterId else "") <> " " <> (if ip then posterIp else ""))
          when uid $
            runDB $ deleteWhere [HellbanUid ==. posterId]
          when ip $
            runDB $ deleteWhere [HellbanIp ==. posterIp]
        _ -> return ()
      case visibility of
        HellbanFormShow -> do
          runDB $ update postKey [PostHellbanned =. False]
          p <- makeExternalRef (postBoard post) (postLocalId post)
          addModlogEntry $ MsgModlogHellbanShowPost p
        HellbanFormShowAll -> do
          when uid $ do
            void $ runDB $ updateWhere [PostPosterId ==. posterId] [PostHellbanned =. False]
            addModlogEntry $ MsgModlogHellbanShowAllPosts posterId
          when ip $ do
            void $ runDB $ updateWhere [PostIp ==. posterIp] [PostHellbanned =. False]
            addModlogEntry $ MsgModlogHellbanShowAllPosts posterIp
        HellbanFormHide -> do
          void $ runDB $ update postKey [PostHellbanned =. True]
          p <- makeExternalRef (postBoard post) (postLocalId post)
          addModlogEntry $ MsgModlogHellbanHidePost p
        HellbanFormHidelAll -> do
          when uid $ do
            void $ runDB $ updateWhere [PostPosterId ==. posterId] [PostHellbanned =. True]
            addModlogEntry $ MsgModlogHellbanHideAllPosts posterId
          when ip $ do
            void $ runDB $ updateWhere [PostIp ==. posterIp] [PostHellbanned =. True]
            addModlogEntry $ MsgModlogHellbanHideAllPosts posterIp
        _ -> return ()
      selectRep $ provideJson $ object [("ok", toJSON $ msgrender MsgSuccessEx)]

hellbanForm :: Html -> -- ^ Extra token
              MForm Handler (FormResult ( Bool
                                        , Bool
                                        , HellbanFormAction
                                        , HellbanFormVisibility
                                        )
                         , Widget)
hellbanForm extra = do
  msgrender   <- getMessageRender
  AppSettings{..} <- appSettings <$> getYesod

  let hb :: [(Text, HellbanFormAction)]
      hb = [(msgrender MsgHellbanDo, HellbanFormDo), (msgrender MsgHellbanUndo, HellbanFormUndo), ("---",HellbanFormDoNothing)]
      display :: [(Text, HellbanFormVisibility)]
      display = [(msgrender MsgHellbanShowPost, HellbanFormShow), (msgrender MsgHellbanHidePost, HellbanFormHide), (msgrender MsgHellbanShowAllPosts, HellbanFormShowAll), (msgrender MsgHellbanHideAllPosts, HellbanFormHidelAll), ("---", HellbanFormVisibilityNothing)]
  (hbIPRes     , hbIPView    ) <- mreq checkBoxField             "" Nothing
  (hbUIDRes    , hbUIDView   ) <- mreq checkBoxField             "" Nothing
  (hbRes       , hbView      ) <- mreq (selectFieldList hb     ) "" Nothing
  (displayRes  , displayView ) <- mreq (selectFieldList display) "" Nothing
  let result = (,,,) <$>  hbIPRes <*> hbUIDRes <*> hbRes <*> displayRes
      widget = $(widgetFile "admin/hellban-form")
  return (result, widget)

getHellBanGetFormR :: Int -> Handler Html
getHellBanGetFormR postId = do
  let postKey = (toSqlKey $ fromIntegral postId) :: Key Post
  post <- runDB $ get404 postKey
  let posterId = postPosterId post
      posterIp = postIp post
  bannedUID <- runDB $ selectFirst [HellbanUid ==. posterId] []
  bannedIP  <- runDB $ selectFirst [HellbanIp ==. posterIp] []
  (form, enc) <- generateFormPost hellbanForm
  bareLayout [whamlet|<form id="hb-form-#{postId}" .hellban-form enctype=#{enc} action="@{HellBanActionR postId}" method=post>
                        ^{form}
                        $if isJust bannedUID || isJust bannedIP
                          _{MsgHellbanUserIsBanned}: 
                          $maybe _ <- bannedUID
                            UID 
                          $maybe _ <- bannedIP
                            IP
                     |]
