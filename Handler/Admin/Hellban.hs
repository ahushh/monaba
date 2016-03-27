module Handler.Admin.Hellban where

import           Import
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
  let boards'     = mapMaybe (getIgnoredBoard group) boards
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
data HellbanFormAction = HellbanFormDo | HellbanFormUndo
     deriving (Show, Ord, Read, Eq, Bounded, Enum)

data HellbanFormVisibility = HellbanFormShow | HellbanFormShowAll | HellbanFormHide | HellbanFormHidelAll
     deriving (Show, Ord, Read, Eq, Bounded, Enum)

postHellBanActionR :: Int -> Handler Html
postHellBanActionR postId = do
  let postKey = (toSqlKey $ fromIntegral postId) :: Key Post
  post <- runDB $ get404 postKey
  let posterId = postPosterId post
  ((result, _), _) <- runFormPost hellbanForm
  case result of
    FormFailure []                     -> setMessageI MsgUnknownError >> redirectUltDest AdminR
    FormFailure xs                     -> setMessageI MsgUnknownError >> redirectUltDest AdminR
    FormMissing                        -> setMessageI MsgUnknownError >> redirectUltDest AdminR
    FormSuccess (action, visibility)   -> do
      case action of
        HellbanFormDo -> do
          addModlogEntry $ MsgModlogHellban posterId
          void $ runDB $ insert $ Hellban { hellbanUid = posterId, hellbanIp = postIp post }
        HellbanFormUndo -> do
          addModlogEntry (MsgModlogUnhellban posterId)
          runDB $ deleteWhere [HellbanUid ==. postPosterId post]
      case visibility of
        HellbanFormShow -> do
          runDB $ update postKey [PostHellbanned =. False]
          p <- makeExternalRef (postBoard post) (postLocalId post)
          addModlogEntry $ MsgModlogHellbanShowPost p
        HellbanFormShowAll -> undefined
        HellbanFormHide -> do
          void $ runDB $ update postKey [PostHellbanned =. True]
          p <- makeExternalRef (postBoard post) (postLocalId post)
          addModlogEntry $ MsgModlogHellbanHidePost p
        HellbanFormHidelAll -> undefined
      setMessageI MsgSuccessEx
      redirectUltDest AdminR

hellbanForm :: Html -> -- ^ Extra token
              MForm Handler (FormResult ( HellbanFormAction,
                                          HellbanFormVisibility
                                        )
                         , Widget)
hellbanForm extra = do
--  msgrender   <- getMessageRender
  AppSettings{..} <- appSettings <$> getYesod

  let hb :: [(Text, HellbanFormAction)]
      hb = [("Hellban", HellbanFormDo), ("Unhellban", HellbanFormUndo)]
      display :: [(Text, HellbanFormVisibility)]
      display = [("show post", HellbanFormShow), ("hide post", HellbanFormHide), ("show all posts", HellbanFormShowAll), ("hide all posts", HellbanFormHidelAll)]
  (hbRes       , hbView      ) <- mreq (selectFieldList hb     ) "" Nothing
  (displayRes  , displayView ) <- mreq (selectFieldList display) "" Nothing
  let result = (,) <$> hbRes <*> displayRes
      widget = $(widgetFile "admin/hellban-form")
  return (result, widget)

getHellBanGetFormR :: Int -> Handler Html
getHellBanGetFormR postId = do
  let postKey = (toSqlKey $ fromIntegral postId) :: Key Post
  post <- runDB $ get404 postKey
  let posterId = postPosterId post
  banned <- runDB $ selectFirst [HellbanUid ==. posterId] []
  (form, enc) <- generateFormPost hellbanForm
  bareLayout [whamlet|<form id="hb-form-#{postId}" .hellban-form enctype=#{enc} action="@{HellBanActionR postId}" method=post>
                        ^{form}
                        $maybe _ <- banned
                          user is banned
                     |]
