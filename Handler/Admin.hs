{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin where

import           Import
import           Yesod.Auth
import qualified Data.Text         as T
import           Yesod.Auth.HashDB (setPassword)
import           Handler.Delete    (deletePosts)
import           Control.Monad     (mplus)
import           Control.Arrow     (first, (&&&))
-------------------------------------------------------------------------------------------------------------
getAdminR :: Handler Html
getAdminR = do
  muser           <- maybeAuth
  permissions     <- getPermissions <$> getMaybeGroup muser
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", msgrender MsgManagement]
    $(widgetFile "admin")

-------------------------------------------------------------------------------------------------------------
-- Thread options    
-------------------------------------------------------------------------------------------------------------
getStickR :: Text -> Int -> Handler Html
getStickR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> runDB (update pId [PostSticked =. not (postSticked p)]) >> redirectUltDest AdminR
    _                   -> setMessageI MsgNoSuchThread >> redirectUltDest AdminR
      
getLockR :: Text -> Int -> Handler Html
getLockR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> runDB (update pId [PostLocked =. not (postLocked p)]) >> redirectUltDest AdminR
    _                   -> setMessageI MsgNoSuchThread >> redirectUltDest AdminR
      
getAutoSageR :: Text -> Int -> Handler Html
getAutoSageR board thread = do
  maybePost <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybePost of
    Just (Entity pId p) -> runDB (update pId [PostAutosage =. not (postAutosage p)]) >> redirectUltDest AdminR
    _                   -> setMessageI MsgNoSuchThread >> redirectUltDest AdminR
-------------------------------------------------------------------------------------------------------------
---------------------------------- WHERE IS YESOD'S BUILTIN CRUD ??? ----------------------------------------
-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------
-- Bans    
-------------------------------------------------------------------------------------------------------------
banByIpForm :: Text -> Text -> Html -> MForm Handler (FormResult (Text, Text, Maybe Text, Maybe Int), Widget)
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
  muser       <- maybeAuth
  permissions <- getPermissions <$> getMaybeGroup muser

  (formWidget, formEnctype) <- generateFormPost $ banByIpForm ip board
  
  bans            <- runDB $ selectList ([]::[Filter Ban]) []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", msgrender MsgBanManagement]
    $(widgetFile "admin/ban")
  
postBanByIpR :: Text -> Text -> Handler Html
postBanByIpR _ _ = do
  ((result, _), _) <- runFormPost $ banByIpForm "" ""
  let msgRedirect msg = setMessageI msg >> redirect (BanByIpR "" "")
  case result of
    FormFailure _                            -> msgRedirect MsgBadFormData
    FormMissing                              -> msgRedirect MsgNoFormData
    FormSuccess (ip, reason, board, expires) -> do
      now <- liftIO getCurrentTime
      let newBan = Ban { banIp      = ip
                       , banReason  = reason
                       , banBoard   = board
                       , banExpires = (\n -> addUTCTime' (60*60*n) now) <$> expires
                       }
      void $ runDB $ insert newBan
      msgRedirect MsgBanAdded

getBanDeleteR :: Int -> Handler Html
getBanDeleteR bId = runDB (delete (toKey bId :: Key Ban)) >> setMessageI MsgBanDeleted >> redirect (BanByIpR "" "")
-------------------------------------------------------------------------------------------------------------
-- Boards management
-------------------------------------------------------------------------------------------------------------
getManageBoardsR :: Text -> Handler Html
getManageBoardsR board = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup

  maybeBoard  <- runDB $ selectFirst [BoardName ==. board] []
  groups      <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  bCategories <- map (id &&& id) <$> getConfig configBoardCategories

  (formWidget, formEnctype) <- generateFormPost $ updateBoardForm maybeBoard board bCategories groups

  boards          <- runDB $ selectList ([]::[Filter Board]) []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", msgrender MsgBoardManagement]
    $(widgetFile "admin/boards")
    
updateBoardForm :: Maybe (Entity Board) -> 
                  Text          -> -- board name
                  [(Text,Text)] -> -- board categoreis
                  [(Text,Text)] -> -- user groups
                  Html          -> -- extra
                  MForm Handler (FormResult ( Maybe Text -- name
                                            , Maybe Text -- description
                                            , Maybe Int  -- bump limit
                                            , Maybe Int  -- number of files
                                            , Maybe Text -- allowed file typs
                                            , Maybe Text -- default name
                                            , Maybe Int  -- max msg length
                                            , Maybe Int  -- thumb size
                                            , Maybe Int  -- threads per page
                                            , Maybe Int  -- previews per thread
                                            , Maybe Int  -- thread limit
                                            , Maybe Text -- allow OP without file (Enable,Disable,DoNotChange)
                                            , Maybe Text -- is hidden (Enable,Disable,DoNotChange)
                                            , Maybe Text -- enable captcha (Enable,Disable,DoNotChange)
                                            , Maybe Text -- category
                                            , Maybe Text -- view access
                                            , Maybe Text -- reply access
                                            , Maybe Text -- thread access
                                            , Maybe Text -- allow OP moderate his/her thread
                                            , Maybe Text -- extra rules
                                            )
                                , Widget)
updateBoardForm board bname' bCategories groups extra = do
  msgrender   <- getMessageRender
  let helper g  = (Just . g . entityVal) <$> board
      helper :: forall a. (Board -> a) -> Maybe (Maybe a)
      -----------------------------------------------------------------------------
      bool2Text True  = Just $ Just "Enable"
      bool2Text False = Just $ Just "Disable"
      bool2Text :: Bool -> Maybe (Maybe Text)
      -----------------------------------------------------------------------------
      helper' g = maybe Nothing (bool2Text . g . entityVal) board
      onoff     = map (first msgrender) [(MsgEnable,"Enable"),(MsgDisable,"Disable")]
      onoff     :: [(Text, Text)]
      -----------------------------------------------------------------------------
      helper'' :: forall a. (Board -> a) -> Maybe a
      helper'' g = (g . entityVal) <$> board
      
  (nameRes             , nameView             ) <- mopt textField     "" (helper boardName)
  (descriptionRes      , descriptionView      ) <- mopt textField     "" (helper boardDescription)
  (bumpLimitRes        , bumpLimitView        ) <- mopt intField      "" (helper boardBumpLimit)
  (numberFilesRes      , numberFilesView      ) <- mopt intField      "" (helper boardNumberFiles)
  (allowedTypesRes     , allowedTypesView     ) <- mopt textField     "" (helper (pack . unwords . boardAllowedTypes))
  (defaultNameRes      , defaultNameView      ) <- mopt textField     "" (helper boardDefaultName)
  (maxMsgLengthRes     , maxMsgLengthView     ) <- mopt intField      "" (helper boardMaxMsgLength)
  (thumbSizeRes        , thumbSizeView        ) <- mopt intField      "" (helper boardThumbSize)
  (threadsPerPageRes   , threadsPerPageView   ) <- mopt intField      "" (helper boardThreadsPerPage)
  (previewsPerThreadRes, previewsPerThreadView) <- mopt intField      "" (helper boardPreviewsPerThread)
  (threadLimitRes      , threadLimitView      ) <- mopt intField      "" (helper boardThreadLimit)
  (categoryRes         , categoryView         ) <- mopt (selectFieldList bCategories) "" (helper'' boardCategory)
  (opWithoutFileRes    , opWithoutFileView    ) <- mopt (selectFieldList onoff) "" (helper'  boardOpWithoutFile)
  (isHiddenRes         , isHiddenView         ) <- mopt (selectFieldList onoff) "" (helper'  boardHidden)
  (enableCaptchaRes    , enableCaptchaView    ) <- mopt (selectFieldList onoff) "" (helper'  boardEnableCaptcha)
  (viewAccessRes       , viewAccessView       ) <- mopt (selectFieldList groups) "" (helper'' boardViewAccess)
  (replyAccessRes      , replyAccessView      ) <- mopt (selectFieldList groups) "" (helper'' boardReplyAccess)
  (threadAccessRes     , threadAccessView     ) <- mopt (selectFieldList groups) "" (helper'' boardThreadAccess)
  (opModerationRes     , opModerationView     ) <- mopt (selectFieldList onoff) "" (helper'  boardOpModeration)
  (extraRulesRes       , extraRulesView       ) <- mopt textField     "" (helper (T.intercalate ";" . boardExtraRules))
  let result = (,,,,,,,,,,,,,,,,,,,) <$>
               nameRes              <*> descriptionRes   <*> bumpLimitRes      <*>
               numberFilesRes       <*> allowedTypesRes  <*> defaultNameRes    <*>
               maxMsgLengthRes      <*> thumbSizeRes     <*> threadsPerPageRes <*>
               previewsPerThreadRes <*> threadLimitRes   <*> opWithoutFileRes  <*>
               isHiddenRes          <*> enableCaptchaRes <*> categoryRes       <*>
               viewAccessRes        <*> replyAccessRes   <*> threadAccessRes   <*>
               opModerationRes      <*> extraRulesRes
      bname  = maybe bname' (boardName . entityVal) board
      widget = $(widgetFile "admin/boards-form")
  return (result, widget)

postManageBoardsR :: Text -> Handler Html
postManageBoardsR board = do
  maybeBoard  <- runDB $ selectFirst [BoardName ==. board] []
  bCategories <- map (id &&& id) <$> getConfig configBoardCategories
  groups      <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  ((result, _), _) <- runFormPost $ updateBoardForm maybeBoard board bCategories groups
  let msgRedirect msg = setMessageI msg >> redirect (ManageBoardsR board)
  case result of
    FormFailure _  -> msgRedirect MsgBadFormData
    FormMissing    -> msgRedirect MsgNoFormData
    FormSuccess ( bName        , bDesc          , bBumpLimit    , bNumberFiles    , bAllowedTypes
                , bDefaultName , bMaxMsgLen     , bThumbSize    , bThreadsPerPage , bPrevPerThread
                , bThreadLimit , bOpWithoutFile , bIsHidden     , bEnableCaptcha  , bCategory
                , bViewAccess  , bReplyAccess   , bThreadAccess , bOpModeration   , bExtraRules
                ) ->
      case board of
        "new-f89d7fb43ef7" -> do
          when (any isNothing [bName, bDesc, bAllowedTypes, bDefaultName] ||
                any isNothing [bThreadLimit, bBumpLimit, bNumberFiles, bMaxMsgLen, bThumbSize, bThreadsPerPage, bPrevPerThread]) $
            setMessageI MsgUpdateBoardsInvalidInput >> redirect (ManageBoardsR board)            
          let onoff (Just "Enable" ) = True
              onoff (Just "Disable") = False
              onoff _                = False
          let newBoard = Board { boardName              = fromJust bName
                               , boardDescription       = fromJust bDesc
                               , boardBumpLimit         = fromJust bBumpLimit
                               , boardNumberFiles       = fromJust bNumberFiles
                               , boardAllowedTypes      = words $ unpack $ fromJust bAllowedTypes
                               , boardDefaultName       = fromJust bDefaultName
                               , boardMaxMsgLength      = fromJust bMaxMsgLen
                               , boardThumbSize         = fromJust bThumbSize
                               , boardThreadsPerPage    = fromJust bThreadsPerPage
                               , boardPreviewsPerThread = fromJust bPrevPerThread
                               , boardThreadLimit       = fromJust bThreadLimit
                               , boardOpWithoutFile     = onoff bOpWithoutFile
                               , boardHidden            = onoff bIsHidden
                               , boardEnableCaptcha     = onoff bEnableCaptcha
                               , boardCategory          = bCategory
                               , boardViewAccess        = bViewAccess
                               , boardReplyAccess       = bReplyAccess
                               , boardThreadAccess      = bThreadAccess
                               , boardOpModeration      = onoff bOpModeration
                               , boardExtraRules        = maybe [] (T.split (==';')) bExtraRules
                               }
          void $ runDB $ insert newBoard
          msgRedirect MsgBoardAdded
        "all-f89d7fb43ef7" -> do -- update all boards
          boards <- runDB $ selectList ([]::[Filter Board]) []
          forM_ boards $ \(Entity oldBoardId oldBoard) ->
              let onoff (Just "Enable" ) = True
                  onoff (Just "Disable") = False
                  onoff _                = boardHidden oldBoard
                  newBoard = Board { boardName              = boardName oldBoard
                                   , boardDescription       = fromMaybe (boardDescription       oldBoard) bDesc
                                   , boardBumpLimit         = fromMaybe (boardBumpLimit         oldBoard) bBumpLimit
                                   , boardNumberFiles       = fromMaybe (boardNumberFiles       oldBoard) bNumberFiles
                                   , boardAllowedTypes      = maybe     (boardAllowedTypes      oldBoard) (words . unpack) bAllowedTypes
                                   , boardDefaultName       = fromMaybe (boardDefaultName       oldBoard) bDefaultName
                                   , boardMaxMsgLength      = fromMaybe (boardMaxMsgLength      oldBoard) bMaxMsgLen
                                   , boardThumbSize         = fromMaybe (boardThumbSize         oldBoard) bThumbSize
                                   , boardThreadsPerPage    = fromMaybe (boardThreadsPerPage    oldBoard) bThreadsPerPage
                                   , boardPreviewsPerThread = fromMaybe (boardPreviewsPerThread oldBoard) bPrevPerThread
                                   , boardThreadLimit       = fromMaybe (boardThreadLimit       oldBoard) bThreadLimit
                                   , boardOpWithoutFile     = onoff bOpWithoutFile
                                   , boardHidden            = onoff bIsHidden
                                   , boardEnableCaptcha     = onoff bEnableCaptcha
                                   , boardCategory          = mplus bCategory     (boardCategory     oldBoard)
                                   , boardViewAccess        = mplus bViewAccess   (boardViewAccess   oldBoard)
                                   , boardReplyAccess       = mplus bReplyAccess  (boardReplyAccess  oldBoard)
                                   , boardThreadAccess      = mplus bThreadAccess (boardThreadAccess oldBoard)
                                   , boardOpModeration      = onoff bOpModeration
                                   , boardExtraRules        = maybe (boardExtraRules oldBoard) (T.split (==';')) bExtraRules
                                   }
                in runDB $ replace oldBoardId newBoard
          msgRedirect MsgBoardsUpdated
        _     -> do -- update one board
          let oldBoard   = entityVal $ fromJust maybeBoard
              oldBoardId = entityKey $ fromJust maybeBoard
              onoff (Just "Enable" ) = True
              onoff (Just "Disable") = False
              onoff _                = boardHidden oldBoard
              newBoard = Board { boardName              = fromMaybe (boardName oldBoard) bName
                               , boardDescription       = fromMaybe (boardDescription       oldBoard) bDesc
                               , boardBumpLimit         = fromMaybe (boardBumpLimit         oldBoard) bBumpLimit
                               , boardNumberFiles       = fromMaybe (boardNumberFiles       oldBoard) bNumberFiles
                               , boardAllowedTypes      = maybe     (boardAllowedTypes      oldBoard) (words . unpack) bAllowedTypes
                               , boardDefaultName       = fromMaybe (boardDefaultName       oldBoard) bDefaultName
                               , boardMaxMsgLength      = fromMaybe (boardMaxMsgLength      oldBoard) bMaxMsgLen
                               , boardThumbSize         = fromMaybe (boardThumbSize         oldBoard) bThumbSize
                               , boardThreadsPerPage    = fromMaybe (boardThreadsPerPage    oldBoard) bThreadsPerPage
                               , boardPreviewsPerThread = fromMaybe (boardPreviewsPerThread oldBoard) bPrevPerThread
                               , boardThreadLimit       = fromMaybe (boardThreadLimit       oldBoard) bThreadLimit
                               , boardOpWithoutFile     = onoff bOpWithoutFile
                               , boardHidden            = onoff bIsHidden
                               , boardEnableCaptcha     = onoff bEnableCaptcha
                               , boardCategory          = mplus bCategory    Nothing
                               , boardViewAccess        = mplus bViewAccess  Nothing 
                               , boardReplyAccess       = mplus bReplyAccess Nothing
                               , boardThreadAccess      = bThreadAccess
                               , boardOpModeration      = onoff bOpModeration
                               , boardExtraRules        = maybe (boardExtraRules oldBoard) (T.split (==';')) bExtraRules
                               }
          runDB $ replace oldBoardId newBoard
          msgRedirect MsgBoardsUpdated

cleanBoard :: Text -> Handler ()
cleanBoard board = case board of
  "all-f89d7fb43ef7" -> do
    boards  <- runDB $ selectList ([]::[Filter Board ]) []
    postIDs <- forM boards $ \(Entity _ b) -> runDB $ selectList [PostBoard ==. boardName b] []
    void $ deletePosts (concat postIDs) False
  "new-f89d7fb43ef7" -> msgRedirect MsgNoSuchBoard
  _     -> do
    maybeBoard <- runDB $ selectFirst [BoardName ==. board] []  
    when (isNothing maybeBoard) $ msgRedirect MsgNoSuchBoard
    postIDs <- runDB $ selectList [PostBoard ==. board] []
    void $ deletePosts postIDs False
  where msgRedirect msg = setMessageI msg >> redirect (ManageBoardsR board)

getCleanBoardR :: Text -> Handler ()
getCleanBoardR board = do
  cleanBoard board
  setMessageI MsgBoardCleaned
  redirect (ManageBoardsR board)

getDeleteBoardR :: Text -> Handler ()
getDeleteBoardR board = do
  cleanBoard board
  case board of
    "all-f89d7fb43ef7" -> runDB $ deleteWhere ([]::[Filter Board ])
    _     -> runDB $ deleteWhere [BoardName ==. board]
  setMessageI MsgBoardDeleted
  redirect (ManageBoardsR "all-f89d7fb43ef7")
-------------------------------------------------------------------------------------------------------------
-- Groups
-------------------------------------------------------------------------------------------------------------
groupsForm :: Html -> MForm Handler (FormResult (Text,Bool,Bool,Bool,Bool,Bool,Bool,Bool), Widget)
groupsForm extra = do
  (nameRes         , nameView        ) <- mreq textField     "" Nothing
  (manageThreadRes , manageThreadView) <- mreq checkBoxField "" Nothing
  (manageBoardRes  , manageBoardView ) <- mreq checkBoxField "" Nothing
  (manageUsersRes  , manageUsersView ) <- mreq checkBoxField "" Nothing
  (manageConfigRes , manageConfigView) <- mreq checkBoxField "" Nothing
  (deletePostsRes  , deletePostsView ) <- mreq checkBoxField "" Nothing    
  (managePanelRes  , managePanelView ) <- mreq checkBoxField "" Nothing
  (manageBanRes    , manageBanView   ) <- mreq checkBoxField "" Nothing

  let result = (,,,,,,,)       <$> nameRes        <*>
               manageThreadRes <*> manageBoardRes <*> manageUsersRes <*>
               manageConfigRes <*> deletePostsRes <*> managePanelRes <*>
               manageBanRes   
      widget = $(widgetFile "admin/groups-form")
  return (result, widget)

showPermission :: Permission -> AppMessage
showPermission p = fromJust $ lookup p xs
  where xs = [(ManageThreadP , MsgManageThread)
             ,(ManageBoardP  , MsgManageBoard )
             ,(ManageUsersP  , MsgManageUsers )
             ,(ManageConfigP , MsgManageConfig)
             ,(DeletePostsP  , MsgDeletePosts )
             ,(ManagePanelP  , MsgManagePanel )
             ,(ManageBanP    , MsgManageBan   )
             ]

getManageGroupsR :: Handler Html
getManageGroupsR = do
  muser  <- maybeAuth
  mgroup   <- getMaybeGroup muser
  let permissions          = getPermissions mgroup

  groups <- map entityVal <$> runDB (selectList ([]::[Filter Group]) [])
  (formWidget, formEnctype) <- generateFormPost groupsForm

  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", msgrender MsgGroups]
    $(widgetFile "admin/groups")
  
postManageGroupsR :: Handler Html
postManageGroupsR = do
  ((result, _), _) <- runFormPost groupsForm 
  let msgRedirect msg = setMessageI msg >> redirect ManageGroupsR
  case result of
    FormFailure _                            -> msgRedirect MsgBadFormData
    FormMissing                              -> msgRedirect MsgNoFormData
    FormSuccess (name, manageThread, manageBoard, manageUsers, manageConfig, deletePostsP, managePanel, manageBan) -> do
      let permissions = [(ManageThreadP,manageThread), (ManageBoardP,manageBoard ), (ManageUsersP,manageUsers)
                        ,(ManageConfigP,manageConfig), (DeletePostsP,deletePostsP), (ManagePanelP,managePanel)
                        ,(ManageBanP   ,manageBan   )
                        ]
          newGroup = Group { groupName        = name
                           , groupPermissions = map fst $ filter snd permissions
                           }
      g <- runDB $ getBy $ GroupUniqName name
      if isJust g
        then void $ runDB $ replace (entityKey $ fromJust g) newGroup
        else void $ runDB $ insert newGroup
      msgRedirect MsgGroupAddedOrUpdated

getDeleteGroupsR :: Text -> Handler ()
getDeleteGroupsR group = do
  groups <- map (groupPermissions . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  when ((>1) $ length $ filter (ManageUsersP `elem`) groups) $ do
    void $ runDB $ deleteWhere [GroupName ==. group]
    setMessageI MsgGroupDeleted >> redirect ManageGroupsR
  setMessageI MsgYouAreTheOnlyWhoCanManageUsers >>  redirect ManageGroupsR
---------------------------
----------------------------------------------------------------------------------
-- Users
-------------------------------------------------------------------------------------------------------------
usersForm :: [(Text,Text)] -> Html -> MForm Handler (FormResult (Text, Text, Text), Widget)
usersForm groups extra = do
  (userNameRes     , userNameView     ) <- mreq textField                "" Nothing
  (userPasswordRes , userPasswordView ) <- mreq textField                "" Nothing
  (userGroupRes    , userGroupView    ) <- mreq (selectFieldList groups) "" Nothing
  let result = (,,) <$> userNameRes <*> userPasswordRes <*> userGroupRes
      widget = $(widgetFile "admin/users-form")
  return (result, widget)

getUsersR :: Handler Html
getUsersR = do
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let permissions = getPermissions mgroup

  groups <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  (formWidget, formEnctype) <- generateFormPost $ usersForm groups

  users           <- runDB $ selectList ([]::[Filter User ]) []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", msgrender MsgUsers]
    $(widgetFile "admin/users")

postUsersR :: Handler Html
postUsersR = do
  groups <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])  
  ((result, _), _) <- runFormPost $ usersForm groups
  let msgRedirect msg = setMessageI msg >> redirect UsersR
  case result of
    FormFailure _                      -> msgRedirect MsgBadFormData
    FormMissing                        -> msgRedirect MsgNoFormData
    FormSuccess (name, password, group) -> do
      let newUser = User { userName     = name
                         , userPassword = ""
                         , userSalt     = ""
                         , userGroup    = group
                         }
      userWithPassword <- liftIO $ setPassword password newUser
      u <- runDB $ getBy $ UserUniqName name
      if isJust u
        then void $ runDB $ replace (entityKey $ fromJust u) userWithPassword
        else void $ runDB $ insert userWithPassword
      msgRedirect MsgUsersAddedOrUpdated

getUsersDeleteR :: Int -> Handler Html
getUsersDeleteR userId = do
  let msgRedirect msg = setMessageI msg >> redirect UsersR
  groups <- map (groupPermissions . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
      
  when ((>1) $ length $ filter (ManageUsersP `elem`) groups) $ do
     runDB $ delete (toKey userId :: Key User)
     msgRedirect MsgUsersDeleted
  msgRedirect MsgYouAreTheOnlyWhoCanManageUsers
-------------------------------------------------------------------------------------------------------------
-- Account  
-------------------------------------------------------------------------------------------------------------
newPasswordForm :: Html -> MForm Handler (FormResult Text, Widget)
newPasswordForm extra = do
  (newPasswordRes , newPasswordView ) <- mreq textField "" Nothing
  let widget = toWidget [whamlet|
                             <form method=post action=@{NewPasswordR}>
                                 #{extra}
                                  <input type=submit value=_{MsgNewPassword}>
                                 ^{fvInput newPasswordView}
                        |]
  return (newPasswordRes, widget)

getAccountR :: Handler Html
getAccountR = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  let permissions = getPermissions mgroup

  (formWidget, formEnctype) <- generateFormPost newPasswordForm

  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", msgrender MsgAccount]
    $(widgetFile "admin/account")
                 
postNewPasswordR :: Handler Html
postNewPasswordR = do
  ((result, _), _) <- runFormPost newPasswordForm
  let msgRedirect msg = setMessageI msg >> redirect AccountR
  case result of
    FormFailure _           -> msgRedirect MsgBadFormData
    FormMissing             -> msgRedirect MsgNoFormData
    FormSuccess newPassword -> do
      eUser               <- fromJust <$> maybeAuth
      userWithNewPassword <- liftIO $ setPassword newPassword (entityVal eUser)
      void $ runDB $ replace (entityKey eUser) userWithNewPassword
      msgRedirect MsgPasswordChanged
      
-------------------------------------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------------------------------------
configForm :: Config -> Html -> MForm Handler (FormResult ( Maybe Int -- captcha length
                                                       , Maybe Int -- adaptive captcha guards
                                                       , Maybe Int -- captcha timeout
                                                       , Maybe Int -- reply delay
                                                       , Maybe Int -- new thread delay
                                                       , Maybe Text -- board categories
                                                       , Maybe Text -- news board
                                                       , Maybe Int  -- show news
                                                       )
                                           , Widget)
configForm config extra = do
  let f g = Just $ Just $ g config
      f :: forall a. (Config -> a) -> Maybe (Maybe a)
  (captchaLengthRes   , captchaLengthView  ) <- mopt intField  "" (f configCaptchaLength  )
  (acaptchaGuardsRes  , acaptchaGuardsView ) <- mopt intField  "" (f configACaptchaGuards )
  (captchaTimeoutRes  , captchaTimeoutView ) <- mopt intField  "" (f configCaptchaTimeout )
  (replyDelayRes      , replyDelayView     ) <- mopt intField  "" (f configReplyDelay     )
  (threadDelayRes     , threadDelayView    ) <- mopt intField  "" (f configThreadDelay    )
  (boardCategoriesRes , boardCategoriesView) <- mopt textField "" (Just $ Just $ T.intercalate "," $ configBoardCategories config)
  (newsBoardRes       , newsBoardView      ) <- mopt textField "" (f configNewsBoard      )
  (showNewsRes        , showNewsView       ) <- mopt intField  "" (f configShowNews       )
  let result = (,,,,,,,) <$>
               captchaLengthRes <*> acaptchaGuardsRes <*> captchaTimeoutRes  <*>
               replyDelayRes    <*> threadDelayRes    <*> boardCategoriesRes <*>
               newsBoardRes     <*> showNewsRes 
      widget = $(widgetFile "admin/config-form")
  return (result, widget)

getConfigR :: Handler Html
getConfigR = do
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let permissions = getPermissions mgroup

  configVal <- entityVal . fromJust <$> runDB (selectFirst ([]::[Filter Config]) [])

  (formWidget, formEnctype) <- generateFormPost $ configForm configVal

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
    FormFailure _                      -> msgRedirect MsgBadFormData
    FormMissing                        -> msgRedirect MsgNoFormData
    FormSuccess (captchaLength, aCaptchaGuards, captchaTimeout, replyDelay, threadDelay, boardCategories,
                 newsBoard    , showNews
                ) -> do
      let newConfig = Config { configCaptchaLength   = fromMaybe (configCaptchaLength   oldConfigVal) captchaLength
                             , configACaptchaGuards  = fromMaybe (configACaptchaGuards  oldConfigVal) aCaptchaGuards
                             , configCaptchaTimeout  = fromMaybe (configCaptchaTimeout  oldConfigVal) captchaTimeout
                             , configReplyDelay      = fromMaybe (configReplyDelay      oldConfigVal) replyDelay
                             , configThreadDelay     = fromMaybe (configThreadDelay     oldConfigVal) threadDelay
                             , configBoardCategories = maybe     (configBoardCategories oldConfigVal) (T.splitOn ",") boardCategories
                             , configNewsBoard       = fromMaybe (configNewsBoard       oldConfigVal) newsBoard
                             , configShowNews        = fromMaybe (configShowNews        oldConfigVal) showNews
                             }
      void $ runDB $ replace oldConfigKey newConfig
      msgRedirect MsgConfigUpdated
