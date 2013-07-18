{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin where

import           Import
import           Yesod.Auth
import qualified Data.Text         as T
import           Yesod.Auth.HashDB (setPassword)
import           Handler.Delete    (deletePosts)
import           Control.Monad     (mplus)
-------------------------------------------------------------------------------------------------------------
getAdminR :: Handler Html
getAdminR = do
  muser     <- maybeAuth
  boards    <- runDB $ selectList ([]::[Filter Board]) []
  posts     <- runDB $ selectList [] [Desc PostDate, LimitTo 10]
  postFiles <- forM posts $ \e -> runDB $ selectList [AttachedfileParentId ==. entityKey e] []
  nameOfTheBoard <- extraSiteName <$> getExtra
  boardCategories <- getConfig configBoardCategories
  let postsAndFiles = zip posts postFiles
  setUltDestCurrent
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", "Management"]
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
  (formWidget, formEnctype) <- generateFormPost $ banByIpForm ip board
  muser  <- maybeAuth
  boards <- runDB $ selectList ([]::[Filter Board]) []
  bans   <- runDB $ selectList ([]::[Filter Ban])   []
  nameOfTheBoard <- extraSiteName <$> getExtra
  boardCategories <- getConfig configBoardCategories
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", "Ban management"]
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
getManageBoardsR :: Handler Html
getManageBoardsR = do
  (formWidget, formEnctype) <- generateFormPost updateBoardForm
  muser  <- maybeAuth
  boards <- runDB $ selectList ([]::[Filter Board]) []
  boardCategories <- getConfig configBoardCategories
  nameOfTheBoard <- extraSiteName <$> getExtra
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", "Board management"]
    $(widgetFile "admin/boards")
    
updateBoardForm :: Html -> MForm Handler (FormResult ( Maybe Int  -- board id
                                                   , Maybe Text -- name
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
                                                   , Maybe Bool -- is hidden
                                                   , Maybe Bool -- enable captcha
                                                   , Maybe Text -- category
                                                   )
                                       , Widget)
updateBoardForm extra = do
  (boardIdRes          , boardIdView          ) <- mopt intField      "" Nothing
  (nameRes             , nameView             ) <- mopt textField     "" Nothing
  (descriptionRes      , descriptionView      ) <- mopt textField     "" Nothing
  (bumpLimitRes        , bumpLimitView        ) <- mopt intField      "" Nothing
  (numberFilesRes      , numberFilesView      ) <- mopt intField      "" Nothing
  (allowedTypesRes     , allowedTypesView     ) <- mopt textField     "" Nothing
  (defaultNameRes      , defaultNameView      ) <- mopt textField     "" Nothing
  (maxMsgLengthRes     , maxMsgLengthView     ) <- mopt intField      "" Nothing
  (thumbSizeRes        , thumbSizeView        ) <- mopt intField      "" Nothing
  (threadsPerPageRes   , threadsPerPageView   ) <- mopt intField      "" Nothing
  (previewsPerThreadRes, previewsPerThreadView) <- mopt intField      "" Nothing
  (threadLimitRes      , threadLimitView      ) <- mopt intField      "" Nothing
  (isHiddenRes         , isHiddenView         ) <- mopt checkBoxField "" Nothing
  (categoryRes         , categoryView         ) <- mopt textField     "" Nothing
  (enableCaptchaRes    , enableCaptchaView    ) <- mopt checkBoxField "" (Just $ Just True)
  let result = (,,,,,,,,,,,,,,) <$> boardIdRes <*> nameRes <*> descriptionRes <*>
               bumpLimitRes <*> numberFilesRes <*> allowedTypesRes <*>
               defaultNameRes <*> maxMsgLengthRes <*> thumbSizeRes <*>
               threadsPerPageRes <*> previewsPerThreadRes <*> threadLimitRes <*>
               isHiddenRes <*> enableCaptchaRes <*> categoryRes
      widget = $(widgetFile "admin/boards-form")
  return (result, widget)

postManageBoardsR :: Handler Html
postManageBoardsR = do
  ((result, _), _) <- runFormPost updateBoardForm
  let msgRedirect msg = setMessageI msg >> redirect ManageBoardsR
  case result of
    FormFailure _                            -> msgRedirect MsgBadFormData
    FormMissing                              -> msgRedirect MsgNoFormData
    FormSuccess (bId, bName, bDesc, bBumpLimit, bNumberFiles, bAllowedTypes,
                bDefaultName, bMaxMsgLen, bThumbSize, bThreadsPerPage,
                bPrevPerThread, bThreadLimit, bIsHidden,
                bEnableCaptcha, bCategory) ->
      case bId of
        Just i -> do -- update existing board
          maybeBoard <- runDB $ get $ toKey i
          when (isNothing maybeBoard) $ msgRedirect MsgWrongBoardId
          let oldBoard = fromJust maybeBoard
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
                               , boardThreadLimit       = mplus bThreadLimit Nothing
                               , boardHidden            = fromMaybe (boardHidden            oldBoard) bIsHidden
                               , boardEnableCaptcha     = fromMaybe (boardEnableCaptcha     oldBoard) bEnableCaptcha
                               , boardCategory          = mplus bCategory Nothing
                               }
          runDB $ replace (toKey i) newBoard
          msgRedirect MsgBoardUpdated
        _        -> do -- create a new board
          when (any isNothing [bName, bDesc, bAllowedTypes, bDefaultName] || -- where is threadlimit?..
                any isNothing [bBumpLimit, bNumberFiles, bMaxMsgLen, bThumbSize, bThreadsPerPage, bPrevPerThread]) $
            msgRedirect MsgUpdateBoardsInvalidInput
          let newBoard = Board { boardName              = fromJust bName
                               , boardDescription       = fromJust bDesc
                               , boardBumpLimit         = fromJust bBumpLimit
                               , boardNumberFiles       = fromJust bNumberFiles
                               , boardAllowedTypes      = words    $ unpack $ fromJust bAllowedTypes
                               , boardDefaultName       = fromJust bDefaultName
                               , boardMaxMsgLength      = fromJust bMaxMsgLen
                               , boardThumbSize         = fromJust bThumbSize
                               , boardThreadsPerPage    = fromJust bThreadsPerPage
                               , boardPreviewsPerThread = fromJust bPrevPerThread
                               , boardThreadLimit       = bThreadLimit
                               , boardHidden            = fromJust bIsHidden
                               , boardEnableCaptcha     = fromJust bEnableCaptcha
                               , boardCategory          = bCategory
                               }
          void $ runDB $ insert newBoard
          msgRedirect MsgBoardAdded

cleanBoard :: Int -> Handler ()
cleanBoard bId = do
  maybeBoard <- runDB $ get $ toKey bId
  when (isNothing maybeBoard) $ msgRedirect MsgNoSuchBoard
  let board = boardName $ fromJust maybeBoard
  postIDs <- runDB $ selectList [PostBoard ==. board] []
  void $ deletePosts postIDs
  where msgRedirect msg = setMessageI msg >> redirect ManageBoardsR

getCleanBoardR :: Int -> Handler ()
getCleanBoardR bId = do
  cleanBoard bId
  setMessageI MsgBoardCleaned
  redirect ManageBoardsR

getDeleteBoardR :: Int -> Handler ()
getDeleteBoardR bId = do
  cleanBoard bId
  runDB $ delete (toKey bId :: Key Board)
  setMessageI MsgBoardDeleted
  redirect ManageBoardsR
-------------------------------------------------------------------------------------------------------------
-- Staff  
-------------------------------------------------------------------------------------------------------------
staffForm :: Html -> MForm Handler (FormResult (Text, Text, RoleOfPerson), Widget)
staffForm extra = do
  (personNameRes     , personNameView     ) <- mreq textField               "" Nothing
  (personPasswordRes , personPasswordView ) <- mreq textField               "" Nothing
  (personRoleRes     , personRoleView     ) <- mreq (selectFieldList roles) "" Nothing
  let result = (,,) <$> personNameRes <*> personPasswordRes <*> personRoleRes
      widget = $(widgetFile "admin/staff-form")
  return (result, widget)
    where roles :: [(Text, RoleOfPerson)]
          roles = [("admin",Admin), ("mod",Moderator)]

getStaffR :: Handler Html
getStaffR = do
  muser     <- maybeAuth
  boards    <- runDB $ selectList ([]::[Filter Board ]) []
  persons   <- runDB $ selectList ([]::[Filter Person]) []
  (formWidget, formEnctype) <- generateFormPost staffForm
  nameOfTheBoard <- extraSiteName <$> getExtra
  boardCategories <- getConfig configBoardCategories
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", "Staff"]
    $(widgetFile "admin/staff")

postStaffR :: Handler Html
postStaffR = do
  ((result, _), _) <- runFormPost staffForm
  let msgRedirect msg = setMessageI msg >> redirect StaffR
  case result of
    FormFailure _                      -> msgRedirect MsgBadFormData
    FormMissing                        -> msgRedirect MsgNoFormData
    FormSuccess (name, password, role) -> do
      let newPerson = Person { personName     = name
                             , personPassword = ""
                             , personSalt     = ""
                             , personRole     = role
                             }
      personWithPassword <- liftIO $ setPassword password newPerson
      void $ runDB $ insert personWithPassword
      msgRedirect MsgStaffAdded

getStaffDeleteR :: Int -> Handler Html
getStaffDeleteR userId = do
  let msgRedirect msg = setMessageI msg >> redirect StaffR
  whenM ((>1) <$> runDB (count [PersonRole ==. Admin])) $ do
     runDB $ delete (toKey userId :: Key Person)
     msgRedirect MsgStaffDeleted
  msgRedirect MsgYouAreTheOnlyAdmin
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
  muser     <- maybeAuth
  boards    <- runDB $ selectList ([]::[Filter Board ]) []
  (formWidget, formEnctype) <- generateFormPost newPasswordForm
  nameOfTheBoard <- extraSiteName <$> getExtra
  boardCategories <- getConfig configBoardCategories
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", "Account"]
    $(widgetFile "admin/account")
                 
postNewPasswordR :: Handler Html
postNewPasswordR = do
  ((result, _), _) <- runFormPost newPasswordForm
  let msgRedirect msg = setMessageI msg >> redirect AccountR
  case result of
    FormFailure _           -> msgRedirect MsgBadFormData
    FormMissing             -> msgRedirect MsgNoFormData
    FormSuccess newPassword -> do
      muser <- maybeAuth
      personWithNewPassword <- liftIO $ setPassword newPassword $ entityVal $ fromJust muser
      void $ runDB $ replace (entityKey $ fromJust muser) personWithNewPassword
      msgRedirect MsgPasswordChanged
      
-------------------------------------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------------------------------------
configForm :: Html -> MForm Handler (FormResult ( Maybe Int -- captcha length
                                              , Maybe Int -- adaptive captcha guards
                                              , Maybe Int -- captcha timeout
                                              , Maybe Int -- reply delay
                                              , Maybe Int -- new thread delay
                                              , Maybe Text -- board categories
                                              )
                                    , Widget)
configForm extra = do
  (captchaLengthRes   , captchaLengthView  ) <- mopt intField  "" Nothing
  (acaptchaGuardsRes  , acaptchaGuardsView ) <- mopt intField  "" Nothing
  (captchaTimeoutRes  , captchaTimeoutView ) <- mopt intField  "" Nothing
  (replyDelayRes      , replyDelayView     ) <- mopt intField  "" Nothing
  (threadDelayRes     , threadDelayView    ) <- mopt intField  "" Nothing
  (boardCategoriesRes , boardCategoriesView) <- mopt textField "" Nothing
  let result = (,,,,,) <$> captchaLengthRes <*> acaptchaGuardsRes <*>
               captchaTimeoutRes <*> replyDelayRes <*> threadDelayRes <*> boardCategoriesRes
      widget = $(widgetFile "admin/config-form")
  return (result, widget)

getConfigR :: Handler Html
getConfigR = do
  muser   <- maybeAuth
  boards <- runDB $ selectList ([]::[Filter Board ]) []
  config <- runDB $ selectFirst ([]::[Filter Config]) []
  (formWidget, formEnctype) <- generateFormPost configForm
  nameOfTheBoard <- extraSiteName <$> getExtra
  boardCategories <- getConfig configBoardCategories
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", "Config"]
    $(widgetFile "admin/config")

postConfigR :: Handler Html
postConfigR = do
  ((result, _), _) <- runFormPost configForm
  let msgRedirect msg = setMessageI msg >> redirect ConfigR
  case result of
    FormFailure _                      -> msgRedirect MsgBadFormData
    FormMissing                        -> msgRedirect MsgNoFormData
    FormSuccess (captchaLength, aCaptchaGuards, captchaTimeout, replyDelay, threadDelay, boardCategories) -> do
      oldConfig <- fromJust <$> runDB (selectFirst ([]::[Filter Config]) [])
      let newConfig = Config { configCaptchaLength   = fromMaybe (configCaptchaLength   $ entityVal oldConfig) captchaLength
                             , configACaptchaGuards  = fromMaybe (configACaptchaGuards  $ entityVal oldConfig) aCaptchaGuards
                             , configCaptchaTimeout  = fromMaybe (configCaptchaTimeout  $ entityVal oldConfig) captchaTimeout
                             , configReplyDelay      = fromMaybe (configReplyDelay      $ entityVal oldConfig) replyDelay
                             , configThreadDelay     = fromMaybe (configThreadDelay     $ entityVal oldConfig) threadDelay
                             , configBoardCategories = maybe     (configBoardCategories $ entityVal oldConfig) (T.splitOn ",") boardCategories
                             }
      void $ runDB $ replace (entityKey oldConfig) newConfig
      msgRedirect MsgConfigUpdated
