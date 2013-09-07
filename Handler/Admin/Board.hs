{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin.Board where

import           Import
import           Yesod.Auth
import qualified Data.Text            as T
import           Handler.Delete       (deletePosts)
import           Control.Monad        (mplus)
import           Handler.Admin.Modlog (addModlogEntry)
-------------------------------------------------------------------------------------------------------------
getManageBoardsR :: ManageBoardAction -> Text -> Handler Html
getManageBoardsR action board = do
  muser    <- maybeAuth
  (permissions, group) <- pair getPermissions ((groupName . entityVal)<$>) <$> getMaybeGroup muser

  maybeBoard  <- runDB $ selectFirst [BoardName ==. board] []
  groups      <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  bCategories <- map (id &&& id) <$> getConfig configBoardCategories

  (formWidget, formEnctype) <- generateFormPost $ updateBoardForm maybeBoard action bCategories groups

  boards          <- runDB $ selectList ([]::[Filter Board]) []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, titleDelimiter, msgrender MsgBoardManagement]
    $(widgetFile "admin/boards")
-------------------------------------------------------------------------------------------------------------    
updateBoardForm :: Maybe (Entity Board) -> -- ^ Selected board
                  ManageBoardAction    -> -- ^ What you are going to do
                  [(Text,Text)]        -> -- ^ Board categories
                  [(Text,Text)]        -> -- ^ User groups
                  Html                 -> -- ^ Extra
                  MForm Handler (FormResult ( Maybe Text   -- ^ Name
                                            , Maybe Text   -- ^ Description
                                            , Maybe Int    -- ^ Bump limit
                                            , Maybe Int    -- ^ Number of files
                                            , Maybe Text   -- ^ Allowed file types
                                            , Maybe Text   -- ^ Default name
                                            , Maybe Int    -- ^ The maximum message length
                                            , Maybe Int    -- ^ Thumbnail size
                                            , Maybe Int    -- ^ Threads per page
                                            , Maybe Int    -- ^ Previews post per thread
                                            , Maybe Int    -- ^ Thread limit
                                            , Maybe Text   -- ^ OP file
                                            , Maybe Text   -- ^ Reply file
                                            , Maybe Text   -- ^ Is hidden (Enable,Disable,DoNotChange)
                                            , Maybe Text   -- ^ Enable captcha (Enable,Disable,DoNotChange)
                                            , Maybe Text   -- ^ Category
                                            , Maybe [Text] -- ^ View access
                                            , Maybe [Text] -- ^ Reply access
                                            , Maybe [Text] -- ^ Thread access
                                            , Maybe Text   -- ^ Allow OP moderate his/her thread
                                            , Maybe Text   -- ^ Extra rules
                                            , Maybe Text   -- ^ Enable geo IP
                                            , Maybe Text   -- ^ Enable OP editing
                                            , Maybe Text   -- ^ Enable post editing
                                            , Maybe Text   -- ^ Show or not editing history
                                            , Maybe Text   -- ^ Long description
                                            )
                                , Widget)
updateBoardForm board action bCategories groups extra = do
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
      onoffreq  = map (first msgrender) [(MsgEnable,"Enabled"),(MsgDisable,"Disabled"),(MsgRequired,"Required")]
      onoffreq  :: [(Text, Text)]
      -----------------------------------------------------------------------------
      helper'' :: forall a. (Board -> a) -> Maybe a
      helper'' g = (g . entityVal) <$> board
      
  (nameRes             , nameView             ) <- mopt textField     "" (helper boardName)
  (descriptionRes      , descriptionView      ) <- mopt textField     "" (helper boardDescription)
  (longDescriptionRes  , longDescriptionView  ) <- mopt textField     "" (helper boardLongDescription)
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
  (opFileRes           , opFileView           ) <- mopt (selectFieldList onoffreq) "" (helper boardOpFile)
  (replyFileRes        , replyFileView        ) <- mopt (selectFieldList onoffreq) "" (helper boardReplyFile)
  (isHiddenRes         , isHiddenView         ) <- mopt (selectFieldList onoff) "" (helper'  boardHidden)
  (enableCaptchaRes    , enableCaptchaView    ) <- mopt (selectFieldList onoff) "" (helper'  boardEnableCaptcha)
  (viewAccessRes       , viewAccessView       ) <- mopt (multiSelectFieldList groups) "" (helper'' boardViewAccess)
  (replyAccessRes      , replyAccessView      ) <- mopt (multiSelectFieldList groups) "" (helper'' boardReplyAccess)
  (threadAccessRes     , threadAccessView     ) <- mopt (multiSelectFieldList groups) "" (helper'' boardThreadAccess)
  (opModerationRes     , opModerationView     ) <- mopt (selectFieldList onoff) "" (helper'  boardOpModeration)
  (extraRulesRes       , extraRulesView       ) <- mopt textField     "" (helper (T.intercalate ";" . boardExtraRules))
  (enableGeoIpRes      , enableGeoIpView      ) <- mopt (selectFieldList onoff) "" (helper'  boardEnableGeoIp)
  (opEditingRes        , opEditingView        ) <- mopt (selectFieldList onoff) "" (helper'  boardOpEditing)
  (postEditingRes      , postEditingView      ) <- mopt (selectFieldList onoff) "" (helper'  boardPostEditing)
  (showEditHistoryRes  , showEditHistoryView  ) <- mopt (selectFieldList onoff) "" (helper'  boardShowEditHistory)
  let result = (,,,,,,,,,,,,,,,,,,,,,,,,,) <$>
               nameRes              <*> descriptionRes     <*> bumpLimitRes      <*>
               numberFilesRes       <*> allowedTypesRes    <*> defaultNameRes    <*>
               maxMsgLengthRes      <*> thumbSizeRes       <*> threadsPerPageRes <*>
               previewsPerThreadRes <*> threadLimitRes     <*> opFileRes         <*>
               replyFileRes         <*> isHiddenRes        <*> enableCaptchaRes  <*>
               categoryRes          <*> viewAccessRes      <*> replyAccessRes    <*>
               threadAccessRes      <*> opModerationRes    <*> extraRulesRes     <*>
               enableGeoIpRes       <*> opEditingRes       <*> postEditingRes    <*>
               showEditHistoryRes   <*> longDescriptionRes
      bname  = maybe Nothing (Just . boardName . entityVal) board
      widget = $(widgetFile "admin/boards-form")
  return (result, widget)
-------------------------------------------------------------------------------------------------------------
postNewBoardsR :: Handler Html
postNewBoardsR = do
  bCategories <- map (id &&& id) <$> getConfig configBoardCategories
  groups      <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  ((result, _), _) <- runFormPost $ updateBoardForm Nothing NewBoard bCategories groups
  let msgRedirect msg = setMessageI msg >> redirect (ManageBoardsR NewBoard "")
  case result of
    FormFailure [] -> msgRedirect MsgBadFormData
    FormFailure xs -> msgRedirect (MsgError $ T.intercalate "; " xs) 
    FormMissing    -> msgRedirect MsgNoFormData
    FormSuccess ( bName            , bDesc        , bBumpLimit   , bNumberFiles    , bAllowedTypes
                , bDefaultName     , bMaxMsgLen   , bThumbSize   , bThreadsPerPage , bPrevPerThread
                , bThreadLimit     , bOpFile      , bReplyFile   , bIsHidden       , bEnableCaptcha
                , bCategory        , bViewAccess  , bReplyAccess , bThreadAccess   , bOpModeration
                , bExtraRules      , bEnableGeoIp , bOpEditing   , bPostEditing    , bShowEditHistory
                , bLongDescription
                ) -> do
      when (any isNothing [bName, bDesc, bAllowedTypes, bDefaultName, bOpFile, bReplyFile] ||
            any isNothing [bThreadLimit, bBumpLimit, bNumberFiles, bMaxMsgLen, bThumbSize, bThreadsPerPage, bPrevPerThread]) $
           setMessageI MsgUpdateBoardsInvalidInput >> redirect (ManageBoardsR NewBoard "")
      let onoff (Just "Enable" ) = True
          onoff (Just "Disable") = False
          onoff _                = False
      let newBoard = Board { boardName              = fromJust bName
                           , boardDescription       = fromJust bDesc
                           , boardLongDescription   = fromMaybe "" bLongDescription
                           , boardBumpLimit         = fromJust bBumpLimit
                           , boardNumberFiles       = fromJust bNumberFiles
                           , boardAllowedTypes      = words $ unpack $ fromJust bAllowedTypes
                           , boardDefaultName       = fromJust bDefaultName
                           , boardMaxMsgLength      = fromJust bMaxMsgLen
                           , boardThumbSize         = fromJust bThumbSize
                           , boardThreadsPerPage    = fromJust bThreadsPerPage
                           , boardPreviewsPerThread = fromJust bPrevPerThread
                           , boardThreadLimit       = fromJust bThreadLimit
                           , boardOpFile            = fromJust bOpFile
                           , boardReplyFile         = fromJust bReplyFile
                           , boardHidden            = onoff bIsHidden
                           , boardEnableCaptcha     = onoff bEnableCaptcha
                           , boardCategory          = bCategory
                           , boardViewAccess        = bViewAccess
                           , boardReplyAccess       = bReplyAccess
                           , boardThreadAccess      = bThreadAccess
                           , boardOpModeration      = onoff bOpModeration
                           , boardExtraRules        = maybe [] (T.split (==';')) bExtraRules
                           , boardEnableGeoIp       = onoff bEnableGeoIp
                           , boardOpEditing         = onoff bOpEditing
                           , boardPostEditing       = onoff bPostEditing
                           , boardShowEditHistory   = onoff bShowEditHistory
                           }
      void $ runDB $ insert newBoard
      addModlogEntry $ MsgModlogNewBoard (fromJust bName)
      msgRedirect MsgBoardAdded

postAllBoardsR :: Handler Html
postAllBoardsR = do
  bCategories <- map (id &&& id) <$> getConfig configBoardCategories
  groups      <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  ((result, _), _) <- runFormPost $ updateBoardForm Nothing AllBoards bCategories groups
  let msgRedirect msg = setMessageI msg >> redirect (ManageBoardsR AllBoards "")
  case result of
    FormFailure [] -> msgRedirect MsgBadFormData
    FormFailure xs -> msgRedirect (MsgError $ T.intercalate "; " xs) 
    FormMissing    -> msgRedirect MsgNoFormData
    FormSuccess ( _                , bDesc        , bBumpLimit   , bNumberFiles    , bAllowedTypes
                , bDefaultName     , bMaxMsgLen   , bThumbSize   , bThreadsPerPage , bPrevPerThread
                , bThreadLimit     , bOpFile      , bReplyFile   , bIsHidden       , bEnableCaptcha
                , bCategory        , bViewAccess  , bReplyAccess , bThreadAccess   , bOpModeration
                , bExtraRules      , bEnableGeoIp , bOpEditing   , bPostEditing    , bShowEditHistory
                , bLongDescription
                ) -> do
      boards <- runDB $ selectList ([]::[Filter Board]) []
      forM_ boards $ (\(Entity oldBoardId oldBoard) ->
        let onoff (Just "Enable" ) _ = True
            onoff (Just "Disable") _ = False
            onoff _                f = f oldBoard
            newBoard = Board { boardName              = boardName oldBoard
                             , boardDescription       = fromMaybe (boardDescription       oldBoard) bDesc
                             , boardLongDescription   = fromMaybe (boardLongDescription   oldBoard) bLongDescription
                             , boardBumpLimit         = fromMaybe (boardBumpLimit         oldBoard) bBumpLimit
                             , boardNumberFiles       = fromMaybe (boardNumberFiles       oldBoard) bNumberFiles
                             , boardAllowedTypes      = maybe     (boardAllowedTypes      oldBoard) (words . unpack) bAllowedTypes
                             , boardDefaultName       = fromMaybe (boardDefaultName       oldBoard) bDefaultName
                             , boardMaxMsgLength      = fromMaybe (boardMaxMsgLength      oldBoard) bMaxMsgLen
                             , boardThumbSize         = fromMaybe (boardThumbSize         oldBoard) bThumbSize
                             , boardThreadsPerPage    = fromMaybe (boardThreadsPerPage    oldBoard) bThreadsPerPage
                             , boardPreviewsPerThread = fromMaybe (boardPreviewsPerThread oldBoard) bPrevPerThread
                             , boardThreadLimit       = fromMaybe (boardThreadLimit       oldBoard) bThreadLimit
                             , boardOpFile            = fromMaybe (boardOpFile            oldBoard) bOpFile
                             , boardReplyFile         = fromMaybe (boardReplyFile         oldBoard) bReplyFile
                             , boardHidden            = onoff bIsHidden boardHidden
                             , boardEnableCaptcha     = onoff bEnableCaptcha boardEnableCaptcha
                             , boardCategory          = mplus bCategory     (boardCategory     oldBoard)
                             , boardViewAccess        = mplus bViewAccess   (boardViewAccess   oldBoard)
                             , boardReplyAccess       = mplus bReplyAccess  (boardReplyAccess  oldBoard)
                             , boardThreadAccess      = mplus bThreadAccess (boardThreadAccess oldBoard)
                             , boardOpModeration      = onoff bOpModeration boardOpModeration
                             , boardExtraRules        = maybe (boardExtraRules oldBoard) (T.split (==';')) bExtraRules
                             , boardEnableGeoIp       = onoff bEnableGeoIp     boardEnableGeoIp
                             , boardOpEditing         = onoff bOpEditing       boardOpEditing
                             , boardPostEditing       = onoff bPostEditing     boardPostEditing
                             , boardShowEditHistory   = onoff bShowEditHistory boardShowEditHistory
                             }
          in runDB $ replace oldBoardId newBoard)
      addModlogEntry $ MsgModlogUpdateAllBoards
      msgRedirect MsgBoardsUpdated

postUpdateBoardsR :: Text -> Handler Html
postUpdateBoardsR board = do
  maybeBoard  <- runDB $ selectFirst [BoardName ==. board] []
  bCategories <- map (id &&& id) <$> getConfig configBoardCategories
  groups      <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  ((result, _), _) <- runFormPost $ updateBoardForm maybeBoard UpdateBoard bCategories groups
  let msgRedirect msg = setMessageI msg >> redirect (ManageBoardsR UpdateBoard board)
  case result of
    FormFailure [] -> msgRedirect MsgBadFormData
    FormFailure xs -> msgRedirect (MsgError $ T.intercalate "; " xs) 
    FormMissing    -> msgRedirect MsgNoFormData
    FormSuccess ( bName            , bDesc        , bBumpLimit   , bNumberFiles    , bAllowedTypes
                , bDefaultName     , bMaxMsgLen   , bThumbSize   , bThreadsPerPage , bPrevPerThread
                , bThreadLimit     , bOpFile      , bReplyFile   , bIsHidden       , bEnableCaptcha
                , bCategory        , bViewAccess  , bReplyAccess , bThreadAccess   , bOpModeration
                , bExtraRules      , bEnableGeoIp , bOpEditing   , bPostEditing    , bShowEditHistory
                , bLongDescription
                ) -> do
      let oldBoard   = entityVal $ fromJust maybeBoard
          oldBoardId = entityKey $ fromJust maybeBoard
          onoff (Just "Enable" ) _ = True
          onoff (Just "Disable") _ = False
          onoff _                f = f oldBoard
          newBoard = Board { boardName              = fromMaybe (boardName oldBoard) bName
                           , boardDescription       = fromMaybe (boardDescription       oldBoard) bDesc
                           , boardLongDescription   = fromMaybe (boardLongDescription   oldBoard) bLongDescription
                           , boardBumpLimit         = fromMaybe (boardBumpLimit         oldBoard) bBumpLimit
                           , boardNumberFiles       = fromMaybe (boardNumberFiles       oldBoard) bNumberFiles
                           , boardAllowedTypes      = maybe     (boardAllowedTypes      oldBoard) (words . unpack) bAllowedTypes
                           , boardDefaultName       = fromMaybe (boardDefaultName       oldBoard) bDefaultName
                           , boardMaxMsgLength      = fromMaybe (boardMaxMsgLength      oldBoard) bMaxMsgLen
                           , boardThumbSize         = fromMaybe (boardThumbSize         oldBoard) bThumbSize
                           , boardThreadsPerPage    = fromMaybe (boardThreadsPerPage    oldBoard) bThreadsPerPage
                           , boardPreviewsPerThread = fromMaybe (boardPreviewsPerThread oldBoard) bPrevPerThread
                           , boardThreadLimit       = fromMaybe (boardThreadLimit       oldBoard) bThreadLimit
                           , boardOpFile            = fromMaybe (boardOpFile            oldBoard) bOpFile
                           , boardReplyFile         = fromMaybe (boardReplyFile         oldBoard) bReplyFile
                           , boardHidden            = onoff bIsHidden boardHidden
                           , boardEnableCaptcha     = onoff bEnableCaptcha boardEnableCaptcha
                           , boardCategory          = mplus bCategory     Nothing
                           , boardViewAccess        = mplus bViewAccess   Nothing 
                           , boardReplyAccess       = mplus bReplyAccess  Nothing
                           , boardThreadAccess      = mplus bThreadAccess Nothing
                           , boardOpModeration      = onoff bOpModeration boardOpModeration
                           , boardExtraRules        = maybe (boardExtraRules oldBoard) (T.split (==';')) bExtraRules
                           , boardEnableGeoIp       = onoff bEnableGeoIp     boardEnableGeoIp
                           , boardOpEditing         = onoff bOpEditing       boardOpEditing
                           , boardPostEditing       = onoff bPostEditing     boardPostEditing
                           , boardShowEditHistory   = onoff bShowEditHistory boardShowEditHistory
                           }
      runDB $ replace oldBoardId newBoard
      addModlogEntry $ MsgModlogUpdateBoard (fromJust bName)
      msgRedirect MsgBoardsUpdated
-------------------------------------------------------------------------------------------------------------
cleanBoard :: ManageBoardAction -> Text -> Handler ()
cleanBoard action board = case action of
  AllBoards -> do
    boards  <- runDB $ selectList ([]::[Filter Board ]) []
    postIDs <- forM boards $ \(Entity _ b) -> runDB $ selectList [PostBoard ==. boardName b] []
    addModlogEntry $ MsgModlogCleanAllBoards
    void $ deletePosts (concat postIDs) False
  NewBoard -> msgRedirect MsgNoSuchBoard
  _        -> do
    maybeBoard <- runDB $ selectFirst [BoardName ==. board] []  
    when (isNothing maybeBoard) $ msgRedirect MsgNoSuchBoard
    postIDs <- runDB $ selectList [PostBoard ==. board] []
    addModlogEntry $ MsgModlogCleanBoard board
    void $ deletePosts postIDs False
  where msgRedirect msg = setMessageI msg >> redirect (ManageBoardsR UpdateBoard board)

getCleanBoardR :: ManageBoardAction -> Text -> Handler ()
getCleanBoardR action board = do
  cleanBoard action board
  setMessageI MsgBoardCleaned
  redirect (ManageBoardsR UpdateBoard board)

getDeleteBoardR :: ManageBoardAction -> Text -> Handler ()
getDeleteBoardR action board = do
  cleanBoard action board
  case action of
    AllBoards -> addModlogEntry MsgModlogDeleteAllBoards     >> runDB (deleteWhere ([]::[Filter Board ]))
    _         -> addModlogEntry (MsgModlogDeleteBoard board) >> runDB (deleteWhere [BoardName ==. board])
  setMessageI MsgBoardDeleted
  redirect (ManageBoardsR AllBoards "")
-------------------------------------------------------------------------------------------------------------
chooseManageBoarUrl :: ManageBoardAction -> Maybe Text -> Route App
chooseManageBoarUrl NewBoard    _     = NewBoardsR
chooseManageBoarUrl AllBoards   _     = AllBoardsR
chooseManageBoarUrl UpdateBoard bname = UpdateBoardsR $ fromJust bname
