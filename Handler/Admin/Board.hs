{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin.Board where

import           Import
import           Yesod.Auth
import qualified Data.Text         as T
import           Handler.Delete    (deletePosts)
import           Control.Monad     (mplus)
-------------------------------------------------------------------------------------------------------------

getManageBoardsR :: Text -> Handler Html
getManageBoardsR board = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  let permissions = getPermissions mgroup

  maybeBoard  <- runDB $ selectFirst [BoardName ==. board] []
  groups      <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  bCategories <- map (id &&& id) <$> getConfig configBoardCategories

  (formWidget, _) <- generateFormPost $ updateBoardForm maybeBoard board bCategories groups

  boards          <- runDB $ selectList ([]::[Filter Board]) []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [nameOfTheBoard, titleDelimiter, msgrender MsgBoardManagement]
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
                                            , Maybe Text -- OP file
                                            , Maybe Text -- reply file
                                            , Maybe Text -- is hidden (Enable,Disable,DoNotChange)
                                            , Maybe Text -- enable captcha (Enable,Disable,DoNotChange)
                                            , Maybe Text -- category
                                            , Maybe [Text] -- view access
                                            , Maybe [Text] -- reply access
                                            , Maybe [Text] -- thread access
                                            , Maybe Text -- allow OP moderate his/her thread
                                            , Maybe Text -- extra rules
                                            , Maybe Text -- enable geo IP
                                            , Maybe Text -- enable OP editing
                                            , Maybe Text -- enable post editing
                                            , Maybe Text -- show or not editing history
                                            , Maybe Text -- long description
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
      onoffreq  = map (first msgrender) [(MsgEnable,"Enabled"),(MsgDisable,"Disabled"),(MsgRequired,"Required")]
      onoffreq  :: [(Text, Text)]
      -----------------------------------------------------------------------------
      helper'' :: forall a. (Board -> a) -> Maybe a
      helper'' g = (g . entityVal) <$> board
      
  (nameRes             , nameView             ) <- mopt textField     "" (helper boardName)
  (titleRes            , titleView            ) <- mopt textField     "" (helper boardTitle)
  (summaryRes          , summaryView          ) <- mopt textField     "" (helper boardSummary)
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
               nameRes              <*> titleRes        <*> bumpLimitRes      <*>
               numberFilesRes       <*> allowedTypesRes <*> defaultNameRes    <*>
               maxMsgLengthRes      <*> thumbSizeRes    <*> threadsPerPageRes <*>
               previewsPerThreadRes <*> threadLimitRes  <*> opFileRes         <*>
               replyFileRes         <*> isHiddenRes     <*> enableCaptchaRes  <*>
               categoryRes          <*> viewAccessRes   <*> replyAccessRes    <*>
               threadAccessRes      <*> opModerationRes <*> extraRulesRes     <*>
               enableGeoIpRes       <*> opEditingRes    <*> postEditingRes    <*>
               showEditHistoryRes   <*> summaryRes
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
    FormFailure [] -> msgRedirect MsgBadFormData
    FormFailure xs -> msgRedirect (MsgError $ T.intercalate "; " xs) 
    FormMissing    -> msgRedirect MsgNoFormData
    FormSuccess ( bName        , bTitle       , bBumpLimit   , bNumberFiles    , bAllowedTypes
                , bDefaultName , bMaxMsgLen   , bThumbSize   , bThreadsPerPage , bPrevPerThread
                , bThreadLimit , bOpFile      , bReplyFile   , bIsHidden       , bEnableCaptcha
                , bCategory    , bViewAccess  , bReplyAccess , bThreadAccess   , bOpModeration
                , bExtraRules  , bEnableGeoIp , bOpEditing   , bPostEditing    , bShowEditHistory
                , bSummary
                ) ->
      case board of
        "new-f89d7fb43ef7" -> do
          when (any isNothing [bName, bTitle, bAllowedTypes, bDefaultName, bOpFile, bReplyFile] ||
                any isNothing [bThreadLimit, bBumpLimit, bNumberFiles, bMaxMsgLen, bThumbSize, bThreadsPerPage, bPrevPerThread]) $
            setMessageI MsgUpdateBoardsInvalidInput >> redirect (ManageBoardsR board)            
          let onoff (Just "Enable" ) = True
              onoff (Just "Disable") = False
              onoff _                = False
          let newBoard = Board { boardName              = fromJust bName
                               , boardTitle             = fromJust bTitle
                               , boardSummary           = fromMaybe "" bSummary
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
          msgRedirect MsgBoardAdded
        "all-f89d7fb43ef7" -> do -- update all boards
          boards <- runDB $ selectList ([]::[Filter Board]) []
          forM_ boards $ \(Entity oldBoardId oldBoard) ->
              let onoff (Just "Enable" ) = True
                  onoff (Just "Disable") = False
                  onoff _                = boardHidden oldBoard
                  newBoard = Board { boardName              = boardName oldBoard
                                   , boardTitle             = fromMaybe (boardTitle       oldBoard) bTitle
                                   , boardSummary           = fromMaybe (boardSummary   oldBoard) bSummary
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
                                   , boardHidden            = onoff bIsHidden
                                   , boardEnableCaptcha     = onoff bEnableCaptcha
                                   , boardCategory          = mplus bCategory     (boardCategory     oldBoard)
                                   , boardViewAccess        = mplus bViewAccess   (boardViewAccess   oldBoard)
                                   , boardReplyAccess       = mplus bReplyAccess  (boardReplyAccess  oldBoard)
                                   , boardThreadAccess      = mplus bThreadAccess (boardThreadAccess oldBoard)
                                   , boardOpModeration      = onoff bOpModeration
                                   , boardExtraRules        = maybe (boardExtraRules oldBoard) (T.split (==';')) bExtraRules
                                   , boardEnableGeoIp       = onoff bEnableGeoIp
                                   , boardOpEditing         = onoff bOpEditing
                                   , boardPostEditing       = onoff bPostEditing
                                   , boardShowEditHistory   = onoff bShowEditHistory
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
                               , boardTitle             = fromMaybe (boardTitle       oldBoard) bTitle
                               , boardSummary           = fromMaybe (boardSummary     oldBoard) bSummary
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
                               , boardHidden            = onoff bIsHidden
                               , boardEnableCaptcha     = onoff bEnableCaptcha
                               , boardCategory          = mplus bCategory     Nothing
                               , boardViewAccess        = mplus bViewAccess   Nothing 
                               , boardReplyAccess       = mplus bReplyAccess  Nothing
                               , boardThreadAccess      = mplus bThreadAccess Nothing
                               , boardOpModeration      = onoff bOpModeration
                               , boardExtraRules        = maybe (boardExtraRules oldBoard) (T.split (==';')) bExtraRules
                               , boardEnableGeoIp       = onoff bEnableGeoIp
                               , boardOpEditing         = onoff bOpEditing
                               , boardPostEditing       = onoff bPostEditing
                               , boardShowEditHistory   = onoff bShowEditHistory
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
